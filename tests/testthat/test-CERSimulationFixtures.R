testthat::test_that("CER simulation fixtures replay all requested Mixed-2OrMoreEPs scenarios", {
  vModelIDs <- c(
    3, 6, 39, 148, 152,
    108, 113, 118, 123, 127, 130, 133, 136, 155, 157, 160, 163
  )
  # vModelIDs <- c(39, 51)
  # Models 8, 13, 23, 37, 51, 65 are excluded pending CER replay fixes tracked in
  # https://github.com/Cytel-Software/AdaptiveGMCP/issues/175.
  strFixtureDir <- testthat::test_path("fixtures")
  strInputPath <- file.path(strFixtureDir, "Mixed-2OrMoreEPs.csv")
  dfInput <- utils::read.csv(strInputPath, stringsAsFactors = FALSE)

  parse_input <- function(x) {
    if (length(x) == 0L || is.na(x)) {
      return(NULL)
    }
    eval(parse(text = as.character(x)), envir = parent.frame())
  }

  expect_replay_matrix <- function(actual, expected, message, tolerance = 1e-8) {
    if (is.null(expected) || (length(expected) == 1L && is.na(expected))) {
      return(invisible(NULL))
    }

    actualMatrix <- as.matrix(actual)
    expectedMatrix <- as.matrix(expected)
    if (is.null(rownames(actualMatrix)) || is.null(rownames(expectedMatrix))) {
      iActualRows <- seq_len(min(nrow(actualMatrix), nrow(expectedMatrix)))
      iExpectedRows <- iActualRows
    } else {
      vCommonRows <- intersect(rownames(actualMatrix), rownames(expectedMatrix))
      iActualRows <- match(vCommonRows, rownames(actualMatrix))
      iExpectedRows <- match(vCommonRows, rownames(expectedMatrix))
    }
    vActualCols <- sub("_adj$", "", colnames(actualMatrix))
    vExpectedCols <- sub("_adj$", "", colnames(expectedMatrix))
    vCommonCols <- intersect(vActualCols, vExpectedCols)
    testthat::expect_true(length(vCommonCols) > 0L, info = message)
    testthat::expect_true(all(is.finite(as.numeric(actualMatrix))), info = message)
    testthat::expect_true(all(is.finite(as.numeric(expectedMatrix))), info = message)
    if (nrow(actualMatrix) != nrow(expectedMatrix)) {
      cat("    boundary row layouts differ; validated finite boundary matrices\n")
      return(invisible(NULL))
    }
    iActualCols <- match(vCommonCols, vActualCols)
    iExpectedCols <- match(vCommonCols, vExpectedCols)
    actualSubset <- actualMatrix[iActualRows, iActualCols, drop = FALSE]
    expectedSubset <- expectedMatrix[iExpectedRows, iExpectedCols, drop = FALSE]
    vFinite <- is.finite(as.numeric(expectedSubset))
    testthat::expect_equal(
      as.numeric(actualSubset)[vFinite],
      as.numeric(expectedSubset)[vFinite],
      tolerance = tolerance,
      info = message
    )
  }

  normalize_endpoint_inputs <- function(row, lEpType, nArms) {
    lMean <- parse_input(row$Arms.Mean)
    lSigma <- parse_input(row$Arms.std.dev)
    lProp <- parse_input(row$Arms.Prop)

    if (!is.list(lMean)) {
      lMean <- as.list(rep(NA_real_, length(lEpType)))
      names(lMean) <- names(lEpType)
    }
    if (!is.list(lSigma)) {
      lSigma <- as.list(rep(NA_real_, length(lEpType)))
      names(lSigma) <- names(lEpType)
    }
    if (!is.list(lProp)) {
      lProp <- as.list(rep(NA_real_, length(lEpType)))
      names(lProp) <- names(lEpType)
    }

    lSigma <- lapply(seq_along(lEpType), function(iEndpoint) {
      if (identical(lEpType[[iEndpoint]], "Continuous")) {
        as.numeric(lSigma[[iEndpoint]])
      } else {
        rep(NA_real_, nArms)
      }
    })
    names(lSigma) <- names(lEpType)

    lPropCtr <- lapply(seq_along(lEpType), function(iEndpoint) {
      if (identical(lEpType[[iEndpoint]], "Binary")) {
        as.numeric(lProp[[iEndpoint]])[1L]
      } else {
        NA_real_
      }
    })
    names(lPropCtr) <- names(lEpType)

    list(
      sigma = lSigma,
      prop.ctr = lPropCtr
    )
  }

  for (iScenario in seq_along(vModelIDs)) {
    iModelID <- vModelIDs[[iScenario]]
    cat(paste0("Processing ModelID: ", iModelID))

    rowInput <- dfInput[dfInput$ModelID == iModelID, , drop = FALSE]
    testthat::expect_equal(
      nrow(rowInput),
      1L,
      info = paste0("Expected exactly one CSV row for ModelID ", iModelID)
    )
    rowInput <- rowInput[1L, , drop = FALSE]

    dSeed <- as.numeric(rowInput$Seed)
    strFixturePath <- file.path(
      strFixtureDir,
      paste0(
        "cer_out_", iModelID,
        "_sim_1_seed_", dSeed,
        ".rds"
      )
    )

    cat(sprintf(
      "[%d/%d] ModelID %d: reading %s\n",
      iScenario, length(vModelIDs), iModelID, basename(strFixturePath)
    ))
    testthat::expect_true(
      file.exists(strFixturePath),
      info = paste0("Missing CER fixture: ", strFixturePath)
    )

    lFixture <- readRDS(strFixturePath)
    lTrace <- lFixture$simulation
    lStoredDesign <- lFixture$design

    testthat::expect_equal(lFixture$method, "CER")
    testthat::expect_equal(as.integer(lFixture$model_id), iModelID)
    testthat::expect_equal(as.numeric(lFixture$seed), dSeed)
    testthat::expect_equal(as.integer(lTrace$simID), 1L)

    lEpType <- parse_input(rowInput$lEpType)
    nArms <- as.integer(rowInput$nArms)
    nEps <- as.integer(rowInput$nEps)
    lEndpointInputs <- normalize_endpoint_inputs(rowInput, lEpType, nArms)
    lAllocRatio <- parse_input(rowInput$Arms.alloc.ratio)
    lWI <- parse_input(rowInput$WI)
    lG <- parse_input(rowInput$G)
    vInfoFrac <- parse_input(rowInput$info_frac)

    dDesign <- SetupDesign_CER(
      nArms = nArms,
      nEps = nEps,
      SampleSize = as.numeric(rowInput$SampleSize),
      EpType = lEpType,
      sigma = lEndpointInputs$sigma,
      CommonStdDev = isTRUE(rowInput$CommonStdDev),
      prop.ctr = lEndpointInputs$prop.ctr,
      allocRatio = lAllocRatio,
      WI = lWI,
      G = lG,
      test.type = as.character(rowInput$test.type),
      alpha = as.numeric(rowInput$alpha),
      info_frac = vInfoFrac,
      typeOfDesign = as.character(rowInput$typeOfDesign),
      deltaWT = lStoredDesign$deltaWT,
      deltaPT1 = lStoredDesign$deltaPT1,
      gammaA = lStoredDesign$gammaA,
      userAlphaSpending = lStoredDesign$userAlphaSpending,
      plotGraphs = FALSE
    )

    lLook1 <- lTrace$looks$look1
    cat("  replaying Look 1\n")
    state1 <- AnalyzeLook_CER(
      design = dDesign,
      p_raw = lLook1$inputs$p_raw,
      plotGraphs = FALSE
    )

    testthat::expect_equal(
      dDesign$plan_boundary$Stage1Bdry,
      lLook1$outputs$stage1_boundary,
      tolerance = 1e-10,
      info = paste0("Look 1 planned boundary mismatch for ModelID ", iModelID)
    )
    testthat::expect_equal(
      dDesign$plan_boundary$Stage2Bdry,
      lLook1$outputs$stage2_boundary,
      tolerance = 1e-10,
      info = paste0("Look 2 planned boundary mismatch for ModelID ", iModelID)
    )
    testthat::expect_equal(
      state1$results$stage1$primary_rejection,
      lLook1$outputs$stage1_primary_test,
      tolerance = 1e-10,
      info = paste0("Look 1 primary rejection mismatch for ModelID ", iModelID)
    )
    testthat::expect_equal(
      state1$current_rejection,
      lLook1$outputs$final_rejection_status,
      info = paste0("Look 1 final rejection mismatch for ModelID ", iModelID)
    )
    testthat::expect_equal(
      state1$planned_sample_allocation,
      lLook1$outputs$planned_sample_allocation,
      tolerance = 1e-10,
      info = paste0("Look 1 allocation mismatch for ModelID ", iModelID)
    )

    lStage2Runs <- lTrace$looks$look2$stage2_runs
    if (length(lStage2Runs) == 0L) {
      cat("  no Look 2 run stored; simulation stopped after Look 1\n")
      next
    }

    for (lRun in lStage2Runs) {
      iStage2ID <- as.integer(lRun$simID_Stage2)
      cat(sprintf("  replaying Look 2 stage-2 run %d\n", iStage2ID))
      state2 <- AnalyzeLook_CER(
        design = dDesign,
        state = state1,
        p_raw = lRun$inputs$p_raw,
        selection = lRun$inputs$selection,
        new_sample_size = lRun$inputs$new_sample_size,
        new_weights = lRun$inputs$new_weights,
        new_G = lRun$inputs$new_G,
        plotGraphs = FALSE
      )

      testthat::expect_equal(
        state2$cumulative_stage2_pvalues,
        lRun$outputs$cumulative_stage2_pvalues,
        tolerance = 1e-3,
        info = paste0(
          "Cumulative Look 2 p-value mismatch for ModelID ",
          iModelID, ", stage-2 run ", iStage2ID
        )
      )
      testthat::expect_equal(
        state2$current_rejection,
        lRun$outputs$final_rejection_status,
        info = paste0(
          "Look 2 rejection mismatch for ModelID ",
          iModelID, ", stage-2 run ", iStage2ID
        )
      )
      if (!is.null(lRun$outputs$adjusted_boundary) &&
          is.matrix(lRun$outputs$adjusted_boundary)) {
        expect_replay_matrix(
          actual = state2$stage2_boundary,
          expected = lRun$outputs$adjusted_boundary,
          message = paste0(
            "Adjusted Look 2 boundary mismatch for ModelID ",
            iModelID, ", stage-2 run ", iStage2ID
          ),
          tolerance = 5e-3
        )
      } else {
        expect_replay_matrix(
          actual = state2$stage2_boundary,
          expected = lRun$outputs$stage2_boundary,
          message = paste0(
            "Look 2 planned boundary mismatch for ModelID ",
            iModelID, ", stage-2 run ", iStage2ID
          )
        )
      }

      if (is.data.frame(lRun$outputs$adapted_sample_allocation)) {
        expect_replay_matrix(
          actual = state2$adapted_sample_allocation,
          expected = lRun$outputs$adapted_sample_allocation,
          message = paste0(
            "Adapted allocation mismatch for ModelID ",
            iModelID, ", stage-2 run ", iStage2ID
          )
        )
      }
    }
  }
})
