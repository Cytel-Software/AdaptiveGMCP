# --------------------------------------------------------------------------------------------------
#
# Equivalence tests between CER simulation outputs and the non-interactive CER API.
#
# --------------------------------------------------------------------------------------------------

# Build a CER design from one row of the simulation batch input.
BuildCerDesignFromSimulationRow <- function( input_row )
{
  evaluation_environment <- list2env(
    list(
      nArms = input_row$nArms,
      nEps = input_row$nEps
    ),
    parent = parent.frame()
  )
  eval_expression <- function( expression )
  {
    eval( parse( text = expression ), envir = evaluation_environment )
  }

  lEpType <- eval_expression( input_row$lEpType )
  Arms.std.dev <- eval_expression( input_row$Arms.std.dev )
  Arms.Prop <- eval_expression( input_row$Arms.Prop )
  Arms.alloc.ratio <- eval_expression( input_row$Arms.alloc.ratio )
  EP.Corr <- eval_expression( input_row$EP.Corr )
  WI <- eval_expression( input_row$WI )
  G <- eval_expression( input_row$G )
  info_frac <- eval_expression( input_row$info_frac )

  if (!is.list(Arms.std.dev)) {
    Arms.std.dev <- rep(list(NA_real_), length(lEpType))
    names(Arms.std.dev) <- names(lEpType)
  }
  if (!is.list(Arms.Prop)) {
    Arms.Prop <- rep(list(NA_real_), length(lEpType))
    names(Arms.Prop) <- names(lEpType)
  }

  prop.ctr <- lapply(seq_along(lEpType), function( endpoint_index ) {
    if (lEpType[[endpoint_index]] == "Binary") {
      Arms.Prop[[endpoint_index]][1]
    } else {
      NA_real_
    }
  })
  names(prop.ctr) <- names(lEpType)

  SetupDesign_CER(
    nArms = input_row$nArms,
    nEps = input_row$nEps,
    SampleSize = input_row$SampleSize,
    EpType = lEpType,
    sigma = Arms.std.dev,
    CommonStdDev = ifelse(is.na(input_row$CommonStdDev), FALSE, input_row$CommonStdDev),
    prop.ctr = prop.ctr,
    allocRatio = Arms.alloc.ratio,
    WI = WI,
    G = G,
    test.type = input_row$test.type,
    alpha = input_row$alpha,
    info_frac = info_frac,
    typeOfDesign = ifelse(is.na(input_row$typeOfDesign), "asOF", input_row$typeOfDesign),
    plotGraphs = FALSE
  )
}

# Extract one named p-value vector from the incremental simulation output.
BuildNamedIncrementalPValues <- function( p_values, hypotheses = NULL )
{
  if (is.null(hypotheses)) {
    hypotheses <- paste0("H", seq_along(p_values))
    selected_indices <- seq_along(p_values)
  } else {
    selected_indices <- as.integer(sub("^H", "", hypotheses))
  }
  stats::setNames(as.numeric(p_values[selected_indices]), hypotheses)
}

# Return the non-missing cumulative stage-2 arm sizes stored by the simulation.
BuildStage2SampleSize <- function( stage2_sample_size )
{
  arm_names <- colnames(stage2_sample_size)
  stage2_sample_size <- as.numeric(stage2_sample_size[2, ])
  names(stage2_sample_size) <- arm_names
  stage2_sample_size[!is.na(stage2_sample_size)]
}

RunCerSimulationEquivalence <- function(fixture_path, input_data)
{
  model_id <- as.integer(sub("^SimOutDump_([0-9]+)_1_.*$", "\\1", basename(fixture_path)))
  cat(paste0("Testing model ", model_id, "\n"))
  input_row <- input_data[input_data$ModelID == model_id, , drop = FALSE]
  testthat::expect_equal(nrow(input_row), 1L, info = basename(fixture_path))
  input_row <- input_row[1, , drop = FALSE]
  sim_out <- readRDS(fixture_path)

  testthat::expect_equal(input_row$FWERControl, "CombinationTest", info = basename(fixture_path))
  testthat::expect_true(is.list(sim_out$IncrRawPVals), info = basename(fixture_path))

  design <- BuildCerDesignFromSimulationRow(input_row)
  testthat::expect_equal(
    design$plan_boundary$Stage1Bdry,
    sim_out$PlannedStage1Bdry,
    tolerance = 1e-10,
    info = basename(fixture_path)
  )
  testthat::expect_equal(
    design$plan_boundary$Stage2Bdry,
    sim_out$PlannedStage2Bdry,
    tolerance = 1e-10,
    info = basename(fixture_path)
  )

  stage1_p_values <- BuildNamedIncrementalPValues(sim_out$IncrRawPVals$stage1)
  cer_state <- AnalyzeLook_CER(
    design = design,
    p_raw = stage1_p_values,
    look = 1L
  )

  testthat::expect_equal(
    cer_state$results$stage1$planned_boundary$Stage1Bdry,
    sim_out$PlannedStage1Bdry,
    tolerance = 1e-10,
    info = basename(fixture_path)
  )

  if (isTRUE(cer_state$trial_completed)) {
    return(invisible(NULL))
  }

  adapted <- isTRUE(sim_out$AdaptStage2)
  stage2_hypotheses <- if (adapted) {
    selected_index <- as.character(sim_out$SelectedIndex)
    if (length(selected_index) > 0 && all(!is.na(selected_index))) {
      selected_index
    } else {
      paste0("H", which(!is.na(sim_out$IncrRawPVals$stage2)))
    }
  } else {
    NULL
  }
  if (adapted && length(stage2_hypotheses) == 0) {
    return(invisible(NULL))
  }
  if (is.null(sim_out$IncrRawPVals$stage2) ||
      length(sim_out$IncrRawPVals$stage2) == 0) {
    return(invisible(NULL))
  }
  stage2_p_values <- BuildNamedIncrementalPValues(
    sim_out$IncrRawPVals$stage2,
    hypotheses = stage2_hypotheses
  )
  stage2_arguments <- list(
    design = design,
    state = cer_state,
    p_raw = stage2_p_values,
    look = 2L
  )
  if (adapted) {
    stage2_arguments$selection <- stage2_hypotheses
    stage2_arguments$new_sample_size <- BuildStage2SampleSize(
      sim_out$Stage2SampleSize
    )
  }
  cer_state <- do.call(AnalyzeLook_CER, stage2_arguments)

  expected_stage2_boundary <- if (adapted) {
    sim_out$AdjStage2Bdry
  } else {
    sim_out$PlannedStage2Bdry
  }
  testthat::expect_equal(
    cer_state$stage2_boundary,
    expected_stage2_boundary,
    tolerance = 1e-10,
    info = basename(fixture_path)
  )
  if (adapted) {
    testthat::expect_equal(
      cer_state$selected_hypotheses,
      stage2_hypotheses,
      info = basename(fixture_path)
    )
  }
  invisible(NULL)
}

fixture_dir <- testthat::test_path("sim-anal-equiv-fixtures")
fixture_paths <- list.files(
  fixture_dir,
  pattern = "^SimOutDump_[0-9]+_1_[0-9]{8}_[0-9]{6}\\.rds$",
  full.names = TRUE
)
testthat::skip_if(
  length(fixture_paths) == 0,
  "No CER simulation fixtures are available."
)
input_path <- testthat::test_path("sim-anal-equiv-fixtures", "Mixed-2OrMoreEPs.csv")
input_data <- utils::read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE)

# testthat::test_that("CER simulation equivalence for model 3", {
#   RunCerSimulationEquivalence(
#     file.path(fixture_dir, "SimOutDump_3_1_20260918_160548.rds"),
#     input_data
#   )
# })

# testthat::test_that("CER simulation equivalence for model 6", {
#   RunCerSimulationEquivalence(
#     file.path(fixture_dir, "SimOutDump_6_1_20260918_160549.rds"),
#     input_data
#   )
# })

# testthat::test_that("CER simulation equivalence for model 8", {
#   RunCerSimulationEquivalence(
#     file.path(fixture_dir, "SimOutDump_8_1_20260918_160549.rds"),
#     input_data
#   )
# })

# testthat::test_that("CER simulation equivalence for model 13", {
#   RunCerSimulationEquivalence(
#     file.path(fixture_dir, "SimOutDump_13_1_20260918_160549.rds"),
#     input_data
#   )
# })

# testthat::test_that("CER simulation equivalence for model 23", {
#   RunCerSimulationEquivalence(
#     file.path(fixture_dir, "SimOutDump_23_1_20260918_160733.rds"),
#     input_data
#   )
# })

# testthat::test_that("CER simulation equivalence for model 37", {
#   RunCerSimulationEquivalence(
#     file.path(fixture_dir, "SimOutDump_37_1_20260918_160830.rds"),
#     input_data
#   )
# })

testthat::test_that("CER simulation equivalence for model 39", {
  RunCerSimulationEquivalence(
    file.path(fixture_dir, "SimOutDump_39_1_20260918_160927.rds"),
    input_data
  )
})

# testthat::test_that("CER simulation equivalence for model 51", {
#   RunCerSimulationEquivalence(
#     file.path(fixture_dir, "SimOutDump_51_1_20260918_161248.rds"),
#     input_data
#   )
# })

# testthat::test_that("CER simulation equivalence for model 65", {
#   RunCerSimulationEquivalence(
#     file.path(fixture_dir, "SimOutDump_65_1_20260918_161716.rds"),
#     input_data
#   )
# })

# testthat::test_that("CER simulation equivalence for model 108", {
#   RunCerSimulationEquivalence(
#     file.path(fixture_dir, "SimOutDump_108_1_20260918_162136.rds"),
#     input_data
#   )
# })

# testthat::test_that("CER simulation equivalence for model 113", {
#   RunCerSimulationEquivalence(
#     file.path(fixture_dir, "SimOutDump_113_1_20260918_162609.rds"),
#     input_data
#   )
# })

# testthat::test_that("CER simulation equivalence for model 118", {
#   RunCerSimulationEquivalence(
#     file.path(fixture_dir, "SimOutDump_118_1_20260918_163033.rds"),
#     input_data
#   )
# })

# testthat::test_that("CER simulation equivalence for model 123", {
#   RunCerSimulationEquivalence(
#     file.path(fixture_dir, "SimOutDump_123_1_20260918_163515.rds"),
#     input_data
#   )
# })

# testthat::test_that("CER simulation equivalence for model 127", {
#   RunCerSimulationEquivalence(
#     file.path(fixture_dir, "SimOutDump_127_1_20260918_163927.rds"),
#     input_data
#   )
# })

# testthat::test_that("CER simulation equivalence for model 130", {
#   RunCerSimulationEquivalence(
#     file.path(fixture_dir, "SimOutDump_130_1_20260918_164343.rds"),
#     input_data
#   )
# })

# testthat::test_that("CER simulation equivalence for model 133", {
#   RunCerSimulationEquivalence(
#     file.path(fixture_dir, "SimOutDump_133_1_20260918_164814.rds"),
#     input_data
#   )
# })

# testthat::test_that("CER simulation equivalence for model 136", {
#   RunCerSimulationEquivalence(
#     file.path(fixture_dir, "SimOutDump_136_1_20260918_165233.rds"),
#     input_data
#   )
# })

# testthat::test_that("CER simulation equivalence for model 148", {
#   RunCerSimulationEquivalence(
#     file.path(fixture_dir, "SimOutDump_148_1_20260918_165705.rds"),
#     input_data
#   )
# })

# testthat::test_that("CER simulation equivalence for model 152", {
#   RunCerSimulationEquivalence(
#     file.path(fixture_dir, "SimOutDump_152_1_20260918_170111.rds"),
#     input_data
#   )
# })

# testthat::test_that("CER simulation equivalence for model 155", {
#   RunCerSimulationEquivalence(
#     file.path(fixture_dir, "SimOutDump_155_1_20260918_170438.rds"),
#     input_data
#   )
# })

# testthat::test_that("CER simulation equivalence for model 157", {
#   RunCerSimulationEquivalence(
#     file.path(fixture_dir, "SimOutDump_157_1_20260918_170847.rds"),
#     input_data
#   )
# })

# testthat::test_that("CER simulation equivalence for model 160", {
#   RunCerSimulationEquivalence(
#     file.path(fixture_dir, "SimOutDump_160_1_20260918_171319.rds"),
#     input_data
#   )
# })

# testthat::test_that("CER simulation equivalence for model 163", {
#   RunCerSimulationEquivalence(
#     file.path(fixture_dir, "SimOutDump_163_1_20260918_171733.rds"),
#     input_data
#   )
# })
