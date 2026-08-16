testthat::test_that("SetupAnalysis_PC validates info_frac_tolerance bounds", {
  vWi <- c(0.5, 0.5)
  mG <- matrix(c(0, 1, 1, 0), nrow = 2, byrow = TRUE)

  testthat::expect_error(
    SetupAnalysis_PC(
      WI = vWi,
      G = mG,
      test.type = "Sidak",
      info_frac = c(0.5, 1.0),
      info_frac_tolerance = 0,
      plotGraphs = FALSE
    ),
    regexp = "strictly > 0 and < 1"
  )

  testthat::expect_error(
    SetupAnalysis_PC(
      WI = vWi,
      G = mG,
      test.type = "Sidak",
      info_frac = c(0.5, 1.0),
      info_frac_tolerance = 1,
      plotGraphs = FALSE
    ),
    regexp = "strictly > 0 and < 1"
  )
})

testthat::test_that("AnalyzeLook_PC treats info_frac_cur == 1 - tolerance as final", {
  vWi <- c(0.5, 0.5)
  mG <- matrix(c(0, 1, 1, 0), nrow = 2, byrow = TRUE)

  state <- SetupAnalysis_PC(
    WI = vWi,
    G = mG,
    test.type = "Sidak",
    alpha = 0.025,
    info_frac = c(0.5, 1.0),
    info_frac_tolerance = 0.05,
    plotGraphs = FALSE
  )

  state <- AnalyzeLook_PC(
    state = state,
    p_raw = c(H1 = 0.20, H2 = 0.25),
    info_frac_cur = 0.95,
    plotGraphs = FALSE
  )

  testthat::expect_true(state$trial_completed)
  testthat::expect_identical(state$completion_reason, "final_look")
  testthat::expect_equal(state$look_history[[1]]$info_frac_cur, 0.95)
})

testthat::test_that("AnalyzeLook_PC final IF > 1 uses standard rpact cutoff with capped IF", {
  vWi <- c(0.5, 0.5)
  mG <- matrix(c(0, 1, 1, 0), nrow = 2, byrow = TRUE)

  dAlpha <- 0.025
  vInfoFrac <- c(0.5, 1.0)

  state <- SetupAnalysis_PC(
    WI = vWi,
    G = mG,
    test.type = "Sidak",
    alpha = dAlpha,
    info_frac = vInfoFrac,
    info_frac_tolerance = 0.05,
    typeOfDesign = "asOF",
    plotGraphs = FALSE
  )

  state <- AnalyzeLook_PC(
    state = state,
    p_raw = c(H1 = 0.30, H2 = 0.35),
    info_frac_cur = 0.50,
    plotGraphs = FALSE
  )

  state <- AnalyzeLook_PC(
    state = state,
    p_raw = c(H1 = 0.20, H2 = 0.25),
    info_frac_cur = 1.02,
    plotGraphs = FALSE
  )

  desBase <- rpact::getDesignGroupSequential(
    kMax = 2,
    alpha = dAlpha,
    informationRates = c(0.50, 1.00),
    typeOfDesign = "asOF"
  )

  testthat::expect_equal(state$mcpObj$CutOff, desBase$stageLevels[2], tolerance = 1e-12)
  testthat::expect_true(state$trial_completed)
  testthat::expect_identical(state$completion_reason, "final_look")
})
