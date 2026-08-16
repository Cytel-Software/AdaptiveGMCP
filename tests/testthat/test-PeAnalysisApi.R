testthat::test_that( "SetupAnalysis_PE_PC creates a design-only PE state", {
  wi <- rep( 1 / 8, 8 )
  g <- matrix( 0, nrow = 8, ncol = 8 )
  infoFrac <- c( 0.5, 1 )

  peState <- SetupAnalysis_PE_PC(
    WI = wi,
    G = g,
    test.type = "Partly-Parametric",
    alpha = 0.025,
    info_frac = infoFrac,
    typeOfDesign = "asOF",
    plotGraphs = FALSE
  )

  manualState <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = "Partly-Parametric",
    alpha = 0.025,
    info_frac = infoFrac,
    typeOfDesign = "asOF",
    plotGraphs = FALSE
  )

  testthat::expect_s3_class( peState, "PCAnalysisState" )
  testthat::expect_equal( peState$mcpObj$Correlation, manualState$mcpObj$Correlation )
  testthat::expect_equal( peState$mcpObj$bdryTab, manualState$mcpObj$bdryTab )
} )

testthat::test_that( "AnalyzeLook_PE_PC derives a generic look-level correlation matrix", {
  wi <- rep( 1 / 8, 8 )
  g <- matrix( 0, nrow = 8, ncol = 8 )
  infoFrac <- c( 0.5, 1 )

  fullpopSampleSizes <- c( 120, 80, 70 )
  subpopSampleSizes <- c( 90, 55, 45 )

  peState <- SetupAnalysis_PE_PC(
    WI = wi,
    G = g,
    test.type = "Partly-Parametric",
    alpha = 0.025,
    info_frac = infoFrac,
    typeOfDesign = "asOF",
    plotGraphs = FALSE
  )

  expectedCorrelation <- AdaptGMCP:::BuildPECorrelationMatrix(
    fullpop_sample_sizes = fullpopSampleSizes,
    subpop_sample_sizes = subpopSampleSizes,
    n_hypotheses = length( peState$mcpObj$IntialHypothesis ),
    hypothesis_names = peState$mcpObj$IntialHypothesis
  )

  testthat::expect_true( is.na( expectedCorrelation[ "H1", "H3" ] ) )
  testthat::expect_false( is.na( expectedCorrelation[ "H1", "H2" ] ) )
  testthat::expect_false( is.na( expectedCorrelation[ "H1", "H5" ] ) )

  peState <- AnalyzeLook_PE_PC(
    state = peState,
    p_raw = c( H1 = 0.01, H2 = 0.04, H3 = 0.02, H4 = 0.03, H5 = 0.05, H6 = 0.06, H7 = 0.07, H8 = 0.08 ),
    fullpop_sample_sizes = fullpopSampleSizes,
    subpop_sample_sizes = subpopSampleSizes,
    plotGraphs = FALSE
  )

  manualState <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = "Partly-Parametric",
    alpha = 0.025,
    info_frac = infoFrac,
    typeOfDesign = "asOF",
    plotGraphs = FALSE
  )

  manualState <- AnalyzeLook_PC(
    state = manualState,
    p_raw = c( H1 = 0.01, H2 = 0.04, H3 = 0.02, H4 = 0.03, H5 = 0.05, H6 = 0.06, H7 = 0.07, H8 = 0.08 ),
    info_frac_cur = infoFrac[1],
    Correlation = expectedCorrelation,
    plotGraphs = FALSE
  )

  testthat::expect_equal( peState$mcpObj$Correlation, expectedCorrelation )
  testthat::expect_equal( peState$mcpObj$AdjPValues, manualState$mcpObj$AdjPValues )
  testthat::expect_equal( peState$mcpObj$rej_flag_Curr, manualState$mcpObj$rej_flag_Curr )
  testthat::expect_equal( peState$completed_looks, manualState$completed_looks )
} )

testthat::test_that( "AnalyzeLook_PE_PC validates look-level sample sizes", {
  wi <- rep( 1 / 4, 4 )
  g <- matrix( 0, nrow = 4, ncol = 4 )
  infoFrac <- c( 0.5, 1 )

  peState <- SetupAnalysis_PE_PC(
    WI = wi,
    G = g,
    test.type = "Partly-Parametric",
    alpha = 0.025,
    info_frac = infoFrac,
    typeOfDesign = "asOF",
    plotGraphs = FALSE
  )

  testthat::expect_error(
    AnalyzeLook_PE_PC(
      state = peState,
      p_raw = c( H1 = 0.01, H2 = 0.02, H3 = 0.03, H4 = 0.04 ),
      fullpop_sample_sizes = c( 50, 25 ),
      subpop_sample_sizes = c( 60, 20 ),
      plotGraphs = FALSE
    ),
    "Subgroup sample sizes must not exceed full-population sample sizes"
  )
} )

testthat::test_that( "AnalyzeLook_PE_PC does not update PE sample history on PC early return", {
  wi <- rep( 1 / 4, 4 )
  g <- matrix( 0, nrow = 4, ncol = 4 )
  infoFrac <- c( 0.5, 1 )

  vFullpopLook1 <- c( 100, 60 )
  vSubpopLook1 <- c( 80, 45 )
  vFullpopAttemptedLook2 <- c( 120, 70 )
  vSubpopAttemptedLook2 <- c( 90, 50 )

  peState <- SetupAnalysis_PE_PC(
    WI = wi,
    G = g,
    test.type = "Partly-Parametric",
    alpha = 0.025,
    info_frac = infoFrac,
    typeOfDesign = "asOF",
    plotGraphs = FALSE
  )

  peState <- AnalyzeLook_PE_PC(
    state = peState,
    p_raw = c( H1 = 0.01, H2 = 0.02, H3 = 0.03, H4 = 0.04 ),
    fullpop_sample_sizes = vFullpopLook1,
    subpop_sample_sizes = vSubpopLook1,
    plotGraphs = FALSE
  )

  peState$mcpObj$IndexSet <- character( 0 )
  peState$trial_completed <- FALSE
  peState$completion_reason <- NULL

  peState <- AnalyzeLook_PE_PC(
    state = peState,
    p_raw = numeric( 0 ),
    fullpop_sample_sizes = vFullpopAttemptedLook2,
    subpop_sample_sizes = vSubpopAttemptedLook2,
    plotGraphs = FALSE
  )

  vRecordedLooks <- which( !vapply( peState$pe_sample_history, is.null, logical( 1 ) ) )

  testthat::expect_equal( peState$completed_looks, 1L )
  testthat::expect_true( peState$trial_completed )
  testthat::expect_identical( peState$completion_reason, "all_hypotheses_dropped" )
  testthat::expect_equal( vRecordedLooks, 1L )
  testthat::expect_equal( peState$pe_sample_history[[ 1 ]]$fullpop, vFullpopLook1 )
  testthat::expect_equal( peState$pe_sample_history[[ 1 ]]$subpop, vSubpopLook1 )
} )
