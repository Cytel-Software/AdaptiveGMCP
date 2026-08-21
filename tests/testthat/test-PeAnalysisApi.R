testthat::test_that( "SetupAnalysis_PE_PC creates a design-only PE state", {
  wi <- rep( 1 / 8, 8 )
  g <- matrix( 0, nrow = 8, ncol = 8 )
  infoFrac <- c( 0.5, 1 )
  plannedFull <- c( 200, 130, 120 )
  plannedSub <- c( 150, 90, 80 )

  peState <- SetupAnalysis_PE_PC(
    WI = wi,
    G = g,
    test.type = "Partly-Parametric",
    alpha = 0.025,
    planned_info_frac = infoFrac,
    typeOfDesign = "asOF",
    planned_fullpop_sample_sizes = plannedFull,
    planned_subpop_sample_sizes = plannedSub,
    plotGraphs = FALSE
  )

  manualState <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = "Partly-Parametric",
    alpha = 0.025,
    planned_info_frac = infoFrac,
    typeOfDesign = "asOF",
    plotGraphs = FALSE
  )

  testthat::expect_s3_class( peState, "PCAnalysisState" )
  testthat::expect_equal( peState$mcpObj$Correlation, manualState$mcpObj$Correlation )
  testthat::expect_equal( peState$mcpObj$bdryTab, manualState$mcpObj$bdryTab )
  testthat::expect_equal( peState$design_params$planned_fullpop_sample_sizes, plannedFull )
  testthat::expect_equal( peState$design_params$planned_subpop_sample_sizes, plannedSub )
} )

testthat::test_that( "SetupAnalysis_PE_PC supports one-look and rejects >2-look PE workflows", {
  wi <- rep( 1 / 4, 4 )
  g <- matrix( 0, nrow = 4, ncol = 4 )

  oneLookState <- SetupAnalysis_PE_PC(
    WI = wi,
    G = g,
    test.type = "Partly-Parametric",
    alpha = 0.025,
    planned_info_frac = c( 1 ),
    typeOfDesign = "asOF",
    planned_fullpop_sample_sizes = c( 100, 60 ),
    planned_subpop_sample_sizes = c( 80, 45 ),
    plotGraphs = FALSE
  )

  testthat::expect_s3_class( oneLookState, "PCAnalysisState" )
  testthat::expect_equal( nrow( oneLookState$mcpObj$bdryTab ), 1L )

  testthat::expect_error(
    SetupAnalysis_PE_PC(
      WI = wi,
      G = g,
      test.type = "Partly-Parametric",
      alpha = 0.025,
      planned_info_frac = c( 0.5, 0.7, 1 ),
      typeOfDesign = "asOF",
      planned_fullpop_sample_sizes = c( 100, 60 ),
      planned_subpop_sample_sizes = c( 80, 45 ),
      plotGraphs = FALSE
    ),
    "PE analysis currently supports one-look or two-look workflows only"
  )

  testthat::expect_error(
    SetupAnalysis_PE_PC(
      WI = wi,
      G = g,
      test.type = "Partly-Parametric",
      alpha = 0.025,
      planned_info_frac = c( NA_real_, 1 ),
      typeOfDesign = "asOF",
      planned_fullpop_sample_sizes = c( 100, 60 ),
      planned_subpop_sample_sizes = c( 80, 45 ),
      plotGraphs = FALSE
    ),
    "planned_info_frac must contain finite, non-missing numeric values"
  )

  testthat::expect_error(
    SetupAnalysis_PE_PC(
      WI = wi,
      G = g,
      test.type = "Partly-Parametric",
      alpha = 0.025,
      planned_info_frac = c( 0, 1 ),
      typeOfDesign = "asOF",
      planned_fullpop_sample_sizes = c( 100, 60 ),
      planned_subpop_sample_sizes = c( 80, 45 ),
      plotGraphs = FALSE
    ),
    "planned_info_frac values must be in"
  )

  testthat::expect_error(
    SetupAnalysis_PE_PC(
      WI = wi,
      G = g,
      test.type = "Partly-Parametric",
      alpha = 0.025,
      planned_info_frac = c( 0.7, 0.7 ),
      typeOfDesign = "asOF",
      planned_fullpop_sample_sizes = c( 100, 60 ),
      planned_subpop_sample_sizes = c( 80, 45 ),
      plotGraphs = FALSE
    ),
    "planned_info_frac must be strictly increasing"
  )
} )

testthat::test_that( "AnalyzeLook_PE_PC derives a generic look-level correlation matrix", {
  wi <- rep( 1 / 8, 8 )
  g <- matrix( 0, nrow = 8, ncol = 8 )
  infoFrac <- c( 0.5, 1 )

  plannedFull <- c( 200, 130, 120 )
  plannedSub <- c( 150, 90, 80 )
  fullpopSampleSizes <- c( 120, 80, 70 )
  subpopSampleSizes <- c( 90, 55, 45 )

  peState <- SetupAnalysis_PE_PC(
    WI = wi,
    G = g,
    test.type = "Partly-Parametric",
    alpha = 0.025,
    planned_info_frac = infoFrac,
    typeOfDesign = "asOF",
    planned_fullpop_sample_sizes = plannedFull,
    planned_subpop_sample_sizes = plannedSub,
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
    planned_info_frac = infoFrac,
    typeOfDesign = "asOF",
    plotGraphs = FALSE
  )

  manualState <- AnalyzeLook_PC(
    state = manualState,
    p_raw = c( H1 = 0.01, H2 = 0.04, H3 = 0.02, H4 = 0.03, H5 = 0.05, H6 = 0.06, H7 = 0.07, H8 = 0.08 ),
    info_frac_cur = fullpopSampleSizes[ 1 ] / plannedFull[ 1 ],
    Correlation = expectedCorrelation,
    plotGraphs = FALSE
  )

  testthat::expect_equal( peState$mcpObj$Correlation, expectedCorrelation )
  testthat::expect_equal( peState$mcpObj$AdjPValues, manualState$mcpObj$AdjPValues )
  testthat::expect_equal( peState$mcpObj$rej_flag_Curr, manualState$mcpObj$rej_flag_Curr )
  testthat::expect_equal( peState$completed_looks, manualState$completed_looks )
  testthat::expect_equal( peState$look_history[[ 1 ]]$pe_info_frac$info_frac_cur, 0.6 )
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
    planned_info_frac = infoFrac,
    typeOfDesign = "asOF",
    planned_fullpop_sample_sizes = c( 100, 60 ),
    planned_subpop_sample_sizes = c( 80, 45 ),
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

  testthat::expect_error(
    AnalyzeLook_PE_PC(
      state = peState,
      p_raw = c( H1 = 0.01, H2 = 0.02, H3 = 0.03, H4 = 0.04 ),
      fullpop_sample_sizes = c( 101, 60 ),
      subpop_sample_sizes = c( 80, 45 ),
      plotGraphs = FALSE
    ),
    "Cumulative full-population sample sizes at a look must be <= planned full-population sample sizes"
  )

  testthat::expect_error(
    AnalyzeLook_PE_PC(
      state = peState,
      p_raw = c( H1 = 0.01, H2 = 0.02, H3 = 0.03, H4 = 0.04 ),
      fullpop_sample_sizes = c( 90, 55 ),
      subpop_sample_sizes = c( 81, 46 ),
      plotGraphs = FALSE
    ),
    "Cumulative subgroup sample sizes at a look must be <= planned subgroup sample sizes"
  )
} )

testthat::test_that( "AnalyzeLook_PE_PC allows up to two looks and rejects a third call", {
  wi <- rep( 0.5, 2 )
  g <- matrix( c( 0, 1, 1, 0 ), byrow = TRUE, nrow = 2 )

  peState <- SetupAnalysis_PE_PC(
    WI = wi,
    G = g,
    test.type = "Bonf",
    alpha = 0.025,
    planned_info_frac = c( 1 ),
    typeOfDesign = "asOF",
    planned_fullpop_sample_sizes = c( 100, 100 ),
    planned_subpop_sample_sizes = c( 50, 50 ),
    plotGraphs = FALSE
  )

  peState <- AnalyzeLook_PE_PC(
    state = peState,
    p_raw = c( H1 = 0.5, H2 = 0.6 ),
    fullpop_sample_sizes = c( 40, 40 ),
    subpop_sample_sizes = c( 20, 20 ),
    plotGraphs = FALSE
  )

  peState <- AnalyzeLook_PE_PC(
    state = peState,
    p_raw = c( H1 = 0.45, H2 = 0.55 ),
    fullpop_sample_sizes = c( 70, 70 ),
    subpop_sample_sizes = c( 35, 35 ),
    plotGraphs = FALSE
  )

  testthat::expect_error(
    AnalyzeLook_PE_PC(
      state = peState,
      p_raw = c( H1 = 0.4, H2 = 0.5 ),
      fullpop_sample_sizes = c( 90, 90 ),
      subpop_sample_sizes = c( 45, 45 ),
      plotGraphs = FALSE
    ),
    "PE analysis currently supports a maximum of 2 analyzed looks per design"
  )
} )

testthat::test_that( "AnalyzeLook_PE_PC does not update PE sample history on PC early return", {
  wi <- rep( 1 / 4, 4 )
  g <- matrix( 0, nrow = 4, ncol = 4 )
  infoFrac <- c( 0.5, 1 )

  vPlannedFull <- c( 150, 90 )
  vPlannedSub <- c( 110, 65 )
  vFullpopLook1 <- c( 100, 60 )
  vSubpopLook1 <- c( 80, 45 )
  vFullpopAttemptedLook2 <- c( 120, 70 )
  vSubpopAttemptedLook2 <- c( 90, 50 )

  peState <- SetupAnalysis_PE_PC(
    WI = wi,
    G = g,
    test.type = "Partly-Parametric",
    alpha = 0.025,
    planned_info_frac = infoFrac,
    typeOfDesign = "asOF",
    planned_fullpop_sample_sizes = vPlannedFull,
    planned_subpop_sample_sizes = vPlannedSub,
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
  testthat::expect_equal(
    peState$pe_sample_history[[ 1 ]]$denominator_planned_fullpop_control,
    vPlannedFull[ 1 ]
  )
} )

testthat::test_that( "AnalyzeLook_PE_PC computes IF from full-population control arm only", {
  wi <- rep( 1 / 8, 8 )
  g <- matrix( 0, nrow = 8, ncol = 8 )
  infoFrac <- c( 0.5, 1 )

  plannedFull <- c( 200, 150, 120 )
  plannedSub <- c( 140, 90, 70 )
  fullpopSampleSizes <- c( 100, 30, 100 )
  subpopSampleSizes <- c( 70, 20, 60 )

  peState <- SetupAnalysis_PE_PC(
    WI = wi,
    G = g,
    test.type = "Partly-Parametric",
    alpha = 0.025,
    planned_info_frac = infoFrac,
    typeOfDesign = "asOF",
    planned_fullpop_sample_sizes = plannedFull,
    planned_subpop_sample_sizes = plannedSub,
    plotGraphs = FALSE
  )

  peState <- AnalyzeLook_PE_PC(
    state = peState,
    p_raw = c( H1 = 0.02, H2 = 0.03, H3 = 0.01, H4 = 0.04, H5 = 0.06, H6 = 0.05, H7 = 0.07, H8 = 0.08 ),
    fullpop_sample_sizes = fullpopSampleSizes,
    subpop_sample_sizes = subpopSampleSizes,
    plotGraphs = FALSE
  )

  testthat::expect_equal( peState$look_history[[ 1 ]]$info_frac_cur, 0.5 )
  testthat::expect_equal(
    peState$look_history[[ 1 ]]$pe_info_frac$numerator_fullpop_control,
    fullpopSampleSizes[ 1 ]
  )
  testthat::expect_equal(
    peState$look_history[[ 1 ]]$pe_info_frac$denominator_planned_fullpop_control,
    plannedFull[ 1 ]
  )
} )
