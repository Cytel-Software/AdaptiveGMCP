testthat::test_that( "SetupAnalysisPEPC creates a design-only PE state", {
  wi <- rep( 1 / 8, 8 )
  g <- matrix( 0, nrow = 8, ncol = 8 )
  infoFrac <- c( 0.5, 1 )

  peState <- SetupAnalysisPEPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = "Partly-Parametric",
    dAlpha = 0.025,
    vInfoFracts = infoFrac,
    strDesignType = "asOF",
    bPlotGraphs = FALSE
  )

  manualState <- SetupAnalysisPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = "Partly-Parametric",
    dAlpha = 0.025,
    vInfoFracts = infoFrac,
    strDesignType = "asOF",
    bPlotGraphs = FALSE
  )

  testthat::expect_s3_class( peState, "PCAnalysisState" )
  testthat::expect_equal( peState$mcpObj$Correlation, manualState$mcpObj$Correlation )
  testthat::expect_equal( peState$mcpObj$bdryTab, manualState$mcpObj$bdryTab )
} )

testthat::test_that( "SetupAnalysisPEPC supports one-look and rejects >2-look PE workflows", {
  wi <- rep( 1 / 4, 4 )
  g <- matrix( 0, nrow = 4, ncol = 4 )

  oneLookState <- SetupAnalysisPEPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = "Partly-Parametric",
    dAlpha = 0.025,
    vInfoFracts = c( 1 ),
    strDesignType = "asOF",
    bPlotGraphs = FALSE
  )

  testthat::expect_s3_class( oneLookState, "PCAnalysisState" )
  testthat::expect_equal( nrow( oneLookState$mcpObj$bdryTab ), 1L )

  testthat::expect_error(
    SetupAnalysisPEPC(
      vWeights = wi,
      mGMatrix = g,
      strTestType = "Partly-Parametric",
      dAlpha = 0.025,
      vInfoFracts = c( 0.5, 0.7, 1 ),
      strDesignType = "asOF",
      bPlotGraphs = FALSE
    ),
    "PE analysis currently supports one-look or two-look workflows only"
  )

  testthat::expect_error(
    SetupAnalysisPEPC(
      vWeights = wi,
      mGMatrix = g,
      strTestType = "Partly-Parametric",
      dAlpha = 0.025,
      vInfoFracts = c( NA_real_, 1 ),
      strDesignType = "asOF",
      bPlotGraphs = FALSE
    ),
    "vInfoFracts must contain finite, non-missing numeric values"
  )

  testthat::expect_error(
    SetupAnalysisPEPC(
      vWeights = wi,
      mGMatrix = g,
      strTestType = "Partly-Parametric",
      dAlpha = 0.025,
      vInfoFracts = c( 0, 1 ),
      strDesignType = "asOF",
      bPlotGraphs = FALSE
    ),
    "vInfoFracts values must be in"
  )

  testthat::expect_error(
    SetupAnalysisPEPC(
      vWeights = wi,
      mGMatrix = g,
      strTestType = "Partly-Parametric",
      dAlpha = 0.025,
      vInfoFracts = c( 0.7, 0.7 ),
      strDesignType = "asOF",
      bPlotGraphs = FALSE
    ),
    "vInfoFracts must be strictly increasing"
  )
} )

testthat::test_that( "AnalyzeLookPEPC derives a generic look-level correlation matrix", {
  wi <- rep( 1 / 8, 8 )
  g <- matrix( 0, nrow = 8, ncol = 8 )
  infoFrac <- c( 0.5, 1 )

  fullpopSampleSizes <- c( 120, 80, 70 )
  subpopSampleSizes <- c( 90, 55, 45 )

  peState <- SetupAnalysisPEPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = "Partly-Parametric",
    dAlpha = 0.025,
    vInfoFracts = infoFrac,
    strDesignType = "asOF",
    bPlotGraphs = FALSE
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

  peState <- AnalyzeLookPEPC(
    cState = peState,
    vRawPVals = c( H1 = 0.01, H2 = 0.04, H3 = 0.02, H4 = 0.03, H5 = 0.05, H6 = 0.06, H7 = 0.07, H8 = 0.08 ),
    vFullPopulationSampleSizes = fullpopSampleSizes,
    vSubpopulationSampleSizes = subpopSampleSizes,
    bPlotGraphs = FALSE
  )

  manualState <- SetupAnalysisPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = "Partly-Parametric",
    dAlpha = 0.025,
    vInfoFracts = infoFrac,
    strDesignType = "asOF",
    bPlotGraphs = FALSE
  )

  manualState <- AnalyzeLookPC(
    cState = manualState,
    vRawPVals = c( H1 = 0.01, H2 = 0.04, H3 = 0.02, H4 = 0.03, H5 = 0.05, H6 = 0.06, H7 = 0.07, H8 = 0.08 ),
    mCorrelation = expectedCorrelation,
    bPlotGraphs = FALSE
  )

  testthat::expect_equal( peState$mcpObj$Correlation, expectedCorrelation )
  testthat::expect_equal( peState$mcpObj$AdjPValues, manualState$mcpObj$AdjPValues )
  testthat::expect_equal( peState$mcpObj$rej_flag_Curr, manualState$mcpObj$rej_flag_Curr )
  testthat::expect_equal( peState$completed_looks, manualState$completed_looks )
  testthat::expect_equal( peState$pe_sample_history[[ 1 ]]$fullpop, fullpopSampleSizes )
  testthat::expect_equal( peState$pe_sample_history[[ 1 ]]$subpop, subpopSampleSizes )
} )

testthat::test_that( "AnalyzeLookPEPC ignores look-1 selection and strategy inputs", {
  wi <- rep( 1 / 8, 8 )
  g <- matrix( 0, nrow = 8, ncol = 8 )
  fullpopSampleSizes <- c( 120, 80, 70 )
  subpopSampleSizes <- c( 90, 55, 45 )
  pRaw <- setNames( rep( 0.4, 8 ), paste0( "H", seq_len( 8 ) ) )
  state0 <- SetupAnalysisPEPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = "Partly-Parametric",
    dAlpha = 0.025,
    vInfoFracts = c( 0.5, 1 ),
    strDesignType = "asOF",
    bPlotGraphs = FALSE
  )

  testthat::expect_warning(
    AnalyzeLookPEPC(
      cState = state0,
      vRawPVals = pRaw,
      vFullPopulationSampleSizes = fullpopSampleSizes,
      vSubpopulationSampleSizes = subpopSampleSizes,
      vSelectHypo = "H1",
      vWeights = wi,
      mGMatrix = g,
      bPlotGraphs = FALSE
    ),
    regexp = "Look 1 uses the initial design.*vSelectHypo, vWeights, mGMatrix will be ignored"
  )

  peState <- suppressWarnings(
    AnalyzeLookPEPC(
      cState = state0,
      vRawPVals = pRaw,
      vFullPopulationSampleSizes = fullpopSampleSizes,
      vSubpopulationSampleSizes = subpopSampleSizes,
      vSelectHypo = "H1",
      vWeights = wi,
      mGMatrix = g,
      bPlotGraphs = FALSE
    )
  )

  testthat::expect_equal( peState$mcpObj$IndexSet, paste0( "H", seq_len( 8 ) ) )
  testthat::expect_length( peState$mcpObj$SelectionLook, 0 )
  testthat::expect_false( peState$mcpObj$Modify )
  testthat::expect_length( peState$pe_sample_history, 1 )
} )

testthat::test_that( "AnalyzeLookPEPC validates look-level sample sizes", {
  wi <- rep( 1 / 4, 4 )
  g <- matrix( 0, nrow = 4, ncol = 4 )
  infoFrac <- c( 0.5, 1 )

  peState <- SetupAnalysisPEPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = "Partly-Parametric",
    dAlpha = 0.025,
    vInfoFracts = infoFrac,
    strDesignType = "asOF",
    bPlotGraphs = FALSE
  )

  testthat::expect_error(
    AnalyzeLookPEPC(
      cState = peState,
      vRawPVals = c( H1 = 0.01, H2 = 0.02, H3 = 0.03, H4 = 0.04 ),
      vFullPopulationSampleSizes = c( 50, 25 ),
      vSubpopulationSampleSizes = c( 60, 20 ),
      bPlotGraphs = FALSE
    ),
    "Subgroup sample sizes must not exceed full-population sample sizes"
  )

} )

testthat::test_that( "AnalyzeLookPEPC rejects a call after the planned final look", {
  wi <- rep( 0.5, 2 )
  g <- matrix( c( 0, 1, 1, 0 ), byrow = TRUE, nrow = 2 )

  peState <- SetupAnalysisPEPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = "Bonf",
    dAlpha = 0.025,
    vInfoFracts = c( 0.5, 1 ),
    strDesignType = "asOF",
    bPlotGraphs = FALSE
  )

  peState <- AnalyzeLookPEPC(
    cState = peState,
    vRawPVals = c( H1 = 0.5, H2 = 0.6 ),
    vFullPopulationSampleSizes = c( 40, 40 ),
    vSubpopulationSampleSizes = c( 20, 20 ),
    bPlotGraphs = FALSE
  )

  peState <- AnalyzeLookPEPC(
    cState = peState,
    vRawPVals = c( H1 = 0.45, H2 = 0.55 ),
    vFullPopulationSampleSizes = c( 70, 70 ),
    vSubpopulationSampleSizes = c( 35, 35 ),
    bPlotGraphs = FALSE
  )

  testthat::expect_error(
    AnalyzeLookPEPC(
      cState = peState,
      vRawPVals = c( H1 = 0.4, H2 = 0.5 ),
      vFullPopulationSampleSizes = c( 90, 90 ),
      vSubpopulationSampleSizes = c( 45, 45 ),
      bPlotGraphs = FALSE
    ),
    "Trial already concluded - stopping criteria met"
  )
} )

testthat::test_that( "AnalyzeLookPEPC short-circuits completed trials", {
  peState <- SetupAnalysisPEPC(
    vWeights = c( 0.5, 0.5 ),
    mGMatrix = matrix( c( 0, 1, 1, 0 ), byrow = TRUE, nrow = 2 ),
    strTestType = "Bonf",
    vInfoFracts = c( 1 ),
    bPlotGraphs = FALSE
  )

  peState$trial_completed <- TRUE

  testthat::expect_error(
    AnalyzeLookPEPC(
      cState = peState,
      vRawPVals = c( H1 = 0.5, H2 = 0.6 ),
      vFullPopulationSampleSizes = numeric( 0 ),
      vSubpopulationSampleSizes = numeric( 0 ),
      bPlotGraphs = FALSE
    ),
    "Trial already concluded - stopping criteria met"
  )
} )

testthat::test_that( "AnalyzeLookPEPC does not update PE sample history on PC early return", {
  wi <- rep( 1 / 4, 4 )
  g <- matrix( 0, nrow = 4, ncol = 4 )
  infoFrac <- c( 0.5, 1 )

  vFullpopLook1 <- c( 100, 60 )
  vSubpopLook1 <- c( 80, 45 )
  vFullpopAttemptedLook2 <- c( 120, 70 )
  vSubpopAttemptedLook2 <- c( 90, 50 )

  peState <- SetupAnalysisPEPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = "Partly-Parametric",
    dAlpha = 0.025,
    vInfoFracts = infoFrac,
    strDesignType = "asOF",
    bPlotGraphs = FALSE
  )

  peState <- AnalyzeLookPEPC(
    cState = peState,
    vRawPVals = c( H1 = 0.01, H2 = 0.02, H3 = 0.03, H4 = 0.04 ),
    vFullPopulationSampleSizes = vFullpopLook1,
    vSubpopulationSampleSizes = vSubpopLook1,
    bPlotGraphs = FALSE
  )

  peState$mcpObj$IndexSet <- character( 0 )
  peState$trial_completed <- FALSE
  peState$completion_reason <- NULL

  peState <- AnalyzeLookPEPC(
    cState = peState,
    vRawPVals = numeric( 0 ),
    vFullPopulationSampleSizes = vFullpopAttemptedLook2,
    vSubpopulationSampleSizes = vSubpopAttemptedLook2,
    bPlotGraphs = FALSE
  )

  vRecordedLooks <- which( !vapply( peState$pe_sample_history, is.null, logical( 1 ) ) )

  testthat::expect_equal( peState$completed_looks, 1L )
  testthat::expect_true( peState$trial_completed )
  testthat::expect_identical( peState$completion_reason, "all_hypotheses_dropped" )
  testthat::expect_equal( vRecordedLooks, 1L )
  testthat::expect_equal( peState$pe_sample_history[[ 1 ]]$fullpop, vFullpopLook1 )
  testthat::expect_equal( peState$pe_sample_history[[ 1 ]]$subpop, vSubpopLook1 )
} )

testthat::test_that( "AnalyzeLookPEPC retains sample-size history and correlation", {
  wi <- rep( 1 / 8, 8 )
  g <- matrix( 0, nrow = 8, ncol = 8 )
  infoFrac <- c( 0.5, 1 )

  fullpopSampleSizes <- c( 100, 30, 100 )
  subpopSampleSizes <- c( 70, 20, 60 )

  peState <- SetupAnalysisPEPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = "Partly-Parametric",
    dAlpha = 0.025,
    vInfoFracts = infoFrac,
    strDesignType = "asOF",
    bPlotGraphs = FALSE
  )

  peState <- AnalyzeLookPEPC(
    cState = peState,
    vRawPVals = c( H1 = 0.02, H2 = 0.03, H3 = 0.01, H4 = 0.04, H5 = 0.06, H6 = 0.05, H7 = 0.07, H8 = 0.08 ),
    vFullPopulationSampleSizes = fullpopSampleSizes,
    vSubpopulationSampleSizes = subpopSampleSizes,
    bPlotGraphs = FALSE
  )

  testthat::expect_equal( peState$pe_sample_history[[ 1 ]]$fullpop, fullpopSampleSizes )
  testthat::expect_equal( peState$pe_sample_history[[ 1 ]]$subpop, subpopSampleSizes )
} )
