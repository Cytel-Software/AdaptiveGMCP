# --------------------------------------------------------------------------------------------------
#
# Non-interactive CER API tests
#
# --------------------------------------------------------------------------------------------------

testthat::test_that( "SetupDesignCER creates a design-only object", {
  d <- SetupDesignCER(
    nArms = 3,
    nEps = 1,
    nSampleSize = 300,
    lEpTypes = list( EP1 = "Continuous" ),
    lSigma = list( EP1 = c( 1, 1, 1 ) ),
    bCommonStdDev = TRUE,
    lCtrlProps = list( EP1 = NA ),
    vAllocRatios = c( 1, 1, 1 ),
    vWeights = c( 0.5, 0.5 ),
    mGMatrix = matrix( c( 0, 1, 1, 0 ), nrow = 2, byrow = TRUE ),
    strTestType = "Parametric"
  )

  testthat::expect_s3_class( d, "CERDesign" )
  testthat::expect_equal( dim( d$plan_boundary$Stage1Bdry ), c( 3L, 2L ) )
  testthat::expect_identical( d$lPlanned$plan_boundary, d$plan_boundary )
  testthat::expect_false( "completed_looks" %in% names( d ) )
  testthat::expect_false( "p_raw" %in% names( d ) )
} )

testthat::test_that( "SetupDesignCER names arm-specific endpoint inputs", {
  d <- SetupDesignCER(
    nArms = 3,
    nEps = 1,
    nSampleSize = 300,
    lEpTypes = list( EP1 = "Continuous" ),
    lSigma = list( EP1 = c( 1, 1.1, 1.2 ) ),
    lCtrlProps = list( EP1 = NA ),
    vAllocRatios = c( 1, 2, 3 ),
    vWeights = c( .5, .5 ),
    mGMatrix = matrix( c( 0, 1, 1, 0 ), nrow = 2, byrow = TRUE ),
    strTestType = "Parametric"
  )

  vArmNames <- c( "Control", "Treatment1", "Treatment2" )
  testthat::expect_named( d$allocRatio, vArmNames )
  testthat::expect_named( d$sigma$EP1, vArmNames )
} )

testthat::test_that( "AnalyzeLookCER exposes direct strategy adaptation arguments", {
  testthat::expect_false( "strategy_update" %in% names( formals( AnalyzeLookCER ) ) )
})

testthat::test_that( "CER Look 1 reuses the planned design", {
  d <- SetupDesignCER(
    nArms = 3,
    nEps = 1,
    nSampleSize = 300,
    lEpTypes = list( EP1 = "Continuous" ),
    lSigma = list( EP1 = c( 1, 1, 1 ) ),
    bCommonStdDev = TRUE,
    lCtrlProps = list( EP1 = NA ),
    vAllocRatios = c( 1, 1, 1 ),
    vWeights = c( 0.5, 0.5 ),
    mGMatrix = matrix( c( 0, 1, 1, 0 ), nrow = 2, byrow = TRUE ),
    strTestType = "Parametric"
  )

  state <- testthat::with_mocked_bindings(
    AnalyzeLookCER( d, vRawPVals = c( H1 = 0.1, H2 = 0.2 ) ),
    ComputeCERPlannedDesign = function( ... )
    {
      stop( "ComputeCERPlannedDesign should not be called for a CERDesign." )
    }
  )

  testthat::expect_s3_class( state, "CERAnalysisState" )
} )

testthat::test_that( "CER API advances two looks without console input", {
  d <- SetupDesignCER(
    nArms = 3,
    nEps = 1,
    nSampleSize = 300,
    lEpTypes = list( EP1 = "Continuous" ),
    lSigma = list( EP1 = c( 1, 1, 1 ) ),
    bCommonStdDev = TRUE,
    lCtrlProps = list( EP1 = NA ),
    vAllocRatios = c( 1, 1, 1 ),
    vWeights = c( 0.5, 0.5 ),
    mGMatrix = matrix( c( 0, 1, 1, 0 ), nrow = 2, byrow = TRUE ),
    strTestType = "Parametric"
  )
  state1 <- AnalyzeLookCER( d, vRawPVals = c( H1 = 0.1, H2 = 0.2 ) )
  state2 <- AnalyzeLookCER( d, state1, vRawPVals = c( H1 = 0.0001, H2 = 0.2 ) )

  testthat::expect_s3_class( state1, "CERAnalysisState" )
  testthat::expect_equal( state1$completed_looks, 1L )
  testthat::expect_equal( state2$completed_looks, 2L )
  testthat::expect_equal( length( state2$look_history ), 2L )
  testthat::expect_equal( state2$completion_reason, "final_look" )
  testthat::expect_true( is.data.frame( state2$results$stage2$intersection_rejection ) )
  testthat::expect_equal(
    state2$cumulative_stage2_pvalues[ c( "H1", "H2" ) ],
    c( H1 = 0.0002031669, H2 = 0.116977578 ),
    tolerance = 1e-8
  )
  testthat::expect_equal(
    state2$results$stage2$cumulative_pvalues,
    state2$cumulative_stage2_pvalues
  )
  testthat::expect_false( "design" %in% names( state2 ) )
} )

testthat::test_that( "CER API applies selection, sample size, and strategy updates", {
  d <- SetupDesignCER(
    nArms = 3,
    nEps = 2,
    nSampleSize = 210,
    lEpTypes = list( EP1 = "Continuous", EP2 = "Continuous" ),
    lSigma = list( EP1 = c( 1, 1, 1 ), EP2 = c( 1, 1, 1 ) ),
    lCtrlProps = list( EP1 = NA, EP2 = NA ),
    vAllocRatios = c( 1, 1, 1 ),
    vWeights = c( 0.5, 0.5, 0, 0 ),
    mGMatrix = matrix(
      c( 0, .5, .5, 0, .5, 0, 0, .5, 0, 1, 0, 0, 1, 0, 0, 0 ),
      nrow = 4, byrow = TRUE
    ),
    strTestType = "Parametric"
  )
  state1 <- AnalyzeLookCER(
    d, vRawPVals = c( H1 = .00045, H2 = .0952, H3 = .0225, H4 = .1104 )
  )
  testthat::expect_error(
    AnalyzeLookCER(
      d, state1, vRawPVals = c( H2 = .0299, H4 = .0586 ),
      vSelectHypo = c( "H2", "H4" ),
      vNewSampleSizes = c( 88, 87, 86 )
    ),
    "exactly one sample size"
  )
  state2 <- AnalyzeLookCER(
    d, state1, vRawPVals = c( H2 = .0299, H4 = .0586 ),
    vSelectHypo = c( "H2", "H4" ),
    vNewSampleSizes = c( ignored1 = 88, ignored2 = 87 ),
    vWeights = c( H2 = .5, H4 = .5 ),
    mGMatrix = matrix( c( 0, 1, 1, 0 ), nrow = 2, byrow = TRUE )
  )

  testthat::expect_equal( state2$selected_hypotheses, c( "H2", "H4" ) )
  testthat::expect_equal( state2$adapted_sample_allocation[ 2, "Control" ], 88 )
  testthat::expect_equal( state2$adapted_sample_allocation[ 2, "Treatment2" ], 87 )
  testthat::expect_equal( state2$completed_looks, 2L )
  testthat::expect_true( is.list( state2$adaptation ) )
  testthat::expect_equal(
    state2$cumulative_stage2_pvalues,
    c(
      H1 = NA_real_,
      H2 = 0.0111233933131594,
      H3 = NA_real_,
      H4 = 0.0234119137484848
    ),
    tolerance = 1e-12
  )
  testthat::expect_equal(
    as.logical( unlist( state2$results$stage2$primary_rejection ) ),
    c( TRUE, TRUE, FALSE, TRUE )
  )
  testthat::expect_equal(
    state2$adaptation$Stage2AdjBdry[ 3L, ],
    c(
      a12_adj = 0,
      a22_adj = 0.013733642749567,
      a32_adj = 0,
      a42_adj = 0.013733642749567
    ),
    tolerance = 1e-12
  )
} )

testthat::test_that( "CER API ignores sample-size adaptation names", {
  d <- SetupDesignCER(
    nArms = 3,
    nEps = 1,
    nSampleSize = 300,
    lEpTypes = list( EP1 = "Continuous" ),
    lSigma = list( EP1 = c( 1, 1, 1 ) ),
    bCommonStdDev = TRUE,
    lCtrlProps = list( EP1 = NA ),
    vAllocRatios = c( 1, 1, 1 ),
    vWeights = c( .5, .5 ),
    mGMatrix = matrix( c( 0, 1, 1, 0 ), nrow = 2, byrow = TRUE ),
    strTestType = "Parametric"
  )
  state1 <- AnalyzeLookCER( d, vRawPVals = c( H1 = .1, H2 = .2 ) )
  state2Unnamed <- AnalyzeLookCER(
    d, state1, vRawPVals = c( H1 = .01, H2 = .02 ),
    vNewSampleSizes = c( 175, 180, 185 )
  )
  state2Named <- AnalyzeLookCER(
    d, state1, vRawPVals = c( H1 = .01, H2 = .02 ),
    vNewSampleSizes = c( Treatment2 = 175, Control = 180, Treatment1 = 185 )
  )

  testthat::expect_equal(
    unlist( state2Unnamed$adapted_sample_allocation[ 2L, ] ),
    c( Control = 175, Treatment1 = 180, Treatment2 = 185 )
  )
  testthat::expect_equal(
    state2Named$adapted_sample_allocation,
    state2Unnamed$adapted_sample_allocation
  )
  testthat::expect_error(
    AnalyzeLookCER(
      d, state1, vRawPVals = c( H1 = .01, H2 = .02 ),
      vNewSampleSizes = c( 175, 175 )
    ),
    "exactly one sample size"
  )
  testthat::expect_error(
    AnalyzeLookCER(
      d, state1, vRawPVals = c( H1 = .01, H2 = .02 ),
      vNewSampleSizes = c( 175, 175, 175, 175 )
    ),
    "exactly one sample size"
  )
} )

testthat::test_that( "CER API supports non-parametric designs", {
  d <- SetupDesignCER(
    nArms = 3, nEps = 1, nSampleSize = 300,
    lEpTypes = list( EP1 = "Binary" ),
    lSigma = list( EP1 = rep( NA_real_, 3 ) ),
    lCtrlProps = list( EP1 = 0.4 ),
    vAllocRatios = c( 1, 1, 1 ),
    vWeights = c( 0.5, 0.5 ),
    mGMatrix = matrix( c( 0, 1, 1, 0 ), nrow = 2, byrow = TRUE ),
    strTestType = "Non-Parametric"
  )

  testthat::expect_true( is.list( d$Sigma ) )
  state1 <- AnalyzeLookCER( d, vRawPVals = c( H1 = 0.03, H2 = 0.2 ) )
  state2 <- AnalyzeLookCER(
    d, state1, vRawPVals = c( H1 = 0.01, H2 = 0.1 )
  )

  testthat::expect_s3_class( state2, "CERAnalysisState" )
  testthat::expect_equal(
    state2$cumulative_stage2_pvalues,
    c( H1 = 0.00146542862005128, H2 = 0.0666377148658839 ),
    tolerance = 1e-12
  )
  testthat::expect_equal(
    as.logical( unlist( state2$results$stage2$primary_rejection ) ),
    c( TRUE, FALSE )
  )
} )

testthat::test_that( "CER API supports binary-first mixed endpoints", {
  d <- SetupDesignCER(
    nArms = 3, nEps = 2, nSampleSize = 300,
    lEpTypes = list( EP1 = "Binary", EP2 = "Continuous" ),
    lSigma = list( EP1 = NA, EP2 = rep( 1, 3 ) ),
    lCtrlProps = list( EP1 = 0.4, EP2 = NA ),
    vAllocRatios = c( 1, 1, 1 ),
    vWeights = rep( 0.25, 4 ),
    mGMatrix = matrix( 0, nrow = 4, ncol = 4 ),
    strTestType = "Partly-Parametric"
  )

  testthat::expect_equal( length( d$Sigma$SigmaZ ), 2L )
  testthat::expect_equal( nrow( d$Sigma$SigmaZ[[ "EP2" ]] ), 4L )

  state1 <- AnalyzeLookCER(
    d, vRawPVals = c( H1 = 0.03, H2 = 0.2, H3 = 0.04, H4 = 0.3 )
  )
  state2 <- AnalyzeLookCER(
    d, state1, vRawPVals = c( H1 = 0.01, H2 = 0.1, H3 = 0.02, H4 = 0.2 )
  )

  testthat::expect_equal(
    state2$cumulative_stage2_pvalues,
    c(
      H1 = 0.00146542862005128,
      H2 = 0.0666377148658839,
      H3 = 0.00357108346346435,
      H4 = 0.167041296151069
    ),
    tolerance = 1e-12
  )
  testthat::expect_equal(
    as.logical( unlist( state2$results$stage2$primary_rejection ) ),
    c( TRUE, FALSE, TRUE, FALSE )
  )
} )

testthat::test_that( "CER computes later endpoint covariance rows within group", {
  d <- SetupDesignCER(
    nArms = 4, nEps = 2, nSampleSize = 400,
    lEpTypes = list( EP1 = "Continuous", EP2 = "Continuous" ),
    lSigma = list( EP1 = rep( 1, 4 ), EP2 = rep( 1, 4 ) ),
    lCtrlProps = list( EP1 = NA, EP2 = NA ),
    vAllocRatios = c( 1, 2, 3, 4 ),
    vWeights = rep( 1 / 6, 6 ),
    mGMatrix = matrix( 0, nrow = 6, ncol = 6 ),
    strTestType = "Parametric"
  )
  state <- AnalyzeLookCER(
    d, vRawPVals = setNames( rep( 0.2, 6 ), paste0( "H", 1:6 ) )
  )
  vEndpoint1 <- state$results$stage1$cer_pcer$data$CER[
    state$results$stage1$cer_pcer$data$SubSets == "P :1,2,3,NP:"
  ][[ 1L ]]
  vEndpoint2 <- state$results$stage1$cer_pcer$data$CER[
    state$results$stage1$cer_pcer$data$SubSets == "P :4,5,6,NP:"
  ][[ 1L ]]
  testthat::expect_equal( vEndpoint2, vEndpoint1, tolerance = 1e-8 )
} )

testthat::test_that( "CER indexes non-parametric PCER by hypothesis", {
  d <- SetupDesignCER(
    nArms = 3, nEps = 2, nSampleSize = 300,
    lEpTypes = list( EP1 = "Binary", EP2 = "Continuous" ),
    lSigma = list( EP1 = rep( NA_real_, 3 ), EP2 = rep( 1, 3 ) ),
    lCtrlProps = list( EP1 = 0.4, EP2 = NA ),
    vAllocRatios = c( 1, 2, 4 ),
    vWeights = rep( 0.25, 4 ),
    mGMatrix = matrix( 0, nrow = 4, ncol = 4 ),
    strTestType = "Partly-Parametric"
  )
  vPValues <- c( H1 = 0.2, H2 = 0.4, H3 = 0.2, H4 = 0.2 )
  state <- AnalyzeLookCER( d, vRawPVals = vPValues )
  iRow <- which(
    state$results$stage1$cer_pcer$data$SubSets == "P :3,4,NP:2"
  )
  testthat::expect_equal(
    unname( state$results$stage1$cer_pcer$data$PCER[[ iRow ]][[ 1L ]] ),
    0.0009919540885684,
    tolerance = 1e-8
  )
} )

testthat::test_that( "CER API accepts full Look 2 p-value vectors after Look 1 rejection", {
  d <- SetupDesignCER(
    nArms = 3,
    nEps = 1,
    nSampleSize = 300,
    lEpTypes = list( EP1 = "Continuous" ),
    lSigma = list( EP1 = c( 1, 1, 1 ) ),
    bCommonStdDev = TRUE,
    lCtrlProps = list( EP1 = NA ),
    vAllocRatios = c( 1, 1, 1 ),
    vWeights = c( 0.5, 0.5 ),
    mGMatrix = matrix( c( 0, 1, 1, 0 ), nrow = 2, byrow = TRUE ),
    strTestType = "Parametric"
  )
  state1 <- AnalyzeLookCER( d, vRawPVals = c( H1 = 0.00001, H2 = 0.02 ) )
  state2 <- AnalyzeLookCER( d, state1, vRawPVals = c( H1 = 0.0001, H2 = 0.01 ) )

  testthat::expect_equal( state1$active_hypotheses, "H2" )
  testthat::expect_equal( state2$completed_looks, 2L )
} )

testthat::test_that( "CER API rejects invalid design and look inputs", {
  testthat::expect_error(
    SetupDesignCER( vInfoFracts = c( .5, .75, 1 ) ),
    "two"
  )
  testthat::expect_error(
    SetupDesignCER( dAlpha = 1 ),
    "alpha"
  )
  d <- SetupDesignCER( nEps = 1, lEpTypes = list( EP1 = "Continuous" ),
                        lSigma = list( EP1 = c( 1, 1, 1 ) ),
                        lCtrlProps = list( EP1 = NA ), bPlotGraphs = FALSE )
  testthat::expect_error(
    AnalyzeLookCER( d, vRawPVals = c( H1 = .1 ) ),
    "names"
  )
  state <- AnalyzeLookCER( d, vRawPVals = c( H1 = .1, H2 = .2 ) )
  testthat::expect_error(
    AnalyzeLookCER( d, state, vRawPVals = c( H1 = .1, H2 = .2 ),
                     vNewSampleSizes = c( Control = 1, Treatment1 = 1,
                                          Treatment2 = 1 ) ),
    "Look 1"
  )
  testthat::expect_false(
    "stage2_cumulative_sample_size" %in% names( formals( AnalyzeLookCER ) )
  )
  state$planned_sample_allocation <- state$planned_sample_allocation[ , -1L,
                                                                       drop = FALSE ]
  testthat::expect_error(
    AnalyzeLookCER( d, state, vRawPVals = c( H1 = .1, H2 = .2 ) ),
    "sample allocations"
  )
} )
