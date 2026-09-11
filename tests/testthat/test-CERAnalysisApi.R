# --------------------------------------------------------------------------------------------------
#
# Non-interactive CER API tests
#
# --------------------------------------------------------------------------------------------------

testthat::test_that( "SetupDesign_CER creates a design-only object", {
  d <- SetupDesign_CER(
    nArms = 3,
    nEps = 1,
    SampleSize = 300,
    EpType = list( EP1 = "Continuous" ),
    sigma = list( EP1 = c( 1, 1, 1 ) ),
    CommonStdDev = TRUE,
    prop.ctr = list( EP1 = NA ),
    allocRatio = c( 1, 1, 1 ),
    WI = c( 0.5, 0.5 ),
    G = matrix( c( 0, 1, 1, 0 ), nrow = 2, byrow = TRUE ),
    test.type = "Parametric"
  )

  testthat::expect_s3_class( d, "CERDesign" )
  testthat::expect_equal( dim( d$plan_boundary$Stage1Bdry ), c( 3L, 2L ) )
  testthat::expect_false( "completed_looks" %in% names( d ) )
  testthat::expect_false( "p_raw" %in% names( d ) )
} )

testthat::test_that( "CER API advances two looks without console input", {
  d <- SetupDesign_CER(
    nArms = 3,
    nEps = 1,
    SampleSize = 300,
    EpType = list( EP1 = "Continuous" ),
    sigma = list( EP1 = c( 1, 1, 1 ) ),
    CommonStdDev = TRUE,
    prop.ctr = list( EP1 = NA ),
    allocRatio = c( 1, 1, 1 ),
    WI = c( 0.5, 0.5 ),
    G = matrix( c( 0, 1, 1, 0 ), nrow = 2, byrow = TRUE ),
    test.type = "Parametric"
  )
  state1 <- AnalyzeLook_CER( d, p_raw = c( H1 = 0.1, H2 = 0.2 ) )
  state2 <- AnalyzeLook_CER( d, state1, p_raw = c( H1 = 0.0001, H2 = 0.2 ) )

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
  d <- SetupDesign_CER(
    nArms = 3,
    nEps = 2,
    SampleSize = 210,
    EpType = list( EP1 = "Continuous", EP2 = "Continuous" ),
    sigma = list( EP1 = c( 1, 1, 1 ), EP2 = c( 1, 1, 1 ) ),
    prop.ctr = list( EP1 = NA, EP2 = NA ),
    allocRatio = c( 1, 1, 1 ),
    WI = c( 0.5, 0.5, 0, 0 ),
    G = matrix(
      c( 0, .5, .5, 0, .5, 0, 0, .5, 0, 1, 0, 0, 1, 0, 0, 0 ),
      nrow = 4, byrow = TRUE
    ),
    test.type = "Parametric"
  )
  state1 <- AnalyzeLook_CER(
    d, p_raw = c( H1 = .00045, H2 = .0952, H3 = .0225, H4 = .1104 )
  )
  state2 <- AnalyzeLook_CER(
    d, state1, p_raw = c( H2 = .0299, H4 = .0586 ),
    selection = c( "H2", "H4" ),
    new_sample_size = c( Control = 88, Treatment2 = 87 ),
    new_weights = c( H2 = .5, H4 = .5 ),
    new_G = matrix( c( 0, 1, 1, 0 ), nrow = 2, byrow = TRUE )
  )

  testthat::expect_equal( state2$selected_hypotheses, c( "H2", "H4" ) )
  testthat::expect_equal( state2$adapted_sample_allocation[ 2, "Control" ], 88 )
  testthat::expect_equal( state2$adapted_sample_allocation[ 2, "Treatment2" ], 87 )
  testthat::expect_equal( state2$completed_looks, 2L )
  testthat::expect_true( is.list( state2$adaptation ) )
  testthat::expect_true( any( as.logical( unlist( state2$results$stage2$primary_rejection ) ) ) )
} )

testthat::test_that( "CER API supports non-parametric designs", {
  d <- SetupDesign_CER(
    nArms = 3, nEps = 1, SampleSize = 300,
    EpType = list( EP1 = "Binary" ),
    sigma = list( EP1 = rep( NA_real_, 3 ) ),
    prop.ctr = list( EP1 = 0.4 ),
    allocRatio = c( 1, 1, 1 ),
    WI = c( 0.5, 0.5 ),
    G = matrix( c( 0, 1, 1, 0 ), nrow = 2, byrow = TRUE ),
    test.type = "Non-Parametric"
  )

  testthat::expect_true( is.list( d$Sigma ) )
  state <- AnalyzeLook_CER( d, p_raw = c( H1 = 0.1, H2 = 0.2 ) )
  testthat::expect_s3_class( state, "CERAnalysisState" )
} )

testthat::test_that( "CER API supports binary-first mixed endpoints", {
  d <- SetupDesign_CER(
    nArms = 3, nEps = 2, SampleSize = 300,
    EpType = list( EP1 = "Binary", EP2 = "Continuous" ),
    sigma = list( EP1 = rep( NA_real_, 3 ), EP2 = rep( 1, 3 ) ),
    prop.ctr = list( EP1 = 0.4, EP2 = NA ),
    allocRatio = c( 1, 1, 1 ),
    WI = rep( 0.25, 4 ),
    G = matrix( 0, nrow = 4, ncol = 4 ),
    test.type = "Partly-Parametric"
  )

  testthat::expect_equal( length( d$Sigma$SigmaZ ), 2L )
  testthat::expect_equal( nrow( d$Sigma$SigmaZ[[ "EP2" ]] ), 4L )
} )

testthat::test_that( "CER computes later endpoint covariance rows within group", {
  d <- SetupDesign_CER(
    nArms = 4, nEps = 2, SampleSize = 400,
    EpType = list( EP1 = "Continuous", EP2 = "Continuous" ),
    sigma = list( EP1 = rep( 1, 4 ), EP2 = rep( 1, 4 ) ),
    prop.ctr = list( EP1 = NA, EP2 = NA ),
    allocRatio = c( 1, 2, 3, 4 ),
    WI = rep( 1 / 6, 6 ),
    G = matrix( 0, nrow = 6, ncol = 6 ),
    test.type = "Parametric"
  )
  state <- AnalyzeLook_CER(
    d, p_raw = setNames( rep( 0.2, 6 ), paste0( "H", 1:6 ) )
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
  d <- SetupDesign_CER(
    nArms = 3, nEps = 2, SampleSize = 300,
    EpType = list( EP1 = "Binary", EP2 = "Continuous" ),
    sigma = list( EP1 = rep( NA_real_, 3 ), EP2 = rep( 1, 3 ) ),
    prop.ctr = list( EP1 = 0.4, EP2 = NA ),
    allocRatio = c( 1, 2, 4 ),
    WI = rep( 0.25, 4 ),
    G = matrix( 0, nrow = 4, ncol = 4 ),
    test.type = "Partly-Parametric"
  )
  vPValues <- c( H1 = 0.2, H2 = 0.4, H3 = 0.2, H4 = 0.2 )
  state <- AnalyzeLook_CER( d, p_raw = vPValues )
  iRow <- which(
    state$results$stage1$cer_pcer$data$SubSets == "P :3,4,NP:2"
  )
  testthat::expect_equal(
    unname( state$results$stage1$cer_pcer$data$PCER[[ iRow ]][[ 1L ]] ),
    0.0009152201,
    tolerance = 1e-8
  )
} )

testthat::test_that( "CER API accepts full Look 2 p-value vectors after Look 1 rejection", {
  d <- SetupDesign_CER(
    nArms = 3,
    nEps = 1,
    SampleSize = 300,
    EpType = list( EP1 = "Continuous" ),
    sigma = list( EP1 = c( 1, 1, 1 ) ),
    CommonStdDev = TRUE,
    prop.ctr = list( EP1 = NA ),
    allocRatio = c( 1, 1, 1 ),
    WI = c( 0.5, 0.5 ),
    G = matrix( c( 0, 1, 1, 0 ), nrow = 2, byrow = TRUE ),
    test.type = "Parametric"
  )
  state1 <- AnalyzeLook_CER( d, p_raw = c( H1 = 0.00001, H2 = 0.02 ) )
  state2 <- AnalyzeLook_CER( d, state1, p_raw = c( H1 = 0.0001, H2 = 0.01 ) )

  testthat::expect_equal( state1$active_hypotheses, "H2" )
  testthat::expect_equal( state2$completed_looks, 2L )
} )

testthat::test_that( "CER API rejects invalid design and look inputs", {
  testthat::expect_error(
    SetupDesign_CER( info_frac = c( .5, .75, 1 ) ),
    "two"
  )
  testthat::expect_error(
    SetupDesign_CER( alpha = 1 ),
    "alpha"
  )
  d <- SetupDesign_CER( nEps = 1, EpType = list( EP1 = "Continuous" ),
                        sigma = list( EP1 = c( 1, 1, 1 ) ),
                        prop.ctr = list( EP1 = NA ), plotGraphs = FALSE )
  testthat::expect_error(
    AnalyzeLook_CER( d, p_raw = c( H1 = .1 ) ),
    "names"
  )
  state <- AnalyzeLook_CER( d, p_raw = c( H1 = .1, H2 = .2 ) )
  testthat::expect_error(
    AnalyzeLook_CER( d, state, p_raw = c( H1 = .1, H2 = .2 ),
                     new_sample_size = c( Control = 1, Treatment1 = 1,
                                          Treatment2 = 1 ) ),
    "Look 1"
  )
  state$planned_sample_allocation <- state$planned_sample_allocation[ , -1L,
                                                                       drop = FALSE ]
  testthat::expect_error(
    AnalyzeLook_CER( d, state, p_raw = c( H1 = .1, H2 = .2 ) ),
    "sample allocations"
  )
} )
