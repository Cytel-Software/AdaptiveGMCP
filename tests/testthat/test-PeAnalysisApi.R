testthat::test_that( "SetupAnalysis_PE_PC computes Martin correlation and delegates to SetupAnalysis_PC", {
  wi <- c( 0.7, 0.3 )
  g <- matrix( c( 0, 1, 1, 0 ), byrow = TRUE, nrow = 2 )
  infoFrac <- c( 0.5, 1 )

  fullD <- 100
  full0 <- 100
  subD <- 50
  sub0 <- 50

  dExpectedRho <- sqrt( ( 1 / fullD + 1 / full0 ) / ( 1 / subD + 1 / sub0 ) )

  peState <- SetupAnalysis_PE_PC(
    WI = wi,
    G = g,
    test.type = "Partly-Parametric",
    alpha = 0.025,
    info_frac = infoFrac,
    typeOfDesign = "asOF",
    full_d = fullD,
    full_0 = full0,
    sub_d = subD,
    sub_0 = sub0,
    plotGraphs = FALSE
  )

  manualState <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = "Partly-Parametric",
    alpha = 0.025,
    info_frac = infoFrac,
    typeOfDesign = "asOF",
    Correlation = matrix( c( 1, dExpectedRho, dExpectedRho, 1 ), byrow = TRUE, nrow = 2 ),
    plotGraphs = FALSE
  )

  testthat::expect_s3_class( peState, "PCAnalysisState" )
  testthat::expect_equal( peState$pe_metadata$martin_rho, dExpectedRho )
  testthat::expect_equal( unname( peState$mcpObj$Correlation[ "H1", "H2" ] ), dExpectedRho )
  testthat::expect_equal( peState$mcpObj$Correlation, manualState$mcpObj$Correlation )
  testthat::expect_equal( peState$mcpObj$bdryTab, manualState$mcpObj$bdryTab )
} )

testthat::test_that( "AnalyzeLook_PE_PC delegates per-look computation to AnalyzeLook_PC", {
  wi <- c( 0.7, 0.3 )
  g <- matrix( c( 0, 1, 1, 0 ), byrow = TRUE, nrow = 2 )
  infoFrac <- c( 0.5, 1 )

  fullD <- 120
  full0 <- 120
  subD <- 60
  sub0 <- 60

  dRho <- sqrt( ( 1 / fullD + 1 / full0 ) / ( 1 / subD + 1 / sub0 ) )

  peState <- SetupAnalysis_PE_PC(
    WI = wi,
    G = g,
    test.type = "Partly-Parametric",
    alpha = 0.025,
    info_frac = infoFrac,
    typeOfDesign = "asOF",
    full_d = fullD,
    full_0 = full0,
    sub_d = subD,
    sub_0 = sub0,
    plotGraphs = FALSE
  )

  manualState <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = "Partly-Parametric",
    alpha = 0.025,
    info_frac = infoFrac,
    typeOfDesign = "asOF",
    Correlation = matrix( c( 1, dRho, dRho, 1 ), byrow = TRUE, nrow = 2 ),
    plotGraphs = FALSE
  )

  peState <- AnalyzeLook_PE_PC(
    state = peState,
    p_raw = c( H1 = 0.01, H2 = 0.04 ),
    plotGraphs = FALSE
  )

  manualState <- AnalyzeLook_PC(
    state = manualState,
    p_raw = c( H1 = 0.01, H2 = 0.04 ),
    plotGraphs = FALSE
  )

  testthat::expect_equal( peState$mcpObj$AdjPValues, manualState$mcpObj$AdjPValues )
  testthat::expect_equal( peState$mcpObj$rej_flag_Curr, manualState$mcpObj$rej_flag_Curr )
  testthat::expect_equal( peState$completed_looks, manualState$completed_looks )
} )

testthat::test_that( "SetupAnalysis_PE_PC validates sample-size constraints", {
  wi <- c( 0.7, 0.3 )
  g <- matrix( c( 0, 1, 1, 0 ), byrow = TRUE, nrow = 2 )

  testthat::expect_error(
    SetupAnalysis_PE_PC(
      WI = wi,
      G = g,
      full_d = 50,
      full_0 = 50,
      sub_d = 60,
      sub_0 = 40,
      plotGraphs = FALSE
    ),
    "Subgroup sample sizes must not exceed full-population sample sizes"
  )
} )