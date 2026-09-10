# --------------------------------------------------------------------------------------------------
#
# ©2026 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

# Script to generate RDS regression fixtures for the
# Overall_Powers_df tests, replacing the earlier expect_snapshot()/.md approach.
# Each scenario below must stay in sync with the corresponding test file's
# simMAMSMEP() call.

suppressPackageStartupMessages({
  library(devtools)
})

load_all(quiet = TRUE)

#------ -------- -------- -------- -------- -------- -------- -------- --------
# GenerateOverallPowersFixtures
#------ -------- -------- -------- -------- -------- -------- -------- --------
GenerateOverallPowersFixtures <- function()
{
  strOutDir <- file.path( "tests", "testthat" )
  if( !dir.exists( strOutDir ) ) dir.create( strOutDir, recursive = TRUE )

  # Fixture for test-overall_powers_df_snapshot_pvaluecomb.R (3-arm, 2-ep)
  nArms <- 3
  nEps <- 2
  alpha <- 0.025
  info_frac <- c( 0.5, 1 )

  result1 <- simMAMSMEP(
    Method = "CombPValue",
    alpha = 0.025,
    SampleSize = 500,
    TestStatCont = "t-equal",
    TestStatBin = "UnPooled",
    FWERControl = "None",
    nArms = nArms,
    nEps = nEps,
    lEpType = list( EP1 = "Continuous", EP2 = "Binary" ),
    Arms.Mean = list( EP1 = c( 0, 0.4, 0.3 ), EP2 = NA ),
    Arms.std.dev = list( EP1 = c( 1.1, 1.2, 1.3 ), EP2 = NA ),
    CommonStdDev = FALSE,
    Arms.Prop = list( EP1 = NA, EP2 = c( 0.2, 0.35, 0.45 ) ),
    Arms.alloc.ratio = c( 1, 1, 1 ),
    EP.Corr = matrix( c( 1, 0.5, 0.5, 1 ), nrow = 2 ),
    WI = c( 0.5, 0.5, 0, 0 ),
    G = matrix( c( 0, 0.5, 0.5, 0, 0.5, 0, 0, 0.5, 0, 1, 0, 0, 1, 0, 0, 0 ), nrow = nEps *
                  ( nArms - 1 ), byrow = TRUE ),
    test.type = "Partly-Parametric",
    info_frac = c( 0.5, 1 ),
    typeOfDesign = "asOF",
    deltaWT = 0,
    deltaPT1 = 0,
    gammaA = 2,
    userAlphaSpending = rpact::getDesignGroupSequential( sided = 1, alpha = alpha,
                                                         informationRates = info_frac, typeOfDesign = "asOF" )$alphaSpent,
    MultipleWinners = TRUE,
    Selection = TRUE,
    SelectionLook = 1,
    SelectEndPoint = 1,
    SelectionScale = "pvalue",
    SelectionCriterion = "best",
    SelectionParameter = 1,
    KeepAssociatedHypo = TRUE,
    ImplicitSSR = "All",
    nSimulation = 100,
    nSimulation_Stage2 = 1,
    Seed = 100,
    SummaryStat = FALSE,
    plotGraphs = FALSE,
    EastSumStat = NULL,
    Parallel = FALSE
  )

  saveRDS( result1$Overall_Powers_df, file.path( strOutDir, "overall_powers_df_pvaluecomb.rds" ), compress = "xz" )
  cat( "Saved overall_powers_df_pvaluecomb.rds\n" )

  # Fixture for test-overall_powers_df_snapshot_pvaluecomb_5arm2ep.R (5-arm, 2-ep)
  nArms <- 5
  nEps <- 2
  alpha <- 0.025
  info_frac <- c( 0.5, 1 )

  result2 <- simMAMSMEP(
    Method = "CombPValue",
    alpha = alpha,
    SampleSize = 500,
    TestStatCont = "t-equal",
    TestStatBin = "UnPooled",
    FWERControl = "None",
    nArms = nArms,
    nEps = nEps,
    lEpType = list( EP1 = "Continuous", EP2 = "Continuous" ),
    Arms.Mean = list( "EP1" = c( 0, 0, 0, 0.4, 0.4 ), "EP2" = c( 0, 0, 0, 0.4, 0.4 ) ),
    Arms.std.dev = list( "EP1" = c( 1, 1, 1, 1, 1 ), "EP2" = c( 1, 1, 1, 1, 1 ) ),
    CommonStdDev = FALSE,
    Arms.Prop = list( EP1 = NA, EP2 = NA ),
    Arms.alloc.ratio = c( 1, 1, 1, 1, 1 ),
    EP.Corr = matrix( c( 1, 0.75, 0.75, 1 ), nrow = 2 ),
    WI = c( 0.25, 0.25, 0.25, 0.25, 0, 0, 0, 0 ),
    G = matrix( c( 0, 0, 0, 0, 1, 0, 0, 0,
                   0, 0, 0, 0, 0, 1, 0, 0,
                   0, 0, 0, 0, 0, 0, 1, 0,
                   0, 0, 0, 0, 0, 0, 0, 1,
                   0, 1/3, 1/3, 1/3, 0, 0, 0, 0,
                   1/3, 0, 1/3, 1/3, 0, 0, 0, 0,
                   1/3, 1/3, 0, 1/3, 0, 0, 0, 0,
                   1/3, 1/3, 1/3, 0, 0, 0, 0, 0 ),
                nrow = nEps * ( nArms - 1 ), byrow = TRUE ),
    test.type = "Partly-Parametric",
    info_frac = info_frac,
    typeOfDesign = "asOF",
    deltaWT = 0,
    deltaPT1 = 0,
    gammaA = 2,
    userAlphaSpending = rpact::getDesignGroupSequential( sided = 1, alpha = alpha,
                                                         informationRates = info_frac, typeOfDesign = "asOF" )$alphaSpent,
    MultipleWinners = TRUE,
    Selection = TRUE,
    SelectionLook = 1,
    SelectEndPoint = 1,
    SelectionScale = "teststat",
    SelectionCriterion = "threshold",
    SelectionParameter = -0.6745,
    KeepAssociatedHypo = TRUE,
    ImplicitSSR = "Selection",
    nSimulation = 10,
    nSimulation_Stage2 = 1,
    Seed = 4817,
    SummaryStat = FALSE,
    plotGraphs = FALSE,
    EastSumStat = NULL,
    Parallel = FALSE
  )

  saveRDS( result2$Overall_Powers_df, file.path( strOutDir, "overall_powers_df_pvaluecomb_5arm2ep.rds" ), compress = "xz" )
  cat( "Saved overall_powers_df_pvaluecomb_5arm2ep.rds\n" )

  # Fixture for test-overall_powers_df_snapshot_CER.R (5-arm, 2-ep)
  nArms <- 5
  nEps <- 2
  alpha <- 0.025

  Arms.Mean <- list( "EP1" = c( 0, 0.4, 0.4, 0.4, 0.4 ),
                     "EP2" = c( 0, 0.4, 0.4, 0.4, 0.4 ) )
  Arms.std.dev <- list( "EP1" = c( 1, 1, 1, 1, 1 ),
                        "EP2" = c( 1, 1, 1, 1, 1 ) )
  Arms.alloc.ratio <- c( 1, 1, 1, 1, 1 )
  EP.Corr <- matrix( c( 1, 0.75, 0.75, 1 ), nrow = 2 )
  WI <- c( 0.25, 0.25, 0.25, 0.25, 0, 0, 0, 0 )
  ws <- 0.75
  wp <- ( 1 - ws ) / 3
  m <- rbind(
    H1 = c( 0, wp, wp, wp, ws, 0, 0, 0 ),
    H2 = c( wp, 0, wp, wp, 0, ws, 0, 0 ),
    H3 = c( wp, wp, 0, wp, 0, 0, ws, 0 ),
    H4 = c( wp, wp, wp, 0, 0, 0, 0, ws ),
    H5 = c( 0, 1 / 3, 1 / 3, 1 / 3, 0, 0, 0, 0 ),
    H6 = c( 1 / 3, 0, 1 / 3, 1 / 3, 0, 0, 0, 0 ),
    H7 = c( 1 / 3, 1 / 3, 0, 1 / 3, 0, 0, 0, 0 ),
    H8 = c( 1 / 3, 1 / 3, 1 / 3, 0, 0, 0, 0, 0 )
  )
  G <- matrix( m, nrow = 8, byrow = FALSE )

  result3 <- simMAMSMEP(
    alpha = alpha,
    SampleSize = 500,
    nArms = nArms,
    nEps = nEps,
    lEpType = list( "EP1" = "Continuous", "EP2" = "Continuous" ),
    TestStatCon = "t-equal",
    TestStatBin = "UnPooled",
    FWERControl = "None",
    Arms.Mean = Arms.Mean,
    Arms.std.dev = Arms.std.dev,
    CommonStdDev = FALSE,
    Arms.Prop = list( "EP1" = NA, "EP2" = c( 0.1, 0.4, 0.4, 0.4 ) ),
    Arms.alloc.ratio = Arms.alloc.ratio,
    EP.Corr = EP.Corr,
    WI = WI,
    G = G,
    test.type = "Partly-Parametric",
    info_frac = c( 1 / 2, 1 ),
    typeOfDesign = "asOF",
    MultipleWinners = TRUE,
    Selection = TRUE,
    SelectionLook = 1,
    SelectEndPoint = 1,
    SelectionScale = "pvalue",
    SelectionCriterion = "threshold",
    SelectionParameter = 0.75,
    KeepAssociatedHypo = TRUE,
    ImplicitSSR = "Selection",
    nSimulation = 10,
    nSimulation_Stage2 = 10,
    Seed = 1354,
    SummaryStat = FALSE,
    Method = "CER",
    plotGraphs = FALSE,
    Parallel = FALSE
  )

  saveRDS( result3$Overall_Powers_df, file.path( strOutDir, "overall_powers_df_CER.rds" ), compress = "xz" )
  cat( "Saved overall_powers_df_CER.rds\n" )

  return( invisible( TRUE ) )
}

# IMPORTANT: Uncomment and execute the following line only to regenerate the fixtures.
# Not otherwise as the fixtures are already checked into the repository and should not be changed unless necessary.
# GenerateOverallPowersFixtures()
