# --------------------------------------------------------------------------------------------------
#
# ©2025 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

library(testthat)
# library(AdaptGMCP)

test_that("p value combination  results match benchmark for 5 arm 2 ep", {
  nArms = 5
  nEps = 2
  alpha = 0.025
  info_frac = c(0.5, 1)

  result <- simMAMSMEP(
    Method = "CombPValue",
    alpha = alpha,
    SampleSize = 500,
    TestStatCont = "t-equal",
    TestStatBin = "UnPooled",
    FWERControl = "None",
    nArms = nArms,
    nEps = nEps,
    lEpType = list(EP1 = "Continuous", EP2 = "Continuous"),
    Arms.Mean = list('EP1' = c(0,0,0,0.4, 0.4),'EP2' = c(0,0,0,0.4,0.4)),
    Arms.std.dev = list('EP1' = c(1,1,1, 1, 1),'EP2' = c(1,1,1, 1, 1)),
    CommonStdDev = FALSE,
    Arms.Prop = list(EP1 = NA, EP2 = NA),
    Arms.alloc.ratio = c(1,1,1,1,1),
    EP.Corr = matrix(c(1,0.75,0.75,1),nrow = 2),
    WI = c(0.25,0.25,0.25,0.25, 0, 0, 0, 0),
    G = matrix(c(0,0,0,0,1,0,0,0,
                 0,0,0,0,0,1,0,0,
                 0,0,0,0,0,0,1,0,
                 0,0,0,0,0,0,0,1,
                 0,1/3,1/3,1/3,0,0,0,0,
                 1/3,0,1/3,1/3,0,0,0,0,
                 1/3,1/3,0,1/3,0,0,0,0,
                 1/3,1/3,1/3,0,0,0,0,0),
               nrow = nEps*(nArms-1), byrow = TRUE),
    test.type = "Partly-Parametric",
    info_frac = info_frac,
    typeOfDesign = "asOF",
    deltaWT = 0,
    deltaPT1 = 0,
    gammaA = 2,
    userAlphaSpending = rpact::getDesignGroupSequential(sided = 1, alpha = alpha,
                                                        informationRates = info_frac, typeOfDesign = "asOF")$alphaSpent,
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


  expected <- readRDS(testthat::test_path("overall_powers_df_pvaluecomb_5arm2ep.rds"))
  expect_equal(result$Overall_Powers_df, expected, tolerance = 1e-8)
})
