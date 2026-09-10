# --------------------------------------------------------------------------------------------------
#
# ©2025 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

library(testthat)
# library(AdaptGMCP)

test_that("p value combination simulation produces expected results", {
  nArms = 3
  nEps = 2
  alpha = 0.025
  info_frac = c(0.5, 1)

  result <- simMAMSMEP(
    Method = "CombPValue",
    alpha = 0.025,
    SampleSize = 500,
    TestStatCont = "t-equal",
    TestStatBin = "UnPooled",
    FWERControl = "None",
    nArms = nArms,
    nEps = nEps,
    lEpType = list(EP1 = "Continuous", EP2 = "Binary"),
    Arms.Mean = list(EP1 = c(0, 0.4, 0.3), EP2 = NA),
    Arms.std.dev = list(EP1 = c(1.1, 1.2, 1.3), EP2 = NA),
    CommonStdDev = FALSE,
    Arms.Prop = list(EP1 = NA, EP2 = c(0.2, 0.35, 0.45)),
    Arms.alloc.ratio = c(1, 1, 1),
    EP.Corr = matrix(c(1, 0.5, 0.5, 1), nrow = 2),
    WI = c(0.5, 0.5, 0, 0),
    G = matrix(c(0, 0.5, 0.5, 0, 0.5, 0, 0, 0.5, 0, 1, 0, 0, 1, 0, 0, 0), nrow = nEps *
                 (nArms - 1), byrow = T),
    test.type = "Partly-Parametric",
    info_frac = c(0.5, 1),
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


expected <- readRDS(testthat::test_path("overall_powers_df_pvaluecomb.rds"))
expect_equal(result$Overall_Powers_df, expected, tolerance = 1e-8)
})
