# --------------------------------------------------------------------------------------------------
#
# ©2025 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

# Perform Stage-1 Test -------------------------- -
PerformStage1Test <- function(nArms, nEps, EpType, nLooks, nHypothesis, 
    sigma, prop.ctr, allocRatio, SampleSize, alpha, info_frac, typeOfDesign, 
    deltaWT, deltaPT1, gammaA, des.type, test.type, Stage1Pvalues, HypoMap,
    CommonStdDev, WH, mvtnorm_algo) {
  # Stage-Wise Cumulative Sample Size
  SS_alloc <- getPlanAllocatedSamples(SS = SampleSize, allocRatio = allocRatio, info_frac = info_frac)
  SS_Cum <- SS_alloc$CumulativeSamples

  # Computed covariance matrix
  if (test.type == "Partly-Parametric" || test.type == "Parametric") {
    Sigma <- getSigma(
      SS_Cum = SS_Cum, EpType = EpType, sigma = sigma,
      prop.ctr = prop.ctr, allocRatio = allocRatio,
      CommonStdDev = CommonStdDev, info_frac = info_frac
    )
  } else {
    Sigma <- NA
  }

  # Planned Boundaries
  plan_Bdry <- planBdryCER(
    nHypothesis = nHypothesis, nEps = nEps, nLooks = nLooks,
    alpha = alpha, info_frac = info_frac, typeOfDesign = typeOfDesign,
    deltaWT = deltaWT, deltaPT1 = deltaPT1, gammaA = gammaA, userAlphaSpending = userAlphaSpending,
    test.type = test.type,
    Sigma = Sigma, WH = WH, HypoMap = HypoMap, Scale = "Score", planSSCum = SS_Cum,
    mvtnorm_algo = mvtnorm_algo
  )

  # Stage1 Analysis
  Stage1Analysis <- closedTest(
    WH = WH,
    boundary = plan_Bdry$Stage1Bdry,
    pValues = Stage1Pvalues,
    Stage1RejStatus = rep(FALSE, length(Stage1Pvalues))
  )

  intHypTab <- Stage1Analysis$IntersectHypoTest
  HypoTab <- intHypTab[, grep("H", names(intHypTab))]
  InterHyp <- apply(HypoTab, 1, function(h) {
    paste(names(HypoTab)[which(h == 1)], collapse = ",")
  })

  intRejStat <- sapply(intHypTab$Rejected, function(x) {
    ifelse(x, "Rejected", "Not_Rejected")
  })
  IntersectHypoTest <- knitr::kable(data.frame(
    "Hypotheses" = InterHyp,
    "Status" = intRejStat,
    row.names = NULL
  ), align = "c")

  rejTab <- Stage1Analysis$PrimaryHypoTest
  FinRejStat <- sapply(rejTab, function(x) {
    ifelse(x, "Rejected", "Not_Rejected")
  })
  FinalRejTab <- knitr::kable(data.frame(
    "Hypotheses" = names(rejTab),
    "Status" = FinRejStat,
    row.names = NULL
  ), align = "c")

  Stage1Tables <- list(
    "PlannedSampleSize(Cum.)" = SS_Cum,
    "Stage1_Tables" = plan_Bdry$PlanBdryTable,
    "Test_Intersection_Hypothesis" = IntersectHypoTest,
    "Final_Rejection_Status" = FinalRejTab
  )

  Stage1Obj <- list(
    "HypoMap" = HypoMap, "info_frac" = info_frac, "AllocSampleSize" = SS_Cum,
    "Sigma" = Sigma, "WH" = WH, "plan_Bdry" = plan_Bdry, "Stage1Analysis" = Stage1Analysis
  )

  list("Stage1Tables" = Stage1Tables, "Stage1Obj" = Stage1Obj)
}
