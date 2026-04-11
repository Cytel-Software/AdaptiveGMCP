  # File: Batch_2Lk_AdaptGMCP_Sim_Bin.R
  # This file contains code for executing a batch of simulations for 2 look
  # GMCP designs with binary endpoints.

  library(tidyverse)

  #########################################
  # # Example for testing against adagraph
  # nSim <- 1
  # nSim2 <- 1
  # nEps <- 1
  # nArms <- 5
  # lEpType <- list('EP1' = 'Continuous')
  # means <- list('EP1' = c(0, 0, 0, 0, 0))
  # sigma <- list("EP1" = c(1, 1, 1, 1, 1))
  # nTotSS <- 500
  # alloc <- c(1, 1, 1, 1, 1)
  # EP.Corr <- c(1)
  # wi <- c(rep(1/4, 4))
  # g <- rbind(H1=c(0, 1/3, 1/3, 1/3), H2=c(1/3, 0, 1/3, 1/3),
  #            H3=c(1/3, 1/3, 0, 1/3), H4=c(1/3, 1/3, 1/3, 0))
  # t <- c(0.5, 1)
  #
  # bUseCC <- F
  # bParallel <- F
  #
  # out <- simMAMSMEP(
  #   Method = "CER", SampleSize = nTotSS, alpha = 0.025, TestStatCont = "z", CommonStdDev = T,
  #   TestStatBin = NA, FWERControl = "CombinationTest", nArms = nArms, nEps = nEps,
  #   lEpType = lEpType, Arms.Mean = means, Arms.std.dev = sigma,
  #   Arms.Prop = NA, Arms.alloc.ratio = alloc, EP.Corr = EP.Corr,
  #   WI = wi, G = g, test.type = "Partly-Parametric",
  #   info_frac = t, typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = FALSE,
  #   SelectionLook = NA, SelectEndPoint = NA, SelectionScale = NA, SelectionCriterion = NA,
  #   SelectionParameter = NA, KeepAssociatedHypo = NA, ImplicitSSR = "None",
  #   nSimulation = nSim, nSimulation_Stage2 = nSim2, Seed = 1234, SummaryStat = TRUE,
  #   plotGraphs = FALSE, Parallel = bParallel, UseCC = bUseCC)
  #
  # print(out)

  #####################################
  # nSim <- 100 # 1000 # 10000 # 50000
  # nSim2 <- 50 # 100 # 1000 # 1000
  # nEps <- 2
  # nArms <- 3
  # lEpType <- list('EP1' = 'Binary', 'EP2' = 'Binary')
  # props <- list('EP1' = c(0.4, 0.4, 0.4), 'EP2' = c(0.5, 0.5, 0.5))
  # # props <- list('EP1' = c(0.1, 0.1, 0.1))
  # # # props <- list('EP1' = c(0.4, 0.4, 0.4))
  # # # props <- list('EP1' = c(0.5, 0.5, 0.5))
  # # # nTotSS <- 162
  # alloc <- c(1,1,1)
  # EP.Corr <- matrix(c(1, 0.5, 0.5, 1), nrow = nEps) # c(1)
  # wi <- c(rep(1/4, 4)) # c(rep(1/2, 2))
  # g <- rbind(H1=c(0, 1/3, 1/3, 1/3), H2=c(1/3, 0, 1/3, 1/3),
  #            H3=c(1/3, 1/3, 0, 1/3), H4=c(1/3, 1/3, 1/3, 0)) # rbind(H1=c(0,1), H2=c(1,0))
  # t <- c(0.5, 1)
  #
  # nTotSS <- 3000 # 600 # 162
  # bUseCC <- T # F
  # bParallel <- T
  #
  # out <- simMAMSMEP(
  #   Method = "CER", SampleSize = nTotSS, alpha = 0.025, TestStatCont = NA, CommonStdDev = F,
  #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = nArms, nEps = nEps,
  #   lEpType = lEpType, Arms.Mean = NA, Arms.std.dev = NA,
  #   Arms.Prop = props, Arms.alloc.ratio = alloc, EP.Corr = EP.Corr,
  #   WI = wi, G = g, test.type = "Partly-Parametric",
  #   info_frac = t, typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = FALSE,
  #   SelectionLook = NA, SelectEndPoint = NA, SelectionScale = NA, SelectionCriterion = NA,
  #   SelectionParameter = NA, KeepAssociatedHypo = NA, ImplicitSSR = "None",
  #   nSimulation = nSim, nSimulation_Stage2 = nSim2, Seed = 1234, SummaryStat = TRUE,
  #   plotGraphs = FALSE, Parallel = bParallel, UseCC = bUseCC)
  #
  # print(out)
  #####################################

  # # nTotSS=600, all pi=0.4: FWER is preserved with and without CC.
  # # nTotSS=162, all pi=0.4: FWER not preserved without CC, but is preserved with CC.
  # # nTotSS=600, all pi=0.1: No CC - FWER preserved
  #
  # out <- simMAMSMEP(
  #   Method = "CER", alpha = 0.025, SampleSize = nTotSS, nArms = 3, nEps = 1,
  #   lEpType=list('EP1' = 'Binary'), TestStatBin = "UnPooled", UseCC = bUseCC,
  #   FWERControl = "CombinationTest",
  #   Arms.Prop = props,
  #   Arms.alloc.ratio = c(1, 1, 1), EP.Corr = matrix(1), WI = c(rep(1/2,2)),
  #   G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Partly-Parametric",
  #   info_frac = c(0.75,1), typeOfDesign = "asOF",
  #   MultipleWinners = T, Selection = F, SelectionLook = NA, SelectEndPoint = NA,
  #   SelectionScale = NA, SelectionCriterion = NA, SelectionParameter = NA,
  #   KeepAssociatedHypo = NA, ImplicitSSR = "None",
  #   nSimulation = nSim, nSimulation_Stage2 = nSim2, Seed = 1234, SummaryStat = T,
  #   plotGraphs = F, Parallel = bParallel
  # )
  #
  # print(out)


  ### HELP EXAMPLE ##############################################
      # Method <- 'CER'
      # SampleSize <- 400
      # alpha <- 0.025
      # nArms <- 4
      # nEps  <- 2
      # # nArms <- 3
      # # nEps  <- 3
      # # EpType <- list("EP1" = "Continuous",
      # #                "EP2" = "Continuous")
      # # EpType <- list("EP1" = "Binary",
      # #                "EP2" = "Binary")
      # EpType <- list("EP1" = "Continuous",
      #                "EP2" = "Binary")
      # # EpType <- list("EP1" = "Binary",
      # #                "EP2" = "Continuous",
      # #                "EP3" = "Binary")
      # TestStatCon <- "t-equal"
      # TestStatBin <- "Pooled" # "UnPooled"
      # FWERControl <- "CombinationTest" # None"
      # # Arms.Mean <- list('EP1' = c(0, 0.4, 0.4, 0.4),
      # #                   'EP2' = c(0, 0.4, 0.4, 0.4))
      # # Arms.Mean <- list('EP1' = NA,
      # #                   'EP2' = c(0, 0.4, 0.4),
      # #                   'EP3' = NA)
      # # Arms.Mean <- list('EP1' = NA, 'EP2' = NA)
      # Arms.Mean <- list('EP1' = c(0, 0.4, 0.4, 0.4), 'EP2' = NA)
      # # Arms.std.dev <- list('EP1' = c(1.1, 1.2, 1.3, 1.4),
      # #                      'EP2' = c(1.1, 1.2, 1.3, 1.4))
      # # Arms.std.dev <- list('EP1' = NA,
      # #                      'EP2' = c(1.1, 1.2, 1.3),
      # #                      'EP3' = NA)
      # # Arms.std.dev <- list('EP1' = NA, 'EP2' = NA)
      # Arms.std.dev <- list('EP1' = c(1.1, 1.2, 1.3, 1.4), 'EP2' = NA)
      #
      # # Arms.Prop <- list(EP1 = NA, EP2 = NA)
      # # Arms.Prop <- list(EP1 = c(0.2, 0.35, 0.45), EP2 = NA, EP3 = c(0.1, 0.1, 0.1))
      # # Arms.Prop <- list(EP1 = c(0.2, 0.35, 0.45, 0.2), EP3 = c(0.1, 0.1, 0.1, 0.1))
      # Arms.Prop <- list(EP1 = NA, EP2 = c(0.2, 0.35, 0.45, 0.2))
      #
      # CommonStdDev <- FALSE
      # Arms.alloc.ratio <- c(1, 1, 1, 1)
      # # Arms.alloc.ratio <- c(1, 1, 1)
      # EP.Corr <- matrix(c(1, 0.5,
      #                     0.5, 1),
      #                   nrow = nEps)
      # # EP.Corr <- matrix(c(1, 0.5, 0.5,
      # #                     0.5, 1, 0.5,
      # #                     0.5, 0.5, 1),
      # #                   nrow = nEps)
      # WI <-  c(1/3, 1/3, 1/3, 0, 0, 0)
      # G <- matrix(c(0,0,0,     1,0,0,
      #               0,0,0,     0,1,0,
      #               0,0,0,     0,0,1,
      #               0,1/2,1/2, 0,0,0,
      #               1/2,0,1/2, 0,0,0,
      #               1/2,1/2,0, 0,0,0),
      #             nrow = nEps*(nArms-1), byrow = TRUE)
      # test.type <- 'Partly-Parametric'
      # info_frac <-  c(1/2,1)
      # typeOfDesign <- "asOF"
      # MultipleWinners <- TRUE
      # Selection <- FALSE # TRUE
      # SelectionLook <- 1
      # SelectEndPoint <- 1
      # SelectionScale <- 'teststat'
      # SelectionCriterion <- 'threshold'
      # SelectionParameter <- 0.6745
      # KeepAssociatedHypo <- TRUE
      # ImplicitSSR <- 'Selection'
      # nSimulation <- 10
      # nSimulation_Stage2 <- 10
      # Seed <- 100
      # SummaryStat <- FALSE
      # plotGraphs <- FALSE
      # Parallel <- FALSE
      # out <- simMAMSMEP(
      #   alpha = alpha, SampleSize = SampleSize, nArms = nArms, nEps = nEps,lEpType=EpType,
      #   TestStatCon = TestStatCon, TestStatBin = TestStatBin, FWERControl = FWERControl,
      #   Arms.Mean = Arms.Mean, Arms.std.dev = Arms.std.dev, CommonStdDev = CommonStdDev,
      #   Arms.Prop = Arms.Prop, Arms.alloc.ratio = Arms.alloc.ratio,
      #   EP.Corr = EP.Corr, WI = WI, G = G, test.type = test.type, info_frac = info_frac,
      #   typeOfDesign = typeOfDesign, MultipleWinners = MultipleWinners,
      #   Selection = Selection, SelectionLook = SelectionLook, SelectEndPoint = SelectEndPoint, SelectionScale = SelectionScale,
      #   SelectionCriterion = SelectionCriterion, SelectionParameter = SelectionParameter, KeepAssociatedHypo = KeepAssociatedHypo,
      #   ImplicitSSR = ImplicitSSR, nSimulation = nSimulation, nSimulation_Stage2 = nSimulation_Stage2, Seed = Seed, SummaryStat = SummaryStat,
      #   Method = Method, plotGraphs = plotGraphs, Parallel = Parallel
      # )
      #
      # outPower <- out$Overall_Powers
      # outPower
  # HELP EXAMPLE OVER
  ###############################################################
    # nSim <- 5 # 50000
    # nSim2 <- 10 # 1000
    #
    # # ModelID=1, Scenario=No selection, Partly-Parametric
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 162, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "None", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.1, 0.1, 0.1)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Partly-Parametric",
    #   info_frac = c(0.75,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = FALSE,
    #   SelectionLook = NA, SelectEndPoint = NA, SelectionScale = NA, SelectionCriterion = NA,
    #   SelectionParameter = NA, KeepAssociatedHypo = NA, ImplicitSSR = "None", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 1234, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=2, Scenario=No selection, Partly-Parametric
  # ##$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  #   out <- simMAMSMEP(
  #     Method = "CER", SampleSize = 1000, alpha = 0.025, TestStatCont = NA, CommonStdDev = F,
  #     TestStatBin = "UnPooled", FWERControl = "None", nArms = 3, nEps = 1,
  #     lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
  #     Arms.Prop = list('EP1' = c(0.1, 0.1, 0.1)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
  #     WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Partly-Parametric",
  #     info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = FALSE,
  #     SelectionLook = NA, SelectEndPoint = NA, SelectionScale = NA, SelectionCriterion = NA,
  #     SelectionParameter = NA, KeepAssociatedHypo = NA, ImplicitSSR = "None", nSimulation = 10,
  #     nSimulation_Stage2 = 100, Seed = 1234, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
  #     UseCC = TRUE
  #   )
    # print(out)
    #
    # # ModelID=3, Scenario=Select Best=1, Partly-Parametric
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 162, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "None", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.1, 0.1, 0.1)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Partly-Parametric",
    #   info_frac = c(0.75,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "delta", SelectionCriterion = "best",
    #   SelectionParameter = 1, KeepAssociatedHypo = FALSE, ImplicitSSR = "None", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 1234, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=4, Scenario=Select Best=1, Partly-Parametric
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 162, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.1, 0.1, 0.1)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Partly-Parametric",
    #   info_frac = c(0.75,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "delta", SelectionCriterion = "best",
    #   SelectionParameter = 1, KeepAssociatedHypo = FALSE, ImplicitSSR = "None", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 1234, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=5, Scenario=No selection, Parametric
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 162, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "None", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.1, 0.1, 0.1)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.75,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = FALSE,
    #   SelectionLook = NA, SelectEndPoint = NA, SelectionScale = NA, SelectionCriterion = NA,
    #   SelectionParameter = NA, KeepAssociatedHypo = NA, ImplicitSSR = "None", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 1234, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=6, Scenario=No selection, Parametric
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 162, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.1, 0.1, 0.1)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.75,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = FALSE,
    #   SelectionLook = NA, SelectEndPoint = NA, SelectionScale = NA, SelectionCriterion = NA,
    #   SelectionParameter = NA, KeepAssociatedHypo = NA, ImplicitSSR = "None", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 1234, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=7, Scenario=Select Best=1, Parametric
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 162, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "None", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.1, 0.1, 0.1)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.75,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "delta", SelectionCriterion = "best",
    #   SelectionParameter = 1, KeepAssociatedHypo = FALSE, ImplicitSSR = "None", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 1234, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=8, Scenario=Select Best=1, Parametric
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 162, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.1, 0.1, 0.1)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.75,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "delta", SelectionCriterion = "best",
    #   SelectionParameter = 1, KeepAssociatedHypo = FALSE, ImplicitSSR = "None", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 1234, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=9, Scenario=No sel, Param, SS=600, all props=0.4, lk1=0.75, no CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 600, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.75,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = FALSE,
    #   SelectionLook = NA, SelectEndPoint = NA, SelectionScale = NA, SelectionCriterion = NA,
    #   SelectionParameter = NA, KeepAssociatedHypo = NA, ImplicitSSR = "None", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 64564, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = FALSE
    # )
    # print(out)
    #
    # # ModelID=10, Scenario=Sel Best=1, Param, SS=600, all props=0.4, SSR=None, lk1=0.75, no CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 600, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.75,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "delta", SelectionCriterion = "best",
    #   SelectionParameter = 1, KeepAssociatedHypo = FALSE, ImplicitSSR = "None", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 73236, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = FALSE
    # )
    # print(out)
    #
    # # ModelID=11, Scenario=Fut=0.5, Param, SS=600, all props=0.4, SSR=None, lk1=0.75, no CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 600, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.75,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "threshold",
    #   SelectionParameter = 0.5, KeepAssociatedHypo = FALSE, ImplicitSSR = "None", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 11111, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = FALSE
    # )
    # print(out)
    #
    # # ModelID=12, Scenario=Fut=0.75, no CC, Param, SS=600, all props=0.4, SSR=Sel, lk1=0.75, no CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 600, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.75,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "threshold",
    #   SelectionParameter = 0.75, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 343, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = FALSE
    # )
    # print(out)
    #
    # # ModelID=13, Scenario=Fut=0.5, Param, SS=600, all props=0.4, SSR=Sel, lk1=0.75, no CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 600, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.75,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "threshold",
    #   SelectionParameter = 0.5, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 84234, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = FALSE
    # )
    # print(out)
    #
    # # ModelID=14, Scenario=Fut=0.25, Param, SS=600, all props=0.4, SSR=Sel, lk1=0.75, no CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 600, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.75,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "threshold",
    #   SelectionParameter = 0.25, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 4346, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = FALSE
    # )
    # print(out)
    #
    # # ModelID=15, Scenario=Sel Best=1, Param, SS=600, all props=0.4, SSR=Sel, lk1=0.75, no CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 600, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.75,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "best",
    #   SelectionParameter = 1, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 9504, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = FALSE
    # )
    # print(out)
    #
    # # ModelID=16, Scenario=Sel Best=2, Param, SS=600, all props=0.4, SSR=Sel, lk1=0.75, no CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 600, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.75,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "best",
    #   SelectionParameter = 2, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 2300, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = FALSE
    # )
    # print(out)
    #
    # # ModelID=17, Scenario=No sel, Param, SS=400, all props=0.4, lk1=0.5, no CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = FALSE,
    #   SelectionLook = NA, SelectEndPoint = NA, SelectionScale = NA, SelectionCriterion = NA,
    #   SelectionParameter = NA, KeepAssociatedHypo = NA, ImplicitSSR = "None", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 97827, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = FALSE
    # )
    # print(out)
    #
    # # ModelID=18, Scenario=Sel Best=1, Param, SS=400, all props=0.4, SSR=None, lk1=0.5, no CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "delta", SelectionCriterion = "best",
    #   SelectionParameter = 1, KeepAssociatedHypo = FALSE, ImplicitSSR = "None", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 70743, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = FALSE
    # )
    # print(out)
    #
    # # ModelID=19, Scenario=Fut=0.5, Param, SS=400, all props=0.4, SSR=None, lk1=0.5, no CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "threshold",
    #   SelectionParameter = 0.5, KeepAssociatedHypo = FALSE, ImplicitSSR = "None", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 2402, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = FALSE
    # )
    # print(out)
    #
    # # ModelID=20, Scenario=Fut=0.5, no CC, Param, SS=400, all props=0.4, SSR=Sel, lk1=0.5, no CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "threshold",
    #   SelectionParameter = 0.75, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 2637, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = FALSE
    # )
    # print(out)
    #
    # # ModelID=21, Scenario=Fut=0.5, Param, SS=400, all props=0.4, SSR=Sel, lk1=0.5, no CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "threshold",
    #   SelectionParameter = 0.5, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 499, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = FALSE
    # )
    # print(out)
    #
    # # ModelID=22, Scenario=Fut=0.25, Param, SS=400, all props=0.4, SSR=Sel, lk1=0.5, no CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "threshold",
    #   SelectionParameter = 0.25, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 30953, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = FALSE
    # )
    # print(out)
    #
    # # ModelID=23, Scenario=Sel Best=1, Param, SS=400, all props=0.4, SSR=Sel, lk1=0.5, no CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "best",
    #   SelectionParameter = 1, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 59079, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = FALSE
    # )
    # print(out)
    #
    # # ModelID=24, Scenario=Sel Best=2, Param, SS=400, all props=0.4, SSR=Sel, lk1=0.5, no CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "best",
    #   SelectionParameter = 2, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 58217, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = FALSE
    # )
    # print(out)
    #
    # # ModelID=25, Scenario=No sel, Param, SS=400, all props=0.4, lk1=0.5, no CC, FWCtr=None
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "None", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = FALSE,
    #   SelectionLook = NA, SelectEndPoint = NA, SelectionScale = NA, SelectionCriterion = NA,
    #   SelectionParameter = NA, KeepAssociatedHypo = NA, ImplicitSSR = "None", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 97827, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = FALSE
    # )
    # print(out)
    #
    # # ModelID=26, Scenario=Sel Best=1, Param, SS=400, all props=0.4, SSR=None, lk1=0.5, no CC, FWCtr=None
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "None", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "delta", SelectionCriterion = "best",
    #   SelectionParameter = 1, KeepAssociatedHypo = FALSE, ImplicitSSR = "None", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 70743, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = FALSE
    # )
    # print(out)
    #
    # # ModelID=27, Scenario=Fut=0.5, Param, SS=400, all props=0.4, SSR=None, lk1=0.5, no CC, FWCtr=None
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "None", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "threshold",
    #   SelectionParameter = 0.5, KeepAssociatedHypo = FALSE, ImplicitSSR = "None", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 2402, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = FALSE
    # )
    # print(out)
    #
    # # ModelID=28, Scenario=Fut=0.5, no CC, FWCtr=None, Param, SS=400, all props=0.4, SSR=Sel, lk1=0.5, no CC, FWCtr=None
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "None", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "threshold",
    #   SelectionParameter = 0.75, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 2637, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = FALSE
    # )
    # print(out)
    #
    # # ModelID=29, Scenario=Fut=0.5, Param, SS=400, all props=0.4, SSR=Sel, lk1=0.5, no CC, FWCtr=None
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "None", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "threshold",
    #   SelectionParameter = 0.5, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 499, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = FALSE
    # )
    # print(out)
    #
    # # ModelID=30, Scenario=Fut=0.25, Param, SS=400, all props=0.4, SSR=Sel, lk1=0.5, no CC, FWCtr=None
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "None", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "threshold",
    #   SelectionParameter = 0.25, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 30953, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = FALSE
    # )
    # print(out)
    #
    # # ModelID=31, Scenario=Sel Best=1, Param, SS=400, all props=0.4, SSR=Sel, lk1=0.5, no CC, FWCtr=None
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "None", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "best",
    #   SelectionParameter = 1, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 59079, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = FALSE
    # )
    # print(out)
    #
    # # ModelID=32, Scenario=Sel Best=2, Param, SS=400, all props=0.4, SSR=Sel, lk1=0.5, no CC, FWCtr=None
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "None", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "best",
    #   SelectionParameter = 2, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 58217, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = FALSE
    # )
    # print(out)
    #
    # # ModelID=33, Scenario=No sel, Param, SS=600, all props=0.4, lk1=0.75, CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 600, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.75,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = FALSE,
    #   SelectionLook = NA, SelectEndPoint = NA, SelectionScale = NA, SelectionCriterion = NA,
    #   SelectionParameter = NA, KeepAssociatedHypo = NA, ImplicitSSR = "None", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 64564, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=34, Scenario=Sel Best=1, Param, SS=600, all props=0.4, SSR=None, lk1=0.75, CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 600, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.75,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "delta", SelectionCriterion = "best",
    #   SelectionParameter = 1, KeepAssociatedHypo = FALSE, ImplicitSSR = "None", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 73236, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=35, Scenario=Fut=0.5, Param, SS=600, all props=0.4, SSR=None, lk1=0.75, CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 600, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.75,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "threshold",
    #   SelectionParameter = 0.5, KeepAssociatedHypo = FALSE, ImplicitSSR = "None", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 11111, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=36, Scenario=Fut=0.75, CC, Param, SS=600, all props=0.4, SSR=Sel, lk1=0.75, CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 600, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.75,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "threshold",
    #   SelectionParameter = 0.75, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 343, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=37, Scenario=Fut=0.5, Param, SS=600, all props=0.4, SSR=Sel, lk1=0.75, CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 600, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.75,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "threshold",
    #   SelectionParameter = 0.5, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 84234, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=38, Scenario=Fut=0.25, Param, SS=600, all props=0.4, SSR=Sel, lk1=0.75, CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 600, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.75,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "threshold",
    #   SelectionParameter = 0.25, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 4346, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=39, Scenario=Sel Best=1, Param, SS=600, all props=0.4, SSR=Sel, lk1=0.75, CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 600, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.75,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "best",
    #   SelectionParameter = 1, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 9504, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=40, Scenario=Sel Best=2, Param, SS=600, all props=0.4, SSR=Sel, lk1=0.75, CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 600, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.75,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "best",
    #   SelectionParameter = 2, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 2300, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=41, Scenario=No sel, Param, SS=400, all props=0.4, lk1=0.5, CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = FALSE,
    #   SelectionLook = NA, SelectEndPoint = NA, SelectionScale = NA, SelectionCriterion = NA,
    #   SelectionParameter = NA, KeepAssociatedHypo = NA, ImplicitSSR = "None", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 97827, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=42, Scenario=Sel Best=1, Param, SS=400, all props=0.4, SSR=None, lk1=0.5, CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "delta", SelectionCriterion = "best",
    #   SelectionParameter = 1, KeepAssociatedHypo = FALSE, ImplicitSSR = "None", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 70743, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=43, Scenario=Fut=0.5, Param, SS=400, all props=0.4, SSR=None, lk1=0.5, CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "threshold",
    #   SelectionParameter = 0.5, KeepAssociatedHypo = FALSE, ImplicitSSR = "None", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 2402, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=44, Scenario=Fut=0.5, CC, Param, SS=400, all props=0.4, SSR=Sel, lk1=0.5, CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "threshold",
    #   SelectionParameter = 0.75, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 2637, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=45, Scenario=Fut=0.5, Param, SS=400, all props=0.4, SSR=Sel, lk1=0.5, CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "threshold",
    #   SelectionParameter = 0.5, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 499, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=46, Scenario=Fut=0.25, Param, SS=400, all props=0.4, SSR=Sel, lk1=0.5, CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "threshold",
    #   SelectionParameter = 0.25, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 30953, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=47, Scenario=Sel Best=1, Param, SS=400, all props=0.4, SSR=Sel, lk1=0.5, CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "best",
    #   SelectionParameter = 1, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 59079, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=48, Scenario=Sel Best=2, Param, SS=400, all props=0.4, SSR=Sel, lk1=0.5, CC
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "CombinationTest", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "best",
    #   SelectionParameter = 2, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 58217, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=49, Scenario=No sel, Param, SS=400, all props=0.4, lk1=0.5, CC, FWCtr=None
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "None", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = FALSE,
    #   SelectionLook = NA, SelectEndPoint = NA, SelectionScale = NA, SelectionCriterion = NA,
    #   SelectionParameter = NA, KeepAssociatedHypo = NA, ImplicitSSR = "None", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 97827, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=50, Scenario=Sel Best=1, Param, SS=400, all props=0.4, SSR=None, lk1=0.5, CC, FWCtr=None
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "None", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "delta", SelectionCriterion = "best",
    #   SelectionParameter = 1, KeepAssociatedHypo = FALSE, ImplicitSSR = "None", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 70743, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=51, Scenario=Fut=0.5, Param, SS=400, all props=0.4, SSR=None, lk1=0.5, CC, FWCtr=None
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "None", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "threshold",
    #   SelectionParameter = 0.5, KeepAssociatedHypo = FALSE, ImplicitSSR = "None", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 2402, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=52, Scenario=Fut=0.5, CC, FWCtr=None, Param, SS=400, all props=0.4, SSR=Sel, lk1=0.5, CC, FWCtr=None
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "None", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "threshold",
    #   SelectionParameter = 0.75, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 2637, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=53, Scenario=Fut=0.5, Param, SS=400, all props=0.4, SSR=Sel, lk1=0.5, CC, FWCtr=None
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "None", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "threshold",
    #   SelectionParameter = 0.5, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 499, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=54, Scenario=Fut=0.25, Param, SS=400, all props=0.4, SSR=Sel, lk1=0.5, CC, FWCtr=None
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "None", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "threshold",
    #   SelectionParameter = 0.25, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 30953, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=55, Scenario=Sel Best=1, Param, SS=400, all props=0.4, SSR=Sel, lk1=0.5, CC, FWCtr=None
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "None", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "best",
    #   SelectionParameter = 1, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 59079, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)
    #
    # # ModelID=56, Scenario=Sel Best=2, Param, SS=400, all props=0.4, SSR=Sel, lk1=0.5, CC, FWCtr=None
    # out <- simMAMSMEP(
    #   Method = "CER", SampleSize = 400, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    #   TestStatBin = "UnPooled", FWERControl = "None", nArms = 3, nEps = 1,
    #   lEpType = list('EP1' = 'Binary'), Arms.Mean = NA, Arms.std.dev = NA,
    #   Arms.Prop = list('EP1' = c(0.4, 0.4, 0.4)), Arms.alloc.ratio = c(1,1,1), EP.Corr = matrix(1),
    #   WI = c(rep(1/2,2)), G = rbind(H1=c(0,1), H2=c(1,0)), test.type = "Parametric",
    #   info_frac = c(0.5,1), typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    #   SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "best",
    #   SelectionParameter = 2, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection", nSimulation = nSim,
    #   nSimulation_Stage2 = nSim2, Seed = 58217, SummaryStat = TRUE, plotGraphs = FALSE, Parallel = TRUE,
    #   UseCC = TRUE
    # )
    # print(out)


  # ### 1 CONTINUOUS EP TEST ##########################
  # ## WITH TREATMENT SELECTION
  # out <- simMAMSMEP(
  #   Method = "CER", alpha = 0.025, SampleSize = 1000, nArms = 3, nEps = 1,
  #   lEpType=list('EP1' = 'Continuous'), TestStatCon = "t-equal", FWERControl = "None",
  #   Arms.Mean = list('EP1' = c(0, 0, 0)),
  #   Arms.std.dev = list('EP1' = c(5, 5, 5)), CommonStdDev = F,
  #   Arms.alloc.ratio = c(1, 1, 1), EP.Corr = matrix(1), WI = c(rep(1/2,2)),
  #   G = rbind(H1=c(0,1), H2=c(1,0)),
  #   test.type = "Parametric", info_frac = c(1/2,1), typeOfDesign = "asOF",
  #   MultipleWinners = T, Selection = F, SelectionLook = NA, SelectEndPoint = NA,
  #   SelectionScale = NA, SelectionCriterion = NA,
  #   SelectionParameter = NA, KeepAssociatedHypo = NA, ImplicitSSR = "None",
  #   nSimulation = 3, nSimulation_Stage2 = 100, Seed = 1234, SummaryStat = T,
  #   plotGraphs = F, Parallel = F
  # )
  #
  # print(out)

  ## WITHOUT TREATMENT SELECTION
  # out <- simMAMSMEP(
  #   Method = "CER", alpha = 0.025, SampleSize = 500, nArms = 5, nEps = 1,
  #   lEpType=list('EP1' = 'Continuous'), TestStatCon = "t-equal", FWERControl = "None",
  #   Arms.Mean = list('EP1' = c(0, 0, 0, 0, 0.25)),
  #   Arms.std.dev = list('EP1' = c(1, 1, 1, 1, 1)), CommonStdDev = F,
  #   Arms.alloc.ratio = c(1,0.5,0.5,0.5,0.5), EP.Corr = matrix(1), WI = c(rep(1/4,4)),
  #   G = rbind(H1=c(0,1/3,1/3,1/3), H2=c(1/3,0,1/3,1/3), H3=c(1/3,1/3,0,1/3), H4=c(1/3,1/3,1/3,0)),
  #   test.type = "Parametric", info_frac = c(1/2,1), typeOfDesign = "asOF",
  #   MultipleWinners = T, Selection = F, SelectionLook = NA, SelectEndPoint = NA,
  #   SelectionScale = NA, SelectionCriterion = NA, SelectionParameter = NA,
  #   KeepAssociatedHypo = NA, ImplicitSSR = "None",
  #   nSimulation = 3, nSimulation_Stage2 = 100, Seed = 1234, SummaryStat = T,
  #   plotGraphs = F, Parallel = F
  # )
  #
  # out
  #

  # ###################################################
  # We will use the function simMAMSMEP_Wrapper() for executing a batch.

  # dfInput <- read_csv("internalData/CER_Inp_1ep5arms - Continuous.csv")
  # dfInput <- read_csv("internalData/InputScenarios_2ep5arm.csv")
  # dfInput <- read_csv("internalData/CER_Inp_1ep5arms.csv")
  #dfInput <- read_csv("internalData/Inp_CER_Bin_1ep3arms.csv")
  # dfInput <- read_csv("internalData/Inp_CER_Bin_2eps3arms.csv")
  # dfInput <- read_csv("internalData/Inp_CER_Bin_2eps.csv")
  # dfInput <- read_csv("internalData/Inp_CER_Cont_1ep3arms.csv")
  # dfInput <- read_csv("internalData/TestsVsMartin_adagraph.csv")
  dfInput <- read_csv("internalData/Mixed-2OrMoreEPs.csv")

  # sOutFilePrefix <- "Out_CER_Bin_1ep3arms"
  sOutFilePrefix <- "Out_Mixed-2OrMoreEPs"
  # sOutFilePrefix <- "Out_CER_Cont_1ep3arms"
  sOutPath <- "internalData/"

  # nModelsToRun <- dfInput$ModelID # Run all models
  # nModelsToRun <- c(108, 109, 110, 111, 116, 117, 118, 119, 120, 121, 122, 123,
  #                  124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135,
  #                  136, 137, 138, 139)
  nModelsToRun <- c(26, 27)
  # nModelsToRun <- c(138, 137, 136, 134, 132, 130, 128, 127, 125, 139, 123, 121,
  #                  118, 117, 114, 111, 108)

  # TRIAL RUN - START >>>>>>>>>>>>
  # To do a trial run, uncomment this block so that the tests are run with a
  # small number of simulations rather than the number specified in the input
  # file.
  dfInput$nSimulation <- 10000 #1000 # 5 # 100 #
  dfInput$nSimulation_Stage2 <- 500 #100# 5 # 50 # 100 #
  # dfInput$Parallel <- FALSE
  # dfInput$test.type <- "Parametric"
  # dfInput$SampleSize <- 10000

  df <- dfInput %>% filter(ModelID %in% nModelsToRun)
  print(paste0("Method = ", df$Method[1])) # CER / CombPValue
  print(paste0("nSimulation = ", df$nSimulation[1]))
  print(paste0("nSimulation_Stage2 = ", df$nSimulation_Stage2[1]))
  print(paste0("Parallel = ", df$Parallel[1]))
  print(paste0("lEpType = ", df$lEpType[1]))
  print(paste0("test.type = ", df$test.type[1])) # Non-Parametric / Bonf
  print(paste0("Seed = ", df$Seed[1]))
  # TRIAL RUN - OVER >>>>>>>>>>>>

  tStartTime <- Sys.time()

  sTimeNow <- format(tStartTime, "%d%h%y-%H_%M")
  sOutPath <- paste0(sOutPath, sOutFilePrefix, "_", sTimeNow, ".csv")

  dfOutput <- simMAMSMEP_Wrapper(InputDF = dfInput %>%
                                   filter(ModelID %in% nModelsToRun),
                                 sOutPath)

  tElapTime <- Sys.time() - tStartTime

  dfOutput

  # #Save Output
  # sTimeNow <- format(Sys.time(), "%d%h%y-%H_%M")
  # sOutPath1 <- paste0(sOutPath, sOutFilePrefix, "_", sTimeNow, ".csv")
  # # sOutPath1 <- paste0(sOutPath, "Output_FS_GMCP_Sim_Bin", ".csv")
  # write.csv(dfOutput, sOutPath1, row.names = F)

  #Execution details
  SysInfo <- Sys.info()
  cat("Execution performed on ", SysInfo['nodename'],"\n",
      "Execution time", tElapTime)
