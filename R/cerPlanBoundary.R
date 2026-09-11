# --------------------------------------------------------------------------------------------------
#
# ©2025 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

#### Compute plan boundary for parametric, non-parametric and mixed case
planBdryCER <- function(nHypothesis, nEps, nLooks, alpha, info_frac,
                        typeOfDesign, deltaWT, deltaPT1, gammaA,
                        userAlphaSpending, test.type, Sigma, WH,
                        HypoMap, Scale, planSSCum, mvtnorm_algo) {
  Method <- c()
  SubSets <- c()
  SignfLevelStage1 <- SignfLevelStage2 <- c()

  if(typeOfDesign == "WT"){
    alpha1 <- rpact::getDesignGroupSequential(
      kMax = nLooks, alpha = alpha,
      informationRates = info_frac,
      typeOfDesign = typeOfDesign,
      deltaWT = deltaWT
    )$alphaSpent[1]
  }else if(typeOfDesign == "PT"){
    alpha1 <- rpact::getDesignGroupSequential(
      kMax = nLooks, alpha = alpha,
      informationRates = info_frac,
      typeOfDesign = typeOfDesign,
      deltaPT1 = deltaPT1
    )$alphaSpent[1]
  }else if(typeOfDesign == "asHSD" || typeOfDesign == "asKD"){
    alpha1 <- rpact::getDesignGroupSequential(
      kMax = nLooks, alpha = alpha,
      informationRates = info_frac,
      typeOfDesign = typeOfDesign,
      gammaA = gammaA
    )$alphaSpent[1]
  }else if(typeOfDesign == "asUser"){
    alpha1 <- rpact::getDesignGroupSequential(
      kMax = nLooks, alpha = alpha,
      informationRates = info_frac,
      typeOfDesign = typeOfDesign,
      userAlphaSpending = userAlphaSpending
    )$alphaSpent[1]
  }else{
    alpha1 <- rpact::getDesignGroupSequential(
      kMax = nLooks, alpha = alpha,
      informationRates = info_frac,
      typeOfDesign = typeOfDesign
    )$alphaSpent[1]
  }


  Stage1Bdry <- Stage2Bdry <- matrix(0, nrow = nrow(WH), ncol = nHypothesis)
  colnames(Stage1Bdry) <- paste("a", 1:nHypothesis, "1", sep = "")
  colnames(Stage2Bdry) <- paste("a", 1:nHypothesis, "2", sep = "")
#browser()
  for (i in 1:nrow(WH))
  {
    #print(i)
    J <- as.numeric(WH[i, 1:nHypothesis])
    w <- as.numeric(WH[i, (nHypothesis + 1):(2 * nHypothesis)])

    get_Sets <- connSets(J = J, w = w, test.type = test.type, HypoMap = HypoMap)
    conn_Sets <- get_Sets$connSets

    conn_Sets_name <- paste("(", paste(
      unlist(lapply(conn_Sets, function(x) {
        paste(x, collapse = ",")
      })),
      collapse = "),("
    ), ")", sep = "")

    SubSets <- c(SubSets, conn_Sets_name)
    Method <- c(Method, get_Sets$Method)
    siglev1 <- siglev2 <- list()
    siglev1 <- append(siglev1, round(alpha1, 10))
    siglev2 <- append(siglev2, alpha)
    wJ <- as.numeric(w) # wJ : weights for the intersection J(including weights with 0)

    #=================================================================
    #Define function for total brdy crossing probability at stage 1
    #=================================================================
    totalbrdycrossingprobstage1 = function(cJ1){
      # ##########################
      # browser()
      # ##########################

    #siglev <- append(siglev, paste("S1:", alpha1))
    exitproblist = numeric()

    for (edx in conn_Sets) {
      if (length(edx) > 1) {
        ## Dunnett Weighted Parametric ##
        gIDX <- unique(HypoMap[edx, ]$Groups) # which endpoint
        #Exist probability for parametric sub-set at stage 1
        exitproblist <- c(exitproblist,
                          exitProbParamStage1(gIDX = gIDX, hIDX = edx, cJ1, wJ = wJ, 
                            Sigma, Scale, mvtnorm_algo = mvtnorm_algo, 
                            underNull = TRUE))

      } else {
        ## Non-Parametric ##
        #Exist probability for non parametric sub-set at stage 1
        exitproblist <- c(exitproblist, exitProbNParamStage1(cJ1, wJ, hIDX = edx))
      }
    }
    return(sum(na.omit(exitproblist)))
    }

    # Define the function to find the root
    searchcJ1 <- function(x) {
      totalbrdycrossingprobstage1(cJ1 = x) - alpha1
    }

    # Find the root using uniroot
    cJ1 <- uniroot(f = searchcJ1, interval = c(alpha1 * 0.999, alpha1 / max(w[w != 0])), tol = 1E-6)$root

    # Update Stage1 Bdry matrix
    Stage1Bdry[i, as.numeric(unlist(conn_Sets))] <- cJ1 * w[as.numeric(unlist(conn_Sets))]

    #=================================================================
    #Define function for total brdy crossing probability at stage 2
    #=================================================================
   totalbrdycrossingprobstage2 = function(cJ2){
     # #########################
     # browser()
     # #########################
     #siglev <- append(siglev, paste("S2:", alpha))
     exitproblist = numeric()
     for (edx in conn_Sets) {
       if (length(edx) > 1) {
         ## Dunnett Weighted Parametric ##
         gIDX <- unique(HypoMap[edx, ]$Groups)
         #Exist probability for parametric sub-set at stage 2
         exitproblist <- c(exitproblist,
                           exitProbStage2(gIDX = gIDX, hIDX = edx, cJ2, cJ1 = cJ1, wJ = wJ,
                                          Sigma = Sigma, Scale = Scale, 
                                          mvtnorm_algo = mvtnorm_algo, underNull = TRUE))
       } else {
         ## Non-Parametric ##
         PlanSSHyp <- getHypoSS(SS = planSSCum, HypoMap = HypoMap)
         ss1 = PlanSSHyp[[1]][edx]; ss2 = PlanSSHyp[[2]][edx]
         #Exist probability for non parametric sub-set at stage 2
         exitproblist <- c(exitproblist,
                           exitProbStage2Nparam2(cJ2, cJ1, ss1 = ss1, ss2 = ss2,
                                                 wJ, hIDX = edx, t1 = info_frac[1],
                                                 mvtnorm_algo = mvtnorm_algo))
       }
     }
     return(sum(na.omit(exitproblist)))
   }

    # Define the function to find the root
   searchcJ2 = function(x){
     totalbrdycrossingprobstage2(cJ2 = x) - alpha
   }

   # Find the root using uniroot
   cJ2 <- uniroot(f = searchcJ2 , interval = c((alpha - alpha1) * 0.999, alpha / max(w[w!= 0])), tol = 1E-6)$root

   # Update Stage1 Bdry matrix
   Stage2Bdry[i, as.numeric(unlist(conn_Sets))] <- cJ2 * w[as.numeric(unlist(conn_Sets))]

    SignfLevelStage1 <- c(
      SignfLevelStage1,
      paste("(", paste(
        unlist(lapply(siglev1, function(x) {
          paste(x, collapse = ",")
        })),
        collapse = "),("
      ), ")", sep = "")
    )

    SignfLevelStage2 <- c(
      SignfLevelStage2,
      paste("(", paste(
        unlist(lapply(siglev2, function(x) {
          paste(x, collapse = ",")
        })),
        collapse = "),("
      ), ")", sep = "")
    )
  }
  #### Preparation of output tables ###

  # Intersection Weights#
  InterWeightsTab <- WH

  # Weight Table Preparation for outputs
  HypoTab <- WH[, 1:(ncol(WH) / 2)]
  InterHyp <- apply(HypoTab, 1, function(h) {
    paste(names(HypoTab)[which(h == 1)], collapse = ",")
  })
  InterWeight <- apply(WH, 1, function(h) {
    J <- which(h[1:(length(h) / 2)] == 1)
    w <- h[((length(h) / 2) + 1):length(h)]
    paste(w[J], collapse = ",")
  })
  WeightTab <- data.frame(
    "Hypothesis" = InterHyp,
    "Weights" = InterWeight,
    row.names = NULL
  )


  # Test procedure
  TestProcedureTab <- cbind(
    WH[, 1:(ncol(WH) / 2)],
    SubSets,
    Method,
    SignfLevelStage2
  )

  TestProcedureTab1 <- data.frame(
    "Hypotheses" = InterHyp, "Weights" = InterWeight,
    "SubSets" = SubSets, "Method" = Method,
    row.names = NULL
  )
  TestProcedureTab1 <- knitr::kable(TestProcedureTab1, align = "c")
  # Stage-1 Boundary
  Stage1BdryTab <- cbind(WH[, 1:(ncol(WH) / 2)], Stage1Bdry)
  stg1bdry <- c()

  for (hypIDX in 1:nrow(Stage1Bdry)) {
    J <- which(WH[hypIDX, 1:(length(WH) / 2)] == 1)
    stg1bdry <- c(
      stg1bdry,
      paste(round(Stage1Bdry[hypIDX, J], 10), collapse = ",")
    )
  }

  Stage1BdryTab1 <- knitr::kable(data.frame(
    "Hypotheses" = InterHyp,
    "Alpha" = SignfLevelStage1,
    "Stage1Boundary" = stg1bdry,
    row.names = NULL
  ), align = "c")


  PlanBdryTab <- list(
    "Test_Procedure" = TestProcedureTab1,
    "Stage1_Boundary" = Stage1BdryTab1
  )
  if (nLooks == 2) {
    Stage2BdryTab <- cbind(WH[, 1:(ncol(WH) / 2)], Stage2Bdry)
    stg2bdry <- c()
    for (hypIDX in 1:nrow(Stage2Bdry)) {
      J <- which(WH[hypIDX, 1:(length(WH) / 2)] == 1)
      stg2bdry <- c(
        stg2bdry,
        paste(round(Stage2Bdry[hypIDX, J], 10), collapse = ",")
      )
    }
    Stage2BdryTab1 <- knitr::kable(data.frame(
      "Hypotheses" = InterHyp,
      "Alpha" = SignfLevelStage2,
      "Stage2Boundary" = stg2bdry,
      row.names = NULL
    ), align = "c")

    PlanBdryTab <- list(
      "Test_Procedure" = TestProcedureTab1,
      "Stage1_Boundary" = Stage1BdryTab1,
      "Stage2_Boundary" = Stage2BdryTab1
    )
  }

  list(
    "Stage1Bdry" = Stage1Bdry,
    "Stage2Bdry" = Stage2Bdry,
    "PlanBdryTable" = PlanBdryTab,
    "CriticalPoints" = c(cJ1, cJ2)
  )
}
