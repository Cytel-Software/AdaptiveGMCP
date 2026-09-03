# --------------------------------------------------------------------------------------------------
#
# ©2025 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

#Boundary computation based on the mcpObj
#ModifiedStage2Weights : Ajoy.M: Highly risky should be triggered only for modified testing strategy in analysis, implemeted for one specific example not generalized.(13Jun,24)
adaptBdryCER <- function(mcpObj, mvtnorm_algo, ModifiedStage2Weights = F) {
  # browser()
  nHypothesis <- length(mcpObj$IntialHypothesis)
  nLooks <- length(mcpObj$Stage1Obj$info_frac)

  ## The weights before updating the testing strategy
  WH_old <- mcpObj$WH_Prev

  ## The weights after modifying the testing strategy with the targeted intersect hypotheses
  WH_modified <- mcpObj$WH
  Stage2PlanBdry <- mcpObj$Stage1Obj$plan_Bdry$Stage2Bdry
  Stage1PlanBdry <- mcpObj$Stage1Obj$plan_Bdry$Stage1Bdry

  WH_modified_idx <- as.vector(apply(WH_modified[, grep("H", names(WH_modified))], 1, function(x) {
    paste(x, collapse = "")
  }))
  WH_old_idx <- as.vector(apply(WH_old[, grep("H", names(WH_old))], 1, function(x) {
    paste(x, collapse = "")
  }))

  # Computed covariance matrix
  # Bug fix: Earlier Stage2Sigma was calculated only in case of Partly-Parametric
  # and Parametric tests and it was set to NA in case of Non-Parametric.
  # However, this led to problems in the function SingleSimCER() as it
  # requires the fisher info matrix in all cases.
  # Fixed this bug by calculating Stage2Sigma unconditionally.
  # if (mcpObj$test.type == "Partly-Parametric" || mcpObj$test.type == "Parametric") {
    Stage2Sigma <- getStage2Sigma(
      nHypothesis = nHypothesis,
      EpType = mcpObj$lEpType,
      nLooks = nLooks,
      Sigma = mcpObj$Stage1Obj$Sigma,
      AllocSampleSize = mcpObj$AllocSampleSize,
      allocRatio = mcpObj$allocRatio,
      sigma = mcpObj$sigma,
      prop.ctr = mcpObj$prop.ctr,
      Stage2AllocSampleSize = mcpObj$Stage2AllocSampleSize,
      Stage2allocRatio = mcpObj$Stage2allocRatio,
      Stage2sigma = mcpObj$sigma,
      CommonStdDev = mcpObj$CommonStdDev
    )
  # } else {
  #   Stage2Sigma <- NA
  # }

  # planned sample size: for non-parametric tests
  PlanSSHyp <- getHypoSS(SS = mcpObj$AllocSampleSize,
                         HypoMap = mcpObj$HypoMap)
  # adapted sample size: for non-parametric tests
  ModSSHyp <- getHypoSS(SS = mcpObj$Stage2AllocSampleSize,
                        HypoMap = mcpObj$HypoMap)

  SubSets <- c()
  RestrictedSet <- c()
  ConditionalError <- c()
  Stage2AdjBdry <- c()
  InterWeight2 <- c()

  # browser()

  for (i in seq_len(nrow(WH_modified))) {
    #print(i)
    p1 <- as.numeric(mcpObj$p_raw)

    # Intersection hypothesis to test
    J <- as.numeric(WH_modified[i, grep("H", names(WH_modified))])
    oldIdx <- which(WH_old_idx == WH_modified_idx[i]) # Index for the WH_old corresponding to J
    w1 <- as.numeric(WH_old[oldIdx, grep("Weight", names(WH_old))])
    a2 <- as.numeric(Stage2PlanBdry[oldIdx, ])
    a1 <- as.numeric(Stage1PlanBdry[oldIdx, ])

    # Available hypothesis for stage-2
    I2 <- get_numeric_part(mcpObj$IndexSet)
    J2 <- rep(0, length(J))
    J2[I2] <- 1
    J2 <- J * J2 # Weights for the intersection J,J2

    modIdx <- which(WH_modified_idx == paste(J2, collapse = "")) # Index for the WH_old corresponding to J
    w2 <- as.numeric(WH_modified[modIdx, grep("Weight", names(WH_modified))])
    InterWeight2 <- c(InterWeight2, paste(w2[which(w2 != 0)], collapse = ","))
    RestrictedSet <- c(RestrictedSet, paste(paste('H',which(w2 != 0),sep = ''),collapse = ','))


    adaptOut <- getAdaptBdry2(
      J = J, w1 = w1, w2 = w2, a2 = a2, a1 = a1, p1 = p1,
      test.type = mcpObj$test.type,
      HypoMap = mcpObj$HypoMap,
      Sigma = mcpObj$Stage1Obj$Sigma,
      Stage2Sigma = Stage2Sigma,
      Stage2HypoIDX = get_numeric_part(mcpObj$IndexSet),
      PlanSSHyp = PlanSSHyp,
      ModSSHyp = ModSSHyp,
      t1 = mcpObj$Stage1Obj$info_frac[1],
      mvtnorm_algo = mvtnorm_algo,
      ModifiedStage2Weights = ModifiedStage2Weights
    )

    SubSets <- c(SubSets, adaptOut$SubSets)
    ConditionalError <- c(ConditionalError, adaptOut$ConditionalError)
    Stage2AdjBdry <- rbind(Stage2AdjBdry, adaptOut$Stage2AdjBdry)
    #ScaleWeights[i] <- adaptOut$ScaledWeights
  }

  if (exists("gmcpSimObj") && gmcpSimObj$Debug) {
    ### Added for debugging purposes, should be removed once the code is verified to be working fine
    # browser()
  }

  colnames(Stage2AdjBdry) <- paste("a", 1:nHypothesis, "2_adj", sep = "")

  # Modified Sample Size table

  ModfiedSSTab <- knitr::kable(mcpObj$Stage2AllocSampleSize, align = "c")

  # Modified weights table
  HypoTab <- WH_modified[, grep("H", names(WH_modified))]

  InterHyp <- apply(HypoTab, 1, function(h) {
    paste(names(HypoTab)[which(h == 1)], collapse = ",")
  })

  InterWeight <- apply(WH_modified, 1, function(h) {
    J <- which(h[1:(length(h) / 2)] == 1)
    w <- h[((length(h) / 2) + 1):length(h)]
    paste(w[J], collapse = ",")
  })

  ModifiedWeightTab <- WH_modified


  # Adaptive Test Procedure
  # TestProcedureTab <- cbind(WH_modified[1:nHypothesis],SubSets,ConditionalError)
  TestProcedureTab1 <- knitr::kable(data.frame(
    "Hypotheses" = InterHyp,
    #"Weights" = InterWeight,
    #"ScaleWeights" = ScaleWeights,
    "SubSets" = SubSets,
    "Conditional_Error" = ConditionalError,
    row.names = NULL
  ), align = "c")


  # Adjusted Boundary
  # AdjBdryTab <- cbind(WH_modified[1:nHypothesis],Stage2AdjBdry)
  stg2bdry <- c()

  for (hypIDX in 1:nrow(Stage2AdjBdry)) {
    J <- which(WH_modified[hypIDX, 1:(length(WH_modified) / 2)] == 1)
    stg2bdry <- c(
      stg2bdry,
      paste(round(Stage2AdjBdry[hypIDX, J], 6), collapse = ",")
    )
  }
  AdjBdryTab1 <- knitr::kable(data.frame(
    "Hypotheses" = InterHyp,
    "RestrictedSet" = RestrictedSet,
    "Weights" = InterWeight2,
    "Adapt_Boundary" = stg2bdry,
    row.names = NULL
  ), align = "c")


  AdaptTable <- list(
    "Sample_Size" = ModfiedSSTab,
    "Stage2_Test_Procedure" = TestProcedureTab1,
    "Adjusted_Boundary" = AdjBdryTab1
  )

  list(
    "Stage2Tables" = AdaptTable,
    "Stage2AdjBdry" = Stage2AdjBdry,
    "Stage2Sigma" = Stage2Sigma
  )
}


getAdaptBdry2 <- function(J, w1, w2, a2, a1, p1, test.type, HypoMap,
                         Sigma, Stage2Sigma, Stage2HypoIDX, PlanSSHyp, ModSSHyp,
                         t1, mvtnorm_algo, ModifiedStage2Weights = F) {
  get_Sets <- connSets(J = J, w = w1, test.type = test.type, HypoMap = HypoMap)
  conn_Sets <- get_Sets$connSets
  conn_Sets_name <- paste("(", paste(
    unlist(lapply(conn_Sets, function(x) {
      paste(x, collapse = ",")
    })),
    collapse = "),("
  ), ")", sep = "")

  SubSets <- conn_Sets_name
  Method <- get_Sets$Method

  testGrps <- connParamNParmSets(conn_Sets = conn_Sets)
  ParamGrps <- testGrps$ParamGrps
  NParamGrps <- testGrps$NParamGrp

  Stage2AdjBdry <- rep(0, length(J))
  ScaleWeights <- c()

  #function to compute the conditional error based on planned design
  #i.e. planned covariance matrix, planned weights etc.
  stage2ExitProbCond <- function(cJ2){
    cerParamGrps <- pcerNParamGrps <- c()
    for (edx in conn_Sets) {
      if (length(edx) > 1) {
        ## Parametric ##
        gIDX <- unique(HypoMap[edx, ]$Groups)
        pGrp <- edx
        epIDX <- unique(HypoMap[pGrp, ]$Groups)

        # Fixed the following bug in subsetting the covariance matrix:
        # E.g. Suppose we have 2 groups, group 1 with 4 hypo H1-H4 and group 2 with 4 hypo H5-H8.
        # Now, if pGrp = c(6, 7) and epIDX is 2, floor(pGrp / epIDX) gives (3, 3).
        # However, pGrp corresponds to H6 and H7 which are the 2nd and 3rd hypotheses in group 2.
        # So, we need to extract the 2nd and 3rd rows/columns from the covariance matrix of group 2.
        # Use of floor is leading to this wrong behavior.
        ## stage2sigmaS <- Stage2Sigma$SigmaSIncr[[epIDX]][floor(pGrp / epIDX), floor(pGrp / epIDX)]
        ## InfoMatrix <- Sigma$InfoMatrix[[epIDX]][floor(pGrp / epIDX), ]
        # Get within-group indices correctly
        hypothesesInGroup <- which(HypoMap$Groups == epIDX)
        withinGroupIndices <- match(pGrp, hypothesesInGroup)
        stage2sigmaS <- Stage2Sigma$SigmaSIncr[[epIDX]][withinGroupIndices, withinGroupIndices]
        InfoMatrix <- Sigma$InfoMatrix[[epIDX]][withinGroupIndices, ]

        pJh <- p1[pGrp]
        wJh <- w1[pGrp]
        #Exist probability for parametric sub-set at stage 2
        cerParamGrps <- c(cerParamGrps,
                          exitProbStage2Cond(cJ2 = cJ2, p1 = pJh, w = wJh,
                          InfoMatrix = InfoMatrix, stage2sigmaS = stage2sigmaS,
                          mvtnorm_algo = mvtnorm_algo, Conditional = TRUE))

      } else {
        ## Non-Parametric ##
        pJh <- p1[edx]
        wJh <- w1[edx]
        ss1 <- PlanSSHyp[[1]][edx]; ss2 = PlanSSHyp[[2]][edx]
        #Exist probability for non parametric sub-set at stage 2
        pcerNParamGrps <- c(pcerNParamGrps,
          getPCER2(cJ2 = cJ2, wJh = wJh,  p1 = pJh, ss1 = PlanSSHyp[[1]][edx], ss2 = PlanSSHyp[[2]][edx], t1 = t1))
      }
    }
    return(list("cerParamGrps"=cerParamGrps,
                "pcerNParamGrps"=pcerNParamGrps))
  }


  cJ1 <- a1[which(w1 != 0)[1]]/w1[which(w1 != 0)[1]]
  cJ2 <- a2[which(w1 != 0)[1]]/w1[which(w1 != 0)[1]]

  outCER <- stage2ExitProbCond(cJ2 = cJ2)
  totalCER <- sum(outCER$cerParamGrps,
                  outCER$pcerNParamGrps)

  #Computing the stage two boundary
  get_Sets_stage2 <- connSets(J = J, w = w2, test.type = test.type, HypoMap = HypoMap)
  conn_Sets_stage2 <- get_Sets_stage2$connSets

  #function to compute the boundary crossing probabilities based on adaptive design
  #i.e. modified covariance matrix, modified weights etc.
  stage2ExitProbCondAdapt <- function(cJ2){
    exitproblist = numeric()
    for (edx in conn_Sets_stage2) {
      if (length(edx) > 1) {
        ## Parametric ##
        gIDX <- unique(HypoMap[edx, ]$Groups)
        pGrp <- edx
        epIDX <- unique(HypoMap[pGrp, ]$Groups)

        # Fixed the following bug in subsetting the covariance matrix:
        # E.g. Suppose we have 2 groups, group 1 with 4 hypo H1-H4 and group 2 with 4 hypo H5-H8.
        # Now, if pGrp = c(6, 7) and epIDX is 2, floor(pGrp / epIDX) gives (3, 3).
        # However, pGrp corresponds to H6 and H7 which are the 2nd and 3rd hypotheses in group 2.
        # So, we need to extract the 2nd and 3rd rows/columns from the covariance matrix of group 2.
        # Use of floor is leading to this wrong behavior.
        ## stage2sigmaSMod <- Stage2Sigma$Stage2SigmaS[[epIDX]][floor(pGrp / epIDX), floor(pGrp / epIDX)]
        ## InfoMatrixMod <- cbind(
        ##   Sigma$InfoMatrix[[epIDX]][, 1],
        ##   Stage2Sigma$Stage2InfoMatrixCum[[epIDX]]
        ## )[floor(pGrp / epIDX), ]
        # Get within-group indices correctly
        hypothesesInGroup <- which(HypoMap$Groups == epIDX)
        withinGroupIndices <- match(pGrp, hypothesesInGroup)
        stage2sigmaSMod <- Stage2Sigma$Stage2SigmaS[[epIDX]][withinGroupIndices, withinGroupIndices]
        InfoMatrixMod <- cbind(
          Sigma$InfoMatrix[[epIDX]][, 1],
          Stage2Sigma$Stage2InfoMatrixCum[[epIDX]]
        )[withinGroupIndices, ]

        pJh <- p1[pGrp]
        wJh <- w2[pGrp]
        exitproblist <- c(exitproblist,
                          exitProbStage2Cond(cJ2 = cJ2, p1 = pJh, w = wJh,
                          InfoMatrix = InfoMatrixMod, stage2sigmaS = stage2sigmaSMod,
                          mvtnorm_algo = mvtnorm_algo, Conditional = TRUE))

      } else {
        ## Non-Parametric ##
        pJh <- p1[edx]
        wJh <- w2[edx]

        #Exist probability for non parametric sub-set at stage 2
        exitproblist <- c(exitproblist,
                          getPCER2(cJ2 = cJ2, wJh = wJh,
                                   p1 = pJh, ss1 = ModSSHyp[[1]][edx], ss2 = ModSSHyp[[2]][edx],
                                   t1 = ModSSHyp[[1]][edx]/ModSSHyp[[2]][edx]))
      }
    }
    return(sum(na.omit(exitproblist)))
  }

  getStage2AdaptBdry <- function(x){
    stage2ExitProbCondAdapt(cJ2 = x) - totalCER
  }

  #Martin.p: if total CER >= 1 reject the intersection hypothesis
  if(totalCER >= 1){
    ### Added for debugging purposes, should be removed once the code is verified to be working fine
    if(exists("gmcpSimObj") && !gmcpSimObj$Debug) {
      #   gmcpSimObj$Debug <<- TRUE
      # browser()
    }
    ######################################

    Stage2AdjBdry <- rep(0,length(w2))
    Stage2AdjBdry[w2>0] <- 1

  }else{
    cJ2Adapt <- uniroot(f = getStage2AdaptBdry,
                        interval = c(0, 1 / max(w2[w2!= 0])), tol = 1E-6)$root
    Stage2AdjBdry <- cJ2Adapt*w2
  }


  SubSets <- paste(
    paste("P :", paste(
      unlist(lapply(ParamGrps, function(x) {
        paste(x, collapse = ",")
      })),
      collapse = ","
    ), sep = ""),
    ",",
    paste("NP:", paste(
      unlist(lapply(NParamGrps, function(x) {
        paste(x, collapse = ",")
      })),
      collapse = ","
    ), sep = ""),
    sep = ""
  )

  roundDigit <- function(err, digits) {
    if (length(err) != 0) {
      round(err, digits)
    } else {
      0
    }
  }

  outCER$pcerNParamGrps
  ConditionalError <- paste(
    paste("CER :", paste(
      unlist(lapply(roundDigit(outCER$cerParamGrps, 5), function(x) {
        paste(x, collapse = ",")
      })),
      collapse = ","
    ), sep = ""),
    ",",
    paste("PCER :", paste(
      unlist(lapply(roundDigit(outCER$pcerNParamGrps, 5), function(x) {
        paste(x, collapse = ",")
      })),
      collapse = ","
    ), sep = ""),
    sep = ""
  )

  list(
    "SubSets" = SubSets,
    "ConditionalError" = ConditionalError,
    "Stage2AdjBdry" = Stage2AdjBdry
  )
}


# to get samples hypothesis wise
getHypoSS <- function(SS, HypoMap) {
  SS_H <- lapply(1:nrow(SS), function(j) {
    unlist(lapply(1:nrow(HypoMap), function(i) {
      sum(SS[j, ][as.numeric(HypoMap[i, c("Control", "Treatment")])], na.rm = T)
    }))
  })
  SS_H
}
