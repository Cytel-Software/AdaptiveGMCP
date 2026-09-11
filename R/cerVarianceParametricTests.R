# --------------------------------------------------------------------------------------------------
#
# ©2025 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

################# Computation of Stage-Wise covariance Matrix ###########

# Function for computing the covariance matrix for Z statistics under the null hypothesis
# Under the null, we assume that all treatment arms have the same standard deviation as control.
# This is required for computing the efficacy boundary.
# With this assumption, the covariance matrix computation simplifies greatly and all dependence on
# nuisance parameters and arm wise sample sizes goes away. Also, the covariance formulas become endpoint independent.
# This function is intended to replace the varCovZ function in boundary computation.
# (i1,i2) : index for the hypothesis
# (k1,k2) : index for the looks
# allocRatio : vector providing arm wise allocation ratios including control arm
# info_frac : information fraction at look 1
varCovZ_Null <- function(i1, k1, i2, k2, allocRatio, info_frac) {
  val <- if (i1 == i2 & k1 == k2) { # Variance Case-1: Same treatment, same look
      1
    } else if (k1 == k2 & i1 != i2) # Covariance Case-2: Different treatments, same look
    {
        sqrt((allocRatio[i1] / (1 + allocRatio[i1])) * (allocRatio[i2] / (1 + allocRatio[i2])))
    } else if (i1 == i2 & k1 != k2) # Covariance Case-3: Same treatment, different looks
    {
      sqrt(info_frac[1])
    } else if (i1 != i2 & k1 != k2) # Covariance Case-4: Different treatments, different looks
    {
      sqrt(info_frac[1] * 
        (allocRatio[i1] / (1 + allocRatio[i1])) * (allocRatio[i2] / (1 + allocRatio[i2])))
    } else {
      stop("Error in varCovZ_Null: Invalid indices")
    }

  return(val)
}

##### Computation of Covariance Matrix #############
# EpType : endpoint type
# SS_Cum : look-wise arm-wise cumulative samples
# sigma : list containing Arm-wise planned standard deviation for Continuous endpoints
# prop.ctr : list containing control proportion for Binary endpoints
# allocRatio : allocation ratio for the arms
# CommonStdDev : Logical flag indicating whether common standard deviation is assumed across arms
# info_frac : information fraction at look 1
#------------------------------------------------ -
getSigma <- function(EpType, SS_Cum, sigma, prop.ctr, allocRatio, CommonStdDev, info_frac) {

  #Treatment standard Deviation is same is control
  if(CommonStdDev == T){
    for(sigIDX in 1:length(sigma))
      if(all(!is.na(sigma[[sigIDX]]))){
        sigma[[sigIDX]] <- sapply(sigma[[sigIDX]], function(x)sigma[[sigIDX]][1])
      }
  }

  ctrSS <- SS_Cum[, 1]
  trtSS <- t(SS_Cum[, -1]) # Column represents looks

  SigmaZ <- list() # Z-Scale sigma
  SigmaS <- list() # Score Scale sigma
  InfoMat <- list() # Information Matrix

  nEps <- length(EpType)


  for (i in 1:nEps) {
    if (EpType[[i]] == "Continuous") {
      nHypothesisEp <- length(sigma[[i]]) - 1
      nLooksEp <- length(ctrSS)

      epSig <- sigma[[i]]
      sigma_0 <- epSig[1]
      sigma_trt <- epSig[-1]

      # capLambda <- (sigma_0^2 + sigma_trt^2 / allocRatio[-1])^(-1)
      # Fixing the bug identified by Cyrus in Nov 2025 - earlier capLambda formula used sigma_trt.
      # However, since we are computing the boundaries under the null hypothesis, we should use sigma_0 for all arms.
      capLambda <- (1 / sigma_0^2) * (allocRatio[-1] / (1 + allocRatio[-1]))
    } else if (EpType[[i]] == "Binary") {
      nHypothesisEp <- length(allocRatio) - 1 # Two equal dimension sigma matrix for two endpoints
      nLooksEp <- length(ctrSS)
      pi_c <- prop.ctr[[i]]
      capLambda <- (1 / (pi_c * (1 - pi_c))) * (allocRatio[-1] / (1 + allocRatio[-1]))
    }


    InfoMatrix <- sapply(ctrSS, function(x) {
      x * capLambda
    }) # row=hypothesis, col=looks
    InfoMat[[paste("EP", i, sep = "")]] <- InfoMatrix

    ############### Computation of Z scale Sigma Matrix ################
    sigmaZ <- matrix(NA, nrow = nHypothesisEp * nLooksEp, ncol = nHypothesisEp * nLooksEp)
    hIDX <- rep(1:nHypothesisEp, nLooksEp)
    lIDX <- rep(1:nLooksEp, each = nHypothesisEp)

    for (l in 1:length(hIDX))
    {
      for (m in l:length(hIDX)) {
        sigmaZ[l, m] <- sigmaZ[m, l] <- varCovZ_Null(
          i1 = hIDX[l], k1 = lIDX[l],
          i2 = hIDX[m], k2 = lIDX[m],
          allocRatio = allocRatio,
          info_frac = info_frac
        )
      }
    }
    rownames(sigmaZ) <- colnames(sigmaZ) <- paste("Z", hIDX, lIDX, sep = "")
    SigmaZ[[paste("EP", i, sep = "")]] <- sigmaZ

    #----------------------------End of SigmaZ-------------------------

    ################## Computation of Score scale Sigma Matrix #########
    if (nLooksEp == 2) {
      l <- c(sqrt(InfoMatrix[, 1]), sqrt(InfoMatrix[, 2])) # this code is only for two stages
    } else if (nLooksEp == 1) {
      l <- c(sqrt(InfoMatrix[, 1])) # this code is only for  FSD
    } else {
      stop("Error in getSigma: The conversion of Z-Score is not available for Stages > 2")
    }

    sigmaS <- matrix(NA, nrow = nrow(sigmaZ), ncol = ncol(sigmaZ))
    for (m in 1:nrow(sigmaS)) {
      for (n in m:ncol(sigmaS)) {
        sigmaS[m, n] <- sigmaS[n, m] <- l[m] * l[n] * sigmaZ[m, n]
      }
    }
    rownames(sigmaS) <- colnames(sigmaS) <- paste("S", hIDX, lIDX, sep = "")
    SigmaS[[paste("EP", i, sep = "")]] <- sigmaS
    #----------------------------End of SigmaS-------------------------
  }
  list("InfoMatrix" = InfoMat, "SigmaZ" = SigmaZ, "SigmaS" = SigmaS)
}



##### Covariance Matrix for CER & Stage-2 boundary computations#############
getStage2Sigma <- function(nHypothesis, EpType, nLooks, Sigma,
                           AllocSampleSize, allocRatio, sigma, prop.ctr,
                           Stage2AllocSampleSize, Stage2allocRatio, Stage2sigma,CommonStdDev) {
  #Treatment standard Deviation is same is control
  if(CommonStdDev == T){
    for(sigIDX in 1:length(sigma))
      if(all(!is.na(sigma[[sigIDX]]))){
        sigma[[sigIDX]] <- sapply(sigma[[sigIDX]], function(x)sigma[[sigIDX]][1])
      }
  }
  Stage2sigma <- sigma
  ############################################

  SigmaZIncr <- list() # Z-Scale Incremental sigma
  SigmaSIncr <- list() # Score Incremental Scale sigma
  Stage2SigmaZ <- list() # Z-Scale Modified sigma
  Stage2SigmaS <- list() # Score-Scale Modified sigma
  Stage2InfoMat <- list() # Stage-2 information matrix
  Stage2InfoMat_Incr <- list() # Stage-2 information matrix (incremental)

  ############# With planned Samples for CER computations##############
  nGrps <- length(Sigma$SigmaZ)
  for (i in 1:nGrps) # For all groups
  {
    # m <- ncol(Sigma$SigmaZ[[i]])
    # A <- getAmatrix(nrow = m/2, ncol = m)
    # SigmaZIncr[[names(Sigma$SigmaZ)[i]]] <- A %*% Sigma$SigmaZ[[i]] %*% t(A)
    # SigmaSIncr[[names(Sigma$SigmaS)[i]]] <- A %*% Sigma$SigmaS[[i]] %*% t(A)

    if (EpType[[i]] == "Continuous") {
      sigma_0 <- sigma[[i]][1]
      sigma_trt <- sigma[[i]][-1]
      # capLambda <- (sigma_0^2 + sigma_trt^2 / allocRatio[-1])^(-1)
      # Fixing the bug identified by Cyrus in Nov 2025 - earlier capLambda formula used sigma_trt.
      # However, since we are computing the boundaries under the null hypothesis, we should use sigma_0 for all arms.
      capLambda <- (1 / sigma_0^2) * (allocRatio[-1] / (1 + allocRatio[-1]))
    } else if (EpType[[i]] == "Binary") {
      pi_c <- prop.ctr[[i]]
      capLambda <- (1 / (pi_c * (1 - pi_c))) * (allocRatio[-1] / (1 + allocRatio[-1]))
    }

    # AllocSampleSize : the planned sample size
    SSIncr <- as.numeric(AllocSampleSize[2, ]) - as.numeric(AllocSampleSize[1, ])
    ctrSS <- SSIncr[1]
    trtSS <- SSIncr[-1]
    InfoMatrix <- matrix(ctrSS * capLambda, ncol = 1)

    ########## Computation of  Z-scale Covariance Matrix #################
    k <- ncol(AllocSampleSize) - 1
    sigmaZ <- matrix(NA, nrow = k, ncol = k)

    for (l in 1:k)
    {
      for (m in l:k) {
        sigmaZ[l, m] <- sigmaZ[m, l] <- varCovZ_Null(
          i1 = l, k1 = 1, i2 = m, k2 = 1,
          allocRatio = allocRatio,
          info_frac = 1
        )
      }
    }
    SigmaZIncr[[names(Sigma$SigmaZ)[i]]] <- sigmaZ

    l <- c(sqrt(InfoMatrix[, 1])) # this code is only for  2-stage
    sigmaS <- matrix(NA, nrow = nrow(sigmaZ), ncol = ncol(sigmaZ))
    for (m in 1:nrow(sigmaS)) {
      for (n in m:ncol(sigmaS)) {
        sigmaS[m, n] <- sigmaS[n, m] <- l[m] * l[n] * sigmaZ[m, n]
      }
    }
    SigmaSIncr[[names(Sigma$SigmaZ)[i]]] <- sigmaS
  }

  ################ With modified Samples for Stage-2 boundary computations ######
  for (epIDX in 1:nGrps)
  {
    # The Cumulative InfoMatrix is needed for transformation(z to score)
    Stage2SSCum <- (Stage2AllocSampleSize[2, ])
    for (s in 1:length(Stage2SSCum)) {
      if (is.na(Stage2SSCum[s])) Stage2SSCum[s] <- AllocSampleSize[1, s]
    }
    Stage2allocRatio <- as.numeric(Stage2SSCum) / as.numeric(Stage2SSCum[1])

    if (EpType[[epIDX]] == "Continuous") {
      Stage2sigma_0 <- Stage2sigma[[epIDX]][1]
      Stage2sigma_trt <- Stage2sigma[[epIDX]][-1]
      # Stage2capLambda <- (Stage2sigma_0^2 + Stage2sigma_trt^2 / Stage2allocRatio[-1])^(-1)
      # Fixing the bug identified by Cyrus in Nov 2025 - earlier capLambda formula used sigma_trt.
      # However, since we are computing the boundaries under the null hypothesis, we should use sigma_0 for all arms.
      Stage2capLambda <- (1 / Stage2sigma_0^2) * (Stage2allocRatio[-1] / (1 + Stage2allocRatio[-1]))
    } else if (EpType[[epIDX]] == "Binary") {
      pi_c <- prop.ctr[[epIDX]]
      Stage2capLambda <- (1 / (pi_c * (1 - pi_c))) * (Stage2allocRatio[-1] / (1 + Stage2allocRatio[-1]))
    }

    # The following adjustment is due to change in distribution(as the sample size modified)
    Stage2SSIncr <- as.numeric(Stage2AllocSampleSize[2, ]) - as.numeric(Stage2AllocSampleSize[1, ])
    Stage2ctrSS <- Stage2SSIncr[1]
    Stage2trtSS <- Stage2SSIncr[-1]
    Stage2InfoMatrix <- matrix(Stage2ctrSS * Stage2capLambda, ncol = 1)
    Stage2InfoMatrixCum <- matrix(as.numeric(Stage2SSCum[, 1]) * Stage2capLambda, ncol = 1)


    ########## Computation of  Z-scale Covariance Matrix #################
    k <- ncol(AllocSampleSize) - 1
    Stage2sigmaZ <- matrix(NA, nrow = k, ncol = k)

    for (l in 1:k)
    {
      for (m in l:k) {
        Stage2sigmaZ[l, m] <- Stage2sigmaZ[m, l] <- varCovZ_Null(
          i1 = l, k1 = 1, i2 = m, k2 = 1,
          allocRatio = Stage2allocRatio,
          info_frac = 1
        )
      }
    }
    Stage2SigmaZ[[names(Sigma$SigmaZ)[epIDX]]] <- Stage2sigmaZ
    Stage2InfoMat[[names(Sigma$SigmaZ)[epIDX]]] <- Stage2InfoMatrixCum
    Stage2InfoMat_Incr[[names(Sigma$SigmaZ)[epIDX]]] <- Stage2InfoMatrix

    ########## Computation of  Score-scale Covariance Matrix #################
    l <- c(sqrt(Stage2InfoMatrix[, 1])) # this code is only for  2-stage
    Stage2sigmaS <- matrix(NA, nrow = nrow(Stage2sigmaZ), ncol = ncol(Stage2sigmaZ))
    for (m in 1:nrow(Stage2sigmaS)) {
      for (n in m:ncol(Stage2sigmaS)) {
        Stage2sigmaS[m, n] <- Stage2sigmaS[n, m] <- l[m] * l[n] * Stage2sigmaZ[m, n]
      }
    }
    Stage2SigmaS[[names(Sigma$SigmaS)[epIDX]]] <- Stage2sigmaS
  }
  list(
    "SigmaZIncr" = SigmaZIncr,
    "SigmaSIncr" = SigmaSIncr,
    "Stage2SigmaZ" = Stage2SigmaZ,
    "Stage2SigmaS" = Stage2SigmaS,
    "Stage2InfoMatrixCum" = Stage2InfoMat,
    "Stage2InfoMatrixIncr" = Stage2InfoMat_Incr
  )
}

######## Correlation for Combining p-value dunnett test#############
getPlanCorrelation <- function(nHypothesis, EpType, SS_Incr, Arms.std.dev, prop.ctr, test.type, CommonStdDev) {
  #Treatment standard Deviation is same is control
  if(CommonStdDev == T){
    for(sigIDX in 1:length(Arms.std.dev))
      if(all(!is.na(Arms.std.dev[[sigIDX]]))){
        Arms.std.dev[[sigIDX]] <- sapply(Arms.std.dev[[sigIDX]], function(x)Arms.std.dev[[sigIDX]][1])
      }
  }
  ############################################

  nEps <- length(EpType)
  nLooks <- nrow(SS_Incr)

  Sigma <- list()
  for (lkIDX in 1:nLooks) {
    SigmaZ <- list() # ith look Z-Scale sigma
    SS_lk <- as.numeric(SS_Incr[lkIDX, ])
    ctrSS <- SS_lk[1]
    trtSS <- SS_lk[-1] # Column represents looks
    allocRatio <- c(1, trtSS / ctrSS)

    #Expected Output : SS_lk(the correlation between test statistics of dimension nHypothesisxnHypothesis)
    if(test.type == "Dunnett" || test.type == "Parametric" || test.type == "Partly-Parametric"){
      for (epIDX in 1:nEps){
        if (EpType[[epIDX]] == "Continuous") {
          epSig <- Arms.std.dev[[epIDX]]
          sigma_0 <- epSig[1]
          sigma_trt <- epSig[-1]
          # capLambda <- (sigma_0^2 + sigma_trt^2 / allocRatio[-1])^(-1)
          # Fixing the bug identified by Cyrus in Nov 2025 - earlier capLambda formula used sigma_trt.
          # However, since we are computing the boundaries under the null hypothesis, we should use sigma_0 for all arms.
          capLambda <- (1 / sigma_0^2) * (allocRatio[-1] / (1 + allocRatio[-1]))
        } else if (EpType[[epIDX]] == "Binary") {
          pi_c <- prop.ctr[[epIDX]]
          capLambda <- (1 / (pi_c * (1 - pi_c))) * (allocRatio[-1] / (1 + allocRatio[-1]))
        }

        InfoMatrix <- sapply(ctrSS, function(x) {
          x * capLambda
        }) # row=hypothesis, col=looks

        # Sigma/Corr for ith stage incremental data
        sigmaZ <- matrix(NA, nrow = length(capLambda), ncol = length(capLambda))

        for (l in 1:length(capLambda))
        {
          for (m in l:length(capLambda)) {
            sigmaZ[l, m] <- sigmaZ[m, l] <- varCovZ_Null(
              i1 = l, k1 = 1, i2 = m, k2 = 1,
              allocRatio = allocRatio,
              info_frac = 1
            )
          }
        }
        SigmaZ[[paste("EP", epIDX, sep = "")]] <- sigmaZ
      } # end of for (epIDX in 1:nEps)
       Sigmalk <- as.matrix(Matrix::bdiag(SigmaZ))
       Sigmalk[Sigmalk == 0] <- NA
       rownames(Sigmalk) <- colnames(Sigmalk) <- paste("Z", 1:nrow(Sigmalk), sep = "")
      }else if(test.type == "Bonf"){
        Sigmalk <- matrix(NA, nrow = nHypothesis, ncol = nHypothesis)
        diag(Sigmalk) <- 1
        rownames(Sigmalk) <- colnames(Sigmalk) <- paste("Z", 1:nrow(Sigmalk), sep = "")
      }else if(test.type == "Sidak" || test.type == "Simes"){
        Sigmalk <- NA
      }
    Sigma[[paste("Stage", lkIDX, sep = "")]] <- Sigmalk
  }
  Sigma
}
