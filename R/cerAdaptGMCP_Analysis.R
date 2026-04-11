# --------------------------------------------------------------------------------------------------
#
# ©2025 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

#' Function to perform Adaptive GMCP Analysis following Conditional Error Rate method
#' @param nArms Number of Arms
#' @param nEps Number of End points
#' @param SampleSize Plan Sample Size
#' @param EpType list of endpoint types (options : "Continuous","Binary")
#' @param sigma Arm-Wise sigma for each endpoint(for EpType = "Continuous")
#' @param CommonStdDev TRUE = the treatment standard deviations assumed to be same as the control for boundary computations for continuous endpoints, FALSE = the treatment standard deviations assumed to be same as given in Arms.std.dev.
#' @param prop.ctr proportion for control arm for each endpoint(for EpType = "Binary")
#' @param allocRatio Arm-Wise allocation ratio
#' @param WI Vector of Initial Weights for Global Null(default = \code{rep(1/4,4)})
#' @param G  Transition Matrix (default = \code{matrix(c(0,1/3,1/3,1/3,  1/3,0,1/3,1/3, 1/3,1/3,0,1/3, 1/3,1/3,1/3,0),nrow = 4)})
#' @param test.type Character to specify the type of test want to perform; "Parametric": Weighted Dunnett , "Non-Parametric": Weighted Bonferroni and  'Partly-Parametric': Mixed type Tests.
#' @param alpha Type-1 error
#' @param info_frac Vector of information fraction
#' @param typeOfDesign The type of design. Type of design is one of the following: O'Brien & Fleming ("OF"), Pocock ("P"), Wang & Tsiatis Delta class ("WT"), Pampallona & Tsiatis ("PT"), Haybittle & Peto ("HP"), Optimum design within Wang & Tsiatis class ("WToptimum"), O'Brien & Fleming type alpha spending ("asOF"), Pocock type alpha spending ("asP"), Kim & DeMets alpha spending ("asKD"), Hwang, Shi & DeCani alpha spending ("asHSD"), no early efficacy stop ("noEarlyEfficacy") default is "asOF".
#' @param deltaWT Parameter for alpha spending function for typeOfDesign = "WT"
#' @param deltaPT1 Parameter for alpha spending function for typeOfDesign = "PT"
#' @param gammaA 	Parameter for alpha spending function for typeOfDesign = "asHSD" & "asKD"
#' @param AdaptStage2 TRUE: Adaptation option will be given for stage-2, FALSE : proceed as planned.
#' @param plotGraphs TRUE: plot intermediate graphs
#' @example ./internalData/AdaptGMCP_CER_Analysis_Example.R
#' @export
adaptGMCP_CER <- function(
    nArms = 3,
    nEps = 2,
    SampleSize = 500,
    EpType = list("EP1" = "Continuous", "EP2" = "Continuous"),
    sigma = list("EP1" = c(1, 1.1, 1.2), "EP2" = c(1, 1.1, 1.2)),
    CommonStdDev = FALSE,
    prop.ctr = list("EP1" = NA, "EP2" = NA),
    allocRatio = c(1, 1, 1),
    WI = c(0.5, 0.5, 0, 0),
    G = matrix(
      c(
        0, 0.5, 0.5, 0,
        0.5, 0, 0, 0.5,
        0, 1, 0, 0,
        1, 0, 0, 0
      ),
      nrow = nEps * (nArms - 1), byrow = T
    ),
    test.type = "Partly-Parametric",
    alpha = 0.025,
    info_frac = c(0.5, 1),
    typeOfDesign = "asOF",
    deltaWT = 0,
    deltaPT1 = 0,
    gammaA = 2,
    AdaptStage2 = TRUE,
    plotGraphs = TRUE) {
  ###### Input Validation #####
  # stopifnot('Number of Arms must be > 2',length(nArms) <= 2)
  # stopifnot('Number of End points must be >= 1',length(nEps) < 1)

  #############################
  TailType <- "RightTail" ## Default Right
  Hypothesis <- "CommonControl" ## Default CommonControl

  # Adaptation Choice
  MultipleWinners <- TRUE
  Selection <- TRUE
  UpdateStrategy <- TRUE
  ModifySamples <- TRUE

  nLooks <- length(info_frac)
  des.type <- "MAMSMEP"

  nHypothesis <- nEps * (nArms - 1)
  GlobalIndexSet <- paste("H", 1:nHypothesis, sep = "")
  ArmsPresent <- 1:nArms

  # SETTING MVTNORM ALGORITHM TYPE ###############################
  # Dimension-based algorithm selection for mvtnorm::pmvnorm()
  # Calculate the dimension of the multivariate normal distribution
  mvtnorm_dimension <- (nArms - 1) * nEps
  mvtnorm_algo <- chooseMVTAlgo(mvtnorm_dimension)
  ################################################################

  # Map for arms and hypothesis
  HypoMap <- getHypoMap2(
    des.type = des.type, nHypothesis = nHypothesis,
    nEps = nEps, nArms = nArms,lEpType = EpType
  )

  # Computation of Intersection weights
  allGraphs <- genWeights(w = WI, g = G, HypothesisName = GlobalIndexSet)
  WH <- allGraphs$IntersectionWeights

  # Plot the initial Graph
  SubText <- getPlotText(HypoMap)
  if (plotGraphs) {
    plotGraph(
      HypothesisName = GlobalIndexSet,
      w = WI,
      G = G,
      Title = "Initial Graph",
      Text = SubText
    )
  }

  rej_flag_Prev <- rej_flag_Curr <- DroppedFlag <- rep(FALSE, nHypothesis)
  names(rej_flag_Prev) <- names(rej_flag_Curr) <- names(DroppedFlag) <- paste("H", 1:nHypothesis, sep = "")

  # info to run per Stage-Wise analysis
  mcpObj <- list(
    "CurrentLook" = NA,
    "lEpType" = EpType,
    "IntialHypothesis" = GlobalIndexSet,
    "test.type" = test.type,
    "IndexSet" = GlobalIndexSet,
    "ArmsPresent" = ArmsPresent,
    "sigma" = sigma,
    "CommonStdDev" = CommonStdDev,
    "prop.ctr" = prop.ctr,
    "allocRatio" = allocRatio,
    "Stage2allocRatio" = allocRatio,
    "p_raw" = NA,
    "WH_Prev" = WH,
    "WH" = WH,
    "MultipleWinners" = MultipleWinners,
    "rej_flag_Prev" = rej_flag_Prev,
    "rej_flag_Curr" = rej_flag_Curr,
    "SelectionLook" = c(),
    "SelectedIndex" = NULL,
    "DroppedFlag" = DroppedFlag,
    "LastLook" = nLooks,
    "Modify" = F,
    "ModificationLook" = c(),
    "newWeights" = NA,
    "newG" = NA,
    "bdryTab" = NA,
    "HypoMap" = HypoMap,
    "AllocSampleSize" = NA,
    "Stage2AllocSampleSize" = NA,
    "Stage1Obj" = NA,
    "AdaptObj" = NA,
    "allGraphsPrev" = allGraphs,
    "allGraphs" = allGraphs,
    "SubText" = SubText
  )

  look <- 1
  ContTrial <- T
  while (ContTrial) ## Loop for each Interim Analysis
  {
    Prev_mcpObj <- mcpObj # to store inputs before modification

    mcpObj$CurrentLook <- look
    p_raw <- getRawPValues(mcpObj) ## User Input raw p-values

    mcpObj$p_raw <- addNAPvalue(p_raw, GlobalIndexSet)

    if (mcpObj$CurrentLook == 1) {
      mcpObj$p_raw_stage1 <- mcpObj$p_raw
      Stage1Test <- PerformStage1Test(
        nArms = nArms, nEps = nEps, EpType = EpType, nLooks = nLooks,
        nHypothesis = nHypothesis, sigma = sigma, prop.ctr = prop.ctr,
        allocRatio = allocRatio, SampleSize = SampleSize,
        alpha = alpha, info_frac = info_frac,
        typeOfDesign = typeOfDesign,deltaWT = deltaWT,deltaPT1 = deltaPT1,
        gammaA = gammaA,
        des.type = des.type,
        test.type = test.type, Stage1Pvalues = mcpObj$p_raw,
        HypoMap = mcpObj$HypoMap,CommonStdDev = mcpObj$CommonStdDev,
        WH = mcpObj$WH, mvtnorm_algo = mvtnorm_algo
      )
      cat("Planned Variance Covariance Matrix \n")
      print(Stage1Test$Stage1Obj$Sigma)

      cat("Stage-1 Output Tables \n")
      print(Stage1Test$Stage1Tables)

      mcpObj$rej_flag_Prev <- mcpObj$rej_flag_Curr <- Stage1Test$Stage1Obj$Stage1Analysis$PrimaryHypoTest
      mcpObj$IndexSet <- paste("H", which(!mcpObj$rej_flag_Curr), sep = "")
      mcpObj$AllocSampleSize <- mcpObj$Stage2AllocSampleSize <- Stage1Test$Stage1Obj$AllocSampleSize
      mcpObj$Stage1Obj <- Stage1Test$Stage1Obj

      if (length(which(mcpObj$rej_flag_Curr == T)) != 0) {
        rejected <- paste("H", which(mcpObj$rej_flag_Curr == T), sep = "")
      } else {
        rejected <- c()
      }

      mcpObj$WH_Prev <- Stage1Test$Stage1Obj$WH

      if (length(rejected) == 0) {
        mcpObj$WH <- mcpObj$WH_Prev
      } else {
        mcpObj$WH <- mcpObj$WH_Prev[
          apply(mcpObj$WH_Prev[rejected], 1, function(x) {
            any(x == 1)
          }) != 1,
        ]
        row.names(mcpObj$WH) <- NULL
      }

      #Compute the CER & PCER for the remaining intersection hypothesis after stage-1 test rejection
      WH_modified_idx <- as.vector(apply(
        mcpObj$WH[, grep("H", names(mcpObj$WH))], 1,
        function(x) {
          paste(x, collapse = "")
        }
      ))

      WH_old_idx <- as.vector(apply(
        mcpObj$WH_Prev[, grep("H", names(mcpObj$WH_Prev))], 1,
        function(x) {
          paste(x, collapse = "")
        }
      ))

      oldIdx <- unlist(lapply(
        1:nrow(mcpObj$WH),
        function(i) {
          which(WH_old_idx == WH_modified_idx[i])
        }
      ))

      CERTab <- getCER(b2 = Stage1Test$Stage1Obj$plan_Bdry$Stage2Bdry[oldIdx, ],
                       WH = mcpObj$WH,
                       p1 = mcpObj$p_raw,
                       test.type = mcpObj$test.type,
                       HypoMap = mcpObj$HypoMap,
                       CommonStdDev = mcpObj$CommonStdDev,
                       allocRatio = mcpObj$allocRatio,
                       sigma = mcpObj$sigma,
                       Sigma = Stage1Test$Stage1Obj$Sigma,
                       AllocSampleSize = Stage1Test$Stage1Obj$AllocSampleSize,
                       EpType = mcpObj$lEpType,
                       prop.ctr = mcpObj$prop.ctr,
                       t1 = info_frac[1],
                       mvtnorm_algo = mvtnorm_algo)
      cat("Table of CER and PCER values conditional on stage one p-values \n")
      print(CERTab)
      # #--------------------------------------

      if (plotGraphs) # Plot after Stage-1 analysis
        {
          HypothesisName <- mcpObj$allGraphs$HypothesisName
          HypoIDX <- get_numeric_part(HypothesisName)
          #Active => not rejected and not dropped
          activeStatus <- (!unlist(mcpObj$rej_flag_Curr[HypoIDX])) &(!mcpObj$DroppedFlag)
          graphIDX <- which(mcpObj$allGraphs$IntersectIDX == paste(as.integer(activeStatus), collapse = ""))

          if (length(graphIDX) == 0) {
            nodes <- edges <- NULL
          } else {
            nodes <- mcpObj$allGraphs$IntersectionWeights[
              graphIDX,
              grep("Weight", names(mcpObj$allGraphs$IntersectionWeights))
            ]
            edges <- mcpObj$allGraphs$Edges[[graphIDX]]
          }

          plotGraph(
            HypothesisName = HypothesisName,
            w = unlist(nodes),
            G = edges,
            activeStatus = activeStatus,
            Title = paste("Graph After Stage ", mcpObj$CurrentLook, " analysis"),
            Text = mcpObj$SubText
          )
        }
    } else {
      # Stage 2 Analysis
      Stage2Test <- PerformStage2Test(mcpObj = mcpObj, AdaptStage2 = AdaptStage2)
      if (AdaptStage2) {
        cat("Variance Covariance matrix after adaptation \n")
        print(mcpObj$AdaptObj$Stage2Sigma)
      }
      cat("Stage-2 Output Tables \n")
      print(Stage2Test$Stage2Tables)
      mcpObj$rej_flag_Curr <- Stage2Test$RejStat

      if (plotGraphs) # Plot after Stage-2 analysis
        {
          HypothesisName <- mcpObj$allGraphs$HypothesisName
          HypoIDX <- get_numeric_part(HypothesisName)
          activeStatus <- !unlist(mcpObj$rej_flag_Curr[HypoIDX])
          graphIDX <- which(mcpObj$allGraphs$IntersectIDX == paste(as.integer(activeStatus), collapse = ""))

          if (length(graphIDX) == 0) {
            nodes <- edges <- NULL
          } else {
            nodes <- mcpObj$allGraphs$IntersectionWeights[
              graphIDX,
              grep("Weight", names(mcpObj$allGraphs$IntersectionWeights))
            ]
            edges <- mcpObj$allGraphs$Edges[[graphIDX]]
          }
          plotGraph(
            HypothesisName = HypothesisName,
            w = unlist(nodes),
            G = edges,
            activeStatus = activeStatus,
            Title = paste("Graph After Stage ", mcpObj$CurrentLook, " analysis"),
            Text = mcpObj$SubText
          )
        }
    }

    # Pre-computation for the next look
    # Choice to proceed to next look or start over
    trialTermUserInput <- trialContinuationDecision(mcpObj)

    if (trialTermUserInput == "y") # proceed to next look
      {
        if (AdaptStage2) {
          # Selection for next look
          if (Selection & (look < nLooks)) {
            mcpObj <- do_Selection(mcpObj)
            if (length(mcpObj$SelectedIndex) != 0) # If the selection set is non empty
              {
                contArms <- getArmsFromHypo(SetH = mcpObj$SelectedIndex, Hypo_map = mcpObj$HypoMap)
                mcpObj$ArmsPresent <- contArms

                if (plotGraphs) # Plot after selection
                  {
                    HypothesisName <- mcpObj$allGraphs$HypothesisName
                    HypoIDX <- get_numeric_part(HypothesisName)
                    ActiveIDX <- get_numeric_part(mcpObj$IndexSet)
                    activeStatus <- rep(F, length(HypothesisName))
                    activeStatus[which(HypoIDX %in% ActiveIDX)] <- T
                    graphIDX <- which(mcpObj$allGraphs$IntersectIDX == paste(as.integer(activeStatus), collapse = ""))

                    nodes <- mcpObj$allGraphs$IntersectionWeights[
                      graphIDX,
                      grep("Weight", names(mcpObj$allGraphs$IntersectionWeights))
                    ]
                    edges <- mcpObj$allGraphs$Edges[[graphIDX]]
                    plotGraph(
                      HypothesisName = HypothesisName,
                      w = unlist(nodes),
                      G = edges,
                      activeStatus = activeStatus,
                      Title = paste("Graph After Selection"),
                      Text = mcpObj$SubText
                    )
                  }
              } else # If the selection set is empty
            {
              mcpObj$ContTrial <- F
            }
          }

          # Stage-2 Samples
          if (ModifySamples) {
            Stage2ModSS <- do_ModifyStage2Sample(
              allocRatio = mcpObj$allocRatio,
              ArmsPresent = mcpObj$ArmsPresent,
              AllocSampleSize = mcpObj$AllocSampleSize
            )

            mcpObj$Stage2AllocSampleSize <- Stage2ModSS$newAllocSampleSize
            mcpObj$Stage2allocRatio <- Stage2ModSS$newallocRatio
          }

          # Modify the weights and testing strategy
          if (UpdateStrategy & (look < nLooks) & (length(mcpObj$IndexSet) > 1)) {
            mcpObj <- do_modifyStrategy(mcpObj, showExistingStrategy = F)
          }

          # Modify the Stage2 boundaries
          AdaptResults <- adaptBdryCER(mcpObj, mvtnorm_algo = mvtnorm_algo)
          mcpObj$AdaptObj <- AdaptResults
        }

        look <- look + 1
      } else if (trialTermUserInput == "n") # terminate the trial
      {
        break
      } else if (trialTermUserInput == "s") # Start over from the last look inputs
      {
        mcpObj <- Prev_mcpObj
      }
    #------------------End of the while loop------------------------#
  }
}
