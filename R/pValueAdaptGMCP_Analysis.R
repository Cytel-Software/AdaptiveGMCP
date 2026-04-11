# --------------------------------------------------------------------------------------------------
#
# ©2025 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

#' Function to perform Adaptive GMCP Analysis following Combined P-Value(Inverse Normal) method
#' @param WI Vector of Initial Weights for Global Null(default = \code{rep(1/4,4)})
#' @param G  Transition Matrix (default = \code{matrix(c(0,1/3,1/3,1/3,  1/3,0,1/3,1/3, 1/3,1/3,0,1/3, 1/3,1/3,1/3,0),nrow = 4)})
#' @param test.type Character to specify the type of test want to perform; 'Bonf': Bonferroni, 'Sidak': Sidak, 'Simes': Simes, 'Dunnett': Dunnett and  'Partly-Parametric': Partly Parametric Tests
#' @param alpha Type-1 error
#' @param info_frac Vector of information fraction
#' @param typeOfDesign The type of design. Type of design is one of the following: O'Brien & Fleming ("OF"), Pocock ("P"), Wang & Tsiatis Delta class ("WT"), Pampallona & Tsiatis ("PT"), Haybittle & Peto ("HP"), Optimum design within Wang & Tsiatis class ("WToptimum"), O'Brien & Fleming type alpha spending ("asOF"), Pocock type alpha spending ("asP"), Kim & DeMets alpha spending ("asKD"), Hwang, Shi & DeCani alpha spending ("asHSD"), no early efficacy stop ("noEarlyEfficacy"), user specified alpha("asUser") default is "asOF".
#' @param deltaWT Parameter for alpha spending function for typeOfDesign = "WT"
#' @param deltaPT1 Parameter for alpha spending function for typeOfDesign = "PT"
#' @param gammaA 	Parameter for alpha spending function for typeOfDesign = "asHSD" & "asKD"
#' @param userAlphaSpending Parameter for alpha spending function for typeOfDesign = "asUser"
#' @param Correlation Matrix of correlation between test statistics, NA if the correlation is unknown
#' @param MultipleWinners Logical: TRUE if multiple winners over the looks is required.
#' @param Selection Logical: TRUE if selection required at interim(default = FALSE)
#' @param UpdateStrategy Logical: TRUE if modification of weights and testing strategy is required at interim(default = FALSE)
#' @param plotGraphs TRUE: plot intermediate graphs
#' @example ./internalData/AdaptGMCP_Analysis_Example.R
#' @export
adaptGMCP_PC <- function(
    WI = c(0.5, 0.5, 0, 0),
    G = matrix(c(
      0, 0.5, 0.5, 0,
      0.5, 0, 0, 0.5,
      0, 1, 0, 0,
      1, 0, 0, 0
    ), byrow = T, nrow = 4),
    test.type = "Partly-Parametric",
    alpha = 0.025,
    info_frac = c(1 / 2, 1),
    typeOfDesign = "asOF",
    deltaWT = 0,
    deltaPT1 = 0,
    gammaA = 2,
    userAlphaSpending = rpact::getDesignGroupSequential(
      sided = 1, alpha = alpha,informationRates =info_frac,
      typeOfDesign = "asOF")$alphaSpent,
    Correlation = matrix(c(1, 0.5, NA, NA, 0.5, 1, NA, NA, NA, NA, 1, 0.5, NA, NA, 0.5, 1), nrow = 4),
    MultipleWinners = TRUE,
    Selection = TRUE,
    UpdateStrategy = TRUE,
    plotGraphs = TRUE) {

  D <- length(WI)
  K <- length(info_frac)
  GlobalIndexSet <- paste("H", 1:D, sep = "")

  # SETTING MVTNORM ALGORITHM TYPE ###############################
  # Dimension-based algorithm selection for mvtnorm::pmvnorm()
  mvtnorm_algo <- chooseMVTAlgo(D)
  ################################################################

  ##################### Get the stage-wise p-value boundaries############################
  UseExternal <- T
  if (UseExternal) # this part of the code can be replaced later with the internal R-codes
    {
    if(typeOfDesign == "WT"){
      des <- rpact::getDesignGroupSequential(
        kMax = nLooks, alpha = alpha,
        informationRates = info_frac,
        typeOfDesign = typeOfDesign,
        deltaWT = deltaWT
      )
    }else if(typeOfDesign == "PT"){
      des <- rpact::getDesignGroupSequential(
        kMax = K, alpha = alpha,
        informationRates = info_frac,
        typeOfDesign = typeOfDesign,
        deltaPT1 = deltaPT1
      )
    }else if(typeOfDesign == "asHSD" || typeOfDesign == "asKD"){
      des <- rpact::getDesignGroupSequential(
        kMax = K, alpha = alpha,
        informationRates = info_frac,
        typeOfDesign = typeOfDesign,
        gammaA = gammaA
      )
    }else if(typeOfDesign == "asUser"){
      des <- rpact::getDesignGroupSequential(
        kMax = K, alpha = alpha,
        informationRates = info_frac,
        typeOfDesign = typeOfDesign,
        userAlphaSpending = userAlphaSpending
      )
    }else{
      des <- rpact::getDesignGroupSequential(
        kMax = K, alpha = alpha,
        informationRates = info_frac,
        typeOfDesign = typeOfDesign
      )
    }
    Threshold <- des$stageLevels
    incr_alpha <- c(des$alphaSpent[1], diff(des$alphaSpent))
    # BdryTab
    bdryTab <- data.frame(
        "Look" = 1:K, "Information_Fraction" = info_frac,
        "Incr_alpha_spent" = incr_alpha,
        "ZScale_Eff_Bbry" = des$criticalValues,
        "PValue_Eff_Bbry" = Threshold,
        row.names = NULL
      )
    }
  ######################################################################################

  ############# Weights for all intersection hypothesis#########################
  allGraphs <- genWeights(w = WI, g = G, HypothesisName = GlobalIndexSet)
  WH <- allGraphs$IntersectionWeights

  # Plot the initial Graph
  if (plotGraphs) {
    plotGraph(HypothesisName = GlobalIndexSet, w = WI, G = G, Title = "Initial Graph")
  }

  rej_flag_Prev <- rej_flag_Curr <- DroppedFlag <- rep(FALSE, D)
  names(rej_flag_Prev) <- names(rej_flag_Curr) <- names(DroppedFlag) <- paste("H", 1:D, sep = "")

  if (test.type == "Bonf") # Using the partly parametric function to perform Bonferroni test
    {
      Correlation <- diag(length(WI))
      Correlation[Correlation == 0] <- NA
    } else if (test.type == "Sidak" || test.type == "Simes") {
    Correlation <- NA
  } else if (test.type == "Dunnett" || test.type == "Partly-Parametric") {
    rownames(Correlation) <- colnames(Correlation) <- GlobalIndexSet
  }


  ############## Computation of Inverse normal weights from information fraction########
  info_frac_incr <- c(info_frac[1], diff(info_frac))

  W_Norm <- matrix(NA, nrow = K, ncol = K)
  for (i in 1:nrow(W_Norm))
  {
    for (j in 1:i) W_Norm[i, j] <- sqrt(info_frac_incr[j] / info_frac[i])
  }

  InvNormWeights <- W_Norm
  colnames(InvNormWeights) <- paste("W", 1:K, sep = "")
  rownames(InvNormWeights) <- paste("Look", 1:K, sep = "")

  W_Norm <- W_Norm[-1, ] # Removing the first row


  # info to run per look analysis
  mcpObj <- list(
    "CurrentLook" = NA,
    "IntialWeights" = WI,
    "IntialHypothesis" = GlobalIndexSet,
    "test.type" = test.type,
    "IndexSet" = GlobalIndexSet,
    "p_raw" = NA,
    "WH_Prev" = WH,
    "WH" = WH,
    "Correlation" = Correlation,
    "WeightTable" = NA,
    "AdjPValues" = NA,
    "AdjPValueTable" = NA,
    "CombinedPValuesTable" = NA,
    "W_Norm" = W_Norm,
    "CutOff" = NA,
    "MultipleWinners" = MultipleWinners,
    "rej_flag_Prev" = rej_flag_Prev,
    "rej_flag_Curr" = rej_flag_Curr,
    "SelectionLook" = c(),
    "SelectedIndex" = NA,
    "DroppedFlag" = DroppedFlag,
    "LastLook" = K,
    "Modify" = F,
    "ModificationLook" = c(),
    "newWeights" = NA,
    "newG" = NA,
    "bdryTab" = bdryTab,
    "InvNormWeights" = InvNormWeights,
    "allGraphsPrev" = allGraphs,
    "allGraphs" = allGraphs
  )

  # to Store all look info
  allInfo <- list()

  look <- 1
  ContTrial <- T

  while (ContTrial) ## Loop for each Interim Analysis
  {
    Prev_mcpObj <- mcpObj # to store inputs before modification

    mcpObj$CurrentLook <- look
    p_raw <- getRawPValues(mcpObj) ## User Input raw p-values

    mcpObj$p_raw <- addNAPvalue(p_raw, GlobalIndexSet)
    mcpObj$CutOff <- Threshold[look]
    mcpObj <- PerLookMCPAnalysis(mcpObj, mvtnorm_algo = mvtnorm_algo)

    # Pre-computation for the next look
    mcpObj$rej_flag_Prev <- mcpObj$rej_flag_Curr

    # Print Look-wise results in the console
    ShowResults(mcpObj)

    mcpObj$WH_Prev <- mcpObj$WH

    if (plotGraphs) # Plot after Stage-wise analysis
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
          Title = paste("Graph After Stage ", mcpObj$CurrentLook, " analysis")
        )
      }

    # Choice to proceed to next look or start over
    curLkUserInp <- trialContinuationDecision(mcpObj)

    if (curLkUserInp == "y") # proceed to next look
      {
        # Selection for next look
        if (Selection & (look < K)) {
          mcpObj <- do_Selection(mcpObj)

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
                Title = paste("Graph After Selection")
              )
            }
        }

        # Modify the weights and testing strategy
        if (UpdateStrategy & (look < K) & (length(mcpObj$IndexSet) > 1)) {
          mcpObj <- do_modifyStrategy(mcpObj, showExistingStrategy = F)
        }

        # Modify the correlation for parametric tests
        if (test.type == "Dunnett" || test.type == "Partly-Parametric") {
          mcpObj <- do_modifyCorrelation(mcpObj)
        }
        look <- look + 1
      } else if (curLkUserInp == "n") # terminate the trial
      {
        break
      } else if (curLkUserInp == "s") # Start over from the last look inputs
      {
        mcpObj <- Prev_mcpObj
      }
    #------------------End of the while loop------------------------#
  }
}
