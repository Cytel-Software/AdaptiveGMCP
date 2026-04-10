# --------------------------------------------------------------------------------------------------
#
# ©2025 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

# The file contains supporting functions for Adaptive GMCP analysis function adaptGMCP_PC(.) ----
#----------------------- -
# For Per look testing
#----------------------- -
PerLookMCPAnalysis <- function(mcpObj, mvtnorm_algo) {
  if (mcpObj$CurrentLook == 1) {
    WH_Collection <- mcpObj$WH
  } else {
    WH_Collection <- getICIndex(mcpObj)
  }

  # Weight Table Preparation for outputs
  HypoTab <- WH_Collection[, 1:(ncol(WH_Collection) / 2)]
  InterHyp <- apply(HypoTab, 1, function(h) {
    paste(names(HypoTab)[which(h == 1)], collapse = ",")
  })
  InterWeight <- apply(WH_Collection, 1, function(h) {
    J <- which(h[1:(length(h) / 2)] == 1)
    w <- h[((length(h) / 2) + 1):length(h)]
    paste(w[J], collapse = ",")
  })
  WeightTab <- data.frame(
    "Hypothesis" = InterHyp,
    "Weights" = InterWeight,
    row.names = NULL
  )
  #------------------------------------------

  P_Adj0 <- Adj_Method <- c()
  for (intHyp in 1:nrow(WH_Collection)) {
    # print(intHyp)
    adjOut <- compute_adjP(
      h = WH_Collection[intHyp, ],
      cr = mcpObj$Correlation,
      p = mcpObj$p_raw,
      test.type = mcpObj$test.type,
      mvtnorm_algo = mvtnorm_algo
    )

    if (is.list(adjOut)) {
      P_Adj0 <- c(P_Adj0, adjOut$adj_pj)
      Adj_Method <- c(Adj_Method, adjOut$adj_method)
    } else {
      P_Adj0 <- c(P_Adj0, NA)
      Adj_Method <- c(Adj_Method, NA)
    }
  }

  P_Adj <- data.frame(P_Adj0,row.names = NULL)
  colnames(P_Adj) <- paste("PAdj", mcpObj$CurrentLook, sep = "")
  PooledDF <- cbind(mcpObj$WH[, grep("H", names(mcpObj$WH))], P_Adj)


  # Adjusted p-value Table Preparation for outputs
  AdjPValTab <- knitr::kable(data.frame(
    "Hypothesis" = InterHyp,
    "Weights" = InterWeight,
    "Adj_PValue" = P_Adj0,
    "Adj_Method" = Adj_Method,
    row.names = NULL
  ), align = "c")

  mcpObj$AdjPValueTable <- AdjPValTab

  if (mcpObj$CurrentLook == 1) {
    mcpObj$AdjPValues <- PooledDF
  } else {
    PooledDFIDX <- as.vector(apply(PooledDF[, grep("H", names(PooledDF))], 1, function(x) {
      paste(x, collapse = "")
    }))
    AdjPValuesIDX <- as.vector(apply(mcpObj$AdjPValues[, grep("H", names(mcpObj$AdjPValues))], 1, function(x) {
      paste(x, collapse = "")
    }))

    AdjPval <- data.frame(unlist(lapply(AdjPValuesIDX, function(x) {
      # print(x)
      idx <- which(PooledDFIDX == x)
      ifelse(length(idx) == 0, NA, PooledDF[idx, grep("PAdj", names(PooledDF))])
    })))

    names(AdjPval) <- paste("PAdj", mcpObj$CurrentLook, sep = "")

    mcpObj$AdjPValues <- cbind(mcpObj$AdjPValues, AdjPval)
  }

  if (mcpObj$CurrentLook > 1) {
    Comb_p <- c()
    for (i in 1:nrow(mcpObj$AdjPValues))
    {
      Comb_p[i] <- CombinedPvalue(
        CurrentLook = mcpObj$CurrentLook,
        adjPValue = mcpObj$AdjPValues[i, ],
        W_Norm = mcpObj$W_Norm
      )
    }
    Comb_p <- as.data.frame(Comb_p)
    Comb_P_name <- paste("Comb_P", mcpObj$CurrentLook, sep = "")
    names(Comb_p) <- Comb_P_name
    mcpObj$AdjPValues <- cbind(mcpObj$AdjPValues, Comb_p)

    # Combined p-value table
    HypoTabComb <- mcpObj$AdjPValues[, grep("H", names(mcpObj$AdjPValues))]
    InterHypComb <- apply(HypoTabComb, 1, function(h) {
      paste(names(HypoTabComb)[which(h == 1)], collapse = ",")
    })
    combPvalTab0 <- mcpObj$AdjPValues[, (length(mcpObj$IntialHypothesis) + 1):ncol(mcpObj$AdjPValues)]
    combPvalTab1 <- data.frame("Hypothesis" = InterHypComb,row.names = NULL)
    combPvalTab1 <- cbind(combPvalTab1, combPvalTab0)
    mcpObj$CombinedPValuesTable <- knitr::kable(combPvalTab1, align = "c")
  }

  # Analysis
  for (idx in get_numeric_part(mcpObj$IndexSet)) ## Closed test for each primary hypothesis
  {
    if (!mcpObj$rej_flag_Prev[idx]) # If not rejected in earlier look
      {
        if (mcpObj$CurrentLook == 1) {
          Intersect_IDX <- which(mcpObj$AdjPValues[idx] == 1)
          mcpObj$rej_flag_Curr[idx] <- all(mcpObj$AdjPValues["PAdj1"][Intersect_IDX, ] <= mcpObj$CutOff)
        } else # Combined P-Values
        {
          Intersect_IDX <- which(mcpObj$AdjPValues[idx] == 1 &
            !is.na(mcpObj$AdjPValues[paste("PAdj", mcpObj$CurrentLook, sep = "")]))

          mcpObj$rej_flag_Curr[idx] <- all(mcpObj$AdjPValues[Intersect_IDX, Comb_P_name] <= mcpObj$CutOff)
        }

        if (mcpObj$rej_flag_Curr[idx]) {
          # If any hypothesis is rejected all the intersection hypothesis containing that can be removed
          mcpObj$WH <- mcpObj$WH[mcpObj$WH[idx] != 1, ]
        }
      }
  }
  notRejected <- paste("H", which(!mcpObj$rej_flag_Curr), sep = "")
  if (all(mcpObj$DroppedFlag == F)) {
    Dropped <- c()
  } else {
    Dropped <- paste("H", which(mcpObj$DroppedFlag), sep = "")
  }
  # notRejected <- names(mcpObj$rej_flag_Curr[!mcpObj$rej_flag_Curr])
  # Dropped <- names(mcpObj$DroppedFlag[mcpObj$DroppedFlag])
  mcpObj$IndexSet <- setdiff(notRejected, Dropped)
  mcpObj
}

#----------------------- -
# get intersection of available arms and closed testing hypothesis i.e(IC from chapter 8)
# Objective: correction of intersection weights based on existance status
# e.g.-  The weights of 1,1,1,1 needs to be replace by 1,0,1,0 if H2 and H3 are dropped
# Note: By modification the row position should remain intact(means the corrected weight of 1,0,1,0 should be is the same row as in 1,1,1,1)
#----------------------- -
getICIndex <- function(mcpObj) {
  existIndex <- as.integer(!is.na(mcpObj$p_raw))
  ClosedIndex <- mcpObj$WH[, grep("H", names(mcpObj$WH))]

  # Multiplication on indices implies the intersection
  interIndex <- lapply(1:nrow(ClosedIndex), function(x) {
    ClosedIndex[x, ] * existIndex
  })

  primaryIDX <- unlist(apply(ClosedIndex, 1, function(x) {
    paste(x, collapse = "")
  }))
  correctedIDX <- unlist(lapply(interIndex, function(x) {
    paste(x, collapse = "")
  }))

  locIDX <- unlist(lapply(correctedIDX, function(x) {
    which(primaryIDX == x)
  }))
  WH_corr <- cbind(
    ClosedIndex,
    mcpObj$WH[locIDX, (ncol(mcpObj$WH) / 2 + 1):ncol(mcpObj$WH)]
  )
  row.names(WH_corr) <- NULL
  # Intersection set
  WH_corr
}

#---------------------- -
# Combined P-value(Inverse Normal) assuming equal spacing
#---------------------- -
CombinedPvalue <- function(CurrentLook, adjPValue, W_Norm) {
  if (is.vector(W_Norm) & CurrentLook == 2) # W_Norm is a vector for 2 looks
    {
      W_Inv <- W_Norm
    } else if (is.matrix(W_Norm)) # W_Norm is a matrix for more than 2 looks
    {
      W_Inv <- W_Norm[CurrentLook - 1, 1:CurrentLook]
    } else {
    return("Error in CombinedPvalue function")
  }
  if (abs(sum(W_Inv^2) - 1) > 1E-6) stop("Error: abs(sum(W_Inv^2)-1) < 1E-6 not true| function: CombinedPvalue")


  p_look <- as.numeric(adjPValue[, grep("PAdj", names(adjPValue))])

  if (any(is.na(p_look))) {
    return(NA)
  } else {
    1 - pnorm(sum(W_Inv * qnorm(1 - p_look)))
  }
}

#---------------------- -
# Collect User Input Raw P-values
getRawPValues <- function(mcpObj) {
  P_raw <- c()
  cat("User Input for the look : ", mcpObj$CurrentLook, "\n")
  for (i in mcpObj$IndexSet) {
    if (mcpObj$CurrentLook == 1) {
      inpP <- readline(prompt = paste("Enter the raw P-Values for ", i, " : "))
    } else
    {
      inpP <- readline(prompt = paste("Enter the incremental raw P-Values for ", i, " : "))
    }
    inpP <- unlist(lapply(inpP, function(x) eval(parse(text = x))))
    P_raw[i] <- inpP
  }
  P_raw
}
#---------------------- -

#---------------------- -
trialContinuationDecision <- function(mcpObj) {
  Stop_Trial <- StopTrial(mcpObj)

  if (!Stop_Trial) {
    cat("Proceed to the next look( look-", mcpObj$CurrentLook + 1, ")?\n")
    cat("y : the trial will be continued to next look \n")
    cat("n : the trial will be terminated \n")
    cat("s : start over for the look ", mcpObj$CurrentLook, "\n")
    TrialTerm <- readline()
    TrialTerm
  } else {
    cat("Re-Compute the ( look-", mcpObj$CurrentLook, ")?\n")
    cat("n : the trial will be terminated \n")
    cat("s : start over for the look ", mcpObj$CurrentLook, "\n")
    TrialTerm <- readline()
    TrialTerm
  }
}



#---------------------- -
# User Input Selection
do_Selection <- function(mcpObj) {
  cat("Retain hypotheses for the look ", (mcpObj$CurrentLook + 1), " (y/n) : \n")
  cat("y : Option to choose from the available hypotheses \n")
  cat("n : proceed with the available hypotheses \n")
  SelectFlag <- readline(prompt = paste())

  if (SelectFlag == "y") {
    mcpObj$SelectionLook <- c(mcpObj$SelectionLook, (mcpObj$CurrentLook + 1))

    cat(
      "Out of ", paste(mcpObj$IndexSet, collapse = ", "),
      "Select the primary hypothesis to carry forward into the next look (e.g. H2,H3)", "\n"
    )

    select_index <- readline()

    mcpObj$SelectedIndex <- stringr::str_trim(
      unlist(strsplit(select_index, split = ",")),
      "both"
    )

    for (i in 1:length(mcpObj$DroppedFlag))
    {
      if (!mcpObj$DroppedFlag[i] & !mcpObj$rej_flag_Prev[i]) {
        mcpObj$DroppedFlag[i] <- !any(mcpObj$SelectedIndex == names(mcpObj$DroppedFlag[i]))
      }
    }

    mcpObj$IndexSet <- intersect(mcpObj$IndexSet, mcpObj$SelectedIndex)
    mcpObj$WH <- mcpObj$WH[which(apply(mcpObj$WH[mcpObj$IndexSet], 1, sum, na.rm = T) != 0), ]
    row.names(mcpObj$WH) <- NULL
  }
  mcpObj
}
#---------------------- -

#---------------------- -
# Extract the numeric part from the name e.g Arm1 -> 1
#---------------------- -
get_numeric_part <- function(vec) {
  numeric_part <- as.numeric(gsub("[^0-9]", "", vec))
  return(numeric_part)
}
#---------------------- -

#---------------------- -
# Modification of weights and graph for the continuing hypothesis
#---------------------- -
do_modifyStrategy <- function(mcpObj, showExistingStrategy = T) {
  ModificationFlag <- readline(prompt = paste("Change the testing Strategy from the look :", (mcpObj$CurrentLook + 1), " (y/n) : \n"))

  if (ModificationFlag == "y") {
    if (showExistingStrategy) {
      cat("Existing Strategy for reference \n")
      print(mcpObj$WH)
    }

    mcpObj$ModificationLook <- c(mcpObj$ModificationLook, mcpObj$CurrentLook)

    cont <- TRUE
    plotNewGraph <- TRUE
    mcpObj_old <- mcpObj

    while (cont) {
      mcpObj_new <- get_newGraph(mcpObj = mcpObj_old)

      # nameHypo <- mcpObj$IndexSet
      # newGrp <- gmcpPlot(WI = mcpObj_new$newWeights,
      #                    G = mcpObj_new$newG,
      #                    nameHypotheses = nameHypo)
      # plot(newGrp)

      # Plot the new strategy
      plotGraph(
        HypothesisName = mcpObj$IndexSet,
        w = mcpObj_new$newWeights,
        G = mcpObj_new$newG,
        Title = "Modified Graph"
      )


      cat("Confirm the new strategy (y/n) : \n")
      cat("y : procced with the new strategy \n")
      cat("n : Option to change the new graph \n")
      confGraph <- readline()

      if (confGraph == "y") {
        mcpObj <- mcpObj_new
        break
      }
    }

    modifiedWeights <- modifyIntersectWeights(mcpObj)
    return(modifiedWeights$mcpObj)
  } else {
    return(mcpObj)
  }
}
#---------------------- -

get_newGraph <- function(mcpObj) {
  # User input weights
  eg_text <- paste("(e.g.",
                   paste(rep(round(1/length(mcpObj$IndexSet),3),length(mcpObj$IndexSet)),collapse = ","),")")
  cat("Enter the new weights for (", paste(mcpObj$IndexSet, collapse = ", "), ") as comma seperated values",eg_text,":\n")
  inpWeight <- readline()
  inpWeight <- stringr::str_trim(unlist(strsplit(inpWeight, split = ",")), "both")
  inpWeight <- unlist(lapply(inpWeight, function(x) eval(parse(text = x))))
  mcpObj$newWeights <- inpWeight
  names(mcpObj$newWeights) <- paste("Weight", get_numeric_part(mcpObj$IndexSet), sep = "")

  # User input transition matrix
  m <- length(mcpObj$IndexSet)
  new_G <- matrix(0, nrow = m, ncol = m)
  cat("\n Enter the elements of the transition matrix \n")
  for (i in 1:m)
  {
    for (j in 1:m) {
      if (i != j) {
        cat("Enter edge weight (", paste(mcpObj$IndexSet[i], "->", mcpObj$IndexSet[j], sep = ""), ") :\n")
        inpEdge <- readline()
        inpEdge <- unlist(lapply(inpEdge, function(x) eval(parse(text = x))))
        new_G[i, j] <- inpEdge
      }
    }
  }
  colnames(new_G) <- mcpObj$IndexSet
  mcpObj$newG <- new_G
  mcpObj
}



#---------------------- -
# Modification of the correlation
do_modifyCorrelation <- function(mcpObj) {
  ModificationFlag <- readline(prompt = paste("Change the correlation for the look :", (mcpObj$CurrentLook + 1), " (y/n) : \n"))
  if (ModificationFlag == "y") {
    m <- get_numeric_part(mcpObj$IndexSet)
    new_Corr <- mcpObj$Correlation

    cat("\n Pre-Specified correlation matrix for the reference \n")
    print(mcpObj$Correlation)

    cat("\n Enter the elements of the correlation matrix \n")
    countModified <- 0
    for (i in m)
    {
      for (j in m) {
        if (i < j) {
          if (!is.na(mcpObj$Correlation[i, j])) {
            cat(
              "Correlation of (",
              paste(paste("TestStat", i, sep = ""), paste("TestStat", j, sep = ""), sep = ","),
              ") :\n"
            )
            new_Corr[i, j] <- new_Corr[j, i] <- as.numeric(readline())
            countModified <- countModified + 1
          }
        }
      }
    }
    # rownames(new_Corr) <- colnames(new_Corr) <- paste('H',m,sep = '')

    if (countModified == 0) cat("Modification not required \n")
    mcpObj$Correlation <- new_Corr
    return(mcpObj)
  } else {
    return(mcpObj)
  }
}

#---------------------- -

#---------------------- -
# Replace the old weights with Modified weights
# Replace the old weights with Modified weights
modifyIntersectWeights <- function(mcpObj) {
  A <- mcpObj$WH
  allModGraphs <- genWeights(
    w = mcpObj$newWeights,
    g = mcpObj$newG,
    HypothesisName = mcpObj$IndexSet
  )
  B <- allModGraphs$IntersectionWeights
  mcpObj$allGraphsPrev <- mcpObj$allGraphs
  mcpObj$allGraphs <- allModGraphs

  ## If the weights for the hypothesis belongs to IA set(Section 8.2) needs to be
  ## considered from the before modification strategy

  isIAsameIC <- F # Ajoy.M: it should consider the earlier strategy(4th Sept,23)

  if (isIAsameIC == F) {
    HypNotAvail <- setdiff(names(A)[grep("H", names(A))], mcpObj$IndexSet)
    WNotAvail <- paste("Weight", get_numeric_part(HypNotAvail), sep = "")
    if (length(HypNotAvail) < 2) {
      B[HypNotAvail] <- rep(0, nrow(B))
      B[WNotAvail] <- rep(0, nrow(B))
    } else {
      B[HypNotAvail] <- matrix(0, nrow = nrow(B), ncol = length(HypNotAvail))
      B[WNotAvail] <- matrix(0, nrow = nrow(B), ncol = length(WNotAvail))
    }
    UpdatedWeights <- A
    B <- B[, colnames(A)] # Reordering columns of B
    HA <- apply(A[, grep("H", names(A))], 1, paste, collapse = "")
    HB <- apply(B[, grep("H", names(B))], 1, paste, collapse = "")

    if (!all(HB %in% HA)) {
      return("Error in MergeWeights")
    }
    modrows <- c()

    for (i in 1:nrow(B)) {
      r <- which(HA == HB[i])
      modrows <- c(modrows, r)

      l1 <- A[r, grep("Weight", names(A))]
      l2 <- B[i, grep("Weight", names(B))]
      for (l in names(l1))
      {
        if (l %in% names(l2)) {
          UpdatedWeights[r, l] <- l2[l]
        } else {
          UpdatedWeights[r, l] <- NA
        }
      }
    }

    mcpObj$WH <- UpdatedWeights
    return(list("mcpObj" = mcpObj, "modifiedColumns" = modrows))
  } else {
    mcpObj$WH <- B
    return(list("mcpObj" = mcpObj))
  }
}
#---------------------- -


# Add NA for already rejected hypothesis
addNAPvalue <- function(p_raw, GlobalIndexSet) {
  p_raw_na <- rep(NA, length(GlobalIndexSet))
  names(p_raw_na) <- GlobalIndexSet
  p_raw_na[names(p_raw)] <- p_raw
  p_raw_na
}
#---------------------- -

#---------------------- -
# Console Output of Look Wise results
#---------------------- -
ShowResults <- function(mcpObj) {
  cat("\n")
  cat("Analysis results for Look : ", mcpObj$CurrentLook, "\n")
  cat("\n")
  cat("\n")

  cat("Design Boundary : \n")
  BdryTable <- mcpObj$bdryTab
  colnames(BdryTable) <- c(
    "Look", "InfoFrac", "Alpha(Incr.)",
    "Boundary(Z)", "Boundary(P-Value)"
  )
  print(knitr::kable(BdryTable, align = "c"))
  cat("\n")
  cat("\n")

  cat("Inverse Normal Weights : \n")
  print(knitr::kable(mcpObj$InvNormWeights, align = "c"))
  cat("\n")
  cat("\n")

  # cat('Weights for the intersection hypothesis at Look : ',mcpObj$CurrentLook,'\n')
  # print(mcpObj$WH_Prev)
  # cat('\n')
  # cat('\n')

  cat("Adj P-values for the intersection hypotheses at Look : ", mcpObj$CurrentLook, "\n")
  print(mcpObj$AdjPValueTable)
  cat("\n")
  cat("\n")

  if (mcpObj$CurrentLook > 1) {
    cat("Combined P-values for the intersection hypotheses at Look : ", mcpObj$CurrentLook, "\n")
    print(mcpObj$CombinedPValuesTable)
    cat("\n")
    cat("\n")
  }

  cat("Final rejection status of primary hypotheses at Look : ", mcpObj$CurrentLook, "\n")
  status <- sapply(mcpObj$rej_flag_Curr, function(x) ifelse(x, "Rejected", "Not_Rejected"))
  rej_df <- data.frame(
    "Hypothesis" = names(status),
    "Status" = status, row.names = NULL
  )
  print(knitr::kable(rej_df, align = "c"))
  cat("\n")
  cat("\n")
}
#---------------------- -

#---------------------- -
# Stop Trial
#---------------------- -
StopTrial <- function(mcpObj) {
  StopByEfficacy <- ifelse(mcpObj$MultipleWinners,
    all(mcpObj$rej_flag_Curr[!mcpObj$DroppedFlag] == T), # if all the primary hypothesis are rejected
    any(mcpObj$rej_flag_Curr[!mcpObj$DroppedFlag] == T)
  ) # if any primary hypothesis is rejected
  if (StopByEfficacy) # Stop
    {
      return(T)
    } else if (mcpObj$CurrentLook == mcpObj$LastLook) {
    return(T)
  } else {
    F
  }
}
#---------------------- -

#---------------------- -
# Combination of parametric and non-parametric test(one sided)
comb.test <- function(p, cr, w, mvtnorm_algo) {
  if (length(cr) > 1) {
    # We have stopped using conn.comp() in favor of clique.partition().
    # conn.comp() finds connected components in the correlation graph,
    # which can lead to groups with unknown correlations (NA) that can't be handled by pmvnorm.
    # conn <- conn.comp(cr)
    conn <- clique.partition(cr)
  } else {
    conn <- 1
  }
  # twosided <- alternatives==rep("two.sided", length(w))
  conn <- lapply(conn, as.numeric)

  conn_Len <- length(conn)
  set_len <- sapply(conn, length)

  if (conn_Len > 1 & any(set_len > 1)) { # Multiple sets of different length
    adjMethod <- "Mixed"
  } else if (conn_Len == 1 & all(set_len == 1)) {
    adjMethod <- "NA"
  } else if (conn_Len == 1 & all(set_len > 1)) {
    adjMethod <- "Parametric"
  } else if (conn_Len > 1 & all(set_len == 1)) {
    adjMethod <- "Bonferroni"
  }


  e <- sapply(conn, function(edx) {
    ###############
    # browser()
    ###############
    if (length(edx) > 1) # disjoint set with known distribution: Parametric One Sided Test
      {
        q <- min(as.numeric(p[edx]) / as.numeric(w[edx]))
        upper <- qnorm(1 - as.numeric(w[edx]) * q) # z-scale upper bound for right tailed tests

        # Use dimension-based algorithm selected in simMAMSMEP()
        corr_mat <- cr[edx, edx]

        p_param <- (1 - mvtnorm::pmvnorm(
          lower = -Inf, upper = upper, corr = corr_mat, algorithm = mvtnorm_algo
        ))
        return(min(1, p_param / sum(as.numeric(w[edx])))) # Partial Parametric
      } else { # disjoint set with unknown distribution: Non-Parametric One Sided Test

      return(min(1, min(1, as.numeric(p[edx]) / as.numeric(w[edx]))))
    }
  })

  e <- min(e, 1)
  list("AdjPvalue" = e, "adjMethod" = adjMethod)
}
#---------------------- -

#---------------------- -
# Non-parametric Sidak test (one sided)
sidak.test <- function(p, w) {
  min(1, min(1 - (1 - p)^(1 / w)))
}
#---------------------- -

#---------------------- -
# Preparation of inputs for adjusted p-value computation
compute_adjP <- function(h, cr, p, test.type, mvtnorm_algo) {
  n <- length(h)
  I <- h[1:(n / 2)]
  w <- h[((n / 2) + 1):n]
  hw <- sapply(w, function(x) !isTRUE(all.equal(x, 0)))
  e <- which(I > 0 & hw & !is.na(p))
  adj_pj <- NA
  if (length(e) == 0) {
    return(adj_pj)
  }

  if (test.type == "Partly-Parametric" || test.type == "Dunnett" || test.type == "Bonf") {
    testOut <- comb.test(p[e], cr[e, e], w[e], mvtnorm_algo = mvtnorm_algo)
    adj_pj <- testOut$AdjPvalue
    adj_method <- testOut$adjMethod
  } else if (test.type == "Sidak") {
    adj_pj <- sidak.test(p[e], w[e])
    adj_method <- "Sidak"
  } else if (test.type == "Simes") {
    adj_pj <- gMCPLite::simes.test(pvalues = p[e], weights = w[e], adjPValues = T)
    adj_method <- "Simes"
  }

  list("adj_pj" = adj_pj, "adj_method" = adj_method)
}
#---------------------- -

#---------------------- -
### We have stopped using conn.comp() in favor of clique.partition().
### conn.comp() finds connected components in the correlation graph,
### which can lead to groups with unknown correlations (NA) that can't be handled by pmvnorm.
### E.g. using conn.comp with a correlation matrix like the following results in
### a single group of all 4 hypotheses, even though the correlation between
### H1-H4 and H2-H3 is unknown (NA), which would cause pmvnorm to fail:
### corr <- matrix(c(1, 0.5, 0.5, NA,
###                  0.5, 1, NA, 0.5,
###                  0.5, NA, 1, 0.5,
###                  NA, 0.5, 0.5, 1), byrow = T, nrow = 4)
### Such correlation matrices can occur in problems like population enrichment.
### The newly written function clique.partition() handles such matrices properly.
# To find connected components in an adjacency matrix m(taken from gmcpLite codebase)
conn.comp <- function(m) {
  N <- 1:ncol(m)
  M <- numeric(0)
  out <- list()
  while (length(N) > 0) {
    Q <- setdiff(N, M)[1]
    while (length(Q) > 0) {
      w <- Q[1]
      M <- c(M, w)
      Q <- setdiff(unique(c(Q, which(!is.na(m[w, ])))), M)
    }
    out <- c(out, list(M))
    N <- setdiff(N, M)
    M <- numeric(0)
  }
  return(out)
}
#---------------------- -

#---------------------- -
# Partition hypotheses into cliques (complete subgraphs) of the known-correlation graph.
# Unlike conn.comp which finds connected components, this ensures every pair within
# a group has a known (non-NA) correlation, so the submatrix can safely be passed
# to pmvnorm. Uses a greedy algorithm: nodes with the most unknown correlations
# are placed first, and each node is added to the first clique where it fits.
#---------------------- -
clique.partition <- function(m) {
  n <- ncol(m)
  if (n == 1) return(list(1))

  # known[i,j] = TRUE iff correlation between i and j is specified (non-NA) and i != j
  known <- !is.na(m) & (row(m) != col(m))

  # Order nodes by number of unknown (NA) correlations descending — hardest to place first
  na.count <- sapply(seq_len(n), function(i) sum(is.na(m[i, -i])))
  node.order <- order(na.count, decreasing = TRUE)

  cliques <- list()
  for (node in node.order) {
    placed <- FALSE
    for (k in seq_along(cliques)) {
      # Node can join clique k only if it has known correlation with every existing member
      if (all(known[node, cliques[[k]]])) {
        cliques[[k]] <- c(cliques[[k]], node)
        placed <- TRUE
        break
      }
    }
    if (!placed) {
      cliques <- c(cliques, list(node))
    }
  }
  return(cliques)
}
#---------------------- -

# Choose algorithm based on dimension
chooseMVTAlgo <- function(mvtnorm_dimension) {
  # Choose algorithm based on dimension:
  # - Miwa: Fast and accurate for dimensions <= 20
  # - GenzBretz: For dimensions > 20 (Miwa becomes inaccurate beyond 20 dimensions)
  mvtnorm_algo <- if (mvtnorm_dimension <= 20) {
    mvtnorm::Miwa(steps = 128, checkCorr = FALSE, maxval = 1e3)
  } else {
    mvtnorm::GenzBretz(maxpts = 25000, abseps = 0.001, releps = 0)
  }

  return(mvtnorm_algo)
}
