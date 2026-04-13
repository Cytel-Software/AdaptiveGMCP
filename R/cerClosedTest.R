# --------------------------------------------------------------------------------------------------
#
# ©2025 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

# test one intersection hypothesis
testInter <- function(pValues, b) {
  idx <- which(b != 0 & !is.na(pValues))
  any(pValues[idx] <= b[idx])
}


# Closed testing
checkRejection <- function(pValues, boundary) {
  if (is.null(nrow(boundary))) {
    return(testInter(pValues = pValues, b = boundary))
  } else {
    return(unlist(lapply(1:nrow(boundary), function(x) {
      testInter(pValues = pValues, b = boundary[x, ])
    })))
  }
}

# Overall decision
getRejStatus <- function(pValues, IntTestDF, Stage1RejStatus = NULL) {
  length(pValues)
  rej_flag <- rep(NA, length(pValues))
  namesHypo <- paste("H", 1:length(pValues), sep = "")

  for (i in 1:length(pValues))
  {
    if (!is.na(pValues[i])) {
      rej_flag[i] <- all(IntTestDF[IntTestDF[namesHypo[i]] == 1, ]$Rejected,
        na.rm = T
      )
    } else {
      if (is.null(Stage1RejStatus)) {
        rej_flag[i] <- FALSE
      } else {
        rej_flag[i] <- Stage1RejStatus[i]
      }
    }
  }
  RejStatus <- as.data.frame(matrix(rej_flag, nrow = 1), row.names = NULL)
  colnames(RejStatus) <- namesHypo
  RejStatus
}

# Closed Test
closedTest <- function(WH, boundary, pValues, Stage1RejStatus = NULL) {
  AnalysisTable <- data.frame(WH[, 1:(ncol(WH) / 2)],
    "Rejected" = checkRejection(
      pValues = pValues,
      boundary = boundary
    ),row.names = NULL
  )


  FinalAnalysis <- getRejStatus(
    pValues = pValues,
    IntTestDF = AnalysisTable,
    Stage1RejStatus = Stage1RejStatus
  )

  list(
    "IntersectHypoTest" = AnalysisTable,
    "PrimaryHypoTest" = FinalAnalysis
  )
}
