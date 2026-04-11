# --------------------------------------------------------------------------------------------------
#
# Stateless analysis interface (p-value combination method) — state object and S3 methods
#
# --------------------------------------------------------------------------------------------------

new_pc_analysis_state <- function(
    mcpObj,
    mvtnorm_algo,
    thresholds,
    setup_mcpObj,
    design_params,
    completed_looks = 0L,
    trial_completed = FALSE,
    look_history = list()) {
  if (!is.list(mcpObj)) stop("mcpObj must be a list")
  if (!is.numeric(completed_looks) || length(completed_looks) != 1) {
    stop("completed_looks must be a scalar numeric or integer")
  }
  completed_looks <- as.integer(completed_looks)

  state <- list(
    mcpObj = mcpObj,
    setup_mcpObj = setup_mcpObj,
    mvtnorm_algo = mvtnorm_algo,
    thresholds = thresholds,
    design_params = design_params,
    completed_looks = completed_looks,
    trial_completed = isTRUE(trial_completed),
    look_history = look_history
  )
  class(state) <- "PCAnalysisState"
  return(state)
}

#' @export
print.PCAnalysisState <- function(x, ...) {
  if (!is.list(x) || is.null(x$mcpObj)) {
    stop("Invalid PCAnalysisState object")
  }

  cat("\n")
  cat("PCAnalysisState\n")
  cat("- Looks completed:", x$completed_looks, "of", x$mcpObj$LastLook, "\n")
  if (isTRUE(x$trial_completed)) {
    cat("- Status: TRIAL CONCLUDED (all hypotheses rejected or dropped)\n")
  }
  cat("\n")

  cat("Design Boundary:\n")
  bdry <- x$mcpObj$bdryTab
  colnames(bdry) <- c(
    "Look", "InfoFrac", "Alpha(Incr.)",
    "Boundary(Z)", "Boundary(P-Value)"
  )
  print(knitr::kable(bdry, align = "c"))
  cat("\n")

  cat("Inverse Normal Weights:\n")
  print(knitr::kable(x$mcpObj$InvNormWeights, align = "c"))
  cat("\n")

  if (x$completed_looks == 0) {
    cat("No look-wise analysis has been performed yet.\n\n")
    return(invisible(x))
  }

  for (k in seq_len(x$completed_looks)) {
    snap <- x$look_history[[k]]
    if (is.null(snap) || is.null(snap$mcpObj)) next
    ShowResults(snap$mcpObj)
  }

  return(invisible(x))
}

#' @export
plot.PCAnalysisState <- function(x, ...) {
  mcpObj <- x$mcpObj
  title <- if (x$completed_looks == 0) {
    "Initial Graph"
  } else {
    paste0("Graph After Stage ", x$completed_looks, " analysis")
  }

  HypothesisName <- mcpObj$allGraphs$HypothesisName
  HypoIDX <- get_numeric_part(HypothesisName)

  rej.flag.curr <- unlist(mcpObj$rej_flag_Curr[HypoIDX])
  dropped.flag.curr <- if (!is.null(names(mcpObj$DroppedFlag)) &&
    all(HypothesisName %in% names(mcpObj$DroppedFlag))) {
    unname(mcpObj$DroppedFlag[HypothesisName])
  } else {
    mcpObj$DroppedFlag[HypoIDX]
  }
  activeStatus <- (!rej.flag.curr) & (!dropped.flag.curr)
  graphIDX <- which(
    mcpObj$allGraphs$IntersectIDX == paste(as.integer(activeStatus), collapse = "")
  )

  if (length(graphIDX) == 0) {
    nodes <- edges <- NULL
  } else {
    nodes <- mcpObj$allGraphs$IntersectionWeights[
      graphIDX,
      grep("Weight", names(mcpObj$allGraphs$IntersectionWeights), fixed = TRUE)
    ]
    edges <- mcpObj$allGraphs$Edges[[graphIDX]]
  }

  widget <- plotGraph(
    HypothesisName = HypothesisName,
    w = unlist(nodes),
    G = edges,
    activeStatus = activeStatus,
    Title = title
  )

  return(widget)
}
