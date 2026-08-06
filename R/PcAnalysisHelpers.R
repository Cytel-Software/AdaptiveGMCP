# --------------------------------------------------------------------------------------------------
#
# Stateless analysis interface (p-value combination method) — non-interactive helpers
#
# --------------------------------------------------------------------------------------------------

validate_named_numeric_vector <- function(x, required_names, arg_name) {
  if (!is.numeric(x) || is.null(x)) stop(arg_name, " must be numeric")
  if (length(x) != length(required_names)) {
    stop(arg_name, " must have length ", length(required_names))
  }
  if (is.null(names(x)) || anyNA(names(x)) || any(!nzchar(names(x), keepNA = TRUE))) {
    names(x) <- required_names
  }

  if (!setequal(names(x), required_names)) {
    stop(
      arg_name, " names must match active hypotheses exactly. Expected: ",
      toString(required_names)
    )
  }
  return(x[required_names])
}

validate_p_raw <- function(p_raw, index_set) {
  if (!is.numeric(p_raw)) stop("p_raw must be numeric")
  if (any(is.na(p_raw))) stop("p_raw cannot contain NA")
  if (any(p_raw < 0) || any(p_raw > 1)) stop("p_raw values must be in [0, 1]")

  if (is.null(names(p_raw)) || anyNA(names(p_raw)) || any(!nzchar(names(p_raw)))) {
    if (length(p_raw) != length(index_set)) {
      stop("Unnamed p_raw must have length equal to current IndexSet")
    }
    names(p_raw) <- index_set
  }

  if (!setequal(names(p_raw), index_set)) {
    stop(
      "p_raw names must match current IndexSet exactly. Expected: ",
      toString(index_set)
    )
  }
  return(p_raw[index_set])
}

normalize_new_weights <- function(new_weights, index_set) {
  if (!is.numeric(new_weights)) stop("new_weights must be numeric")
  if (any(is.na(new_weights))) stop("new_weights cannot contain NA")
  if (any(new_weights < 0)) stop("new_weights must be non-negative")

  if (is.null(names(new_weights)) || anyNA(names(new_weights)) || any(!nzchar(names(new_weights)))) {
    if (length(new_weights) != length(index_set)) {
      stop("Unnamed new_weights must have length equal to current IndexSet")
    }
    names(new_weights) <- index_set
  }

  if (!setequal(names(new_weights), index_set)) {
    stop(
      "new_weights names must match current IndexSet exactly. Expected: ",
      toString(index_set)
    )
  }

  # Internal convention used by modifyIntersectWeights(): Weight<idx>
  idx <- get_numeric_part(index_set)
  out <- new_weights[index_set]
  names(out) <- paste0("Weight", idx)

  s <- sum(out)
  if (s > 1 + 1e-8) stop("new_weights must sum to <= 1")
  return(out)
}

validate_transition_matrix <- function(new_G, index_set) {
  if (!is.matrix(new_G)) stop("new_G must be a matrix")
  m <- length(index_set)
  if (!all(dim(new_G) == c(m, m))) stop("new_G must be a square matrix of size length(IndexSet)")
  if (any(is.na(new_G))) stop("new_G cannot contain NA")
  if (any(new_G < 0)) stop("new_G must be non-negative")

  diag(new_G) <- 0
  rs <- rowSums(new_G)
  if (any(rs > 1 + 1e-8)) stop("Each row sum of new_G must be <= 1")

  rownames(new_G) <- colnames(new_G) <- index_set
  return(new_G)
}

normalize_dunnett_correlation <- function(test.type, correlation, arg_name = "Correlation") {
  if (!identical(test.type, "Dunnett") || is.null(correlation) || !anyNA(correlation)) {
    return(list(test.type = test.type, correlation = correlation))
  }

  warning(
    "test.type = 'Dunnett' requires a fully specified ", arg_name, " matrix. ",
    "Because ", arg_name, " contains NA entries, test.type has been converted to ",
    "'Partly-Parametric'.",
    call. = FALSE
  )

  return(list(test.type = "Partly-Parametric", correlation = correlation))
}

applySelection <- function(mcpObj, selected_hyps, look) {
  if (is.null(selected_hyps)) return(mcpObj)
  if (!is.character(selected_hyps)) stop("selection must be a character vector")
  selected_hyps <- unique(selected_hyps)

  if (!all(selected_hyps %in% mcpObj$IndexSet)) {
    stop(
      "selection must be a subset of current IndexSet. Current IndexSet: ",
      toString(mcpObj$IndexSet)
    )
  }

  if (length(selected_hyps) == 0) stop("selection must retain at least one hypothesis")

  mcpObj$SelectionLook <- c(mcpObj$SelectionLook, as.integer(look))
  mcpObj$SelectedIndex <- selected_hyps

  active_names <- names(mcpObj$DroppedFlag)
  for (nm in active_names) {
    if (!mcpObj$DroppedFlag[[nm]] && !mcpObj$rej_flag_Prev[[nm]]) {
      mcpObj$DroppedFlag[[nm]] <- !(nm %in% selected_hyps)
    }
  }

  mcpObj$IndexSet <- intersect(mcpObj$IndexSet, selected_hyps)

  if (length(mcpObj$IndexSet) == 0) {
    stop("All active hypotheses were dropped by selection")
  }

  mcpObj$WH <- mcpObj$WH[which(rowSums(mcpObj$WH[mcpObj$IndexSet], na.rm = TRUE) != 0), ]
  row.names(mcpObj$WH) <- NULL

  return(mcpObj)
}

applyStrategyUpdate <- function(mcpObj, new_weights, new_G) {
  if (is.null(new_weights) && is.null(new_G)) return(mcpObj)
  if (is.null(new_weights) || is.null(new_G)) {
    stop("Both new_weights and new_G must be provided together")
  }

  mcpObj$newWeights <- normalize_new_weights(new_weights, mcpObj$IndexSet)
  mcpObj$newG <- validate_transition_matrix(new_G, mcpObj$IndexSet)

  modified <- modifyIntersectWeights(mcpObj)
  if (is.character(modified)) {
    stop("modifyIntersectWeights failed: ", toString(modified))
  }
  if (!is.list(modified) || is.null(modified$mcpObj)) {
    stop(
      "modifyIntersectWeights failed: expected a list containing 'mcpObj'"
    )
  }
  return(modified$mcpObj)
}

applyCorrelationUpdate <- function(mcpObj, Correlation, target_hypotheses = NULL) {
  if (is.null(Correlation)) return(mcpObj)
  if (!is.matrix(Correlation)) stop("Correlation must be a matrix")
  if (!is.numeric(Correlation)) stop("Correlation must be a numeric matrix")

  initial_hypotheses <- mcpObj$IntialHypothesis
  d_initial <- length(initial_hypotheses)

  if (is.null(target_hypotheses)) {
    target_hypotheses <- initial_hypotheses
  }

  target_hypotheses <- unique(target_hypotheses)
  if (!all(target_hypotheses %in% initial_hypotheses)) {
    stop(
      "target_hypotheses must be a subset of initial hypotheses. Initial hypotheses: ",
      toString(initial_hypotheses)
    )
  }

  d_target <- length(target_hypotheses)
  corr_dim <- dim(Correlation)

  validate_corr_matrix <- function(corr_matrix) {
    if (any(!is.na(corr_matrix) & (corr_matrix < -1 | corr_matrix > 1))) {
      stop("Correlation entries must be in [-1, 1] (or NA)")
    }

    if (any(is.na(diag(corr_matrix)) | diag(corr_matrix) != 1)) {
      stop("Correlation diagonal must be 1")
    }

    if (!isTRUE(all.equal(corr_matrix, t(corr_matrix), tolerance = 1e-12, check.attributes = FALSE))) {
      stop("Correlation must be symmetric")
    }
  }

  if (all(corr_dim == c(d_initial, d_initial))) {
    full_correlation <- Correlation
    rownames(full_correlation) <- colnames(full_correlation) <- initial_hypotheses
    validate_corr_matrix(full_correlation)
  } else if (all(corr_dim == c(d_target, d_target))) {
    target_correlation <- Correlation
    rownames(target_correlation) <- colnames(target_correlation) <- target_hypotheses
    validate_corr_matrix(target_correlation)

    full_correlation <- mcpObj$Correlation
    if (!is.matrix(full_correlation) || !is.numeric(full_correlation) ||
        !all(dim(full_correlation) == c(d_initial, d_initial))) {
      full_correlation <- matrix(NA_real_, nrow = d_initial, ncol = d_initial)
      diag(full_correlation) <- 1
    }

    rownames(full_correlation) <- colnames(full_correlation) <- initial_hypotheses
    
    # Use numeric indexing for reliable subset assignment
    target_indices <- match(target_hypotheses, initial_hypotheses)
    full_correlation[target_indices, target_indices] <- target_correlation
    validate_corr_matrix(full_correlation)
  } else {
    stop(
      "Correlation must have dimensions ", d_initial, " x ", d_initial,
      " or ", d_target, " x ", d_target
    )
  }

  normalized <- normalize_dunnett_correlation(
    test.type = mcpObj$test.type,
    correlation = full_correlation,
    arg_name = "Correlation"
  )
  mcpObj$test.type <- normalized$test.type
  mcpObj$Correlation <- normalized$correlation
  return(mcpObj)
}

plot_graph_after_analysis <- function(mcpObj, title) {
  HypothesisName <- mcpObj$allGraphs$HypothesisName
  HypoIDX <- get_numeric_part(HypothesisName)

  activeStatus <- (!unlist(mcpObj$rej_flag_Curr[HypothesisName])) &
    (!mcpObj$DroppedFlag[HypothesisName])
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

plot_graph_after_selection <- function(mcpObj, title) {
  HypothesisName <- mcpObj$allGraphs$HypothesisName
  HypoIDX <- get_numeric_part(HypothesisName)
  ActiveIDX <- get_numeric_part(mcpObj$IndexSet)
  activeStatus <- rep(FALSE, length(HypothesisName))
  activeStatus[which(HypoIDX %in% ActiveIDX)] <- TRUE

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
