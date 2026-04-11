# --------------------------------------------------------------------------------------------------
#
# Stateless analysis interface (p-value combination method) — exported API
#
# --------------------------------------------------------------------------------------------------

#' Setup analysis object for Adaptive GMCP (P-value combination method)
#'
#' Creates a non-interactive analysis state object. Use [AnalyzeLook_PC()] to
#' advance the analysis one look at a time.
#'
#' @param WI Vector of node weights for the initial graph
#' @param G Transition matrix for the graph.
#' @param test.type Character specifying test type.
#' Supported values: "Bonf", "Sidak", "Simes", "Dunnett", "Partly-Parametric".
#' @param alpha One-sided type-1 error.
#' @param info_frac Vector of cumulative information fractions.
#' @param typeOfDesign Group sequential design type (rpact).
#' Supported values: "OF" (O'Brien & Fleming), "P" (Pocock), "WT" (Wang & Tsiatis Delta class), 
#'      "PT" (Pampallona & Tsiatis), "HP" (Haybittle & Peto), 
#'      "WToptimum" (Optimum design within Wang & Tsiatis class), 
#'      "asOF" (O'Brien & Fleming type alpha spending), "asP" (Pocock type alpha spending), 
#'      "asKD" (Kim & DeMets alpha spending), "asHSD" (Hwang, Shi & DeCani alpha spending), 
#'      "noEarlyEfficacy" (no early efficacy stop), "asUser" (user specified alpha) default is "asOF".
#' @param deltaWT Parameter for typeOfDesign = "WT".
#' @param deltaPT1 Parameter for typeOfDesign = "PT".
#' @param gammaA Parameter for typeOfDesign = "asHSD" or "asKD".
#' @param userAlphaSpending Alpha spending values for typeOfDesign = "asUser".
#' @param Correlation Correlation matrix (NA allowed for unknown correlations).
#' @param MultipleWinners Logical; TRUE means reject as many hypotheses as possible, 
#'        FALSE means reject at most one hypothesis
#' @param Selection Logical; TRUE if selection of hypotheses is allowed at interim looks
#' @param UpdateStrategy Logical; TRUE if the graphical test strategy can be modified at interim looks
#' @param plotGraphs Logical; if TRUE, plots the initial graph.
#'
#' @return An object of class "PCAnalysisState".
#' @export
SetupAnalysis_PC <- function(
    WI = c(0.5, 0.5, 0, 0),
    G = matrix(c(
      0, 0.5, 0.5, 0,
      0.5, 0, 0, 0.5,
      0, 1, 0, 0,
      1, 0, 0, 0
    ), byrow = TRUE, nrow = 4),
    test.type = "Partly-Parametric",
    alpha = 0.025,
    info_frac = c(0.5, 1.0),
    typeOfDesign = "asOF",
    deltaWT = 0,
    deltaPT1 = 0,
    gammaA = 2,
    userAlphaSpending = NULL,
    Correlation = NULL,
    MultipleWinners = TRUE,
    Selection = TRUE,
    UpdateStrategy = TRUE,
    plotGraphs = TRUE) {
  if (!is.numeric(WI) || anyNA(WI) || any(WI < 0)) stop("WI must be numeric, non-negative, and non-NA")
  if (sum(WI) > 1) stop("Sum of WI (weights) must be less than or equal to 1")
  d <- length(WI)
  if (!is.matrix(G) || !all(dim(G) == c(d, d))) stop("G (transition matrix) must be a ", d, " x ", d, " matrix")

  k <- length(info_frac)
  if (k < 1) stop("info_frac must have length >= 1")
  if (any(info_frac <= 0) || any(info_frac > 1)) stop("info_frac must be in (0, 1]")
  if (any(diff(info_frac) <= 0)) stop("info_frac must be strictly increasing")

  GlobalIndexSet <- paste0("H", seq_len(d))

  # mvtnorm algorithm
  mvtnorm_algo <- chooseMVTAlgo(d)

  # Correlation handling by test type
  if (test.type == "Bonf") {
    Correlation <- diag(d)
    Correlation[Correlation == 0] <- NA
  } else if (test.type == "Sidak" || test.type == "Simes") {
    Correlation <- NA
  } else if (test.type == "Dunnett" || test.type == "Partly-Parametric") {
    if (is.null(Correlation)) {
      stop("Correlation must be provided for Dunnett or Partly-Parametric tests")
    }
    if (!is.matrix(Correlation) || !all(dim(Correlation) == c(d, d))) {
      stop("Correlation must be a ", d, " x ", d, " matrix")
    }
    rownames(Correlation) <- colnames(Correlation) <- GlobalIndexSet
  } else {
    stop("Unsupported test.type: ", test.type)
  }

  # Group sequential boundaries (stage-wise p-value boundaries)
  if (typeOfDesign == "asUser" && is.null(userAlphaSpending)) {
    stop("userAlphaSpending must be provided when typeOfDesign = 'asUser'")
  }

  des <- if (typeOfDesign == "WT") {
    rpact::getDesignGroupSequential(kMax = k, alpha = alpha,
      informationRates = info_frac, typeOfDesign = typeOfDesign, deltaWT = deltaWT)
  } else if (typeOfDesign == "PT") {
    rpact::getDesignGroupSequential(kMax = k, alpha = alpha,
      informationRates = info_frac, typeOfDesign = typeOfDesign, deltaPT1 = deltaPT1)
  } else if (typeOfDesign == "asHSD" || typeOfDesign == "asKD") {
    rpact::getDesignGroupSequential(kMax = k, alpha = alpha,
      informationRates = info_frac, typeOfDesign = typeOfDesign, gammaA = gammaA)
  } else if (typeOfDesign == "asUser") {
    rpact::getDesignGroupSequential(kMax = k, alpha = alpha,
      informationRates = info_frac, typeOfDesign = typeOfDesign, 
      userAlphaSpending = userAlphaSpending)
  } else {
    rpact::getDesignGroupSequential(kMax = k, alpha = alpha,
      informationRates = info_frac, typeOfDesign = typeOfDesign)
  }

  thresholds <- des$stageLevels
  incr_alpha <- c(des$alphaSpent[1], diff(des$alphaSpent))
  bdryTab <- data.frame(
    "Look" = seq_len(k),
    "Information_Fraction" = info_frac,
    "Incr_alpha_spent" = incr_alpha,
    "ZScale_Eff_Bbry" = des$criticalValues,
    "PValue_Eff_Bbry" = thresholds,
    row.names = NULL
  )

  # Weights for all intersection hypotheses
  allGraphs <- genWeights(w = WI, g = G, HypothesisName = GlobalIndexSet)
  WH <- allGraphs$IntersectionWeights

  # Inverse normal weights
  info_frac_incr <- c(info_frac[1], diff(info_frac))
  W_Norm <- matrix(NA_real_, nrow = k, ncol = k)
  for (i in seq_len(nrow(W_Norm))) {
    for (j in seq_len(i)) {
      W_Norm[i, j] <- sqrt(info_frac_incr[j] / info_frac[i])
    }
  }

  InvNormWeights <- W_Norm
  colnames(InvNormWeights) <- paste0("W", seq_len(k))
  rownames(InvNormWeights) <- paste0("Look", seq_len(k))

  W_Norm <- W_Norm[-1, , drop = FALSE]

  # Initialize flags
  rej_flag_Prev <- rej_flag_Curr <- DroppedFlag <- rep(FALSE, d)
  names(rej_flag_Prev) <- names(rej_flag_Curr) <- names(DroppedFlag) <- GlobalIndexSet

  mcpObj <- list(
    "CurrentLook" = 0L,
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
    "LastLook" = k,
    "Modify" = FALSE,
    "ModificationLook" = c(),
    "newWeights" = NA,
    "newG" = NA,
    "bdryTab" = bdryTab,
    "InvNormWeights" = InvNormWeights,
    "allGraphsPrev" = allGraphs,
    "allGraphs" = allGraphs
  )

  design_params <- list(
    WI = WI,
    G = G,
    test.type = test.type,
    alpha = alpha,
    info_frac = info_frac,
    typeOfDesign = typeOfDesign,
    deltaWT = deltaWT,
    deltaPT1 = deltaPT1,
    gammaA = gammaA,
    userAlphaSpending = userAlphaSpending,
    Correlation = Correlation,
    MultipleWinners = MultipleWinners,
    Selection = Selection,
    UpdateStrategy = UpdateStrategy
  )

  state <- new_pc_analysis_state(
    mcpObj = mcpObj,
    mvtnorm_algo = mvtnorm_algo,
    thresholds = thresholds,
    setup_mcpObj = mcpObj,
    design_params = design_params,
    completed_looks = 0L,
    trial_completed = FALSE,
    look_history = vector("list", k)
  )

  if (isTRUE(plotGraphs)) {
    plotGraph(HypothesisName = GlobalIndexSet, w = WI, G = G, Title = "Initial Graph")
  }

  return(state)
}

#' Analyze one look for Adaptive GMCP (P-value combination method)
#'
#' Advances an existing [SetupAnalysis_PC()] state object by exactly one look.
#' Optional selection / strategy modification / correlation updates may be applied
#' before the look is analyzed (these represent decisions made after the previous look).
#'
#' @param state A "PCAnalysisState" object.
#' @param p_raw Named numeric vector of raw p-values for the current look, for the
#'   currently active hypotheses (state$mcpObj$IndexSet).
#' @param look Optional positive integer explicitly naming the current look number.
#'   When provided, it must match the look number implied by the state object
#'   (i.e. \code{state$completed_looks + 1}). This argument exists purely for
#'   readability of user scripts — it does not change program logic.
#'   If \code{NULL} (default), the look number is inferred from the state.
#' @param selection Optional character vector of hypotheses to retain for this look
#'   (only meaningful when look > 1). NULL means no selection.
#' @param new_weights Optional numeric vector of new weights for continuing hypotheses.
#' @param new_G Optional transition matrix for continuing hypotheses.
#' @param new_correlation Optional updated D x D correlation matrix.
#' @param plotGraphs Logical; if TRUE, plots graphs at key points.
#'
#' @return Updated "PCAnalysisState".
#' @export
AnalyzeLook_PC <- function(
    state,
    p_raw,
    look = NULL,
    selection = NULL,
    new_weights = NULL,
    new_G = NULL,
    new_correlation = NULL,
    plotGraphs = TRUE) {
  if (!inherits(state, "PCAnalysisState")) stop("state must be a PCAnalysisState object")

  if (isTRUE(state$trial_completed)) {
    if (state$completed_looks >= state$mcpObj$LastLook) {
      stop(
        "Look ", state$mcpObj$LastLook, " was the final look — ",
        "the trial analysis is already complete."
      )
    }
    stop("Trial already concluded — stopping criteria met.")
  }

  mcpObj <- state$mcpObj
  next_look <- as.integer(state$completed_looks + 1L)

  if (!is.null(look)) {
    if (!is.numeric(look) || length(look) != 1L || look != as.integer(look) || look < 1L) {
      stop("look must be a single positive integer.")
    }
    look <- as.integer(look)
    if (look != next_look) {
      stop(
        "look argument (", look, ") does not match the expected next look (",
        next_look, ") based on the analysis state. ",
        "The next look to be analyzed is look ", next_look, "."
      )
    }
  }

  # Apply optional changes before analyzing look > 1
  if (next_look > 1) {
    if (!is.null(selection)) {
      if (!isTRUE(state$design_params$Selection)) {
        stop("selection cannot be applied: Selection was disabled in SetupAnalysis_PC()")
      }
      mcpObj <- applySelection(mcpObj, selection, look = next_look)
      if (isTRUE(plotGraphs)) {
        plot_graph_after_selection(mcpObj, title = "Graph After Selection")
      }
    }

    if (!is.null(new_weights) || !is.null(new_G)) {
      if (!isTRUE(state$design_params$UpdateStrategy)) {
        stop("new_weights/new_G cannot be applied: UpdateStrategy was disabled in SetupAnalysis_PC()")
      }
      mcpObj$ModificationLook <- c(mcpObj$ModificationLook, as.integer(state$completed_looks))
      mcpObj <- applyStrategyUpdate(mcpObj, new_weights, new_G)
      if (isTRUE(plotGraphs)) {
        plotGraph(
          HypothesisName = mcpObj$IndexSet,
          w = mcpObj$newWeights,
          G = mcpObj$newG,
          Title = "Modified Graph"
        )
      }
    }

    if (!is.null(new_correlation)) {
      mcpObj <- applyCorrelationUpdate(mcpObj, new_correlation)
    }
  } else {
    # For look 1, ensure no selection or strategy modification is applied
    if (!is.null(selection)) {
      warning("Selection cannot be applied at look 1 and will be ignored.")
    }
    if (!is.null(new_weights) || !is.null(new_G)) {
      warning("Strategy modification cannot be applied at look 1 and will be ignored.")
    }
    if (!is.null(new_correlation)) {
      warning("Correlation update cannot be applied at look 1 and will be ignored.")
    }
  }

  if (length(mcpObj$IndexSet) == 0) {
    state$trial_completed <- TRUE
    state$mcpObj <- mcpObj
    return(state)
  }

  mcpObj$CurrentLook <- next_look

  p_raw <- validate_p_raw(p_raw, mcpObj$IndexSet)
  mcpObj$p_raw <- addNAPvalue(p_raw, mcpObj$IntialHypothesis)

  mcpObj$CutOff <- state$thresholds[next_look]

  mcpObj <- PerLookMCPAnalysis(mcpObj, mvtnorm_algo = state$mvtnorm_algo)

  # Pre-computation for the next look
  mcpObj$rej_flag_Prev <- mcpObj$rej_flag_Curr
  mcpObj$WH_Prev <- mcpObj$WH

  if (isTRUE(plotGraphs)) {
    plot_graph_after_analysis(
      mcpObj,
      title = paste("Graph After Stage", mcpObj$CurrentLook, "analysis")
    )
  }

  state$completed_looks <- next_look
  state$mcpObj <- mcpObj

  state$look_history[[next_look]] <- list(
    mcpObj = mcpObj,
    inputs = list(
      p_raw = p_raw,
      selection = selection,
      new_weights = new_weights,
      new_G = new_G,
      new_correlation = new_correlation
    )
  )

  if (isTRUE(StopTrial(mcpObj)) || length(mcpObj$IndexSet) == 0 || next_look == mcpObj$LastLook) {
    state$trial_completed <- TRUE
  }

  return(state)
}

#' Plot analysis graph from a PCAnalysisState object
#'
#' @param state A "PCAnalysisState" object.
#' @param stage Which stage to plot.
#'   - NULL: plot the current (latest) graph
#'   - 0 or "initial": plot the initial graph
#'   - positive integer k: plot graph snapshot after look k analysis
#' @param title Optional title override.
#'
#' @return A visNetwork widget.
#' @export
PlotAnalysisGraph <- function(state, stage = NULL, title = NULL) {
  if (!inherits(state, "PCAnalysisState")) stop("state must be a PCAnalysisState object")

  if (is.null(stage)) {
    if (state$completed_looks == 0) {
      mcpObj <- state$setup_mcpObj
      default_title <- "Initial Graph"
    } else {
      mcpObj <- state$look_history[[state$completed_looks]]$mcpObj
      default_title <- paste("Graph After Stage", state$completed_looks, "analysis")
    }
  } else if (identical(stage, 0L) || identical(stage, 0) || identical(stage, "initial")) {
    mcpObj <- state$setup_mcpObj
    default_title <- "Initial Graph"
  } else if (is.numeric(stage) && length(stage) == 1) {
    if (stage != as.integer(stage)) {
      stop("stage must be a whole number (integer-valued)")
    }
    stage <- as.integer(stage)
    if (stage < 1 || stage > state$completed_looks) {
      stop("stage must be between 1 and completed_looks")
    }
    mcpObj <- state$look_history[[stage]]$mcpObj
    default_title <- paste("Graph After Stage", stage, "analysis")
  } else {
    stop("Unsupported stage value")
  }

  if (is.null(title)) title <- default_title

  widget <- plot_graph_after_analysis(mcpObj, title = title)
  return(widget)
}
