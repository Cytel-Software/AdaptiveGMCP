# --------------------------------------------------------------------------------------------------
#
# Stateless analysis interface (p-value combination method) - exported API
#
# --------------------------------------------------------------------------------------------------

.pc_get_design_group_sequential <- function(design_params,
                                            information_rates,
                                            k_max = length(information_rates),
                                            user_alpha_spending = NULL) {
  type_of_design <- if (is.null(user_alpha_spending)) {
    design_params$typeOfDesign
  } else {
    "asUser"
  }

  if (type_of_design == "WT") {
    return(rpact::getDesignGroupSequential(
      kMax = k_max,
      alpha = design_params$alpha,
      informationRates = information_rates,
      typeOfDesign = type_of_design,
      deltaWT = design_params$deltaWT
    ))
  }

  if (type_of_design == "PT") {
    return(rpact::getDesignGroupSequential(
      kMax = k_max,
      alpha = design_params$alpha,
      informationRates = information_rates,
      typeOfDesign = type_of_design,
      deltaPT1 = design_params$deltaPT1
    ))
  }

  if (type_of_design == "asHSD" || type_of_design == "asKD") {
    return(rpact::getDesignGroupSequential(
      kMax = k_max,
      alpha = design_params$alpha,
      informationRates = information_rates,
      typeOfDesign = type_of_design,
      gammaA = design_params$gammaA
    ))
  }

  if (type_of_design == "asUser") {
    spending <- if (is.null(user_alpha_spending)) {
      design_params$userAlphaSpending
    } else {
      user_alpha_spending
    }

    return(rpact::getDesignGroupSequential(
      kMax = k_max,
      alpha = design_params$alpha,
      informationRates = information_rates,
      typeOfDesign = type_of_design,
      userAlphaSpending = spending
    ))
  }

  return(rpact::getDesignGroupSequential(
    kMax = k_max,
    alpha = design_params$alpha,
    informationRates = information_rates,
    typeOfDesign = type_of_design
  ))
}

.pc_compute_invnorm_weights <- function(info_frac) {
  k <- length(info_frac)
  info_frac_incr <- c(info_frac[1], diff(info_frac))

  inv_norm_weights <- matrix(NA_real_, nrow = k, ncol = k)
  for (i in seq_len(k)) {
    for (j in seq_len(i)) {
      inv_norm_weights[i, j] <- sqrt(info_frac_incr[j] / info_frac[i])
    }
  }

  colnames(inv_norm_weights) <- paste0("W", seq_len(k))
  rownames(inv_norm_weights) <- paste0("Look", seq_len(k))

  if (k > 1L) {
    w_norm <- inv_norm_weights[-1, , drop = FALSE]
  } else {
    w_norm <- matrix(numeric(0), nrow = 0, ncol = k)
    colnames(w_norm) <- colnames(inv_norm_weights)
  }

  return(list(
    InvNormWeights = inv_norm_weights,
    W_Norm = w_norm
  ))
}

#' Setup analysis object for Adaptive GMCP (P-value combination method)
#'
#' Creates a non-interactive analysis state object. Use [AnalyzeLookPC()] to
#' advance through the pre-specified looks one at a time. The inverse-normal
#' weights and stopping boundaries are fixed at setup and are not recalculated
#' during analysis.
#'
#' @param vWeights Vector of node weights for the initial graph
#' @param mGMatrix Transition matrix for the graph.
#' @param strTestType Character specifying test type.
#' Supported values: "Bonf", "Sidak", "Simes", "Dunnett", "Partly-Parametric".
#' @param dAlpha One-sided type-1 error.
#' @param vInfoFracts Vector of cumulative information fractions planned at design
#' time. Its length determines the number of formal analysis looks.
#' @param strDesignType Group sequential design type (rpact).
#' Supported values: "OF" (O'Brien & Fleming), "P" (Pocock), "WT" (Wang & Tsiatis Delta class),
#'      "PT" (Pampallona & Tsiatis), "HP" (Haybittle & Peto),
#'      "WToptimum" (Optimum design within Wang & Tsiatis class),
#'      "asOF" (O'Brien & Fleming type alpha spending), "asP" (Pocock type alpha spending),
#'      "asKD" (Kim & DeMets alpha spending), "asHSD" (Hwang, Shi & DeCani alpha spending),
#'      "noEarlyEfficacy" (no early efficacy stop), "asUser" (user specified alpha); default is "asOF".
#' @param dDeltaWT Parameter for strDesignType = "WT".
#' @param dDeltaPT1 Parameter for strDesignType = "PT".
#' @param dGammaA Parameter for strDesignType = "asHSD" or "asKD".
#' @param vUserAlphaSpending Alpha spending values for strDesignType = "asUser".
#' @param bMultipleWinners Logical; TRUE means reject as many hypotheses as possible,
#'        FALSE means reject at most one hypothesis
#' @param bSelection Logical; TRUE if selection of hypotheses is allowed at interim looks
#' @param bUpdateStrategy Logical; TRUE if the graphical test strategy can be modified at interim looks
#' @param bPlotGraphs Logical; if TRUE, plots the initial graph.
#'
#' @return An object of class "PCAnalysisState".
#' @export
SetupAnalysisPC <- function(
    vWeights = c(0.5, 0.5, 0, 0),
    mGMatrix = matrix(c(
      0, 0.5, 0.5, 0,
      0.5, 0, 0, 0.5,
      0, 1, 0, 0,
      1, 0, 0, 0
    ), byrow = TRUE, nrow = 4),
    strTestType = "Partly-Parametric", # TODO: Check if Partly-Parametric and Dunnett types are really required
                                     # since they can be derived from the correlation matrix provided by the user.
                                     # Only Bonf, Sidak, and Simes are strictly necessary to be specified.
    dAlpha = 0.025,
    vInfoFracts = c(0.5, 1.0),
    strDesignType = "asOF",
    dDeltaWT = 0,
    dDeltaPT1 = 0,
    dGammaA = 2,
    vUserAlphaSpending = NULL,
    bMultipleWinners = TRUE,
    bSelection = TRUE, # TODO: Check if Selection parameter is really required at setup stage.
    bUpdateStrategy = TRUE, # TODO: Check if UpdateStrategy parameter is really required at setup stage.
    bPlotGraphs = TRUE) {
  if (!is.numeric(vWeights) || anyNA(vWeights) || any(vWeights < 0)) stop("vWeights must be numeric, non-negative, and non-NA")
  if (sum(vWeights) > 1) stop("Sum of vWeights must be less than or equal to 1")
  d <- length(vWeights)
  if (!is.matrix(mGMatrix) || !all(dim(mGMatrix) == c(d, d))) stop("mGMatrix must be a ", d, " x ", d, " matrix")

  k <- length(vInfoFracts)
  if (k < 1) stop("vInfoFracts must have length >= 1")
  if (any(vInfoFracts <= 0) || any(vInfoFracts > 1)) stop("vInfoFracts must be in (0, 1]")
  if (any(diff(vInfoFracts) <= 0)) stop("vInfoFracts must be strictly increasing")

  GlobalIndexSet <- paste0("H", seq_len(d))
  HypoMap <- data.frame(
    Hypothesis = GlobalIndexSet,
    row.names = NULL
  )

  # mvtnorm algorithm
  mvtnorm_algo <- chooseMVTAlgo(d)

  # Correlation handling by test type
  if (strTestType == "Bonf") {
    Correlation <- diag(d)
    Correlation[Correlation == 0] <- NA
  } else if (strTestType == "Sidak" || strTestType == "Simes") {
    Correlation <- NA
  } else if (strTestType == "Dunnett" || strTestType == "Partly-Parametric") {
    Correlation <- NULL
  } else {
    stop("Unsupported strTestType: ", strTestType)
  }

  # Group sequential boundaries (stage-wise p-value boundaries)
  if (strDesignType == "asUser" && is.null(vUserAlphaSpending)) {
    stop("vUserAlphaSpending must be provided when strDesignType = 'asUser'")
  }

  tmp_design_params <- list(
    alpha = dAlpha,
    typeOfDesign = strDesignType,
    deltaWT = dDeltaWT,
    deltaPT1 = dDeltaPT1,
    gammaA = dGammaA,
    userAlphaSpending = vUserAlphaSpending
  )

  des <- .pc_get_design_group_sequential(
    design_params = tmp_design_params,
    information_rates = vInfoFracts,
    k_max = k
  )

  thresholds <- des$stageLevels
  incr_alpha <- c(des$alphaSpent[1], diff(des$alphaSpent))
  bdryTab <- data.frame(
    "Look" = seq_len(k),
    "Information_Fraction" = vInfoFracts,
    "Incr_alpha_spent" = incr_alpha,
    "ZScale_Eff_Bbry" = des$criticalValues,
    "PValue_Eff_Bbry" = thresholds,
    row.names = NULL
  )

  # Weights for all intersection hypotheses
  allGraphs <- genWeights(w = vWeights, g = mGMatrix, HypothesisName = GlobalIndexSet)
  WH <- allGraphs$IntersectionWeights

  # Inverse normal weights
  inv_norm <- .pc_compute_invnorm_weights(vInfoFracts)
  InvNormWeights <- inv_norm$InvNormWeights
  W_Norm <- inv_norm$W_Norm

  # Initialize flags
  rej_flag_Prev <- rej_flag_Curr <- DroppedFlag <- rep(FALSE, d)
  names(rej_flag_Prev) <- names(rej_flag_Curr) <- names(DroppedFlag) <- GlobalIndexSet

  mcpObj <- list(
    "CurrentLook" = 0L,
    "IntialWeights" = vWeights,
    "IntialHypothesis" = GlobalIndexSet,
    "HypoMap" = HypoMap,
    "test.type" = strTestType,
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
    "MultipleWinners" = bMultipleWinners,
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
    WI = vWeights,
    G = mGMatrix,
    test.type = strTestType,
    alpha = dAlpha,
    info_frac = vInfoFracts,
    typeOfDesign = strDesignType,
    deltaWT = dDeltaWT,
    deltaPT1 = dDeltaPT1,
    gammaA = dGammaA,
    userAlphaSpending = vUserAlphaSpending,
    MultipleWinners = bMultipleWinners,
    Selection = bSelection,
    UpdateStrategy = bUpdateStrategy
  )

  state <- new_pc_analysis_state(
    mcpObj = mcpObj,
    mvtnorm_algo = mvtnorm_algo,
    thresholds = thresholds,
    setup_mcpObj = mcpObj,
    design_params = design_params,
    completed_looks = 0L,
    trial_completed = FALSE,
    look_history = vector("list", 0)
  )

  if (isTRUE(bPlotGraphs)) {
    plotGraph(HypothesisName = GlobalIndexSet, w = vWeights, G = mGMatrix, Title = "Initial Graph")
  }

  return(state)
}

#' Analyze one look for Adaptive GMCP (P-value combination method)
#'
#' Advances an existing [SetupAnalysisPC()] state object by exactly one look.
#' The state-implied look number selects the pre-specified inverse-normal
#' combination weights and stopping boundary. The caller is responsible for
#' performing each formal analysis at its scheduled information fraction.
#' Optional hypothesis selection, strategy modification, and correlation updates
#' may be applied before looks after look 1 are analyzed (these represent
#' decisions made after the previous look). At look 1, selection and strategy modification inputs
#' are ignored with a warning because the initial design from setup is used.
#'
#' @param cState A "PCAnalysisState" object.
#' @param vRawPVals Named numeric vector of raw p-values for the current look,
#'   for the currently active hypotheses (`cState$mcpObj$IndexSet`).
#' @param mCorrelation Optional D_active x D_active correlation matrix for the
#'   currently active hypotheses (\code{length(cState$mcpObj$IndexSet)}). Mandatory
#'   at look 1 when \code{test.type} is \code{"Dunnett"} or \code{"Partly-Parametric"}.
#'   For subsequent looks, if supplied for those test types it updates the
#'   correlation used in analysis; if \code{NULL}, the most recently stored
#'   correlation is used.
#' @param nLook Optional positive integer explicitly naming the current look number.
#'   When provided, it must match the look number implied by the state object
#'   (i.e. \code{cState$completed_looks + 1}). This argument exists purely for
#'   readability of user scripts - it does not change program logic.
#'   If \code{NULL} (default), the look number is inferred from the state.
#' @param vSelectHypo Optional character vector of hypotheses to retain for this look.
#'   Applied only when look > 1; if supplied at look 1, it is ignored with a warning.
#' @param vWeights Optional numeric vector of new weights for continuing hypotheses.
#'   Applied only when look > 1; if supplied at look 1, it is ignored with a warning.
#' @param mGMatrix Optional transition matrix for continuing hypotheses.
#'   Applied only when look > 1; if supplied at look 1, it is ignored with a warning.
#' @param bPlotGraphs Logical; if TRUE, plots graphs at key points.
#'
#' @return Updated "PCAnalysisState".
#' @export
AnalyzeLookPC <- function(
    cState,
    vRawPVals,
    mCorrelation = NULL,
    nLook = NULL,
    vSelectHypo = NULL,
    vWeights = NULL,
    mGMatrix = NULL,
    bPlotGraphs = TRUE) {
  if (!inherits(cState, "PCAnalysisState")) stop("state must be a PCAnalysisState object")

  if (isTRUE(cState$trial_completed)) {
    stop("Trial already concluded - stopping criteria met.")
  }

  mcpObj <- cState$mcpObj
  next_look <- as.integer(cState$completed_looks + 1L)

  if (next_look > mcpObj$LastLook) {
    stop(
      "All planned looks have been analyzed. ",
      "The trial analysis is already complete."
    )
  }

  if (!is.null(nLook)) {
    if (!is.numeric(nLook) || length(nLook) != 1L || nLook != as.integer(nLook) || nLook < 1L) {
      stop("look must be a single positive integer.")
    }
    nLook <- as.integer(nLook)
    if (nLook != next_look) {
      stop(
        "look argument (", nLook, ") does not match the expected next look (",
        next_look, ") based on the analysis state. ",
        "The next look to be analyzed is look ", next_look, "."
      )
    }
  }

  if (next_look == 1 &&
      mcpObj$test.type %in% c("Dunnett", "Partly-Parametric") &&
      is.null(mCorrelation)) {
    stop("mCorrelation must be provided at look 1 for Dunnett or Partly-Parametric tests")
  }

  # Apply optional changes before analyzing look > 1
  if (next_look > 1) {
    if (!is.null(vSelectHypo)) {
      if (!isTRUE(cState$design_params$Selection)) {
        stop("vSelectHypo cannot be applied: selection was disabled in SetupAnalysisPC()")
      }
      mcpObj <- applySelection(mcpObj, vSelectHypo, look = next_look)
      if (isTRUE(bPlotGraphs)) {
        plot_graph_after_selection(mcpObj, title = "Graph After Selection")
      }
    }

    if (!is.null(vWeights) || !is.null(mGMatrix)) {
      if (!isTRUE(cState$design_params$UpdateStrategy)) {
        stop("vWeights/mGMatrix cannot be applied: strategy updates were disabled in SetupAnalysisPC()")
      }
      mcpObj$ModificationLook <- c(mcpObj$ModificationLook, as.integer(cState$completed_looks))
      mcpObj <- applyStrategyUpdate(mcpObj, vWeights, mGMatrix)
      if (isTRUE(bPlotGraphs)) {
        plotGraph(
          HypothesisName = mcpObj$IndexSet,
          w = mcpObj$newWeights,
          G = mcpObj$newG,
          Title = "Modified Graph"
        )
      }
    }
  } else {
    vIgnoredAdaptations <- character()
    if (!is.null(vSelectHypo)) {
      vIgnoredAdaptations <- c(vIgnoredAdaptations, "vSelectHypo")
    }
    if (!is.null(vWeights)) {
      vIgnoredAdaptations <- c(vIgnoredAdaptations, "vWeights")
    }
    if (!is.null(mGMatrix)) {
      vIgnoredAdaptations <- c(vIgnoredAdaptations, "mGMatrix")
    }
    if (length(vIgnoredAdaptations) > 0L) {
      warning(
        "Look 1 uses the initial design from SetupAnalysisPC(); ",
        paste(vIgnoredAdaptations, collapse = ", "),
        " will be ignored.",
        call. = FALSE
      )
    }
  }

  if (length(mcpObj$IndexSet) == 0) {
    cState$trial_completed <- TRUE
    cState$completion_reason <- "all_hypotheses_dropped"
    cState$mcpObj <- mcpObj
    return(cState)
  }

  mcpObj$CurrentLook <- next_look

  vRawPVals <- validate_p_raw(vRawPVals, mcpObj$IndexSet)

  if (!is.null(mCorrelation) && is.matrix(mCorrelation)) {
    n_active <- length(mcpObj$IndexSet)
    if (!all(dim(mCorrelation) == c(n_active, n_active))) {
      stop(
        "mCorrelation must be a ", n_active, " x ", n_active,
        " matrix matching the currently active hypotheses."
      )
    }
  }

  if (!is.null(mCorrelation) &&
      mcpObj$test.type %in% c("Dunnett", "Partly-Parametric")) {
    mcpObj <- applyCorrelationUpdate(
      mcpObj,
      mCorrelation,
      target_hypotheses = names(vRawPVals)
    )
  }

  mcpObj$p_raw <- addNAPvalue(vRawPVals, mcpObj$IntialHypothesis)
  mcpObj$CutOff <- cState$thresholds[next_look]

  mcpObj <- PerLookMCPAnalysis(mcpObj, mvtnorm_algo = cState$mvtnorm_algo)

  # Pre-computation for the next look
  mcpObj$rej_flag_Prev <- mcpObj$rej_flag_Curr
  mcpObj$WH_Prev <- mcpObj$WH

  if (isTRUE(bPlotGraphs)) {
    plot_graph_after_analysis(
      mcpObj,
      title = paste("Graph After Stage", mcpObj$CurrentLook, "analysis")
    )
  }

  cState$completed_looks <- next_look
  cState$mcpObj <- mcpObj
  cState$design_params$test.type <- mcpObj$test.type

  cState$look_history[[next_look]] <- list(
    is_final_look = next_look == mcpObj$LastLook,
    mcpObj = mcpObj,
    inputs = list(
      p_raw = vRawPVals,
      Correlation = mCorrelation,
      selection = vSelectHypo,
      new_weights = vWeights,
      new_G = mGMatrix
    )
  )

  if (isTRUE(StopTrial(mcpObj)) || length(mcpObj$IndexSet) == 0 || next_look == mcpObj$LastLook) {
    cState$trial_completed <- TRUE
    if (length(mcpObj$IndexSet) == 0) {
      cState$completion_reason <- "all_hypotheses_dropped"
    } else if (next_look == mcpObj$LastLook) {
      cState$completion_reason <- "final_look"
    } else {
      cState$completion_reason <- "early_stop_efficacy"
    }
  }

  return(cState)
}

#' Plot analysis graph from a PCAnalysisState object
#'
#' @param cState A "PCAnalysisState" object.
#' @param nStage Which stage to plot.
#'   - NULL: plot the current (latest) graph
#'   - 0 or "initial": plot the initial graph
#'   - positive integer k: plot graph snapshot after look k analysis
#' @param strTitle Optional title override.
#'
#' @return A visNetwork widget.
#' @export
PlotAnalysisGraph <- function(cState, nStage = NULL, strTitle = NULL) {
  if (!inherits(cState, "PCAnalysisState")) stop("state must be a PCAnalysisState object")

  if (is.null(nStage)) {
    if (cState$completed_looks == 0) {
      mcpObj <- cState$setup_mcpObj
      default_title <- "Initial Graph"
    } else {
      mcpObj <- cState$look_history[[cState$completed_looks]]$mcpObj
      default_title <- paste("Graph After Stage", cState$completed_looks, "analysis")
    }
  } else if (identical(nStage, 0L) || identical(nStage, 0) || identical(nStage, "initial")) {
    mcpObj <- cState$setup_mcpObj
    default_title <- "Initial Graph"
  } else if (is.numeric(nStage) && length(nStage) == 1) {
    if (nStage != as.integer(nStage)) {
      stop("nStage must be a whole number (integer-valued)")
    }
    nStage <- as.integer(nStage)
    if (nStage < 1 || nStage > cState$completed_looks) {
      stop("nStage must be between 1 and completed_looks")
    }
    mcpObj <- cState$look_history[[nStage]]$mcpObj
    default_title <- paste("Graph After Stage", nStage, "analysis")
  } else {
    stop("Unsupported nStage value")
  }

  if (is.null(strTitle)) strTitle <- default_title

  widget <- plot_graph_after_analysis(mcpObj, title = strTitle)
  return(widget)
}
