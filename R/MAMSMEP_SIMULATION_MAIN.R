# --------------------------------------------------------------------------------------------------
#
# ©2025 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

# TODO:
# 1. Deprecate the parameter FWERControl as its name and values are confusing.
#    Instead, add a new parameter for determining whether the stage 2 statistic should be
#    computed using cumulative stage 2 data or as a weighted combination of the incremental
#    stage 1 and stage 2 statistics in case of CER method.
# 2. Verify if ImplicitSSR is correctly coded and understand how its different values are expected to impact the results.
# 3. Disable parameter EastSumStat as it is useful only for debugging. Users will have no use for it.

#' Function to simulate multi-arm, multi-stage, multi-endpoint trials using either 
#' p-value combination method or CER method
#' @param Method Character scalar specifying the analysis method to be used for ensuring strong FWER control.
#'   One of "CombPValue" (p-value combination method) or "CER" (conditional error rate method).
#' @param alpha Numeric scalar in (0, 1) giving the one-sided family-wise type I error rate.
#' @param SampleSize Positive integer scalar giving the maximum planned total sample size.
#' @param TestStatCont Character scalar specifying the test statistic for continuous endpoints.
#'   One of "t-equal", "t-unequal", or "z". Required when `lEpType` includes "Continuous".
#'   Ignored for non-continuous endpoints.
#' @param TestStatBin Character scalar specifying the test statistic for binary endpoints.
#'   One of "UnPooled" or "Pooled". Required when `lEpType` includes "Binary". Ignored for non-binary endpoints.
#' @param UseCC Logical scalar. If `TRUE`, applies a continuity correction when estimating binary
#'   response proportions; ignored for non-binary endpoints.
#' @param FWERControl Character scalar controlling the stage-1/2 combination used under
#'   `Method = "CER"`. One of "CombinationTest" (combine incremental stage statistics) or
#'   "None" (use cumulative statistics). Ignored when `Method != "CER"`.
#' @param nArms Integer scalar giving the number of trial arms (including control). Must be >= 2.
#' @param nEps Integer scalar giving the number of endpoints. Must be >= 1.
#' @param lEpType Named list of length `nEps` specifying endpoint types. Each element must be one
#'   of "Continuous" or "Binary".
#' @param Arms.Mean Named list of length `nEps`. For continuous endpoints, each element is a
#'   numeric vector of length `nArms` giving arm-wise means (control first, then treatments).
#'   Use `NA` for endpoints that are not continuous.
#' @param Arms.std.dev Named list of length `nEps`. For continuous endpoints, each element is a
#'   numeric vector of length `nArms` giving arm-wise standard deviations (control first, then treatments).
#'   Use `NA` for endpoints that are not continuous.
#' @param CommonStdDev Logical scalar for continuous endpoints. If `TRUE`, treatment standard
#'   deviations are assumed equal to the control standard deviation for boundary computations.
#'   Ignored for non-continuous endpoints.
#' @param Arms.Prop Named list of length `nEps`. For binary endpoints, each element is a numeric
#'   vector of length `nArms` giving arm-wise response proportions in [0, 1] (control first, then treatments).
#'   Use `NA` for endpoints that are not binary.
#' @param Arms.alloc.ratio Numeric vector of length `nArms` giving the allocation ratio by arm
#'   (control first, then treatments). Values are internally rescaled so the control ratio equals 1.
#' @param EP.Corr Numeric `nEps x nEps` correlation matrix for endpoints, used to generate
#'   correlated endpoint data. Required when `nEps > 1`, and should be positive semi-definite in that case.
#' @param WI Numeric vector of initial node weights for the graphical procedure. Must have length
#'   `nEps * (nArms - 1)` with `sum(WI) <= 1`.
#'   Hypotheses are ordered by endpoint then treatment, e.g. for two treatments and two endpoints:
#'   H1 = (Trt1 vs Ctrl, EP1), H2 = (Trt2 vs Ctrl, EP1), H3 = (Trt1 vs Ctrl, EP2),
#'   H4 = (Trt2 vs Ctrl, EP2).
#' @param G Numeric transition matrix for the graph with dimension
#'   `nEps * (nArms - 1) x nEps * (nArms - 1)`. Diagonal elements must be 0 and each row sum must
#'   be <= 1.
#' @param test.type Character scalar specifying the testing procedure.
#'   For `Method = "CombPValue"`: one of "Bonf", "Sidak", "Simes", "Dunnett", or "Partly-Parametric".
#'   For `Method = "CER"`: one of "Parametric", "Non-Parametric", or "Partly-Parametric".
#' @param info_frac Numeric vector of information fractions (look positions) with values in (0, 1]
#'   with the final element equal to 1.
#' @param typeOfDesign Character scalar giving the group sequential design type
#'   Must be one of: "OF", "P", "WT", "PT", "HP", "WToptimum", "asOF", "asP", "asKD", "asHSD", 
#'   "noEarlyEfficacy", or "asUser".
#' @param deltaWT Numeric scalar parameter for the Wang & Tsiatis delta class when
#'   `typeOfDesign = "WT"`.
#' @param deltaPT1 Numeric scalar parameter for the Pampallona & Tsiatis class when
#'   `typeOfDesign = "PT"`.
#' @param gammaA Numeric scalar parameter for the alpha-spending functions when
#'   `typeOfDesign` is "asHSD" or "asKD".
#' @param userAlphaSpending Numeric vector of cumulative alpha spent at each look. Only used when
#'   `typeOfDesign = "asUser"` (and `Method = "CombPValue"`); otherwise ignored.
#' @param MultipleWinners Logical scalar. If `TRUE`, the trial continues until efficacy
#'   is possible for no further endpoint; if `FALSE`, it stops once efficacy is observed 
#'   for at least one endpoint.
#' @param Selection Logical scalar indicating whether arm selection is performed at interim looks.
#'   Only relevant when `length(info_frac) > 1`.
#' @param SelectionLook Integer vector of interim look indices at which selection is performed.
#'   For a two-look design, this must be 1 when `Selection` is `TRUE`.
#' @param SelectEndPoint Endpoint considered for selection. Either an integer in `1:nEps` (select based
#'   on that endpoint) or the character scalar "overall" (consider all endpoints).
#' @param SelectionScale Character scalar indicating the metric used for selection. One of
#'   "delta", "teststat", "stderror", or "pvalue".
#' @param SelectionCriterion Character scalar indicating the selection rule. One of "best",
#'   "threshold", "epsilon", or "random". See SelectionParameter for more info.
#' @param SelectionParameter Numeric scalar tuning parameter for the selection rule:
#'   number selected for "best", threshold value for "threshold", or epsilon neighborhood size
#'   for "epsilon".
#' @param KeepAssociatedHypo Logical scalar. If `TRUE`, keeps all hypotheses associated with the
#'   selected treatment arms.
#' @param SelectionParmeter Deprecated. Use \code{SelectionParameter} instead.
#' @param KeepAssosiatedEps Deprecated. Use \code{KeepAssociatedHypo} instead.
#' @param ImplicitSSR Character scalar specifying implicit sample size reallocation after interim.
#'   One of "Selection" (reallocate only from de-selected arms), "All" (allocate all planned
#'   samples to remaining arms), or "None".
#' @param nSimulation Positive integer scalar giving the number of simulation runs to perform.
#' @param nSimulation_Stage2 Positive integer scalar giving the number of stage-2 simulations per
#'   stage-1 simulation. Only applicable for `Method = "CER"`; forced to 1 otherwise.
#' @param Seed Either the character scalar "Random" (generate a run-specific seed) or an integer
#'   scalar seed used for reproducibility.
#' @param SummaryStat Logical scalar. If `TRUE`, returns simulation-level summary data.
#' @param plotGraphs Logical scalar. If `TRUE`, plots the graph specified by WI and G.
#' @param EastSumStat East summary statistics input used for single-look designs with
#'   `Method = "CombPValue"`. Use `NULL` (default) to disable.
#' @param SaveCERSimulationTrace Logical scalar. If `TRUE` and `Method = "CER"`,
#'   stores per-simulation replay traces that can later be written to `.rds`
#'   fixtures for non-interactive CER regression testing. Trace-enabled runs
#'   execute serially so the complete trace payload is retained in all R
#'   environments, including development sessions using `load_all()`.
#' @param Parallel Logical scalar indicating whether to run simulations in parallel.
#' @param Verbose Logical scalar. If `TRUE`, prints additional progress and diagnostic messages.
#' @example ./internalData/MAMSMEP_Simulation_Example.R
#' @export
simMAMSMEP <- function(
    Method = "CombPValue",
    alpha = 0.025,
    SampleSize = 500,
    TestStatCont = "t-equal",
    TestStatBin = "UnPooled",
    UseCC = FALSE,
    FWERControl = "None",
    nArms = 3,
    nEps = 2,
    lEpType = list(
      "EP1" = "Continuous",
      "EP2" = "Binary"
    ),
    Arms.Mean = list(
      "EP1" = c(0, 0.4, 0.3),
      "EP2" = NA
    ),
    Arms.std.dev = list(
      "EP1" = c(1.1, 1.2, 1.3),
      "EP2" = NA
    ),
    CommonStdDev = FALSE,
    Arms.Prop = list(
      "EP1" = NA,
      "EP2" = c(0.2, 0.35, 0.45)
    ),
    Arms.alloc.ratio = c(1, 1, 1),
    EP.Corr = matrix(
      c(
        1, 0.5,
        0.5, 1
      ),
      nrow = 2
    ),
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
    info_frac = c(0.5, 1),
    typeOfDesign = "asOF",
    deltaWT = 0,
    deltaPT1 = 0,
    gammaA = 2,
    userAlphaSpending = rpact::getDesignGroupSequential(
      sided = 1, alpha = alpha,informationRates =info_frac,
      typeOfDesign = "asOF")$alphaSpent,
    MultipleWinners = TRUE,
    Selection = TRUE,
    SelectionLook = 1,
    SelectEndPoint = 1,
    SelectionScale = "pvalue",
    SelectionCriterion = "best",
    SelectionParameter = 1,
    KeepAssociatedHypo = TRUE,
    SelectionParmeter = NULL,
    KeepAssosiatedEps = NULL,
    ImplicitSSR = "All",
    nSimulation = 100,
    nSimulation_Stage2 = 1, # this should always take the value 1 when CER is NOT selected
    Seed = 100,
    SummaryStat = FALSE,
    plotGraphs = TRUE,
    EastSumStat = NULL,
    SaveCERSimulationTrace = FALSE,
    Parallel = TRUE,
    Verbose = FALSE) {

  # Handle deprecated parameter aliases (renamed to fix typos)
  if (!is.null(SelectionParmeter)) {
    warning("'SelectionParmeter' is deprecated; use 'SelectionParameter' instead.")
    SelectionParameter <- SelectionParmeter
  }
  if (!is.null(KeepAssosiatedEps)) {
    warning("'KeepAssosiatedEps' is deprecated; use 'KeepAssociatedHypo' instead.")
    KeepAssociatedHypo <- KeepAssosiatedEps
  }

  if (isTRUE(SaveCERSimulationTrace) && isTRUE(Parallel)) {
    warning("Parallel simulation is disabled when SaveCERSimulationTrace = TRUE.")
    Parallel <- FALSE
  }

  # Ani: Applying correction suggested by Pralay when allocation ratio for
  # control is not equal to 1.
  # This is done by dividing all arm wise alloc ratios by the alloc ratio for
  # control so that the control alloc ratio is always 1 as is assumed in the code.
  Arms.alloc.ratio <- Arms.alloc.ratio / Arms.alloc.ratio[1]

  # nSimulation_Stage2 should always take value 1 if method is not CER
  if (Method != "CER") {
    nSimulation_Stage2 = 1
  }

  TailType <- "RightTail" ## Default Right
  UpdateStrategy <- F ## Not implemented yet
  des.type <- "MAMSMEP" ## Multi-Arm Multi-Stage Multi-EndPoints

  if(nArms == 2){
    # For two arms only non-parametric tests are applicable
    if(Method == "CombPValue"){
      if(test.type == "Dunnett" || test.type == "Partly-Parametric"){
        print("As parametric tests are not applicable 'test.type' converted to 'Bonf'")
        test.type <- "Bonf"
      }
    }else if(Method == "CER"){
      if(test.type == "Parametric" || test.type == "Partly-Parametric"){
        print("As parametric tests are not applicable 'test.type' converted to 'Non-Parametric'")
        test.type <- "Non-Parametric"
      }
    }
  }

  # Dimension-based algorithm selection for mvtnorm::pmvnorm()
  # Calculate the dimension of the multivariate normal distribution
  mvtnorm_dimension <- (nArms - 1) * nEps
  mvtnorm_algo <- chooseMVTAlgo(mvtnorm_dimension)

  # Object to run Simulations
  gmcpSimObj <<- list(
    # Methodology
    "Method" = Method,

    # test parameters
    "TestStatCont" = TestStatCont, "alpha" = alpha,
    "nArms" = nArms, "nLooks" = length(info_frac),
    "nEps" = nEps, "nHypothesis" = nEps * (nArms - 1),
    "TailType" = TailType, "des.type" = des.type,
    "Max_SS" = SampleSize, "test.type" = test.type,
    "IntialWeights" = WI, "G" = G,
    "Correlation" = NA, "FWERControl" = FWERControl,
    "lEpType" = lEpType, "TestStatBin" = TestStatBin,
    "UseCC" = UseCC, # Parameter used to configure whether continuity
                     # correction should be applied while estimating binary
                     # response proportions or not

    # Boundary
    "InfoFrac" = info_frac, "typeOfDesign" = typeOfDesign,
    "CommonStdDev" = CommonStdDev, "deltaWT"= deltaWT,
    "deltaPT1" = deltaPT1, "gammaA" = gammaA,
    "userAlphaSpending" = userAlphaSpending,

    # Multiple Winners
    "MultipleWinners" = MultipleWinners,

    # Response Generation
    "Arms.Mean" = Arms.Mean, "Arms.std.dev" = Arms.std.dev,
    "Arms.alloc.ratio" = Arms.alloc.ratio, "Arms.alloc.ratio" = Arms.alloc.ratio,
    "EP.Corr" = EP.Corr, "Arms.Prop" = Arms.Prop,
    "prop.ctr" = lapply(Arms.Prop, function(x) {
      x[2]
    }),

    # Selection
    "SelectEndPoint" = SelectEndPoint, "Selection" = Selection,
    "SelectionLook" = SelectionLook, "SelectionScale" = SelectionScale,
    "SelectionCriterion" = SelectionCriterion, "SelectionParameter" = SelectionParameter,
    "KeepAssociatedHypo" = KeepAssociatedHypo,

    # Simulation Parameters
    "nSimulation" = nSimulation, "Seed" = Seed,
    "SummaryStat" = SummaryStat, "Parallel" = Parallel,
    "Verbose" = Verbose, # Enable verbose output for detailed simulation progress

    # SSR
    "ImplicitSSR" = ImplicitSSR,

    # Update Strategy
    "UpdateStrategy" = UpdateStrategy,

    # Graph Plot
    "plotGraphs" = plotGraphs,

    #EastSumStat
    "EastSumStat" = EastSumStat,

    # Optional CER replay tracing
    "SaveCERSimulationTrace" = SaveCERSimulationTrace,

    # number of simulations for stage 2 per stage 1
    "nSimulation_Stage2" = nSimulation_Stage2,

    # mvtnorm algorithm (dimension-based selection)
    "mvtnorm_algo" = mvtnorm_algo,

    # Parameter added to enable debugging
    "Debug" = FALSE
  )

  logs <- valInpsimMAMSMEP(inps = gmcpSimObj)
  FailedLogs <- logs[sapply(logs, function(lg) lg != 0)]

  if (length(FailedLogs) != 0) {
    return(FailedLogs)
    stop("Input Error")
  }
  out <- modified_MAMSMEP_sim2(gmcpSimObj)
  if (plotGraphs) {
    # out$iniGraph
    return(out$DetailOutTabs)
  } else {
    return(out$DetailOutTabs)
  }
}
