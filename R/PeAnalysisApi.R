# --------------------------------------------------------------------------------------------------
#
# Stateless analysis interface (population enrichment, p-value combination method)
#
# --------------------------------------------------------------------------------------------------

ValidatePESampleSizes <- function(fullpop_sample_sizes, subpop_sample_sizes)
{
  if( !is.numeric( fullpop_sample_sizes ) || !is.numeric( subpop_sample_sizes ) )
  {
    stop( "fullpop_sample_sizes and subpop_sample_sizes must be numeric vectors." )
  }

  if( length( fullpop_sample_sizes ) != length( subpop_sample_sizes ) )
  {
    stop( "fullpop_sample_sizes and subpop_sample_sizes must have the same length." )
  }

  if( length( fullpop_sample_sizes ) < 2 )
  {
    stop( "Sample size vectors must include a control arm and at least one treatment arm." )
  }

  vArgs <- c( fullpop_sample_sizes, subpop_sample_sizes )

  if( anyNA( vArgs ) || !all( is.finite( vArgs ) ) )
  {
    stop( "Sample sizes must be finite, non-missing numeric values." )
  }

  if( any( vArgs <= 0 ) )
  {
    stop( "Sample sizes must be strictly positive." )
  }

  if( any( subpop_sample_sizes > fullpop_sample_sizes ) )
  {
    stop( "Subgroup sample sizes must not exceed full-population sample sizes." )
  }

  return( invisible( TRUE ) )
}

ComputePETreatmentCorrelation <- function(sample_sizes, treatment_index_1, treatment_index_2)
{
  control_size <- sample_sizes[ 1 ]
  treatment_size_1 <- sample_sizes[ 1 + treatment_index_1 ]
  treatment_size_2 <- sample_sizes[ 1 + treatment_index_2 ]

  dCorr <- 1 / control_size / sqrt(
    ( 1 / control_size + 1 / treatment_size_1 ) *
      ( 1 / control_size + 1 / treatment_size_2 )
  )

  if( !is.finite( dCorr ) || dCorr <= 0 || dCorr > 1 )
  {
    stop( "Derived treatment correlation is outside the valid range (0, 1]." )
  }

  return( dCorr )
}

ComputePENestedPopulationCorrelation <- function(fullpop_sample_sizes, subpop_sample_sizes, treatment_index)
{
  dRho <- sqrt(
    ( 1 / fullpop_sample_sizes[ 1 + treatment_index ] + 1 / fullpop_sample_sizes[ 1 ] ) /
      ( 1 / subpop_sample_sizes[ 1 + treatment_index ] + 1 / subpop_sample_sizes[ 1 ] )
  )

  if( !is.finite( dRho ) || dRho <= 0 || dRho > 1 )
  {
    stop( "Derived population correlation is outside the valid range (0, 1]." )
  }

  return( dRho )
}

BuildPECorrelationMatrix <- function(
    fullpop_sample_sizes,
    subpop_sample_sizes,
    n_hypotheses,
    hypothesis_names = NULL)
{
  ValidatePESampleSizes(
    fullpop_sample_sizes = fullpop_sample_sizes,
    subpop_sample_sizes = subpop_sample_sizes
  )

  n_treatments <- length( fullpop_sample_sizes ) - 1L
  if( n_treatments < 1L )
  {
    stop( "Sample size vectors must contain at least one treatment arm." )
  }

  if( length( n_hypotheses ) != 1 || !is.numeric( n_hypotheses ) ||
      is.na( n_hypotheses ) || n_hypotheses <= 0 || n_hypotheses != as.integer( n_hypotheses ) )
  {
    stop( "n_hypotheses must be a single positive integer value." )
  }

  n_hypotheses <- as.integer( n_hypotheses )

  n_populations <- 2L

  if( n_hypotheses %% n_populations != 0 )
  {
    stop( "The number of hypotheses must be divisible by 2 populations." )
  }

  if( ( n_hypotheses / n_populations ) %% n_treatments != 0 )
  {
    stop(
      "n_hypotheses divided by 2 must be an integer multiple of the number of treatment arms."
    )
  }

  n_endpoints <- ( n_hypotheses / n_populations ) / n_treatments
  if( n_endpoints < 1 )
  {
    stop( "At least one endpoint is required to build the PE correlation matrix." )
  }

  dCorrelation <- matrix( NA_real_, nrow = n_hypotheses, ncol = n_hypotheses )
  diag( dCorrelation ) <- 1

  dPopulationBlockSize <- n_endpoints * n_treatments

  GetHypothesisIndex <- function(population_index, endpoint_index, arm_index)
  {
    ( population_index - 1L ) * dPopulationBlockSize +
      ( endpoint_index - 1L ) * n_treatments +
      arm_index
  }

  for( iEndpoint in seq_len( n_endpoints ) )
  {
    for( iArm in seq_len( n_treatments ) )
    {
      for( jArm in seq_len( n_treatments ) )
      {
        if( iArm == jArm )
        {
          next
        }

        dFullIdx1 <- GetHypothesisIndex( 1L, iEndpoint, iArm )
        dFullIdx2 <- GetHypothesisIndex( 1L, iEndpoint, jArm )
        dSubIdx1 <- GetHypothesisIndex( 2L, iEndpoint, iArm )
        dSubIdx2 <- GetHypothesisIndex( 2L, iEndpoint, jArm )

        dFullCorr <- ComputePETreatmentCorrelation(
          sample_sizes = fullpop_sample_sizes,
          treatment_index_1 = iArm,
          treatment_index_2 = jArm
        )
        dSubCorr <- ComputePETreatmentCorrelation(
          sample_sizes = subpop_sample_sizes,
          treatment_index_1 = iArm,
          treatment_index_2 = jArm
        )

        dCorrelation[ dFullIdx1, dFullIdx2 ] <- dFullCorr
        dCorrelation[ dFullIdx2, dFullIdx1 ] <- dFullCorr
        dCorrelation[ dSubIdx1, dSubIdx2 ] <- dSubCorr
        dCorrelation[ dSubIdx2, dSubIdx1 ] <- dSubCorr

        dCrossCorr <- sqrt(
          ComputePENestedPopulationCorrelation(
            fullpop_sample_sizes = fullpop_sample_sizes,
            subpop_sample_sizes = subpop_sample_sizes,
            treatment_index = iArm
          ) *
            ComputePENestedPopulationCorrelation(
              fullpop_sample_sizes = fullpop_sample_sizes,
              subpop_sample_sizes = subpop_sample_sizes,
              treatment_index = jArm
            )
        ) * sqrt( dFullCorr * dSubCorr )

        dCorrelation[ dFullIdx1, dSubIdx2 ] <- dCrossCorr
        dCorrelation[ dSubIdx2, dFullIdx1 ] <- dCrossCorr
      }

      dFullNested <- ComputePENestedPopulationCorrelation(
        fullpop_sample_sizes = fullpop_sample_sizes,
        subpop_sample_sizes = subpop_sample_sizes,
        treatment_index = iArm
      )

      dFullIdx <- GetHypothesisIndex( 1L, iEndpoint, iArm )
      dSubIdx <- GetHypothesisIndex( 2L, iEndpoint, iArm )

      dCorrelation[ dFullIdx, dSubIdx ] <- dFullNested
      dCorrelation[ dSubIdx, dFullIdx ] <- dFullNested
    }
  }

  if( !is.null( hypothesis_names ) )
  {
    if( length( hypothesis_names ) != n_hypotheses )
    {
      stop( "Length of hypothesis_names must equal n_hypotheses." )
    }

    rownames( dCorrelation ) <- hypothesis_names
    colnames( dCorrelation ) <- hypothesis_names
  }

  return( dCorrelation )
}

#' Setup analysis object for population enrichment (P-value combination method)
#'
#' Creates a non-interactive population-enrichment analysis state object.
#' The look-specific correlation matrix is computed in [AnalyzeLook_PE_PC()] so
#' setup remains design-only.
#'
#' PE wrapper contract:
#' - [SetupAnalysis_PE_PC()] does not compute or store a correlation matrix.
#' - [AnalyzeLook_PE_PC()] requires look-specific population sample sizes and
#'   computes the correlation matrix for that look.
#' - Sample-size vectors follow order \code{(n0, n1, n2, ...)} where \code{n0}
#'   is the control arm.
#' - Hypothesis ordering is endpoint-major, treatment-minor, with a full-
#'   population block followed by a subgroup block.
#'
#' @param WI Vector of node weights for the initial graph.
#' @param G Transition matrix for the graph.
#' @param test.type Character specifying test type.
#' Supported values: "Bonf", "Sidak", "Simes", "Dunnett", "Partly-Parametric".
#' @param alpha One-sided type-1 error.
#' @param info_frac Vector of cumulative information fractions.
#' @param typeOfDesign Group sequential design type (rpact).
#' @param deltaWT Parameter for typeOfDesign = "WT".
#' @param deltaPT1 Parameter for typeOfDesign = "PT".
#' @param gammaA Parameter for typeOfDesign = "asHSD" or "asKD".
#' @param userAlphaSpending Alpha spending values for typeOfDesign = "asUser".
#' @param info_frac_tolerance Numeric scalar in (0, 1); absolute tolerance for
#' matching current information fraction to planned fractions in the PC engine.
#' @param MultipleWinners Logical; TRUE means reject as many hypotheses as
#' possible, FALSE means reject at most one hypothesis.
#' @param Selection Logical; TRUE if selection of hypotheses is allowed at
#' interim looks.
#' @param UpdateStrategy Logical; TRUE if the graphical test strategy can be
#' modified at interim looks.
#' @param plotGraphs Logical; if TRUE, plots the initial graph.
#'
#' @return An object of class "PCAnalysisState".
#' @export
SetupAnalysis_PE_PC <- function(
    WI,
    G,
    test.type = "Partly-Parametric",
    alpha = 0.025,
    info_frac = c( 0.5, 1.0 ),
    typeOfDesign = "asOF",
    deltaWT = 0,
    deltaPT1 = 0,
    gammaA = 2,
    userAlphaSpending = NULL,
    info_frac_tolerance = 0.05,
    MultipleWinners = TRUE,
    Selection = TRUE,
    UpdateStrategy = TRUE,
    plotGraphs = TRUE )
{
  state <- SetupAnalysis_PC(
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
    info_frac_tolerance = info_frac_tolerance,
    MultipleWinners = MultipleWinners,
    Selection = Selection,
    UpdateStrategy = UpdateStrategy,
    plotGraphs = plotGraphs
  )

  return( state )
}

#' Analyze one look for population enrichment (P-value combination method)
#'
#' Advances an existing [SetupAnalysis_PE_PC()] state object by exactly one
#' look. The look-specific population correlation matrix is derived from the
#' supplied full- and sub-population sample sizes and then passed to
#' [AnalyzeLook_PC()].
#'
#' The two sample-size vectors must follow \code{(n0, n1, n2, ...)} where
#' \code{n0} is the control arm and subsequent entries correspond to treatment
#' arms in the same order used to define hypotheses.
#'
#' @param state A "PCAnalysisState" object created by [SetupAnalysis_PE_PC()].
#' @param p_raw Named numeric vector of raw p-values for active hypotheses.
#' @param fullpop_sample_sizes Numeric vector of cumulative full-population sample
#' sizes ordered as control first, then treatment arms. Must be non-decreasing
#' across looks and the same length at every look.
#' @param subpop_sample_sizes Numeric vector of cumulative subgroup sample sizes
#' ordered as control first, then treatment arms. Must be non-decreasing across
#' looks and the same length at every look.
#' @param look Optional positive integer naming the current look number.
#' @param info_frac_cur Optional numeric scalar; current-look cumulative
#' information fraction. If NULL, the planned fraction for this look is used.
#' @param selection Optional character vector of hypotheses to retain for this
#' look (only meaningful when look > 1).
#' @param new_weights Optional numeric vector of new weights for continuing
#' hypotheses.
#' @param new_G Optional transition matrix for continuing hypotheses.
#' @param plotGraphs Logical; if TRUE, plots graphs at key points.
#'
#' @return Updated "PCAnalysisState" object.
#' @export
AnalyzeLook_PE_PC <- function(
    state,
    p_raw,
    fullpop_sample_sizes,
    subpop_sample_sizes,
    look = NULL,
    info_frac_cur = NULL,
    selection = NULL,
    new_weights = NULL,
    new_G = NULL,
    plotGraphs = TRUE )
{
  if( state$completed_looks > 0L )
  {
    vPrevFull <- state$pe_sample_history[[ state$completed_looks ]]$fullpop
    vPrevSub  <- state$pe_sample_history[[ state$completed_looks ]]$subpop

    if( length( fullpop_sample_sizes ) != length( vPrevFull ) ||
        length( subpop_sample_sizes ) != length( vPrevSub ) )
    {
      stop(
        "fullpop_sample_sizes and subpop_sample_sizes must have the same length as at look 1."
      )
    }

    if( any( fullpop_sample_sizes < vPrevFull ) || any( subpop_sample_sizes < vPrevSub ) )
    {
      stop( "Cumulative sample sizes must be non-decreasing across looks." )
    }
  }

  n_initial <- length( state$mcpObj$IntialHypothesis )
  dFullCorrelation <- BuildPECorrelationMatrix(
    fullpop_sample_sizes = fullpop_sample_sizes,
    subpop_sample_sizes  = subpop_sample_sizes,
    n_hypotheses         = n_initial,
    hypothesis_names     = state$mcpObj$IntialHypothesis
  )

  # Use the post-selection active set when selection is specified at look > 1,
  # because AnalyzeLook_PC applies selection before validating the correlation matrix.
  vActiveNames <- if( !is.null( selection ) && state$completed_looks > 0L )
  {
    if( !all( selection %in% state$mcpObj$IndexSet ) )
    {
      stop(
        "selection must be a subset of current IndexSet. Current IndexSet: ",
        toString( state$mcpObj$IndexSet )
      )
    }

    intersect( state$mcpObj$IndexSet, unique( selection ) )
  } else
  {
    state$mcpObj$IndexSet
  }

  dCorrelation <- dFullCorrelation[ vActiveNames, vActiveNames, drop = FALSE ]

  nPreviousLooks <- state$completed_looks

  if (is.null(info_frac_cur)) {
    nNextLook <- state$completed_looks + 1L
    nPlannedLooks <- length(state$design_params$info_frac)
    info_frac_cur <- state$design_params$info_frac[min(nNextLook, nPlannedLooks)]
  }

  state <- do.call(
    AnalyzeLook_PC,
    list(
      state = state,
      p_raw = p_raw,
      info_frac_cur = info_frac_cur,
      Correlation = dCorrelation,
      look = look,
      selection = selection,
      new_weights = new_weights,
      new_G = new_G,
      plotGraphs = plotGraphs
    )
  )

  if(state$completed_looks == nPreviousLooks + 1L)
  {
    # Sample size history should be updated only if the analysis 
    # was successfully completed for this look.
    state$pe_sample_history[[ state$completed_looks ]] <- list(
      fullpop = fullpop_sample_sizes,
      subpop  = subpop_sample_sizes
    )
  }

  return( state )
}