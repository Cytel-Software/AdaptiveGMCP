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

ValidatePEActualWithinPlanned <- function(
    fullpop_sample_sizes,
    subpop_sample_sizes,
    planned_fullpop_sample_sizes,
    planned_subpop_sample_sizes)
{
  if( length( fullpop_sample_sizes ) != length( planned_fullpop_sample_sizes ) ||
      length( subpop_sample_sizes ) != length( planned_subpop_sample_sizes ) )
  {
    stop(
      "Current-look sample-size vectors must have the same length as the planned baseline vectors."
    )
  }

  if( any( fullpop_sample_sizes > planned_fullpop_sample_sizes ) )
  {
    stop( "Cumulative full-population sample sizes at a look must be <= planned full-population sample sizes." )
  }

  if( any( subpop_sample_sizes > planned_subpop_sample_sizes ) )
  {
    stop( "Cumulative subgroup sample sizes at a look must be <= planned subgroup sample sizes." )
  }

  return( invisible( TRUE ) )
}

ComputePEInfoFracFromControl <- function(fullpop_sample_sizes, planned_fullpop_sample_sizes)
{
  dNumerator <- fullpop_sample_sizes[ 1 ]
  dDenominator <- planned_fullpop_sample_sizes[ 1 ]

  if( !is.finite( dDenominator ) || dDenominator <= 0 )
  {
    stop( "Planned full-population control-arm sample size must be finite and > 0." )
  }

  dInfoFrac <- dNumerator / dDenominator

  if( !is.finite( dInfoFrac ) || dInfoFrac <= 0 )
  {
    stop( "Computed information fraction must be finite and > 0." )
  }

  return( list(
    info_frac_cur = dInfoFrac,
    numerator_fullpop_control = dNumerator,
    denominator_planned_fullpop_control = dDenominator
  ) )
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
#' @param planned_info_frac Vector of cumulative information fractions planned at design
#' time for the trial schedule across all looks.
#' @param typeOfDesign Group sequential design type (rpact).
#' @param deltaWT Parameter for typeOfDesign = "WT".
#' @param deltaPT1 Parameter for typeOfDesign = "PT".
#' @param gammaA Parameter for typeOfDesign = "asHSD" or "asKD".
#' @param userAlphaSpending Alpha spending values for typeOfDesign = "asUser".
#' @param info_frac_tolerance Numeric scalar in (0, 1); absolute tolerance for
#' matching current information fraction to planned fractions in the PC engine.
#' @param planned_fullpop_sample_sizes Numeric vector of planned cumulative
#' full-population sample sizes for the entire trial ordered as control first, then treatment arms.
#' @param planned_subpop_sample_sizes Numeric vector of planned cumulative
#' subgroup sample sizes for the entire trial ordered as control first, then treatment arms.
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
    planned_info_frac = c( 0.5, 1.0 ),
    typeOfDesign = "asOF",
    deltaWT = 0,
    deltaPT1 = 0,
    gammaA = 2,
    userAlphaSpending = NULL,
    info_frac_tolerance = 0.05,
    planned_fullpop_sample_sizes,
    planned_subpop_sample_sizes,
    MultipleWinners = TRUE,
    Selection = TRUE,
    UpdateStrategy = TRUE,
    plotGraphs = TRUE )
{
  ValidatePESampleSizes(
    fullpop_sample_sizes = planned_fullpop_sample_sizes,
    subpop_sample_sizes = planned_subpop_sample_sizes
  )

  state <- SetupAnalysis_PC(
    WI = WI,
    G = G,
    test.type = test.type,
    alpha = alpha,
    planned_info_frac = planned_info_frac,
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

  state$design_params$planned_fullpop_sample_sizes <- planned_fullpop_sample_sizes
  state$design_params$planned_subpop_sample_sizes <- planned_subpop_sample_sizes
  state$pe_sample_history <- vector( "list", 0 )

  return( state )
}

#' Analyze one look for population enrichment (P-value combination method)
#'
#' Advances an existing [SetupAnalysis_PE_PC()] state object by exactly one
#' look. The look-specific population correlation matrix is derived from the
#' supplied full- and sub-population sample sizes and then passed to
#' [AnalyzeLook_PC()].
#'
#' The current-look information fraction is computed internally as
#' \code{fullpop_sample_sizes[1] / planned_fullpop_sample_sizes[1]} using the
#' control-arm cumulative sample size only.
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
    selection = NULL,
    new_weights = NULL,
    new_G = NULL,
    plotGraphs = TRUE )
{
  if( !inherits( state, "PCAnalysisState" ) )
  {
    stop( "state must be a PCAnalysisState object" )
  }

  if( is.null( state$design_params$planned_fullpop_sample_sizes ) ||
      is.null( state$design_params$planned_subpop_sample_sizes ) )
  {
    stop(
      "state is missing planned PE sample-size baselines. Recreate state with SetupAnalysis_PE_PC()."
    )
  }

  ValidatePESampleSizes(
    fullpop_sample_sizes = fullpop_sample_sizes,
    subpop_sample_sizes = subpop_sample_sizes
  )

  vPlannedFull <- state$design_params$planned_fullpop_sample_sizes
  vPlannedSub <- state$design_params$planned_subpop_sample_sizes

  ValidatePEActualWithinPlanned(
    fullpop_sample_sizes = fullpop_sample_sizes,
    subpop_sample_sizes = subpop_sample_sizes,
    planned_fullpop_sample_sizes = vPlannedFull,
    planned_subpop_sample_sizes = vPlannedSub
  )

  if( state$completed_looks > 0L )
  {
    vPrevFull <- state$pe_sample_history[[ state$completed_looks ]]$fullpop
    vPrevSub <- state$pe_sample_history[[ state$completed_looks ]]$subpop

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

  lInfoFrac <- ComputePEInfoFracFromControl(
    fullpop_sample_sizes = fullpop_sample_sizes,
    planned_fullpop_sample_sizes = vPlannedFull
  )

  nInitial <- length( state$mcpObj$IntialHypothesis )
  dFullCorrelation <- BuildPECorrelationMatrix(
    fullpop_sample_sizes = fullpop_sample_sizes,
    subpop_sample_sizes = subpop_sample_sizes,
    n_hypotheses = nInitial,
    hypothesis_names = state$mcpObj$IntialHypothesis
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

  state <- do.call(
    AnalyzeLook_PC,
    list(
      state = state,
      p_raw = p_raw,
      info_frac_cur = lInfoFrac$info_frac_cur,
      Correlation = dCorrelation,
      look = look,
      selection = selection,
      new_weights = new_weights,
      new_G = new_G,
      plotGraphs = plotGraphs
    )
  )

  if( state$completed_looks == nPreviousLooks + 1L )
  {
    # Update PE look history only after successful completion of this look.
    lPELookInfo <- list(
      fullpop = fullpop_sample_sizes,
      subpop = subpop_sample_sizes,
      info_frac_cur = lInfoFrac$info_frac_cur,
      numerator_fullpop_control = lInfoFrac$numerator_fullpop_control,
      denominator_planned_fullpop_control = lInfoFrac$denominator_planned_fullpop_control
    )

    state$pe_sample_history[[ state$completed_looks ]] <- lPELookInfo

    state$look_history[[ state$completed_looks ]]$pe_info_frac <- list(
      info_frac_cur = lInfoFrac$info_frac_cur,
      numerator_fullpop_control = lInfoFrac$numerator_fullpop_control,
      denominator_planned_fullpop_control = lInfoFrac$denominator_planned_fullpop_control
    )
  }

  return( state )
}