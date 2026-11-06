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
#' The look-specific correlation matrix is computed in [AnalyzeLookPEPC()] so
#' setup remains design-only.
#'
#' PE wrapper contract:
#' - [SetupAnalysisPEPC()] does not compute or store a correlation matrix.
#' - [AnalyzeLookPEPC()] requires look-specific population sample sizes and
#'   computes the correlation matrix for that look.
#' - \code{vInfoFracts} defines the planned analysis schedule and
#'   boundaries; it is distinct from the cumulative sample sizes supplied to
#'   [AnalyzeLookPEPC()] at each formal look.
#' - Sample-size vectors follow order \code{(n0, n1, n2, ...)} where \code{n0}
#'   is the control arm.
#' - Hypothesis ordering is endpoint-major, treatment-minor, with a full-
#'   population block followed by a subgroup block.
#'
#' Common input requirements are that subgroup sample sizes do not exceed
#' full-population sample sizes, cumulative sample sizes do not decrease
#' between looks, and the same arm order is used for the graph, p-values, and
#' both sample-size vectors.
#'
#' @param vWeights Vector of node weights for the initial graph.
#' @param mGMatrix Transition matrix for the graph.
#' @param strTestType Character specifying test type.
#' Supported values: "Bonf", "Sidak", "Simes", "Dunnett", "Partly-Parametric".
#' @param dAlpha One-sided type-1 error.
#' @param vInfoFracts Vector of cumulative information fractions planned at design
#' time for the trial schedule across looks. PE analysis currently supports
#' one-look or two-look workflows only. Values determine the fixed PC
#' combination weights and stopping boundaries, and must be finite, strictly
#' increasing, and in (0, 1].
#' @param strDesignType Group sequential design type (rpact).
#' @param dDeltaWT Parameter for strDesignType = "WT".
#' @param dDeltaPT1 Parameter for strDesignType = "PT".
#' @param dGammaA Parameter for strDesignType = "asHSD" or "asKD".
#' @param vUserAlphaSpending Alpha spending values for strDesignType = "asUser".
#' @param bMultipleWinners Logical; TRUE means reject as many hypotheses as
#' possible, FALSE means reject at most one hypothesis.
#' @param bSelection Logical; TRUE if selection of hypotheses is allowed at
#' interim looks.
#' @param bUpdateStrategy Logical; TRUE if the graphical test strategy can be
#' modified at interim looks.
#' @param bPlotGraphs Logical; if TRUE, plots the initial graph.
#'
#' @return An object of class "PCAnalysisState".
#' @export
SetupAnalysisPEPC <- function(
    vWeights,
    mGMatrix,
    strTestType = "Partly-Parametric",
    dAlpha = 0.025,
    vInfoFracts = c( 0.5, 1.0 ),
    strDesignType = "asOF",
    dDeltaWT = 0,
    dDeltaPT1 = 0,
    dGammaA = 2,
    vUserAlphaSpending = NULL,
    bMultipleWinners = TRUE,
    bSelection = TRUE,
    bUpdateStrategy = TRUE,
    bPlotGraphs = TRUE )
{
  if( !is.numeric( vInfoFracts ) || length( vInfoFracts ) < 1L ||
      length( vInfoFracts ) > 2L )
  {
    stop(
      "PE analysis currently supports one-look or two-look workflows only. ",
      "vInfoFracts must be a numeric vector of length 1 or 2."
    )
  }

  if( anyNA( vInfoFracts ) || !all( is.finite( vInfoFracts ) ) )
  {
    stop( "vInfoFracts must contain finite, non-missing numeric values." )
  }

  if( any( vInfoFracts <= 0 ) || any( vInfoFracts > 1 ) )
  {
    stop( "vInfoFracts values must be in (0, 1]." )
  }

  if( length( vInfoFracts ) > 1L && any( diff( vInfoFracts ) <= 0 ) )
  {
    stop( "vInfoFracts must be strictly increasing." )
  }

  state <- SetupAnalysisPC(
    vWeights = vWeights,
    mGMatrix = mGMatrix,
    strTestType = strTestType,
    dAlpha = dAlpha,
    vInfoFracts = vInfoFracts,
    strDesignType = strDesignType,
    dDeltaWT = dDeltaWT,
    dDeltaPT1 = dDeltaPT1,
    dGammaA = dGammaA,
    vUserAlphaSpending = vUserAlphaSpending,
    bMultipleWinners = bMultipleWinners,
    bSelection = bSelection,
    bUpdateStrategy = bUpdateStrategy,
    bPlotGraphs = bPlotGraphs
  )

  state$pe_sample_history <- vector( "list", 0 )

  return( state )
}

#' Analyze one look for population enrichment (P-value combination method)
#'
#' Advances an existing state from [SetupAnalysisPEPC()] by exactly one look.
#' The current-look population correlation matrix is derived from the
#' supplied full- and sub-population sample sizes and then passed to
#' [AnalyzeLookPC()].
#' PE analysis currently supports a maximum of two analyzed looks per design state.
#' The caller is responsible for performing each formal analysis at its
#' scheduled information fraction.
#'
#' The two sample-size vectors must follow \code{(n0, n1, n2, ...)} where
#' \code{n0} is the control arm and subsequent entries correspond to treatment
#' arms in the same order used to define hypotheses.
#' Sample sizes are cumulative counts observed at the formal look, not
#' increment-only counts. They may differ from the planned information
#' fractions, but must have the same length at every look, remain
#' non-decreasing, and satisfy subgroup \code{<=} full-population counts.
#'
#' @param cState A "PCAnalysisState" object created by [SetupAnalysisPEPC()].
#' @param vRawPVals Named numeric vector of raw p-values for active hypotheses.
#' @param vFullPopulationSampleSizes Numeric vector of cumulative full-population sample
#' sizes ordered as control first, then treatment arms. Must be non-decreasing
#' across looks and the same length at every look.
#' @param vSubpopulationSampleSizes Numeric vector of cumulative subgroup sample sizes
#' ordered as control first, then treatment arms. Must be non-decreasing across
#' looks and the same length at every look.
#' @param nLook Optional positive integer naming the current look number.
#' @param vSelectHypo Optional character vector of hypotheses to retain for this
#' look (only meaningful when look > 1).
#' @param vWeights Optional numeric vector of new weights for continuing
#' hypotheses.
#' @param mGMatrix Optional transition matrix for continuing hypotheses.
#' @param bPlotGraphs Logical; if TRUE, plots graphs at key points.
#'
#' @return Updated "PCAnalysisState" object.
#' @export
AnalyzeLookPEPC <- function(
    cState,
    vRawPVals,
    vFullPopulationSampleSizes,
    vSubpopulationSampleSizes,
    nLook = NULL,
    vSelectHypo = NULL,
    vWeights = NULL,
    mGMatrix = NULL,
    bPlotGraphs = TRUE )
{
  if( !inherits( cState, "PCAnalysisState" ) )
  {
    stop( "state must be a PCAnalysisState object" )
  }

  if( isTRUE( cState$trial_completed ) )
  {
    stop( "Trial already concluded - stopping criteria met." )
  }

  if( cState$completed_looks >= 2L )
  {
    stop(
      "PE analysis currently supports a maximum of 2 analyzed looks per design."
    )
  }

  ValidatePESampleSizes(
    fullpop_sample_sizes = vFullPopulationSampleSizes,
    subpop_sample_sizes = vSubpopulationSampleSizes
  )

  if( cState$completed_looks > 0L )
  {
    vPrevFull <- cState$pe_sample_history[[ cState$completed_looks ]]$fullpop
    vPrevSub <- cState$pe_sample_history[[ cState$completed_looks ]]$subpop

    if( length( vFullPopulationSampleSizes ) != length( vPrevFull ) ||
        length( vSubpopulationSampleSizes ) != length( vPrevSub ) )
    {
      stop(
        "Full- and sub-population sample-size vectors must match their lengths at look 1."
      )
    }

    if( any( vFullPopulationSampleSizes < vPrevFull ) || any( vSubpopulationSampleSizes < vPrevSub ) )
    {
      stop( "Cumulative sample sizes must be non-decreasing across looks." )
    }
  }

  nInitial <- length( cState$mcpObj$IntialHypothesis )
  dFullCorrelation <- BuildPECorrelationMatrix(
    fullpop_sample_sizes = vFullPopulationSampleSizes,
    subpop_sample_sizes = vSubpopulationSampleSizes,
    n_hypotheses = nInitial,
    hypothesis_names = cState$mcpObj$IntialHypothesis
  )

  # Use the post-selection active set when selection is specified at look > 1,
  # because AnalyzeLookPC applies selection before validating the correlation matrix.
  vActiveNames <- if( !is.null( vSelectHypo ) && cState$completed_looks > 0L )
  {
    if( !all( vSelectHypo %in% cState$mcpObj$IndexSet ) )
    {
      stop(
        "selection must be a subset of current IndexSet. Current IndexSet: ",
        toString( cState$mcpObj$IndexSet )
      )
    }

    intersect( cState$mcpObj$IndexSet, unique( vSelectHypo ) )
  } else
  {
    cState$mcpObj$IndexSet
  }

  dCorrelation <- dFullCorrelation[ vActiveNames, vActiveNames, drop = FALSE ]

  nPreviousLooks <- cState$completed_looks

  cState <- do.call(
    AnalyzeLookPC,
    list(
      cState = cState,
      vRawPVals = vRawPVals,
      mCorrelation = dCorrelation,
      nLook = nLook,
      vSelectHypo = vSelectHypo,
      vWeights = vWeights,
      mGMatrix = mGMatrix,
      bPlotGraphs = bPlotGraphs
    )
  )

  if( cState$completed_looks == nPreviousLooks + 1L )
  {
    # Update PE look history only after successful completion of this look.
    lPELookInfo <- list(
      fullpop = vFullPopulationSampleSizes,
      subpop = vSubpopulationSampleSizes
    )

    cState$pe_sample_history[[ cState$completed_looks ]] <- lPELookInfo
  }

  return( cState )
}