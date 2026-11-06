# --------------------------------------------------------------------------------------------------
#
# Non-interactive Conditional Error Rate analysis API
#
# --------------------------------------------------------------------------------------------------

# Validate one integer-valued scalar used by the public CER API.
# Parameters are the value, its argument name for error reporting, and its
# inclusive lower bound. Return the validated value as an integer.
.cer_validate_scalar_integer <- function( x, arg_name, minimum )
{
  if( length( x ) != 1L || !is.numeric( x ) || is.na( x ) ||
      !is.finite( x ) || x != as.integer( x ) || x < minimum )
  {
    stop( arg_name, " must be a single integer >= ", minimum, "." )
  }
  return( as.integer( x ) )
}

# Validate the complete set of inputs required to construct a CER design.
# Parameters are the design dimensions, endpoint inputs, graph inputs, testing
# settings, information fractions, spending settings, and optional user
# spending vector. Return invisibly after checking all domains and restrictions
# so downstream planning helpers can assume consistent inputs.
.cer_validate_design_inputs <- function(
    nArms, nEps, SampleSize, EpType, sigma, CommonStdDev, prop.ctr, allocRatio,
    WI, G, test.type, alpha, info_frac, typeOfDesign,
    deltaWT, deltaPT1, gammaA, userAlphaSpending )
{
  nArms <- .cer_validate_scalar_integer( nArms, "nArms", 2L )
  nEps <- .cer_validate_scalar_integer( nEps, "nEps", 1L )
  nHypothesis <- nEps * ( nArms - 1L )

  if( length( SampleSize ) != 1L || !is.numeric( SampleSize ) ||
      is.na( SampleSize ) || !is.finite( SampleSize ) || SampleSize <= 0 )
  {
    stop( "SampleSize must be a single finite positive number." )
  }
  if( !is.list( EpType ) || length( EpType ) != nEps ||
      any( !unlist( lapply( EpType, function( x ) length( x ) == 1L &&
        x %in% c( "Continuous", "Binary" ) ) ) ) )
  {
    stop( "EpType must be a list of Continuous or Binary endpoints." )
  }
  if( !is.list( sigma ) || length( sigma ) != nEps )
  {
    stop( "sigma must be a list with one entry per endpoint." )
  }
  if( !is.list( prop.ctr ) || length( prop.ctr ) != nEps )
  {
    stop( "prop.ctr must be a list with one entry per endpoint." )
  }
  if( length( CommonStdDev ) != 1L || !is.logical( CommonStdDev ) ||
      is.na( CommonStdDev ) )
  {
    stop( "CommonStdDev must be a single logical value." )
  }
  # Endpoint-specific checks differ because continuous endpoints use sigma,
  # whereas binary endpoints use the control response proportion.
  for( iEndpoint in seq_len( nEps ) )
  {
    if( EpType[[ iEndpoint ]] == "Continuous" )
    {
      vSigma <- sigma[[ iEndpoint ]]
      if( !is.numeric( vSigma ) || length( vSigma ) != nArms ||
          anyNA( vSigma ) || any( !is.finite( vSigma ) ) || any( vSigma <= 0 ) )
      {
        stop( "sigma for each Continuous endpoint must contain nArms positive values." )
      }
    } else
    {
      dProp <- prop.ctr[[ iEndpoint ]]
      if( length( dProp ) != 1L || !is.numeric( dProp ) || is.na( dProp ) ||
          !is.finite( dProp ) || dProp <= 0 || dProp >= 1 )
      {
        stop( "prop.ctr for each Binary endpoint must be in (0, 1)." )
      }
    }
  }
  if( length( allocRatio ) != nArms || !is.numeric( allocRatio ) ||
      anyNA( allocRatio ) || any( !is.finite( allocRatio ) ) ||
      any( allocRatio <= 0 ) )
  {
    stop( "allocRatio must contain nArms finite positive values." )
  }
  if( !is.numeric( WI ) || length( WI ) != nHypothesis || anyNA( WI ) ||
      any( !is.finite( WI ) ) || any( WI < 0 ) || sum( WI ) > 1 + 1e-8 )
  {
    stop( "WI must contain non-negative finite weights summing to at most 1." )
  }
  if( !is.matrix( G ) || !is.numeric( G ) ||
      !all( dim( G ) == c( nHypothesis, nHypothesis ) ) ||
      anyNA( G ) || any( !is.finite( G ) ) || any( G < 0 ) )
  {
    stop( "G must be a finite non-negative square transition matrix matching WI." )
  }
  if( any( diag( G ) != 0 ) || any( rowSums( G ) > 1 + 1e-8 ) )
  {
    stop( "G must have zero diagonal and row sums no greater than 1." )
  }
  if( !test.type %in% c( "Parametric", "Non-Parametric", "Partly-Parametric" ) )
  {
    stop( "Unsupported CER test.type." )
  }
  if( length( alpha ) != 1L || !is.numeric( alpha ) || is.na( alpha ) ||
      !is.finite( alpha ) || alpha <= 0 || alpha >= 1 )
  {
    stop( "alpha must be a single number in (0, 1)." )
  }
  for( lParameter in list(
    deltaWT = deltaWT, deltaPT1 = deltaPT1, gammaA = gammaA
  ) )
  {
    if( length( lParameter ) != 1L || !is.numeric( lParameter ) ||
        is.na( lParameter ) || !is.finite( lParameter ) )
    {
      stop( "CER spending-function parameters must be finite scalars." )
    }
  }
  if( !is.numeric( info_frac ) || length( info_frac ) != 2L ||
      anyNA( info_frac ) || any( !is.finite( info_frac ) ) ||
      any( info_frac <= 0 ) || any( info_frac > 1 ) ||
      any( diff( info_frac ) <= 0 ) || info_frac[ 2L ] != 1 )
  {
    stop( "info_frac must contain two strictly increasing values ending at 1." )
  }
  vDesignTypes <- c(
    "OF", "P", "WT", "PT", "HP", "WToptimum", "asOF", "asP",
    "asKD", "asHSD", "noEarlyEfficacy", "asUser"
  )
  if( length( typeOfDesign ) != 1L || !typeOfDesign %in% vDesignTypes )
  {
    stop( "Unsupported typeOfDesign." )
  }
  if( typeOfDesign == "asUser" )
  {
    if( !is.numeric( userAlphaSpending ) ||
        length( userAlphaSpending ) != 2L ||
        anyNA( userAlphaSpending ) || any( !is.finite( userAlphaSpending ) ) ||
        any( userAlphaSpending <= 0 ) || any( userAlphaSpending >= 1 ) ||
        any( diff( userAlphaSpending ) <= 0 ) )
    {
      stop( "userAlphaSpending must contain two increasing values in (0, 1)." )
    }
  }

  return( invisible( TRUE ) )
}

# Build the legacy MCP-shaped list consumed by the existing CER analysis
# helpers from `design`, an immutable CERDesign object. Return the initialized
# mutable analysis list.
.cer_plan_mcp <- function( design )
{
  vHypotheses <- design$hypothesis_names
  dHypotheses <- length( vHypotheses )
  vFlags <- rep( FALSE, dHypotheses )
  names( vFlags ) <- vHypotheses
  return( list(
    CurrentLook = 0L,
    lEpType = design$EpType,
    IntialHypothesis = vHypotheses,
    test.type = design$test.type,
    IndexSet = vHypotheses,
    ArmsPresent = seq_len( design$nArms ),
    sigma = design$sigma,
    CommonStdDev = design$CommonStdDev,
    prop.ctr = design$prop.ctr,
    allocRatio = design$allocRatio,
    p_raw = rep( NA_real_, dHypotheses ),
    p_raw_stage1 = rep( NA_real_, dHypotheses ),
    WH_Prev = design$intersection_weights,
    WH = design$intersection_weights,
    MultipleWinners = TRUE,
    rej_flag_Prev = vFlags,
    rej_flag_Curr = vFlags,
    SelectionLook = integer( 0 ),
    SelectedIndex = character( 0 ),
    DroppedFlag = vFlags,
    LastLook = 2L,
    Modify = FALSE,
    ModificationLook = integer( 0 ),
    newWeights = NA,
    newG = NA,
    Stage1Obj = NULL,
    AllocSampleSize = design$allocation$cumulative,
    Stage2AllocSampleSize = design$allocation$cumulative,
    Stage2allocRatio = design$allocRatio,
    Stage2CumPValues = rep( NA_real_, dHypotheses ),
    AdaptObj = NULL,
    allGraphsPrev = design$all_graphs,
    allGraphs = design$all_graphs,
    HypoMap = design$hypothesis_map,
    SubText = design$plot_text
  ) )
}

# Reconstruct the MCP-shaped analysis object from `design` and persisted
# `state`, so look-2 helpers receive the same fields that were available at
# look 1. Return the reconstructed mutable analysis list.
.cer_state_mcp <- function( design, state )
{
  mcpObj <- .cer_plan_mcp( design )
  mcpObj$CurrentLook <- state$completed_looks
  mcpObj$IndexSet <- state$active_hypotheses
  mcpObj$ArmsPresent <- state$continuing_arms
  mcpObj$WH_Prev <- state$wh_previous
  mcpObj$WH <- state$intersection_weights
  mcpObj$rej_flag_Prev <- state$previous_rejection
  mcpObj$rej_flag_Curr <- state$current_rejection
  mcpObj$DroppedFlag <- state$dropped_hypotheses
  mcpObj$SelectedIndex <- state$selected_hypotheses
  mcpObj$SelectionLook <- state$selection_looks
  mcpObj$allGraphsPrev <- state$all_graphs_previous
  mcpObj$allGraphs <- state$all_graphs
  mcpObj$AllocSampleSize <- state$planned_sample_allocation
  mcpObj$Stage2AllocSampleSize <- state$adapted_sample_allocation
  mcpObj$Stage2allocRatio <- state$adapted_alloc_ratio
  mcpObj$Stage1Obj <- list(
    HypoMap = design$hypothesis_map,
    info_frac = design$info_frac,
    AllocSampleSize = design$allocation$cumulative,
    Sigma = design$Sigma,
    WH = design$intersection_weights,
    plan_Bdry = design$plan_boundary
  )
  mcpObj$p_raw_stage1 <- state$p_stage1
  mcpObj$AdaptObj <- state$adaptation
  return( mcpObj )
}

# Match current intersection weights `wh` to corresponding rows in planned
# weights `planned_wh` and `boundary`, preserving current intersection order.
# Return the selected boundary rows.
.cer_map_boundary_rows <- function( wh, planned_wh, boundary )
{
  if( nrow( wh ) == 0L )
  {
    return( boundary[ numeric( 0 ), , drop = FALSE ] )
  }
  # Encode each intersection by its hypothesis indicator pattern for matching.
  vCurrent <- apply( wh[ , grep( "^H", names( wh ) ), drop = FALSE ], 1L,
    paste0, collapse = "" )
  vPlanned <- apply( planned_wh[ , grep( "^H", names( planned_wh ) ), drop = FALSE ],
    1L, paste0, collapse = "" )
  vIndex <- match( vCurrent, vPlanned )
  if( anyNA( vIndex ) )
  {
    stop( "Current intersection hypotheses are incompatible with the planned design." )
  }
  return( boundary[ vIndex, , drop = FALSE ] )
}

# Calculate structured CER/PCER results for `design` using surviving
# intersections `wh`, stage-1 p-values `p_stage1`, and planned `boundary`.
# Return the structured CER/PCER result or an empty result when none remain.
.cer_structured_cer <- function( design, wh, p_stage1)
{
  if( nrow( wh ) == 0L )
  {
    return( list( table = NULL, data = data.frame() ) )
  }
  mBoundary <- .cer_map_boundary_rows(
    wh = wh,
    planned_wh = design$intersection_weights,
    boundary = design$plan_boundary$Stage2Bdry
  )
  return( getCER(
    b2 = mBoundary,
    WH = wh,
    p1 = p_stage1,
    test.type = design$test.type,
    HypoMap = design$hypothesis_map,
    CommonStdDev = design$CommonStdDev,
    allocRatio = design$allocRatio,
    sigma = design$sigma,
    Sigma = design$Sigma,
    AllocSampleSize = design$allocation$cumulative,
    EpType = design$EpType,
    prop.ctr = design$prop.ctr,
    t1 = design$info_frac[ 1L ],
    mvtnorm_algo = design$mvtnorm_algo,
    structured = TRUE
  ) )
}

# Validate and order look-2 incremental p-values `p_raw` for the
# `active_hypotheses`, using `initial_hypotheses` as the accepted full set.
# Return p-values ordered by the active hypotheses.
.cer_validate_stage2_pvalues <- function( p_raw, active_hypotheses, initial_hypotheses )
{
    p_non_missing <- p_raw[ !is.na( p_raw ) ]
    if( !is.numeric( p_raw ) || is.null( names( p_raw ) ) ||
      all( is.na( p_raw ) ) || !all( is.finite( p_non_missing ) ) ||
      !all( p_non_missing >= 0 & p_non_missing <= 1 ) )
  {
    stop( "p_raw must be a named numeric vector with values in [0, 1]." )
  }
  if( anyDuplicated( names( p_raw ) ) || anyNA( names( p_raw ) ) ||
      any( !nzchar( names( p_raw ) ) ) ||
      any( !names( p_raw ) %in% initial_hypotheses ) )
  {
    stop( "p_raw names must be unique and belong to the planned hypotheses." )
  }
  if( setequal( names( p_raw ), active_hypotheses ) )
  {
    return( p_raw[ active_hypotheses ] )
  }
  if( setequal( names( p_raw ), initial_hypotheses ) )
  {
    return( p_raw[ active_hypotheses ] )
  }
  stop(
    "p_raw names must match the current active hypotheses or the full planned ",
    "hypothesis set."
  )
}

# Confirm that persisted `state` is compatible with `design` before applying
# look-2 decisions. Return invisibly after all compatibility checks pass.
.cer_validate_state_compatibility <- function( design, state )
{
  if( !identical( names( state$current_rejection ), design$hypothesis_names ) ||
      length( state$p_stage1 ) != length( design$hypothesis_names ) ||
      !identical( names( state$p_stage1 ), design$hypothesis_names ) )
  {
    stop( "design and state have incompatible hypothesis names." )
  }
  if( !is.data.frame( state$WH ) ||
      ncol( state$WH ) != ncol( design$intersection_weights ) ||
      !identical(
        names( state$WH )[ grep( "^H", names( state$WH ) ) ],
        names( design$intersection_weights )[ grep( "^H", names( design$intersection_weights ) ) ]
      ) )
  {
    stop( "design and state have incompatible intersection weights." )
  }
  if( !is.matrix( state$current_G ) ||
      !all( dim( state$current_G ) == dim( design$G ) ) )
  {
    stop( "design and state have incompatible transition matrices." )
  }
  if( !is.data.frame( state$planned_sample_allocation ) ||
      !identical( colnames( state$planned_sample_allocation ),
                  colnames( design$allocation$cumulative ) ) )
  {
    stop( "design and state have incompatible sample allocations." )
  }
  return( invisible( TRUE ) )
}

# Convert `design`, the look-1 MCP object `mcpObj`, stage-1 result `stage1`,
# and structured CER result `cer_result` into the public state object used to
# continue the trial. Return a CERAnalysisState with its look-1 history.
.cer_new_state <- function( design, mcpObj, stage1, cer_result )
{
  lPrimary <- stage1$Stage1Obj$Stage1Analysis$PrimaryHypoTest
  vRejected <- as.logical( unlist( lPrimary[ 1L, ] ) )
  names( vRejected ) <- design$hypothesis_names
  vActive <- design$hypothesis_names[ !vRejected ]
  vDropped <- rep( FALSE, length( vRejected ) )
  names( vDropped ) <- names( vRejected )
  mcpObj$rej_flag_Prev <- vRejected
  mcpObj$rej_flag_Curr <- vRejected
  mcpObj$IndexSet <- vActive

  # Keep both the current analysis fields and user-facing results so later
  # calls can resume without recomputing look 1.
  state <- list(
    completed_looks = 1L,
    active_hypotheses = vActive,
    continuing_arms = sort( unique( c( 1L, design$hypothesis_map$Treatment[
      design$hypothesis_map$Hypothesis %in% vActive ] ) ) ),
    previous_rejection = vRejected,
    current_rejection = vRejected,
    dropped_hypotheses = vDropped,
    selected_hypotheses = character( 0 ),
    selection_looks = integer( 0 ),
    intersection_weights = mcpObj$WH,
    wh_previous = mcpObj$WH_Prev,
    WH = mcpObj$WH,
    all_graphs = design$all_graphs,
    all_graphs_previous = design$all_graphs,
    allGraphs = design$all_graphs,
    current_weights = design$WI,
    current_G = design$G,
    p_stage1 = mcpObj$p_raw_stage1,
    incremental_stage2_pvalues = NULL,
    cumulative_stage2_pvalues = NULL,
    planned_sample_allocation = design$allocation$cumulative,
    adapted_sample_allocation = design$allocation$cumulative,
    adapted_alloc_ratio = design$allocRatio,
    adapted_covariance = NULL,
    stage2_boundary = NULL,
    adaptation = NULL,
    results = list(
      stage1 = list(
        primary_rejection = lPrimary,
        intersection_rejection = stage1$Stage1Obj$Stage1Analysis$IntersectHypoTest,
        planned_sample_allocation = stage1$Stage1Obj$AllocSampleSize,
        covariance = stage1$Stage1Obj$Sigma,
        planned_boundary = stage1$Stage1Obj$plan_Bdry,
        cer_pcer = cer_result
      )
    ),
    trial_completed = length( vActive ) == 0L,
    completion_reason = if( length( vActive ) == 0L )
      "all_hypotheses_dropped" else NULL,
    look_history = list(
      list(
        look = 1L,
        inputs = list( p_raw = mcpObj$p_raw_stage1 ),
        decisions = list(
          rejected = vRejected,
          active_hypotheses = vActive
        ),
        results = list(
          primary_rejection = lPrimary,
          intersection_rejection = stage1$Stage1Obj$Stage1Analysis$IntersectHypoTest,
          cer_pcer = cer_result
        )
      )
    )
  )
  return( new_cer_analysis_state( state ) )
}

#' Create an immutable planned two-look CER design
#'
#' `SetupDesignCER()` normalizes all planning inputs and computes the planned
#' allocation, covariance, graph intersections, and CER boundaries. It never
#' reads from the console and does not store observed p-values or analysis state.
#'
#' @param nArms Number of arms including control.
#' @param nEps Number of endpoints.
#' @param nSampleSize Planned total sample size.
#' @param lEpTypes Endpoint-type list containing `"Continuous"` or `"Binary"`.
#'   Unnamed entries are assigned endpoint names automatically.
#' @param lSigma Endpoint-specific arm standard deviations. Values are ordered
#'   control, treatment 1, treatment 2, and so on.
#' @param bCommonStdDev Whether treatment standard deviations equal control.
#' @param lCtrlProps Endpoint-specific binary control arm proportions.
#' @param vAllocRatios Arm allocation ratios, control first, followed by
#'   treatment 1, treatment 2, and so on.
#' @param vWeights Initial graph weights.
#' @param mGMatrix Initial graph transition matrix.
#' @param strTestType CER test type: `"Parametric"`, `"Non-Parametric"`, or
#'   `"Partly-Parametric"`.
#' @param dAlpha One-sided type-I error rate.
#' @param vInfoFracts Exactly two increasing cumulative information fractions,
#'   ending at 1.
#' @param strDesignType Group-sequential design type.
#' @param dDeltaWT Wang--Tsiatis parameter.
#' @param dDeltaPT1 Pampallona--Tsiatis parameter.
#' @param dGammaA Alpha-spending parameter.
#' @param vUserAlphaSpending Cumulative spending for `strDesignType = "asUser"`.
#' @param bPlotGraphs If `TRUE`, plot the initial graph.
#' @return A `CERDesign` object.
#' @export
SetupDesignCER <- function(
    nArms = 3,
    nEps = 2,
    nSampleSize = 500,
    lEpTypes = list( EP1 = "Continuous", EP2 = "Continuous" ),
    lSigma = list( EP1 = c( 1, 1.1, 1.2 ), EP2 = c( 1, 1.1, 1.2 ) ),
    bCommonStdDev = FALSE,
    lCtrlProps = list( EP1 = NA, EP2 = NA ),
    vAllocRatios = rep( 1, nArms ),
    vWeights = rep( 1 / ( nEps * ( nArms - 1 ) ), nEps * ( nArms - 1 ) ),
    mGMatrix = matrix( 0, nrow = nEps * ( nArms - 1 ), ncol = nEps * ( nArms - 1 ) ),
    strTestType = "Partly-Parametric",
    dAlpha = 0.025,
    vInfoFracts = c( 0.5, 1 ),
    strDesignType = "asOF",
    dDeltaWT = 0,
    dDeltaPT1 = 0,
    dGammaA = 2,
    vUserAlphaSpending = NULL,
    bPlotGraphs = FALSE )
{
  .cer_validate_design_inputs(
    nArms = nArms, nEps = nEps, SampleSize = nSampleSize,
    EpType = lEpTypes, sigma = lSigma, CommonStdDev = bCommonStdDev,
    prop.ctr = lCtrlProps,
    allocRatio = vAllocRatios, WI = vWeights, G = mGMatrix, test.type = strTestType,
    alpha = dAlpha, info_frac = vInfoFracts, typeOfDesign = strDesignType,
    deltaWT = dDeltaWT, deltaPT1 = dDeltaPT1, gammaA = dGammaA,
    userAlphaSpending = vUserAlphaSpending
  )

  # Normalize names and derive the hypothesis map before calling the shared
  # planning routines.
  nArms <- as.integer( nArms )
  nEps <- as.integer( nEps )
  nHypothesis <- nEps * ( nArms - 1L )
  vHypotheses <- paste0( "H", seq_len( nHypothesis ) )
  names( lEpTypes ) <- if( is.null( names( lEpTypes ) ) ) paste0( "EP", seq_len( nEps ) ) else names( lEpTypes )
  names( lSigma ) <- names( lEpTypes )
  names( lCtrlProps ) <- names( lEpTypes )
  vArmNames <- c( "Control", paste0( "Treatment", seq_len( nArms - 1L ) ) )
  names( vAllocRatios ) <- vArmNames
  for( iEndpoint in seq_len( nEps ) )
  {
    names( lSigma[[ iEndpoint ]] ) <- vArmNames[ seq_along( lSigma[[ iEndpoint ]] ) ]
  }
  mHypoMap <- getHypoMap2( "MAMSMEP", nHypothesis, nEps, nArms, lEpTypes )
  lGraphs <- genWeights( w = vWeights, g = mGMatrix, HypothesisName = vHypotheses )
  nMvtDimension <- nHypothesis
  mvtnorm_algo <- chooseMVTAlgo( nMvtDimension )
  lPlanned <- ComputeCERPlannedDesign(
    nEps = nEps, nLooks = 2L, nHypothesis = nHypothesis,
    EpType = lEpTypes, sigma = lSigma, prop.ctr = lCtrlProps,
    allocRatio = vAllocRatios, SampleSize = nSampleSize, alpha = dAlpha,
    info_frac = vInfoFracts, typeOfDesign = strDesignType, deltaWT = dDeltaWT,
    deltaPT1 = dDeltaPT1, gammaA = dGammaA,
    userAlphaSpending = vUserAlphaSpending, test.type = strTestType,
    HypoMap = mHypoMap, CommonStdDev = bCommonStdDev,
    WH = lGraphs$IntersectionWeights, mvtnorm_algo = mvtnorm_algo
  )
  lAllocation <- lPlanned$allocation
  dSigma <- lPlanned$Sigma
  lPlanBoundary <- lPlanned$plan_boundary

  # Store both normalized inputs and derived planning objects in one immutable
  # container used by both analysis looks.
  design <- new_cer_design( list(
    nArms = nArms,
    nEps = nEps,
    SampleSize = nSampleSize,
    EpType = lEpTypes,
    sigma = lSigma,
    CommonStdDev = isTRUE( bCommonStdDev ),
    prop.ctr = lCtrlProps,
    allocRatio = vAllocRatios,
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
    hypothesis_names = vHypotheses,
    hypothesis_map = mHypoMap,
    all_graphs = lGraphs,
    intersection_weights = lGraphs$IntersectionWeights,
    HypoMap = mHypoMap,
    allGraphs = lGraphs,
    WH = lGraphs$IntersectionWeights,
    allocation = list(
      cumulative = lAllocation$CumulativeSamples,
      incremental = lAllocation$IncrementalSamples
    ),
    planned_sample_allocation = lAllocation$CumulativeSamples,
    planned_incremental_sample_allocation = lAllocation$IncrementalSamples,
    Sigma = dSigma,
    planned_covariance = dSigma,
    plan_boundary = lPlanBoundary,
    planned_boundary = lPlanBoundary,
    lPlanned = lPlanned,
    mvtnorm_algo = mvtnorm_algo,
    plot_text = getPlotText( mHypoMap )
  ) )

  if( isTRUE( bPlotGraphs ) )
  {
    # Plotting is opt-in so design construction remains non-interactive.
    plotGraph(
      HypothesisName = vHypotheses, w = vWeights, G = mGMatrix,
      activeStatus = rep( TRUE, nHypothesis ),
      Title = "Initial Graph", Text = design$plot_text
    )
  }
  return( design )
}

#' Analyze one formal look of a planned CER design
#'
#' Look 1 is called with `cState = NULL`; look 2 receives the state returned by
#' look 1. Stage-2 `vRawPVals` values are incremental p-values. Selection, sample
#' size, and strategy changes are applied in that order before CER boundary
#' recalibration. No console input or output is used.
#'
#' @param cDesign A `CERDesign` object.
#' @param cState `NULL` for look 1, otherwise a `CERAnalysisState`.
#' @param vRawPVals Named raw p-values for the active hypotheses.
#' @param nLook Optional look number, which must match the next look.
#' @param vSelectHypo Hypotheses retained for look 2.
#' @param vNewSampleSizes Cumulative stage-2 sample sizes in control followed
#'   by unique continuing-treatment arm order. Names are ignored. The vector
#'   must contain exactly one value for the control and each unique treatment
#'   arm represented by the continuing hypotheses.
#' @param vWeights New weights for the continuing hypotheses.
#' @param mGMatrix New transition matrix for the continuing hypotheses.
#' @param bPlotGraphs If `TRUE`, plot the graph after analysis.
#' @return A `CERAnalysisState` object.
#' @export
AnalyzeLookCER <- function(
    cDesign, cState = NULL, vRawPVals, nLook = NULL, vSelectHypo = NULL,
    vNewSampleSizes = NULL, vWeights = NULL, mGMatrix = NULL,
    bPlotGraphs = FALSE )
{
  if( !inherits( cDesign, "CERDesign" ) )
  {
    stop( "design must be a CERDesign object." )
  }
  if( is.null( cState ) )
  {
    # Look 1 computes the planned stage-1 test and initializes resumable state.
    if( !is.null( nLook ) && ( length( nLook ) != 1L || nLook != 1L ) )
    {
      stop( "look must be 1 when state is NULL." )
    }

    # Look-2 adaptations are not used in look 1.
    # Give warnings if these parameters are provided, as they will be ignored in look 1.
    if( !is.null( vSelectHypo ) )
    {
      warning( "Parameter 'vSelectHypo' is ignored in look 1." )
    }
    if( !is.null( vNewSampleSizes ) )
    {
      warning( "Parameter 'vNewSampleSizes' is ignored in look 1." )
    }
    if( !is.null( vWeights ) )
    {
      warning( "Parameter 'vWeights' is ignored in look 1." )
    }
    if( !is.null( mGMatrix ) )
    {
      warning( "Parameter 'mGMatrix' is ignored in look 1." )
    }

    mcpObj <- .cer_plan_mcp( cDesign )
    p_stage1 <- validate_p_raw( vRawPVals, cDesign$hypothesis_names )
    mcpObj$CurrentLook <- 1L
    mcpObj$p_raw <- addNAPvalue( p_stage1, cDesign$hypothesis_names )
    mcpObj$p_raw_stage1 <- mcpObj$p_raw
    lStage1 <- PerformStage1Test(
      nArms = cDesign$nArms, nEps = cDesign$nEps, EpType = cDesign$EpType,
      nLooks = 2L, nHypothesis = length( cDesign$hypothesis_names ),
      sigma = cDesign$sigma, prop.ctr = cDesign$prop.ctr,
      allocRatio = cDesign$allocRatio, SampleSize = cDesign$SampleSize,
      alpha = cDesign$alpha, info_frac = cDesign$info_frac,
      typeOfDesign = cDesign$typeOfDesign, deltaWT = cDesign$deltaWT,
      deltaPT1 = cDesign$deltaPT1, gammaA = cDesign$gammaA,
      des.type = "MAMSMEP", test.type = cDesign$test.type,
      Stage1Pvalues = mcpObj$p_raw, HypoMap = cDesign$hypothesis_map,
      CommonStdDev = cDesign$CommonStdDev, WH = cDesign$intersection_weights,
      mvtnorm_algo = cDesign$mvtnorm_algo,
      userAlphaSpending = cDesign$userAlphaSpending,
      lPlannedDesign = cDesign$lPlanned
    )
    mcpObj$Stage1Obj <- lStage1$Stage1Obj
    mcpObj$AllocSampleSize <- lStage1$Stage1Obj$AllocSampleSize
    mcpObj$Stage2AllocSampleSize <- lStage1$Stage1Obj$AllocSampleSize
    vRejected <- as.logical( unlist( lStage1$Stage1Obj$Stage1Analysis$PrimaryHypoTest[ 1L, ] ) )
    names( vRejected ) <- cDesign$hypothesis_names
    if( any( vRejected ) )
    {
      # Remove intersections that depend only on hypotheses already rejected
      # before calculating the structured CER summary.
      mcpObj$WH <- mcpObj$WH[
        rowSums( mcpObj$WH[ names( vRejected )[ vRejected ] ], na.rm = TRUE ) == 0,
        , drop = FALSE
      ]
      row.names( mcpObj$WH ) <- NULL
    }
    lCer <- .cer_structured_cer(
      design = cDesign, wh = mcpObj$WH, p_stage1 = mcpObj$p_raw
    )
    cState <- .cer_new_state( cDesign, mcpObj, lStage1, lCer )
    if( isTRUE( bPlotGraphs ) )
    {
      PlotGraphCER( cDesign, cState = cState )
    }
    return( cState )
  }

  # Look 2 resumes the saved state and applies optional adaptations in order.
  if( !inherits( cState, "CERAnalysisState" ) )
  {
    stop( "state must be NULL or a CERAnalysisState object." )
  }
  if( cState$completed_looks != 1L )
  {
    stop( "CER analysis requires a Look 1 state before Look 2." )
  }
  if( isTRUE( cState$trial_completed ) )
  {
    stop( "Trial already concluded - stopping criteria met." )
  }
  .cer_validate_state_compatibility( cDesign, cState )
  if( !is.null( nLook ) && ( length( nLook ) != 1L || nLook != 2L ) )
  {
    stop( "look must be 2 for the second CER look." )
  }

  mcpObj <- .cer_state_mcp( cDesign, cState )
  mcpObj$CurrentLook <- 2L
  bHasSelection <- !is.null( vSelectHypo )
  if( bHasSelection )
  {
    if( !is.character( vSelectHypo ) )
    {
      stop( "vSelectHypo must be a character vector." )
    }
    if( length( vSelectHypo ) == 0L )
    {
      cState$trial_completed <- TRUE
      cState$completion_reason <- "all_hypotheses_dropped"
      cState$selected_hypotheses <- character( 0 )
      cState$active_hypotheses <- character( 0 )
      cState$look_history[[ 2L ]] <- list(
        look = 2L, inputs = list( selection = vSelectHypo ),
        decisions = list( active_hypotheses = character( 0 ) ),
        results = NULL
      )
      return( cState )
    }

    # Selection changes the active hypothesis set before sample-size or graph
    # adaptation is evaluated.
    mcpObj <- applySelection( mcpObj, selected_hyps = vSelectHypo, look = 2L )
  }

  bHasSampleAdaptation <- !is.null( vNewSampleSizes )
  mStage2Allocation <- cDesign$allocation$cumulative
  vRequiredArms <- c(
    "Control",
    paste0(
      "Treatment",
      sort( unique(
        cDesign$hypothesis_map$Treatment[
          cDesign$hypothesis_map$Hypothesis %in% mcpObj$IndexSet
        ] - 1L
      ) )
    )
  )
  if( bHasSampleAdaptation )
  {
    if( !is.numeric( vNewSampleSizes ) || anyNA( vNewSampleSizes ) ||
        any( !is.finite( vNewSampleSizes ) ) || any( vNewSampleSizes <= 0 ) )
    {
      stop( "vNewSampleSizes must be a finite positive numeric vector." )
    }

    # Derive incremental stage-2 allocation ratios from cumulative sample sizes;
    # the CER adaptation routines require positive ratios for every continuing arm.
    vArmNames <- colnames( mStage2Allocation )
    if( length( vNewSampleSizes ) != length( vRequiredArms ) )
    {
      stop(
        "vNewSampleSizes must specify exactly one sample size for the control ",
        "and each unique treatment arm corresponding to the continuing hypotheses."
      )
    }
    vRequiredIndices <- match( vRequiredArms, vArmNames )
    mStage2Allocation[ 2L, vRequiredIndices ] <- as.numeric( vNewSampleSizes )
    if( any(
      as.numeric( unlist( mStage2Allocation[ 2L, vRequiredIndices ] ) ) <=
        as.numeric( unlist( mStage2Allocation[ 1L, vRequiredIndices ] ) )
    ) )
    {
      stop( "Stage-2 cumulative sample sizes must exceed Look 1 accrual for continuing arms." )
    }
  }
  if( length( mcpObj$IndexSet ) == 0L )
  {
    cState$trial_completed <- TRUE
    cState$completion_reason <- "all_hypotheses_dropped"
    return( cState )
  }
  if( bHasSelection || bHasSampleAdaptation )
  {
    mStage2Allocation[ 2L, setdiff( colnames( mStage2Allocation ), vRequiredArms ) ] <- NA_real_
  }
  mcpObj$Stage2AllocSampleSize <- mStage2Allocation
  mStage2Increment <- mStage2Allocation
  mStage2Increment[ 2L, ] <- mStage2Increment[ 2L, ] -
    mStage2Increment[ 1L, ]
  mcpObj$Stage2allocRatio <- as.numeric( mStage2Increment[ 2L, ] /
    mStage2Increment[ 2L, 1L ] )
  names( mcpObj$Stage2allocRatio ) <- names( cDesign$allocRatio )
  vRequiredRatio <- match( vRequiredArms, names( mcpObj$Stage2allocRatio ) )
  if( any( !is.finite( mcpObj$Stage2allocRatio[ vRequiredRatio ] ) |
           mcpObj$Stage2allocRatio[ vRequiredRatio ] <= 0 ) )
  {
    stop( "Stage-2 sample sizes must produce positive allocation ratios." )
  }

  bHasStrategyAdaptation <- !is.null( vWeights ) || !is.null( mGMatrix )
  if( bHasStrategyAdaptation )
  {
    mcpObj <- applyStrategyUpdate( mcpObj, new_weights = vWeights, new_G = mGMatrix )
  }

  p_stage2 <- .cer_validate_stage2_pvalues(
    p_raw = vRawPVals,
    active_hypotheses = mcpObj$IndexSet,
    initial_hypotheses = cDesign$hypothesis_names
  )
  mcpObj$p_raw <- cState$p_stage1
  bAdapted <- bHasSelection || bHasSampleAdaptation || bHasStrategyAdaptation
  if( bAdapted )
  {
    # Recalibrate boundaries and covariance only when a look-2 adaptation
    # changes the originally planned information structure.
    mcpObj$AdaptObj <- adaptBdryCER(
      mcpObj = mcpObj, mvtnorm_algo = cDesign$mvtnorm_algo
    )
    mStage2Boundary <- mcpObj$AdaptObj$Stage2AdjBdry
    dAdaptedCovariance <- mcpObj$AdaptObj$Stage2Sigma
  } else
  {
    mStage2Boundary <- .cer_map_boundary_rows(
      wh = mcpObj$WH, planned_wh = cDesign$intersection_weights,
      boundary = cDesign$plan_boundary$Stage2Bdry
    )
    dAdaptedCovariance <- NULL
  }
  mcpObj$p_raw <- addNAPvalue( p_stage2, cDesign$hypothesis_names )
  mIncrement <- mcpObj$Stage2AllocSampleSize
  mIncrement[ 2L, ] <- mIncrement[ 2L, ] - mIncrement[ 1L, ]
  dInfo <- rep( NA_real_, length( cDesign$hypothesis_names ) )
  names( dInfo ) <- cDesign$hypothesis_names
  dCumulative <- rep( NA_real_, length( cDesign$hypothesis_names ) )
  names( dCumulative ) <- cDesign$hypothesis_names
  for( strHypothesis in mcpObj$IndexSet )
  {
    iHypothesis <- match( strHypothesis, cDesign$hypothesis_names )
    iRow <- which( cDesign$hypothesis_map$Hypothesis == strHypothesis )
    iTreatment <- cDesign$hypothesis_map$Treatment[ iRow ]
    iArm <- c( 1L, iTreatment )
    dStage1Info <- ( 1 / mIncrement[ 1L, iArm[ 1L ] ] +
      1 / mIncrement[ 1L, iArm[ 2L ] ] )^(-1)
    dStage2Info <- ( 1 / mIncrement[ 2L, iArm[ 1L ] ] +
      1 / mIncrement[ 2L, iArm[ 2L ] ] )^(-1)
    dInfo[ iHypothesis ] <- dStage1Info / ( dStage1Info + dStage2Info )
    # Combine independent stage-wise z statistics into a cumulative p-value
    # using the information fraction implied by the two-stage allocation.
    dCumulative[ iHypothesis ] <- 1 - pnorm(
      sqrt( dInfo[ iHypothesis ] ) * qnorm( 1 - cState$p_stage1[ iHypothesis ] ) +
        sqrt( 1 - dInfo[ iHypothesis ] ) * qnorm(
          1 - mcpObj$p_raw[ iHypothesis ]
        )
    )
  }
  lStage2 <- closedTest(
    WH = mcpObj$WH, boundary = mStage2Boundary,
    pValues = dCumulative,
    Stage1RejStatus = cState$current_rejection
  )
  vRejected <- as.logical( unlist( lStage2$PrimaryHypoTest[ 1L, ] ) )
  names( vRejected ) <- cDesign$hypothesis_names
  vDropped <- mcpObj$DroppedFlag
  vActive <- cDesign$hypothesis_names[ !vRejected & !vDropped ]
  # Persist decisions, adaptations, and derived results so the returned state
  # remains inspectable even though look 2 is the final formal look.
  cState$completed_looks <- 2L
  cState$active_hypotheses <- vActive
  cState$continuing_arms <- sort( unique( c(
    1L, cDesign$hypothesis_map$Treatment[
      cDesign$hypothesis_map$Hypothesis %in% vActive
    ]
  ) ) )
  cState$previous_rejection <- cState$current_rejection
  cState$current_rejection <- vRejected
  cState$dropped_hypotheses <- vDropped
  cState$selected_hypotheses <- if( bHasSelection ) vSelectHypo else cState$selected_hypotheses
  cState$selection_looks <- if( bHasSelection ) c( cState$selection_looks, 2L ) else cState$selection_looks
  cState$intersection_weights <- mcpObj$WH
  cState$WH <- mcpObj$WH
  cState$all_graphs_previous <- mcpObj$allGraphsPrev
  cState$all_graphs <- mcpObj$allGraphs
  cState$allGraphs <- mcpObj$allGraphs
  if( bHasStrategyAdaptation )
  {
    vFullWeights <- rep( 0, length( cDesign$hypothesis_names ) )
    vFullWeights[ get_numeric_part( names( mcpObj$newWeights ) ) ] <-
      as.numeric( mcpObj$newWeights )
    cState$current_weights <- vFullWeights
    names( cState$current_weights ) <- cDesign$hypothesis_names
    mFullGraph <- matrix(
      0, nrow = length( cDesign$hypothesis_names ),
      ncol = length( cDesign$hypothesis_names )
    )
    vActiveIndex <- match( mcpObj$IndexSet, cDesign$hypothesis_names )
    mFullGraph[ vActiveIndex, vActiveIndex ] <- mcpObj$newG
    cState$current_G <- mFullGraph
  }
  cState$adapted_sample_allocation <- mcpObj$Stage2AllocSampleSize
  cState$adapted_alloc_ratio <- mcpObj$Stage2allocRatio
  cState$adapted_covariance <- dAdaptedCovariance
  cState$stage2_boundary <- mStage2Boundary
  cState$adaptation <- mcpObj$AdaptObj
  cState$incremental_stage2_pvalues <- mcpObj$p_raw
  cState$cumulative_stage2_pvalues <- dCumulative
  cState$results$stage2 <- list(
    primary_rejection = lStage2$PrimaryHypoTest,
    intersection_rejection = lStage2$IntersectHypoTest,
    incremental_pvalues = mcpObj$p_raw,
    cumulative_pvalues = dCumulative,
    boundary = mStage2Boundary,
    adapted_sample_allocation = mcpObj$Stage2AllocSampleSize,
    adapted_covariance = dAdaptedCovariance
  )
  cState$trial_completed <- TRUE
  cState$completion_reason <- if( length( vActive ) == 0L )
    "all_hypotheses_dropped" else "final_look"
  cState$look_history[[ 2L ]] <- list(
    look = 2L,
    inputs = list(
      p_raw = mcpObj$p_raw,
      selection = vSelectHypo,
      new_sample_size = vNewSampleSizes,
      new_weights = vWeights,
      new_G = mGMatrix
    ),
    decisions = list(
      rejected = vRejected,
      active_hypotheses = vActive
    ),
    results = cState$results$stage2
  )
  if( isTRUE( bPlotGraphs ) )
  {
    PlotGraphCER( cDesign, cState = cState )
  }
  return( cState )
}

#' Plot a planned or analyzed CER graph
#'
#' @param cDesign A `CERDesign` object.
#' @param cState Optional `CERAnalysisState`; `NULL` plots the planned graph.
#' @param strTitle Optional graph title.
#' @return A visNetwork widget.
#' @export
PlotGraphCER <- function( cDesign, cState = NULL, strTitle = NULL )
{
  if( !inherits( cDesign, "CERDesign" ) )
  {
    stop( "design must be a CERDesign object." )
  }
  if( is.null( cState ) )
  {
    vActive <- rep( TRUE, length( cDesign$hypothesis_names ) )
    vWeights <- cDesign$WI
    mGraph <- cDesign$G
    strTitle <- if( is.null( strTitle ) ) "Initial Graph" else strTitle
  } else
  {
    if( !inherits( cState, "CERAnalysisState" ) )
    {
      stop( "state must be NULL or a CERAnalysisState object." )
    }
    vActive <- !cState$current_rejection & !cState$dropped_hypotheses
    vWeights <- cState$current_weights
    mGraph <- cState$current_G
    strTitle <- if( is.null( strTitle ) ) paste(
      "Graph After Stage", cState$completed_looks, "analysis"
    ) else strTitle
  }
  return( plotGraph(
    HypothesisName = cDesign$hypothesis_names, w = vWeights, G = mGraph,
    activeStatus = vActive, Title = strTitle, Text = cDesign$plot_text
  ) )
}
