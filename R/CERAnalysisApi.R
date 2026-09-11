# --------------------------------------------------------------------------------------------------
#
# Non-interactive Conditional Error Rate analysis API
#
# --------------------------------------------------------------------------------------------------

.cer_validate_scalar_integer <- function( x, arg_name, minimum )
{
  if( length( x ) != 1L || !is.numeric( x ) || is.na( x ) ||
      !is.finite( x ) || x != as.integer( x ) || x < minimum )
  {
    stop( arg_name, " must be a single integer >= ", minimum, "." )
  }
  return( as.integer( x ) )
}

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
      is.null( names( EpType ) ) ||
      any( !unlist( lapply( EpType, function( x ) length( x ) == 1L &&
        x %in% c( "Continuous", "Binary" ) ) ) ) )
  {
    stop( "EpType must be a named list of Continuous or Binary endpoints." )
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
    Stage2allocRatio = design$allocRatio,
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

.cer_map_boundary_rows <- function( wh, planned_wh, boundary )
{
  if( nrow( wh ) == 0L )
  {
    return( boundary[ numeric( 0 ), , drop = FALSE ] )
  }
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

.cer_structured_cer <- function( design, wh, p_stage1, boundary )
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

.cer_validate_stage2_pvalues <- function( p_raw, active_hypotheses, initial_hypotheses )
{
  if( !is.numeric( p_raw ) || is.null( names( p_raw ) ) ||
      anyNA( p_raw ) || any( !is.finite( p_raw ) ) ||
      any( p_raw < 0 | p_raw > 1 ) )
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
#' `SetupDesign_CER()` normalizes all planning inputs and computes the planned
#' allocation, covariance, graph intersections, and CER boundaries. It never
#' reads from the console and does not store observed p-values or analysis state.
#'
#' @param nArms Number of arms including control.
#' @param nEps Number of endpoints.
#' @param SampleSize Planned total sample size.
#' @param EpType Named endpoint-type list containing `"Continuous"` or `"Binary"`.
#' @param sigma Endpoint-specific arm standard deviations.
#' @param CommonStdDev Whether treatment standard deviations equal control.
#' @param prop.ctr Endpoint-specific binary control proportions.
#' @param allocRatio Arm allocation ratios, control first.
#' @param WI Initial graph weights.
#' @param G Initial graph transition matrix.
#' @param test.type CER test type: `"Parametric"`, `"Non-Parametric"`, or
#'   `"Partly-Parametric"`.
#' @param alpha One-sided type-I error rate.
#' @param info_frac Exactly two increasing cumulative information fractions,
#'   ending at 1.
#' @param typeOfDesign Group-sequential design type.
#' @param deltaWT Wang--Tsiatis parameter.
#' @param deltaPT1 Pampallona--Tsiatis parameter.
#' @param gammaA Alpha-spending parameter.
#' @param userAlphaSpending Cumulative spending for `typeOfDesign = "asUser"`.
#' @param plotGraphs If `TRUE`, plot the initial graph.
#' @return A `CERDesign` object.
#' @export
SetupDesign_CER <- function(
    nArms = 3,
    nEps = 2,
    SampleSize = 500,
    EpType = list( EP1 = "Continuous", EP2 = "Continuous" ),
    sigma = list( EP1 = c( 1, 1.1, 1.2 ), EP2 = c( 1, 1.1, 1.2 ) ),
    CommonStdDev = FALSE,
    prop.ctr = list( EP1 = NA, EP2 = NA ),
    allocRatio = rep( 1, nArms ),
    WI = rep( 1 / ( nEps * ( nArms - 1 ) ), nEps * ( nArms - 1 ) ),
    G = matrix( 0, nrow = nEps * ( nArms - 1 ), ncol = nEps * ( nArms - 1 ) ),
    test.type = "Partly-Parametric",
    alpha = 0.025,
    info_frac = c( 0.5, 1 ),
    typeOfDesign = "asOF",
    deltaWT = 0,
    deltaPT1 = 0,
    gammaA = 2,
    userAlphaSpending = NULL,
    plotGraphs = FALSE )
{
  .cer_validate_design_inputs(
    nArms = nArms, nEps = nEps, SampleSize = SampleSize,
    EpType = EpType, sigma = sigma, CommonStdDev = CommonStdDev,
    prop.ctr = prop.ctr,
    allocRatio = allocRatio, WI = WI, G = G, test.type = test.type,
    alpha = alpha, info_frac = info_frac, typeOfDesign = typeOfDesign,
    deltaWT = deltaWT, deltaPT1 = deltaPT1, gammaA = gammaA,
    userAlphaSpending = userAlphaSpending
  )

  nArms <- as.integer( nArms )
  nEps <- as.integer( nEps )
  nHypothesis <- nEps * ( nArms - 1L )
  vHypotheses <- paste0( "H", seq_len( nHypothesis ) )
  names( EpType ) <- if( is.null( names( EpType ) ) ) paste0( "EP", seq_len( nEps ) ) else names( EpType )
  names( sigma ) <- names( EpType )
  names( prop.ctr ) <- names( EpType )
  names( allocRatio ) <- c( "Control", paste0( "Treatment", seq_len( nArms - 1L ) ) )
  mHypoMap <- getHypoMap2( "MAMSMEP", nHypothesis, nEps, nArms, EpType )
  lGraphs <- genWeights( w = WI, g = G, HypothesisName = vHypotheses )
  nMvtDimension <- nHypothesis
  mvtnorm_algo <- chooseMVTAlgo( nMvtDimension )
  lPlanned <- ComputeCERPlannedDesign(
    nEps = nEps, nLooks = 2L, nHypothesis = nHypothesis,
    EpType = EpType, sigma = sigma, prop.ctr = prop.ctr,
    allocRatio = allocRatio, SampleSize = SampleSize, alpha = alpha,
    info_frac = info_frac, typeOfDesign = typeOfDesign, deltaWT = deltaWT,
    deltaPT1 = deltaPT1, gammaA = gammaA,
    userAlphaSpending = userAlphaSpending, test.type = test.type,
    HypoMap = mHypoMap, CommonStdDev = CommonStdDev,
    WH = lGraphs$IntersectionWeights, mvtnorm_algo = mvtnorm_algo
  )
  lAllocation <- lPlanned$allocation
  dSigma <- lPlanned$Sigma
  lPlanBoundary <- lPlanned$plan_boundary

  design <- new_cer_design( list(
    nArms = nArms,
    nEps = nEps,
    SampleSize = SampleSize,
    EpType = EpType,
    sigma = sigma,
    CommonStdDev = isTRUE( CommonStdDev ),
    prop.ctr = prop.ctr,
    allocRatio = allocRatio,
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
    mvtnorm_algo = mvtnorm_algo,
    plot_text = getPlotText( mHypoMap )
  ) )

  if( isTRUE( plotGraphs ) )
  {
    plotGraph(
      HypothesisName = vHypotheses, w = WI, G = G,
      activeStatus = rep( TRUE, nHypothesis ),
      Title = "Initial Graph", Text = design$plot_text
    )
  }
  return( design )
}

#' Analyze one formal look of a planned CER design
#'
#' Look 1 is called with `state = NULL`; look 2 receives the state returned by
#' look 1. Stage-2 `p_raw` values are incremental p-values. Selection, sample
#' size, and strategy changes are applied in that order before CER boundary
#' recalibration. No console input or output is used.
#'
#' @param design A `CERDesign` object.
#' @param state `NULL` for look 1, otherwise a `CERAnalysisState`.
#' @param p_raw Named raw p-values for the active hypotheses.
#' @param look Optional look number, which must match the next look.
#' @param selection Hypotheses retained for look 2.
#' @param new_sample_size Cumulative stage-2 sample sizes, named by arm.
#' @param stage2_cumulative_sample_size Alias for `new_sample_size`.
#' @param new_weights New weights for the continuing hypotheses.
#' @param new_G New transition matrix for the continuing hypotheses.
#' @param strategy_update Optional list containing `new_weights` and `new_G`.
#' @param plotGraphs If `TRUE`, plot the graph after analysis.
#' @return A `CERAnalysisState` object.
#' @export
AnalyzeLook_CER <- function(
    design, state = NULL, p_raw, look = NULL, selection = NULL,
    new_sample_size = NULL, stage2_cumulative_sample_size = NULL,
    new_weights = NULL, new_G = NULL, strategy_update = NULL,
    plotGraphs = FALSE )
{
  if( !inherits( design, "CERDesign" ) )
  {
    stop( "design must be a CERDesign object." )
  }
  if( is.null( state ) )
  {
    if( !is.null( look ) && ( length( look ) != 1L || look != 1L ) )
    {
      stop( "look must be 1 when state is NULL." )
    }
    mcpObj <- .cer_plan_mcp( design )
    p_stage1 <- validate_p_raw( p_raw, design$hypothesis_names )
    mcpObj$CurrentLook <- 1L
    mcpObj$p_raw <- addNAPvalue( p_stage1, design$hypothesis_names )
    mcpObj$p_raw_stage1 <- mcpObj$p_raw
    lStage1 <- PerformStage1Test(
      nArms = design$nArms, nEps = design$nEps, EpType = design$EpType,
      nLooks = 2L, nHypothesis = length( design$hypothesis_names ),
      sigma = design$sigma, prop.ctr = design$prop.ctr,
      allocRatio = design$allocRatio, SampleSize = design$SampleSize,
      alpha = design$alpha, info_frac = design$info_frac,
      typeOfDesign = design$typeOfDesign, deltaWT = design$deltaWT,
      deltaPT1 = design$deltaPT1, gammaA = design$gammaA,
      des.type = "MAMSMEP", test.type = design$test.type,
      Stage1Pvalues = mcpObj$p_raw, HypoMap = design$hypothesis_map,
      CommonStdDev = design$CommonStdDev, WH = design$intersection_weights,
      mvtnorm_algo = design$mvtnorm_algo,
      userAlphaSpending = design$userAlphaSpending
    )
    mcpObj$Stage1Obj <- lStage1$Stage1Obj
    mcpObj$AllocSampleSize <- lStage1$Stage1Obj$AllocSampleSize
    mcpObj$Stage2AllocSampleSize <- lStage1$Stage1Obj$AllocSampleSize
    vRejected <- as.logical( unlist( lStage1$Stage1Obj$Stage1Analysis$PrimaryHypoTest[ 1L, ] ) )
    names( vRejected ) <- design$hypothesis_names
    if( any( vRejected ) )
    {
      mcpObj$WH <- mcpObj$WH[
        rowSums( mcpObj$WH[ names( vRejected )[ vRejected ] ], na.rm = TRUE ) == 0,
        , drop = FALSE
      ]
      row.names( mcpObj$WH ) <- NULL
    }
    lCer <- .cer_structured_cer(
      design = design, wh = mcpObj$WH, p_stage1 = mcpObj$p_raw,
      boundary = design$plan_boundary$Stage2Bdry
    )
    state <- .cer_new_state( design, mcpObj, lStage1, lCer )
    if( isTRUE( plotGraphs ) )
    {
      PlotGraph_CER( design, state = state )
    }
    return( state )
  }

  if( !inherits( state, "CERAnalysisState" ) )
  {
    stop( "state must be NULL or a CERAnalysisState object." )
  }
  if( state$completed_looks != 1L )
  {
    stop( "CER analysis requires a Look 1 state before Look 2." )
  }
  if( isTRUE( state$trial_completed ) )
  {
    stop( "Trial already concluded - stopping criteria met." )
  }
  .cer_validate_state_compatibility( design, state )
  if( !is.null( look ) && ( length( look ) != 1L || look != 2L ) )
  {
    stop( "look must be 2 for the second CER look." )
  }

  mcpObj <- .cer_state_mcp( design, state )
  mcpObj$CurrentLook <- 2L
  bHasSelection <- !is.null( selection )
  if( bHasSelection )
  {
    if( !is.character( selection ) )
    {
      stop( "selection must be a character vector." )
    }
    if( length( selection ) == 0L )
    {
      state$trial_completed <- TRUE
      state$completion_reason <- "all_hypotheses_dropped"
      state$selected_hypotheses <- character( 0 )
      state$active_hypotheses <- character( 0 )
      state$look_history[[ 2L ]] <- list(
        look = 2L, inputs = list( selection = selection ),
        decisions = list( active_hypotheses = character( 0 ) ),
        results = NULL
      )
      return( state )
    }
    mcpObj <- applySelection( mcpObj, selected_hyps = selection, look = 2L )
  }

  bHasSampleAdaptation <- !is.null( new_sample_size ) ||
    !is.null( stage2_cumulative_sample_size )
  if( bHasSampleAdaptation &&
      !is.null( new_sample_size ) && !is.null( stage2_cumulative_sample_size ) )
  {
    stop( "Specify only one of new_sample_size and stage2_cumulative_sample_size." )
  }
  vNewSample <- new_sample_size
  if( is.null( vNewSample ) ) vNewSample <- stage2_cumulative_sample_size
  mStage2Allocation <- design$allocation$cumulative
  if( bHasSampleAdaptation )
  {
    if( !is.numeric( vNewSample ) || is.null( names( vNewSample ) ) ||
        anyNA( vNewSample ) || any( !is.finite( vNewSample ) ) ||
        any( vNewSample <= 0 ) )
    {
      stop( "new_sample_size must be a named finite positive numeric vector." )
    }
    vArmNames <- colnames( mStage2Allocation )
    if( any( !names( vNewSample ) %in% vArmNames ) ||
        anyDuplicated( names( vNewSample ) ) )
    {
      stop( "new_sample_size names must be unique planned arm names." )
    }
    vRequiredArms <- c(
      "Control",
      paste0(
        "Treatment",
        design$hypothesis_map$Treatment[
          design$hypothesis_map$Hypothesis %in% mcpObj$IndexSet
        ] - 1L
      )
    )
    if( any( !vRequiredArms %in% names( vNewSample ) ) )
    {
      stop( "new_sample_size must include the control and every continuing arm." )
    }
    mStage2Allocation[ 2L, names( vNewSample ) ] <- as.numeric( vNewSample )
    vRequiredIndices <- match( vRequiredArms, vArmNames )
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
    state$trial_completed <- TRUE
    state$completion_reason <- "all_hypotheses_dropped"
    return( state )
  }
  mcpObj$Stage2AllocSampleSize <- mStage2Allocation
  mcpObj$Stage2allocRatio <- as.numeric( mStage2Allocation[ 2L, ] /
    mStage2Allocation[ 2L, 1L ] )
  names( mcpObj$Stage2allocRatio ) <- names( design$allocRatio )
  if( any( !is.finite( mcpObj$Stage2allocRatio ) | mcpObj$Stage2allocRatio <= 0 ) )
  {
    stop( "Stage-2 sample sizes must produce positive allocation ratios." )
  }

  if( !is.null( strategy_update ) )
  {
    if( !is.list( strategy_update ) ) stop( "strategy_update must be a list." )
    if( !is.null( strategy_update$new_weights ) ) new_weights <- strategy_update$new_weights
    if( !is.null( strategy_update$new_G ) ) new_G <- strategy_update$new_G
  }
  bHasStrategyAdaptation <- !is.null( new_weights ) || !is.null( new_G )
  if( bHasStrategyAdaptation )
  {
    mcpObj <- applyStrategyUpdate( mcpObj, new_weights = new_weights, new_G = new_G )
  }

  p_stage2 <- .cer_validate_stage2_pvalues(
    p_raw = p_raw,
    active_hypotheses = mcpObj$IndexSet,
    initial_hypotheses = design$hypothesis_names
  )
  mcpObj$p_raw <- state$p_stage1
  bAdapted <- bHasSelection || bHasSampleAdaptation || bHasStrategyAdaptation
  if( bAdapted )
  {
    mcpObj$AdaptObj <- adaptBdryCER(
      mcpObj = mcpObj, mvtnorm_algo = design$mvtnorm_algo
    )
    mStage2Boundary <- mcpObj$AdaptObj$Stage2AdjBdry
    dAdaptedCovariance <- mcpObj$AdaptObj$Stage2Sigma
  } else
  {
    mStage2Boundary <- .cer_map_boundary_rows(
      wh = mcpObj$WH, planned_wh = design$intersection_weights,
      boundary = design$plan_boundary$Stage2Bdry
    )
    dAdaptedCovariance <- NULL
  }
  mcpObj$p_raw <- addNAPvalue( p_stage2, design$hypothesis_names )
  mIncrement <- mcpObj$Stage2AllocSampleSize
  mIncrement[ 2L, ] <- mIncrement[ 2L, ] - mIncrement[ 1L, ]
  dInfo <- rep( NA_real_, length( design$hypothesis_names ) )
  names( dInfo ) <- design$hypothesis_names
  dCumulative <- rep( NA_real_, length( design$hypothesis_names ) )
  names( dCumulative ) <- design$hypothesis_names
  for( strHypothesis in mcpObj$IndexSet )
  {
    iHypothesis <- match( strHypothesis, design$hypothesis_names )
    iRow <- which( design$hypothesis_map$Hypothesis == strHypothesis )
    iTreatment <- design$hypothesis_map$Treatment[ iRow ]
    iArm <- c( 1L, iTreatment )
    dStage1Info <- ( 1 / mIncrement[ 1L, iArm[ 1L ] ] +
      1 / mIncrement[ 1L, iArm[ 2L ] ] )^(-1)
    dStage2Info <- ( 1 / mIncrement[ 2L, iArm[ 1L ] ] +
      1 / mIncrement[ 2L, iArm[ 2L ] ] )^(-1)
    dInfo[ iHypothesis ] <- dStage1Info / ( dStage1Info + dStage2Info )
    dCumulative[ iHypothesis ] <- 1 - pnorm(
      sqrt( dInfo[ iHypothesis ] ) * qnorm( 1 - state$p_stage1[ iHypothesis ] ) +
        sqrt( 1 - dInfo[ iHypothesis ] ) * qnorm(
          1 - mcpObj$p_raw[ iHypothesis ]
        )
    )
  }
  lStage2 <- closedTest(
    WH = mcpObj$WH, boundary = mStage2Boundary,
    pValues = dCumulative,
    Stage1RejStatus = state$current_rejection
  )
  vRejected <- as.logical( unlist( lStage2$PrimaryHypoTest[ 1L, ] ) )
  names( vRejected ) <- design$hypothesis_names
  vDropped <- mcpObj$DroppedFlag
  vActive <- design$hypothesis_names[ !vRejected & !vDropped ]
  state$completed_looks <- 2L
  state$active_hypotheses <- vActive
  state$continuing_arms <- sort( unique( c(
    1L, design$hypothesis_map$Treatment[
      design$hypothesis_map$Hypothesis %in% vActive
    ]
  ) ) )
  state$previous_rejection <- state$current_rejection
  state$current_rejection <- vRejected
  state$dropped_hypotheses <- vDropped
  state$selected_hypotheses <- if( bHasSelection ) selection else state$selected_hypotheses
  state$selection_looks <- if( bHasSelection ) c( state$selection_looks, 2L ) else state$selection_looks
  state$intersection_weights <- mcpObj$WH
  state$WH <- mcpObj$WH
  state$all_graphs_previous <- mcpObj$allGraphsPrev
  state$all_graphs <- mcpObj$allGraphs
  state$allGraphs <- mcpObj$allGraphs
  if( bHasStrategyAdaptation )
  {
    vFullWeights <- rep( 0, length( design$hypothesis_names ) )
    vFullWeights[ get_numeric_part( names( mcpObj$newWeights ) ) ] <-
      as.numeric( mcpObj$newWeights )
    state$current_weights <- vFullWeights
    names( state$current_weights ) <- design$hypothesis_names
    mFullGraph <- matrix(
      0, nrow = length( design$hypothesis_names ),
      ncol = length( design$hypothesis_names )
    )
    vActiveIndex <- match( mcpObj$IndexSet, design$hypothesis_names )
    mFullGraph[ vActiveIndex, vActiveIndex ] <- mcpObj$newG
    state$current_G <- mFullGraph
  }
  state$adapted_sample_allocation <- mcpObj$Stage2AllocSampleSize
  state$adapted_alloc_ratio <- mcpObj$Stage2allocRatio
  state$adapted_covariance <- dAdaptedCovariance
  state$stage2_boundary <- mStage2Boundary
  state$adaptation <- mcpObj$AdaptObj
  state$incremental_stage2_pvalues <- mcpObj$p_raw
  state$cumulative_stage2_pvalues <- dCumulative
  state$results$stage2 <- list(
    primary_rejection = lStage2$PrimaryHypoTest,
    intersection_rejection = lStage2$IntersectHypoTest,
    incremental_pvalues = mcpObj$p_raw,
    cumulative_pvalues = dCumulative,
    boundary = mStage2Boundary,
    adapted_sample_allocation = mcpObj$Stage2AllocSampleSize,
    adapted_covariance = dAdaptedCovariance
  )
  state$trial_completed <- TRUE
  state$completion_reason <- if( length( vActive ) == 0L )
    "all_hypotheses_dropped" else "final_look"
  state$look_history[[ 2L ]] <- list(
    look = 2L,
    inputs = list(
      p_raw = mcpObj$p_raw,
      selection = selection,
      new_sample_size = vNewSample,
      new_weights = new_weights,
      new_G = new_G
    ),
    decisions = list(
      rejected = vRejected,
      active_hypotheses = vActive
    ),
    results = state$results$stage2
  )
  if( isTRUE( plotGraphs ) )
  {
    PlotGraph_CER( design, state = state )
  }
  return( state )
}

#' Plot a planned or analyzed CER graph
#'
#' @param design A `CERDesign` object.
#' @param state Optional `CERAnalysisState`; `NULL` plots the planned graph.
#' @param title Optional graph title.
#' @return A visNetwork widget.
#' @export
PlotGraph_CER <- function( design, state = NULL, title = NULL )
{
  if( !inherits( design, "CERDesign" ) )
  {
    stop( "design must be a CERDesign object." )
  }
  if( is.null( state ) )
  {
    vActive <- rep( TRUE, length( design$hypothesis_names ) )
    vWeights <- design$WI
    mGraph <- design$G
    strTitle <- if( is.null( title ) ) "Initial Graph" else title
  } else
  {
    if( !inherits( state, "CERAnalysisState" ) )
    {
      stop( "state must be NULL or a CERAnalysisState object." )
    }
    vActive <- !state$current_rejection & !state$dropped_hypotheses
    vWeights <- state$current_weights
    mGraph <- state$current_G
    strTitle <- if( is.null( title ) ) paste(
      "Graph After Stage", state$completed_looks, "analysis"
    ) else title
  }
  return( plotGraph(
    HypothesisName = design$hypothesis_names, w = vWeights, G = mGraph,
    activeStatus = vActive, Title = strTitle, Text = design$plot_text
  ) )
}
