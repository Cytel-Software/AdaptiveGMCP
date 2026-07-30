# --------------------------------------------------------------------------------------------------
#
# Stateless analysis interface (population enrichment, p-value combination method)
#
# --------------------------------------------------------------------------------------------------

ValidatePeSampleSizes <- function(full_d, full_0, sub_d, sub_0)
{
  vArgs <- c(full_d = full_d, full_0 = full_0, sub_d = sub_d, sub_0 = sub_0)

  if( anyNA( vArgs ) || !all( is.finite( vArgs ) ) )
  {
    stop( "Sample sizes must be finite, non-missing numeric values." )
  }

  if( any( vArgs <= 0 ) )
  {
    stop( "Sample sizes must be strictly positive." )
  }

  if( sub_d > full_d || sub_0 > full_0 )
  {
    stop( "Subgroup sample sizes must not exceed full-population sample sizes." )
  }

  return( invisible( TRUE ) )
}

ComputeMartinCorrelation <- function(full_d, full_0, sub_d, sub_0)
{
  ValidatePeSampleSizes(
    full_d = full_d,
    full_0 = full_0,
    sub_d = sub_d,
    sub_0 = sub_0
  )

  dRho <- sqrt( ( 1 / full_d + 1 / full_0 ) / ( 1 / sub_d + 1 / sub_0 ) )

  if( !is.finite( dRho ) || dRho <= 0 || dRho > 1 )
  {
    stop( "Derived Martin correlation is outside the valid range (0, 1]." )
  }

  return( dRho )
}

#' Setup analysis object for population enrichment (P-value combination method)
#'
#' Creates a non-interactive population-enrichment analysis state object. This
#' interface computes the full/subgroup correlation internally using Martin's
#' nested-population formula and delegates the core analysis setup to
#' [SetupAnalysis_PC()].
#'
#' This first implementation slice supports only the simple case:
#'
#' - one treatment arm
#' - one endpoint
#' - two populations (full and subgroup)
#'
#' @param WI Vector of node weights for the initial graph. Must have length 2.
#' @param G Transition matrix for the graph. Must be a 2 x 2 matrix.
#' @param test.type Character specifying test type.
#' Supported values: "Bonf", "Sidak", "Simes", "Dunnett", "Partly-Parametric".
#' @param alpha One-sided type-1 error.
#' @param info_frac Vector of cumulative information fractions.
#' @param typeOfDesign Group sequential design type (rpact).
#' @param deltaWT Parameter for typeOfDesign = "WT".
#' @param deltaPT1 Parameter for typeOfDesign = "PT".
#' @param gammaA Parameter for typeOfDesign = "asHSD" or "asKD".
#' @param userAlphaSpending Alpha spending values for typeOfDesign = "asUser".
#' @param full_d Full-population treatment-arm sample size.
#' @param full_0 Full-population control-arm sample size.
#' @param sub_d Subgroup treatment-arm sample size.
#' @param sub_0 Subgroup control-arm sample size.
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
    full_d,
    full_0,
    sub_d,
    sub_0,
    MultipleWinners = TRUE,
    Selection = TRUE,
    UpdateStrategy = TRUE,
    plotGraphs = TRUE )
{
  if( length( WI ) != 2 )
  {
    stop( "SetupAnalysis_PE_PC currently supports exactly 2 hypotheses (H1 full, H2 subgroup)." )
  }

  if( !is.matrix( G ) || !all( dim( G ) == c( 2, 2 ) ) )
  {
    stop( "SetupAnalysis_PE_PC currently requires a 2 x 2 transition matrix." )
  }

  dRho <- ComputeMartinCorrelation(
    full_d = full_d,
    full_0 = full_0,
    sub_d = sub_d,
    sub_0 = sub_0
  )

  mCorrelation <- matrix(
    c( 1, dRho,
       dRho, 1 ),
    byrow = TRUE,
    nrow = 2
  )

  rownames( mCorrelation ) <- colnames( mCorrelation ) <- c( "H1", "H2" )

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
    Correlation = mCorrelation,
    MultipleWinners = MultipleWinners,
    Selection = Selection,
    UpdateStrategy = UpdateStrategy,
    plotGraphs = plotGraphs
  )

  state$pe_metadata <- list(
    sample_sizes = c(
      full_d = full_d,
      full_0 = full_0,
      sub_d = sub_d,
      sub_0 = sub_0
    ),
    martin_rho = dRho,
    endpoint_correlation = NA_real_,
    endpoint_correlation_policy = "unknown_as_na"
  )

  return( state )
}

#' Analyze one look for population enrichment (P-value combination method)
#'
#' Advances an existing [SetupAnalysis_PE_PC()] state object by exactly one
#' look. This wrapper delegates the per-look analysis to [AnalyzeLook_PC()].
#'
#' @param state A "PCAnalysisState" object created by [SetupAnalysis_PE_PC()].
#' @param p_raw Named numeric vector of raw p-values for active hypotheses.
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
    look = NULL,
    selection = NULL,
    new_weights = NULL,
    new_G = NULL,
    plotGraphs = TRUE )
{
  return(
    AnalyzeLook_PC(
      state = state,
      p_raw = p_raw,
      look = look,
      selection = selection,
      new_weights = new_weights,
      new_G = new_G,
      new_correlation = NULL,
      plotGraphs = plotGraphs
    )
  )
}