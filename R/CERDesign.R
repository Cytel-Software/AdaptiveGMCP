# --------------------------------------------------------------------------------------------------
#
# Non-interactive CER design object and S3 methods
#
# --------------------------------------------------------------------------------------------------

# Construct a CERDesign object from normalized planning `fields`.
# Return the same list with the class attribute used by the S3 print method.
new_cer_design <- function(fields)
{
  if( !is.list( fields ) )
  {
    stop( "fields must be a list." )
  }

  class( fields ) <- "CERDesign"
  return( fields )
}

#' Print a planned CER design
#'
#' @param x A `CERDesign` object.
#' @param ... Additional arguments (ignored).
#' @return `x`, invisibly.
#' @export
print.CERDesign <- function( x, ... )
{
  if( !inherits( x, "CERDesign" ) )
  {
    stop( "x must be a CERDesign object." )
  }

  cat( "\nCERDesign\n" )
  cat( "- Arms:", x$nArms, "\n" )
  cat( "- Endpoints:", x$nEps, "\n" )
  cat( "- Hypotheses:", length( x$hypothesis_names ), "\n" )
  cat( "- Looks:", length( x$info_frac ), "\n" )
  cat( "- Test:", x$test.type, "\n" )
  cat( "- Alpha:", x$alpha, "\n" )
  # The allocation and boundary tables summarize the quantities that were
  # fixed during planning and are not changed by later analysis state.
  cat( "\nPlanned cumulative sample allocation:\n" )
  print( x$allocation$cumulative )
  cat( "\n" )
  cat("Planned stopping boundary:")
  print(x$planned_boundary$PlanBdryTable$Stage1_Boundary)
  print(x$planned_boundary$PlanBdryTable$Stage2_Boundary)
  return( invisible( x ) )
}
