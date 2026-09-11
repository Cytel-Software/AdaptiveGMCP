# --------------------------------------------------------------------------------------------------
#
# Non-interactive CER analysis state object and S3 methods
#
# --------------------------------------------------------------------------------------------------

new_cer_analysis_state <- function( fields )
{
  if( !is.list( fields ) )
  {
    stop( "fields must be a list." )
  }

  class( fields ) <- "CERAnalysisState"
  return( fields )
}

#' Print the evolving CER analysis state
#'
#' @param x A `CERAnalysisState` object.
#' @param ... Additional arguments (ignored).
#' @return `x`, invisibly.
#' @export
print.CERAnalysisState <- function( x, ... )
{
  if( !inherits( x, "CERAnalysisState" ) )
  {
    stop( "x must be a CERAnalysisState object." )
  }

  cat( "\nCERAnalysisState\n" )
  cat( "- Looks completed:", x$completed_looks, "of 2\n" )
  cat( "- Active hypotheses:",
       if( length( x$active_hypotheses ) == 0 ) "none" else
         paste( x$active_hypotheses, collapse = ", " ), "\n" )
  if( isTRUE( x$trial_completed ) )
  {
    cat( "- Status:", x$completion_reason, "\n" )
  } else
  {
    cat( "- Status: continuing\n" )
  }

  if( x$completed_looks == 1L && !is.null( x$results$stage1 ) )
  {
    cat("\n Stage 1 raw p-values:\n")
    print(x$p_stage1)
    cat("\nStage 1 primary decisions:\n")
    print(x$results$stage1$primary_rejection)

    cat("\nCER/PCER table:")
    print(x$results$stage1$cer_pcer$table)
  }
  if( x$completed_looks > 1L && !is.null( x$results$stage2 ) )
  {
    cat("\n Stage 2 raw p-values:\n")
    print(x$incremental_stage2_pvalues)
    cat("\nStage 2 primary decisions:\n")
    print(x$results$stage2$primary_rejection)
  }

  return( invisible( x ) )
}
