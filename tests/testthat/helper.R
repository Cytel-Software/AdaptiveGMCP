CompareImportantMcpMembers <- function( actMcp, expMcp, dTolerance = 1e-6 )
{
  bMatches <- TRUE

  bMatches <- bMatches && isTRUE( all.equal( actMcp$CurrentLook, expMcp$CurrentLook ) )
  bMatches <- bMatches && isTRUE( all.equal( actMcp$IndexSet, expMcp$IndexSet ) )
  bMatches <- bMatches && isTRUE( all.equal( actMcp$AdjPValues, expMcp$AdjPValues, tolerance = dTolerance ) )
  bMatches <- bMatches && isTRUE( all.equal( actMcp$WH, expMcp$WH, tolerance = dTolerance ) )

  if( isTRUE( identical( actMcp$CurrentLook, 1L ) ) )
  {
    bMatches <- bMatches && isTRUE( all.equal( actMcp$bdryTab, expMcp$bdryTab, tolerance = dTolerance ) )
    bMatches <- bMatches && isTRUE( all.equal( actMcp$InvNormWeights, expMcp$InvNormWeights, tolerance = dTolerance ) )
  }

  return( bMatches )
}
