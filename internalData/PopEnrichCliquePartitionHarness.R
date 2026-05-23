# --------------------------------------------------------------------------------------------------
#
# ©2026 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

#######################################################################
#####   Harness to Assess Clique-Partition Sensitivity in comb.test ####
#######################################################################
#
# This script enumerates all valid clique partitions of a known-correlation
# graph and evaluates the exact comb.test calculation under each partition,
# without modifying the package source.

requireNamespace( "AdaptGMCP" )

#---------------------- -
# Format a clique partition for compact printing
#---------------------- -
FormatCliquePartition <- function( lCliques )
{
    strPartition <- paste(
        vapply(
            lCliques,
            function( vClique )
            {
                return( paste0( "{", paste( paste0( "H", vClique ), collapse = "," ), "}" ) )
            },
            character( 1 )
        ),
        collapse = " | "
    )

    return( strPartition )
}

#---------------------- -
# Reproduce the package's greedy clique.partition logic locally
#---------------------- -
GreedyCliquePartition <- function( mCorr )
{
    nQtyOfNodes <- ncol( mCorr )
    if( nQtyOfNodes == 1 )
    {
        return( list( 1 ) )
    }

    mKnown <- !is.na( mCorr ) & ( row( mCorr ) != col( mCorr ) )
    vNaCount <- vapply( seq_len( nQtyOfNodes ), function( iNode ) sum( is.na( mCorr[ iNode, -iNode ] ) ), integer( 1 ) )
    vNodeOrder <- order( vNaCount, decreasing = TRUE )

    lCliques <- list()
    for( nNode in vNodeOrder )
    {
        bPlaced <- FALSE
        for( iClique in seq_along( lCliques ) )
        {
            if( all( mKnown[ nNode, lCliques[[ iClique ]] ] ) )
            {
                lCliques[[ iClique ]] <- c( lCliques[[ iClique ]], nNode )
                bPlaced <- TRUE
                break
            }
        }

        if( !bPlaced )
        {
            lCliques <- c( lCliques, list( nNode ) )
        }
    }

    return( lCliques )
}

#---------------------- -
# Reproduce the package's dimension-based mvtnorm algorithm selection locally
#---------------------- -
ChooseMVTAlgo <- function( nQtyOfHypotheses )
{
    if( nQtyOfHypotheses <= 20 )
    {
        return( mvtnorm::Miwa( steps = 128, checkCorr = FALSE, maxval = 1e3 ) )
    }

    return( mvtnorm::GenzBretz( maxpts = 25000, abseps = 0.001, releps = 0 ) )
}

#---------------------- -
# Check whether a node set is a clique in the known-correlation graph
#---------------------- -
IsValidClique <- function( vNodes, mKnown )
{
    if( length( vNodes ) <= 1 )
    {
        return( TRUE )
    }

    for( iNode in seq_len( length( vNodes ) - 1 ) )
    {
        for( jNode in ( iNode + 1 ) : length( vNodes ) )
        {
            if( !mKnown[ vNodes[ iNode ], vNodes[ jNode ] ] )
            {
                return( FALSE )
            }
        }
    }

    return( TRUE )
}

#---------------------- -
# Enumerate all clique partitions of the known-correlation graph
#---------------------- -
EnumerateCliquePartitions <- function( mCorr )
{
    nQtyOfNodes <- ncol( mCorr )
    mKnown <- !is.na( mCorr )
    diag( mKnown ) <- FALSE

    EnumerateRecursive <- function( vRemainingNodes )
    {
        if( length( vRemainingNodes ) == 0 )
        {
            return( list( list() ) )
        }

        nAnchor <- vRemainingNodes[ 1 ]
        vOtherNodes <- vRemainingNodes[ -1 ]
        lCliqueSubsets <- list()

        for( nMask in 0:( 2 ^ length( vOtherNodes ) - 1 ) )
        {
            if( length( vOtherNodes ) == 0 )
            {
                vCandidateClique <- nAnchor
            } else {
                vIncluded <- as.logical( intToBits( nMask ) )[ seq_along( vOtherNodes ) ]
                vCandidateClique <- c( nAnchor, vOtherNodes[ vIncluded ] )
            }

            if( IsValidClique( vCandidateClique, mKnown ) )
            {
                lCliqueSubsets[[ length( lCliqueSubsets ) + 1 ]] <- vCandidateClique
            }
        }

        lPartitions <- list()
        for( vClique in lCliqueSubsets )
        {
            vNextRemaining <- setdiff( vRemainingNodes, vClique )
            lChildPartitions <- EnumerateRecursive( vNextRemaining )

            for( lChildPartition in lChildPartitions )
            {
                lPartitions[[ length( lPartitions ) + 1 ]] <- c( list( sort( vClique ) ), lChildPartition )
            }
        }

        return( lPartitions )
    }

    lPartitions <- EnumerateRecursive( seq_len( nQtyOfNodes ) )
    names( lPartitions ) <- vapply( lPartitions, FormatCliquePartition, character( 1 ) )

    return( unname( lPartitions ) )
}

#---------------------- -
# Evaluate the exact comb.test calculation under a fixed clique partition
#---------------------- -
EvaluateCombTestForPartition <- function( p, cr, w, lCliques, mvtnorm_algo )
{
    lCliques <- lapply( lCliques, as.numeric )

    nQtyOfCliques <- length( lCliques )
    vCliqueSizes <- lengths( lCliques )

    if( nQtyOfCliques > 1 && any( vCliqueSizes > 1 ) )
    {
        strAdjMethod <- "Mixed"
    } else if( nQtyOfCliques == 1 && all( vCliqueSizes == 1 ) )
    {
        strAdjMethod <- "NA"
    } else if( nQtyOfCliques == 1 && all( vCliqueSizes > 1 ) )
    {
        strAdjMethod <- "Parametric"
    } else if( nQtyOfCliques > 1 && all( vCliqueSizes == 1 ) )
    {
        strAdjMethod <- "Bonferroni"
    } else {
        stop( "Unexpected clique partition structure." )
    }

    vPartialAdjP <- vapply(
        lCliques,
        function( vClique )
        {
            if( length( vClique ) > 1 )
            {
                dQ <- min( as.numeric( p[ vClique ] ) / as.numeric( w[ vClique ] ) )
                vUpper <- qnorm( 1 - as.numeric( w[ vClique ] ) * dQ )
                mCorrClique <- cr[ vClique, vClique, drop = FALSE ]

                dParametricP <- 1 - mvtnorm::pmvnorm(
                    lower = -Inf,
                    upper = vUpper,
                    corr = mCorrClique,
                    algorithm = mvtnorm_algo
                )

                return( min( 1, dParametricP / sum( as.numeric( w[ vClique ] ) ) ) )
            }

            return( min( 1, min( 1, as.numeric( p[ vClique ] ) / as.numeric( w[ vClique ] ) ) ) )
        },
        numeric( 1 )
    )

    dAdjPvalue <- min( vPartialAdjP, 1 )

    return( list(
        AdjPvalue = dAdjPvalue,
        adjMethod = strAdjMethod,
        partialAdjP = vPartialAdjP,
        cliquePartition = lCliques
    ) )
}

#---------------------- -
# Run the package's comb.test under every admissible clique partition
#---------------------- -
EnumerateCombTestSensitivity <- function( p, cr, w, mvtnorm_algo = NULL )
{
    if( is.null( mvtnorm_algo ) )
    {
        mvtnorm_algo <- ChooseMVTAlgo( ncol( cr ) )
    }

    if( !is.matrix( cr ) || !all( dim( cr ) == c( length( p ), length( p ) ) ) )
    {
        stop( "cr must be a square matrix with the same dimension as p and w." )
    }

    if( length( w ) != length( p ) )
    {
        stop( "p and w must have the same length." )
    }

    if( is.null( names( p ) ) )
    {
        names( p ) <- paste0( "H", seq_along( p ) )
    }

    if( is.null( names( w ) ) )
    {
        names( w ) <- names( p )
    }

    lPartitions <- EnumerateCliquePartitions( cr )

    dfResults <- do.call(
        rbind,
        lapply(
            seq_along( lPartitions ),
            function( iPartition )
            {
                lEval <- EvaluateCombTestForPartition(
                    p = p,
                    cr = cr,
                    w = w,
                    lCliques = lPartitions[[ iPartition ]],
                    mvtnorm_algo = mvtnorm_algo
                )

                return( data.frame(
                    PartitionId = iPartition,
                    Partition = FormatCliquePartition( lEval$cliquePartition ),
                    AdjMethod = lEval$adjMethod,
                    AdjPvalue = as.numeric( lEval$AdjPvalue ),
                    PartialAdjP = toString( signif( lEval$partialAdjP, 10 ) ),
                    stringsAsFactors = FALSE
                ) )
            }
        )
    )

    dfResults <- dfResults[ order( dfResults$AdjPvalue, dfResults$Partition ), ]
    rownames( dfResults ) <- NULL

    lGreedyPartition <- GreedyCliquePartition( cr )
    strGreedyPartition <- FormatCliquePartition( lGreedyPartition )
    vGreedyIdx <- which( dfResults$Partition == strGreedyPartition )

    lSummary <- list(
        nQtyOfPartitions = nrow( dfResults ),
        nQtyOfDistinctAdjPvalues = length( unique( signif( dfResults$AdjPvalue, 12 ) ) ),
        dMinAdjPvalue = min( dfResults$AdjPvalue ),
        dMaxAdjPvalue = max( dfResults$AdjPvalue ),
        greedyPartition = strGreedyPartition,
        greedyAdjPvalue = if( length( vGreedyIdx ) == 1 ) dfResults$AdjPvalue[ vGreedyIdx ] else NA_real_
    )

    return( list(
        results = dfResults,
        summary = lSummary,
        greedyPartition = lGreedyPartition,
        allPartitions = lPartitions
    ) )
}

#---------------------- -
# Evaluate all hypothesis subsets in Example 4 that have more than one valid partition
#---------------------- -
RunExample4SubsetSensitivity <- function() 
{
    vWeights <- rep( 0.25, 4 )
    names( vWeights ) <- paste0( "H", seq_along( vWeights ) )

    vRawP <- c( H1 = 0.00025, H2 = 0.0952, H3 = 0.0245, H4 = 0.1104 )

    mCorr <- matrix(
        c(
            1,   0.5, 0.5, NA,
            0.5, 1,   NA,  0.5,
            0.5, NA,  1,   0.5,
            NA,  0.5, 0.5, 1
        ),
        byrow = TRUE,
        nrow = 4,
        dimnames = list( names( vRawP ), names( vRawP ) )
    )

    mvtnorm_algo <- ChooseMVTAlgo( ncol( mCorr ) )
    lSubsetResults <- list()

    for( nMask in 1:( 2 ^ length( vRawP ) - 1 ) )
    {
        vIncluded <- as.logical( intToBits( nMask ) )[ seq_along( vRawP ) ]
        vSubsetNames <- names( vRawP )[ vIncluded ]

        if( length( vSubsetNames ) < 2 )
        {
            next
        }

        vSubsetP <- vRawP[ vSubsetNames ]
        vSubsetW <- vWeights[ vSubsetNames ]
        mSubsetCorr <- mCorr[ vSubsetNames, vSubsetNames, drop = FALSE ]

        lSensitivity <- EnumerateCombTestSensitivity(
            p = vSubsetP,
            cr = mSubsetCorr,
            w = vSubsetW,
            mvtnorm_algo = mvtnorm_algo
        )

        if( lSensitivity$summary$nQtyOfPartitions > 1 )
        {
            lSubsetResults[[ paste( vSubsetNames, collapse = "," ) ]] <- lSensitivity
        }
    }

    return( lSubsetResults )
}

#---------------------- -
# Print a compact summary for Example 4 subset sensitivity
#---------------------- -
PrintExample4SubsetSensitivity <- function()
{
    lSubsetResults <- RunExample4SubsetSensitivity()

    for( strSubset in names( lSubsetResults ) )
    {
        lSensitivity <- lSubsetResults[[ strSubset ]]
        cat( "\nSubset:", strSubset, "\n" )
        cat( "Greedy partition:", lSensitivity$summary$greedyPartition, "\n" )
        cat( "Adjusted p-value range:",
             signif( lSensitivity$summary$dMinAdjPvalue, 10 ), "to",
             signif( lSensitivity$summary$dMaxAdjPvalue, 10 ), "\n" )
        print( lSensitivity$results )
    }

    return( invisible( lSubsetResults ) )
}

#---------------------- -
# Example: full-set sensitivity for Example 4 only
#---------------------- -
RunExample4FullSetSensitivity <- function()
{
    vWeights <- rep( 0.25, 4 )
    names( vWeights ) <- paste0( "H", seq_along( vWeights ) )

    vRawP <- c( H1 = 0.00025, H2 = 0.0952, H3 = 0.0245, H4 = 0.1104 )
    mCorr <- matrix(
        c(
            1,   0.5, 0.5, NA,
            0.5, 1,   NA,  0.5,
            0.5, NA,  1,   0.5,
            NA,  0.5, 0.5, 1
        ),
        byrow = TRUE,
        nrow = 4,
        dimnames = list( names( vRawP ), names( vRawP ) )
    )

    return( EnumerateCombTestSensitivity( p = vRawP, cr = mCorr, w = vWeights ) )
}

# Example usage:
# source( "internalData/PopEnrichCliquePartitionHarness.R" )
# RunExample4FullSetSensitivity()
# PrintExample4SubsetSensitivity()