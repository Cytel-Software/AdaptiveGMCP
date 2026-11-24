#!/usr/bin/env Rscript

vArgs <- commandArgs( trailingOnly = TRUE )
if( length( vArgs ) != 1L || !file.exists( vArgs[ 1 ] ) )
{
  stop( "Supply the path to the built AdaptGMCP source archive." )
}

strLibrary <- Sys.getenv( "R_LIBS_USER" )
if( !nzchar( strLibrary ) || !dir.exists( strLibrary ) )
{
  stop( "R_LIBS_USER must name an existing, empty library." )
}
if( length( list.files( strLibrary, all.files = TRUE, no.. = TRUE ) ) > 0L )
{
  stop( "The smoke test requires an empty user library." )
}
vExpectedPaths <- normalizePath( c( strLibrary, .Library ), winslash = "/", mustWork = TRUE )
vActualPaths <- normalizePath( .libPaths(), winslash = "/", mustWork = TRUE )
if( !setequal( vActualPaths, vExpectedPaths ) )
{
  stop( "The smoke test requires an isolated user library and the base R library." )
}

vInitiallyInstalled <- rownames( installed.packages() )
vPlotDependencies <- c( "visNetwork", "reshape2" )
if( any( c( "AdaptGMCP", vPlotDependencies ) %in% vInitiallyInstalled ) )
{
  stop( "AdaptGMCP and its plotting dependencies must not be preinstalled." )
}

strExtractionDir <- tempfile( "adaptgmcp-description-" )
dir.create( strExtractionDir )
utils::untar(
  vArgs[ 1 ],
  files = "AdaptGMCP/DESCRIPTION",
  exdir = strExtractionDir
)
mDescription <- read.dcf(
  file.path( strExtractionDir, "AdaptGMCP", "DESCRIPTION" ),
  fields = c( "Package", "Version", "Depends", "Imports", "LinkingTo" )
)
if( !identical( unname( mDescription[ 1, "Package" ] ), "AdaptGMCP" ) )
{
  stop( "The archive does not contain the expected AdaptGMCP package." )
}
vImports <- tools::package_dependencies(
  "AdaptGMCP", db = mDescription, which = "Imports"
)[[ 1 ]]
if( !all( vPlotDependencies %in% vImports ) )
{
  stop( "The archive must declare both graph plotting packages in Imports." )
}

vRequired <- tools::package_dependencies(
  "AdaptGMCP", db = mDescription, which = c( "Depends", "Imports", "LinkingTo" )
)[[ 1 ]]
vToInstall <- setdiff( vRequired, c( "R", vInitiallyInstalled ) )
if( length( vToInstall ) > 0L )
{
  install.packages(
    vToInstall,
    lib = strLibrary,
    repos = "https://cloud.r-project.org",
    type = "binary"
  )
}
vMissing <- setdiff( vRequired, c( "R", rownames( installed.packages() ) ) )
if( length( vMissing ) > 0L )
{
  stop( "Required dependencies were not installed: ", paste( vMissing, collapse = ", " ) )
}

install.packages( vArgs[ 1 ], lib = strLibrary, repos = NULL, type = "source" )
mInstalled <- installed.packages( lib.loc = strLibrary )
if( !( "AdaptGMCP" %in% rownames( mInstalled ) ) ||
    !identical( unname( mInstalled[ "AdaptGMCP", "Version" ] ),
                unname( mDescription[ 1, "Version" ] ) ) )
{
  stop( "The built AdaptGMCP archive was not installed successfully." )
}

library( "AdaptGMCP", lib.loc = strLibrary, character.only = TRUE )
cState <- SetupAnalysisCER(
  nArms = 3,
  nEps = 1,
  nSampleSize = 300,
  lEpTypes = list( EP1 = "Continuous" ),
  lSigma = list( EP1 = c( 1, 1, 1 ) ),
  bCommonStdDev = TRUE,
  lCtrlProps = list( EP1 = NA ),
  vAllocRatios = c( 1, 1, 1 ),
  vWeights = c( 0.5, 0.5 ),
  mGMatrix = matrix( c( 0, 1, 1, 0 ), nrow = 2, byrow = TRUE ),
  strTestType = "Parametric",
  bPlotGraphs = TRUE
)
if( !inherits( cState, "CERAnalysisState" ) )
{
  stop( "CER setup did not return an analysis state." )
}
lGraph <- PlotGraphCER( cState )
if( length( lGraph ) != 1L || !inherits( lGraph[[ 1 ]], "visNetwork" ) ||
    nrow( lGraph[[ 1 ]]$x$nodes ) != 2L ||
    nrow( lGraph[[ 1 ]]$x$edges ) != 2L )
{
  stop( "CER plotting did not produce the expected graph." )
}
if( any( paste0( "package:", vPlotDependencies ) %in% search() ) )
{
  stop( "Graph plotting attached its dependencies to the search path." )
}
message( "Clean Windows installation and CER plotting smoke test passed." )
