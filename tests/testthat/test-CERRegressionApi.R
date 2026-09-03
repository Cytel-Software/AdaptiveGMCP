# --------------------------------------------------------------------------------------------------
#
# ©2026 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

#' Regression tests for adaptGMCP_CER()
#'
#' These tests compare selected non-interactive CER analysis outputs against
#' checked-in fixtures generated from known scenarios.

BuildCerRegressionScenarios <- function()
{
  lScenario1 <- list(
    rowId = "CER-regression-01",
    nArms = 3,
    nEps = 1,
    sampleSize = 300,
    epType = list( "EP1" = "Continuous" ),
    sigma = list( "EP1" = c( 1, 1, 1 ) ),
    CommonStdDev = TRUE,
    prop.ctr = list( "EP1" = NA ),
    allocRatio = c( 1, 1, 1 ),
    WI = c( 0.5, 0.5 ),
    G = matrix( c( 0, 1, 1, 0 ), nrow = 2, byrow = TRUE ),
    testType = "Parametric",
    alpha = 0.025,
    infoFrac = c( 0.5, 1.0 ),
    typeOfDesign = "asOF",
    lookInputs = list(
      list( p_raw = c( H1 = 0.1, H2 = 0.2 ) ),
      list( p_raw = c( H1 = 0.0001, H2 = 0.2 ) )
    )
  )

  lScenario2 <- list(
    rowId = "CER-regression-02",
    nArms = 3,
    nEps = 1,
    sampleSize = 300,
    epType = list( "EP1" = "Continuous" ),
    sigma = list( "EP1" = c( 1, 1, 1 ) ),
    CommonStdDev = TRUE,
    prop.ctr = list( "EP1" = NA ),
    allocRatio = c( 1, 1, 1 ),
    WI = c( 0.5, 0.5 ),
    G = matrix( c( 0, 1, 1, 0 ), nrow = 2, byrow = TRUE ),
    testType = "Parametric",
    alpha = 0.025,
    infoFrac = c( 0.5, 1.0 ),
    typeOfDesign = "asOF",
    lookInputs = list(
      list( p_raw = c( H1 = 0.1, H2 = 0.2 ) ),
      list( p_raw = c( H2 = 0.02 ), selection = c( "H2" ) )
    )
  )

  lScenario3 <- list(
    rowId = "CER-regression-03",
    nArms = 3,
    nEps = 1,
    sampleSize = 300,
    epType = list( "EP1" = "Continuous" ),
    sigma = list( "EP1" = c( 1, 1, 1 ) ),
    CommonStdDev = TRUE,
    prop.ctr = list( "EP1" = NA ),
    allocRatio = c( 1, 1, 1 ),
    WI = c( 0.5, 0.5 ),
    G = matrix( c( 0, 1, 1, 0 ), nrow = 2, byrow = TRUE ),
    testType = "Parametric",
    alpha = 0.025,
    infoFrac = c( 0.5, 1.0 ),
    typeOfDesign = "asOF",
    lookInputs = list(
      list( p_raw = c( H1 = 0.0001, H2 = 0.2 ) ),
      list(
        p_raw = c( H2 = 0.01 ),
        stage2_cumulative_sample_size = c( Control = 125, Treatment1 = 125, Treatment2 = 125 )
      )
    )
  )

  return( list(
    "CER-regression-01" = lScenario1,
    "CER-regression-02" = lScenario2,
    "CER-regression-03" = lScenario3
  ) )
}

ExtractCerRegressionLook <- function( mcpObj )
{
  mStage1Boundary <- NA
  mStage2Boundary <- NA
  vCumulativeStage2PValues <- NA
  mAdjustedBoundary <- NA

  if( is.list( mcpObj$Stage1Obj ) && is.list( mcpObj$Stage1Obj$plan_Bdry ) )
  {
    if( !is.null( mcpObj$Stage1Obj$plan_Bdry$Stage1Bdry ) )
    {
      mStage1Boundary <- mcpObj$Stage1Obj$plan_Bdry$Stage1Bdry
    }

    if( !is.null( mcpObj$Stage1Obj$plan_Bdry$Stage2Bdry ) )
    {
      mStage2Boundary <- mcpObj$Stage1Obj$plan_Bdry$Stage2Bdry
    }
  }

  if( is.list( mcpObj$AdaptObj ) && !is.null( mcpObj$AdaptObj$Stage2AdjBdry ) )
  {
    mAdjustedBoundary <- mcpObj$AdaptObj$Stage2AdjBdry
  }

  if( !is.null( mcpObj$Stage2CumPValues ) )
  {
    vCumulativeStage2PValues <- mcpObj$Stage2CumPValues
  }

  return( list(
    stage1_boundary = mStage1Boundary,
    stage2_boundary = mStage2Boundary,
    cumulative_stage2_pvalues = vCumulativeStage2PValues,
    adjusted_boundary = mAdjustedBoundary,
    final_rejection_status = mcpObj$rej_flag_Curr
  ) )
}

CaptureCerRegressionOutputsForTest <- function( lScenario )
{
  nPlannedLooks <- length( lScenario$lookInputs )
  eCaptured <- new.env( parent = emptyenv() )
  eCaptured$lLooks <- list()

  testthat::with_mocked_bindings(
    adaptGMCP_CER(
      nArms = lScenario$nArms,
      nEps = lScenario$nEps,
      SampleSize = lScenario$sampleSize,
      EpType = lScenario$epType,
      sigma = lScenario$sigma,
      CommonStdDev = lScenario$CommonStdDev,
      prop.ctr = lScenario$prop.ctr,
      allocRatio = lScenario$allocRatio,
      WI = lScenario$WI,
      G = lScenario$G,
      test.type = lScenario$testType,
      alpha = lScenario$alpha,
      info_frac = lScenario$infoFrac,
      typeOfDesign = lScenario$typeOfDesign,
      AdaptStage2 = TRUE,
      plotGraphs = FALSE
    ),
    getRawPValues = function( mcpObj )
    {
      return( lScenario$lookInputs[[ mcpObj$CurrentLook ]]$p_raw )
    },
    trialContinuationDecision = function( mcpObj )
    {
      eCaptured$lLooks[[ as.character( mcpObj$CurrentLook ) ]] <- ExtractCerRegressionLook( mcpObj )

      if( StopTrial( mcpObj ) )
      {
        return( "n" )
      }

      if( mcpObj$CurrentLook < nPlannedLooks )
      {
        return( "y" )
      }

      return( "n" )
    },
    do_Selection = function( mcpObj )
    {
      nNextLook <- as.integer( mcpObj$CurrentLook + 1L )
      if( nNextLook > nPlannedLooks ) return( mcpObj )

      vSelection <- lScenario$lookInputs[[ nNextLook ]]$selection
      if( is.null( vSelection ) ) return( mcpObj )

      return( applySelection( mcpObj, selected_hyps = vSelection, look = nNextLook ) )
    },
    do_modifyStrategy = function( mcpObj, showExistingStrategy = FALSE )
    {
      return( mcpObj )
    },
    do_ModifyStage2Sample = function( allocRatio, ArmsPresent, AllocSampleSize )
    {
      nNextLook <- as.integer( length( eCaptured$lLooks ) + 1L )
      vStage2SampleSize <- lScenario$lookInputs[[ nNextLook ]]$stage2_cumulative_sample_size

      if( is.null( vStage2SampleSize ) )
      {
        return( list(
          newAllocSampleSize = AllocSampleSize,
          newallocRatio = allocRatio
        ) )
      }

      mNewAllocSampleSize <- AllocSampleSize
      mNewAllocSampleSize[ 2, names( vStage2SampleSize ) ] <- as.numeric( vStage2SampleSize )

      return( list(
        newAllocSampleSize = mNewAllocSampleSize,
        newallocRatio = as.numeric( mNewAllocSampleSize[ 2, ] ) / as.numeric( mNewAllocSampleSize[ 2, 1 ] )
      ) )
    }
  )

  return( eCaptured$lLooks )
}

LoadCerRegressionFixtures <- function( strRowId )
{
  vFiles <- Sys.glob( testthat::test_path( paste0( strRowId, ".regression.l*.rds" ) ) )
  testthat::expect_gt( length( vFiles ), 0 )

  vLooks <- sub( paste0( ".*", strRowId, "\\.regression\\.l([0-9]+)\\.rds$" ), "\\1", vFiles )
  iOrder <- order( as.integer( vLooks ) )
  vFiles <- vFiles[ iOrder ]
  vLooks <- vLooks[ iOrder ]

  return( stats::setNames( lapply( vFiles, readRDS ), vLooks ) )
}

ExpectCerRegressionLookEqual <- function( lActual, lExpected )
{
  testthat::expect_equal( lActual$stage1_boundary, lExpected$stage1_boundary, tolerance = 1e-8 )
  testthat::expect_equal( lActual$stage2_boundary, lExpected$stage2_boundary, tolerance = 1e-8 )
  testthat::expect_equal( lActual$cumulative_stage2_pvalues, lExpected$cumulative_stage2_pvalues, tolerance = 1e-8 )
  testthat::expect_equal( lActual$adjusted_boundary, lExpected$adjusted_boundary, tolerance = 1e-8 )
  testthat::expect_equal( lActual$final_rejection_status, lExpected$final_rejection_status )
}

lCerScenarios <- BuildCerRegressionScenarios()

testthat::test_that( "CER-regression-01 matches fixture outputs", {
  lActual <- CaptureCerRegressionOutputsForTest( lCerScenarios[[ "CER-regression-01" ]] )
  lExpected <- LoadCerRegressionFixtures( "CER-regression-01" )

  testthat::expect_equal( names( lActual ), names( lExpected ) )

  for( strLook in names( lExpected ) )
  {
    ExpectCerRegressionLookEqual( lActual[[ strLook ]], lExpected[[ strLook ]] )
  }
} )

testthat::test_that( "CER-regression-02 matches fixture outputs", {
  lActual <- CaptureCerRegressionOutputsForTest( lCerScenarios[[ "CER-regression-02" ]] )
  lExpected <- LoadCerRegressionFixtures( "CER-regression-02" )

  testthat::expect_equal( names( lActual ), names( lExpected ) )

  for( strLook in names( lExpected ) )
  {
    ExpectCerRegressionLookEqual( lActual[[ strLook ]], lExpected[[ strLook ]] )
  }
} )

testthat::test_that( "CER-regression-03 matches fixture outputs", {
  lActual <- CaptureCerRegressionOutputsForTest( lCerScenarios[[ "CER-regression-03" ]] )
  lExpected <- LoadCerRegressionFixtures( "CER-regression-03" )

  testthat::expect_equal( names( lActual ), names( lExpected ) )

  for( strLook in names( lExpected ) )
  {
    ExpectCerRegressionLookEqual( lActual[[ strLook ]], lExpected[[ strLook ]] )
  }
} )
