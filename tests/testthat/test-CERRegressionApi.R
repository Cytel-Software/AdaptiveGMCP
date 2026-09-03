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

  # CER Scenario 4: Example from AdaptGMCP_CER_Analysis_Example.R
  # 3-arm, 1-endpoint Parametric with stringent p-value thresholds
  lScenario4 <- list(
    rowId = "CER-Examp-AdaptGMCP-01",
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
      list( p_raw = c( H1 = 0.00001, H2 = 0.02 ) ),
      list( p_raw = c( H1 = 0.0001, H2 = 0.01 ) )
    )
  )

  # CER Scenario 5: Example from CER Analysis 3arm-1ep.R
  # 3-arm, 1-endpoint Parametric with varying standard deviations
  lScenario5 <- list(
    rowId = "CER-Examp-3arm-1ep",
    nArms = 3,
    nEps = 1,
    sampleSize = 210,
    epType = list( "EP1" = "Continuous" ),
    sigma = list( "EP1" = c( 2, 2, 2 ) ),
    CommonStdDev = FALSE,
    prop.ctr = list( "EP1" = NA ),
    allocRatio = c( 1, 1, 1 ),
    WI = c( 0.5, 0.5 ),
    G = matrix( c( 0, 1, 1, 0 ), nrow = 2, byrow = TRUE ),
    testType = "Parametric",
    alpha = 0.025,
    infoFrac = c( 0.5, 1.0 ),
    typeOfDesign = "asOF",
    lookInputs = list(
      list( p_raw = c( H1 = 0.05, H2 = 0.15 ) ),
      list( p_raw = c( H1 = 0.02, H2 = 0.08 ) )
    )
  )

  # CER Scenario 6: Example from CER.Analysis.2primary-2secondary.R
  # 3-arm, 2-endpoint Partly-Parametric design with multiple hypotheses
  lScenario6 <- list(
    rowId = "CER-Examp-2ep",
    nArms = 3,
    nEps = 2,
    sampleSize = 400,
    epType = list( "EP1" = "Continuous", "EP2" = "Continuous" ),
    sigma = list( "EP1" = c( 1, 1, 1 ), "EP2" = c( 1, 1, 1 ) ),
    CommonStdDev = TRUE,
    prop.ctr = list( "EP1" = NA, "EP2" = NA ),
    allocRatio = c( 1, 1, 1 ),
    WI = c( 0.5, 0.5, 0, 0 ),
    G = matrix( c( 0, 1/2, 1/2, 0, 1/2, 0, 0, 1/2, 0, 1, 0, 0, 1, 0, 0, 0 ), nrow = 4, byrow = TRUE ),
    testType = "Partly-Parametric",
    alpha = 0.025,
    infoFrac = c( 0.5, 1.0 ),
    typeOfDesign = "asOF",
    lookInputs = list(
      list( p_raw = c( H1 = 0.15, H2 = 0.25, H3 = 0.10, H4 = 0.20 ) ),
      list( p_raw = c( H1 = 0.05, H2 = 0.10, H3 = 0.03, H4 = 0.12 ) )
    )
  )

  return( list(
    "CER-regression-01" = lScenario1,
    "CER-regression-02" = lScenario2,
    "CER-regression-03" = lScenario3,
    "CER-Examp-AdaptGMCP-01" = lScenario4,
    "CER-Examp-3arm-1ep" = lScenario5,
    "CER-Examp-2ep" = lScenario6
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

testthat::test_that( "CER-Examp-AdaptGMCP-01 matches fixture outputs", {
  lActual <- CaptureCerRegressionOutputsForTest( lCerScenarios[[ "CER-Examp-AdaptGMCP-01" ]] )
  lExpected <- LoadCerRegressionFixtures( "CER-Examp-AdaptGMCP-01" )

  testthat::expect_equal( names( lActual ), names( lExpected ) )

  for( strLook in names( lExpected ) )
  {
    ExpectCerRegressionLookEqual( lActual[[ strLook ]], lExpected[[ strLook ]] )
  }
} )

testthat::test_that( "CER-Examp-3arm-1ep matches fixture outputs", {
  lActual <- CaptureCerRegressionOutputsForTest( lCerScenarios[[ "CER-Examp-3arm-1ep" ]] )
  lExpected <- LoadCerRegressionFixtures( "CER-Examp-3arm-1ep" )

  testthat::expect_equal( names( lActual ), names( lExpected ) )

  for( strLook in names( lExpected ) )
  {
    ExpectCerRegressionLookEqual( lActual[[ strLook ]], lExpected[[ strLook ]] )
  }
} )

testthat::test_that( "CER-Examp-2ep matches fixture outputs", {
  lActual <- CaptureCerRegressionOutputsForTest( lCerScenarios[[ "CER-Examp-2ep" ]] )
  lExpected <- LoadCerRegressionFixtures( "CER-Examp-2ep" )

  testthat::expect_equal( names( lActual ), names( lExpected ) )

  for( strLook in names( lExpected ) )
  {
    ExpectCerRegressionLookEqual( lActual[[ strLook ]], lExpected[[ strLook ]] )
  }
} )
