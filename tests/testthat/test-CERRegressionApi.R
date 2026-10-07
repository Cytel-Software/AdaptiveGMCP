# Regression tests for legacy adaptGMCP_CER() outputs.

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

  mAdaptedCovariance <- NA
  if( is.list( mcpObj$AdaptObj ) && !is.null( mcpObj$AdaptObj$Stage2Sigma ) )
  {
    mAdaptedCovariance <- mcpObj$AdaptObj$Stage2Sigma
  }

  return( list(
    stage1_boundary = mStage1Boundary,
    stage2_boundary = mStage2Boundary,
    cumulative_stage2_pvalues = vCumulativeStage2PValues,
    adjusted_boundary = mAdjustedBoundary,
    final_rejection_status = mcpObj$rej_flag_Curr,
    stage1_intersect_test = mcpObj$Stage1Obj$Stage1Analysis$IntersectHypoTest,
    stage1_primary_test = mcpObj$Stage1Obj$Stage1Analysis$PrimaryHypoTest,
    active_hypotheses = mcpObj$IndexSet,
    selected_hypotheses = mcpObj$SelectedIndex,
    dropped_flag = mcpObj$DroppedFlag,
    planned_sample_allocation = mcpObj$AllocSampleSize,
    adapted_sample_allocation = mcpObj$Stage2AllocSampleSize,
    adapted_covariance = mAdaptedCovariance,
    structured_cer_pcer = mcpObj$CERTab
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
      nNextLook <- as.integer( length( eCaptured$lLooks ) + 1L )
      if( nNextLook > nPlannedLooks ) return( mcpObj )

      lStrategyUpdate <- lScenario$lookInputs[[ nNextLook ]]$strategy_update
      if( is.null( lStrategyUpdate ) ) return( mcpObj )

      return( applyStrategyUpdate(
        mcpObj,
        new_weights = lStrategyUpdate$new_weights,
        new_G = lStrategyUpdate$new_G
      ) )
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

testthat::test_that( "CER-GSExample4-01 matches fixture outputs", {
  lActual <- CaptureCerRegressionOutputsForTest( lCerScenarios[[ "CER-GSExample4-01" ]] )
  lExpected <- LoadCerRegressionFixtures( "CER-GSExample4-01" )

  testthat::expect_equal( names( lActual ), names( lExpected ) )

  for( strLook in names( lExpected ) )
  {
    ExpectCerRegressionLookEqual( lActual[[ strLook ]], lExpected[[ strLook ]] )
  }
} )

testthat::test_that( "CER-GSExample4-02 matches fixture outputs", {
  lActual <- CaptureCerRegressionOutputsForTest( lCerScenarios[[ "CER-GSExample4-02" ]] )
  lExpected <- LoadCerRegressionFixtures( "CER-GSExample4-02" )

  testthat::expect_equal( names( lActual ), names( lExpected ) )

  for( strLook in names( lExpected ) )
  {
    ExpectCerRegressionLookEqual( lActual[[ strLook ]], lExpected[[ strLook ]] )
  }
} )

testthat::test_that( "CER-GSExample2-01 matches fixture outputs", {
  lActual <- CaptureCerRegressionOutputsForTest( lCerScenarios[[ "CER-GSExample2-01" ]] )
  lExpected <- LoadCerRegressionFixtures( "CER-GSExample2-01" )

  testthat::expect_equal( names( lActual ), names( lExpected ) )

  for( strLook in names( lExpected ) )
  {
    ExpectCerRegressionLookEqual( lActual[[ strLook ]], lExpected[[ strLook ]] )
  }
} )

testthat::test_that( "CER-AdaptExample2-01 matches fixture outputs", {
  lActual <- CaptureCerRegressionOutputsForTest( lCerScenarios[[ "CER-AdaptExample2-01" ]] )
  lExpected <- LoadCerRegressionFixtures( "CER-AdaptExample2-01" )

  testthat::expect_equal( names( lActual ), names( lExpected ) )

  for( strLook in names( lExpected ) )
  {
    ExpectCerRegressionLookEqual( lActual[[ strLook ]], lExpected[[ strLook ]] )
  }
} )
