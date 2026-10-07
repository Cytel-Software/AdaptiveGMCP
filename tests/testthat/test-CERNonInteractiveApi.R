# Regression tests for the non-interactive CER analysis API.

#' Extract fixture-compatible outputs from a non-interactive CER analysis state
#'
#' @param lState A `CERAnalysisState` returned by `AnalyzeLook_CER()`.
#' @return A named list matching the legacy CER regression fixture structure.
ExtractCerRegressionState <- function( lState )
{
  mAdjustedBoundary <- NA
  if( is.list( lState$adaptation ) &&
      !is.null( lState$adaptation$Stage2AdjBdry ) )
  {
    mAdjustedBoundary <- lState$adaptation$Stage2AdjBdry
  }

  mAdaptedCovariance <- NA
  if( !is.null( lState$adapted_covariance ) )
  {
    mAdaptedCovariance <- lState$adapted_covariance
  }

  vCumulativeStage2PValues <- NA
  if( !is.null( lState$cumulative_stage2_pvalues ) )
  {
    vCumulativeStage2PValues <- lState$cumulative_stage2_pvalues
  }

  lFinalRejectionStatus <- lState$results$stage1$primary_rejection
  vActiveHypotheses <- lState$active_hypotheses
  if( lState$completed_looks == 2L )
  {
    lFinalRejectionStatus <- lState$results$stage2$primary_rejection
    vActiveHypotheses <- names( lState$incremental_stage2_pvalues )[
      !is.na( lState$incremental_stage2_pvalues )
    ]
  }

  vSelectedHypotheses <- lState$selected_hypotheses
  if( length( vSelectedHypotheses ) == 0L )
  {
    vSelectedHypotheses <- NULL
  }

  return( list(
    stage1_boundary = lState$results$stage1$planned_boundary$Stage1Bdry,
    stage2_boundary = lState$results$stage1$planned_boundary$Stage2Bdry,
    cumulative_stage2_pvalues = vCumulativeStage2PValues,
    adjusted_boundary = mAdjustedBoundary,
    final_rejection_status = lFinalRejectionStatus,
    stage1_intersect_test = lState$results$stage1$intersection_rejection,
    stage1_primary_test = lState$results$stage1$primary_rejection,
    active_hypotheses = vActiveHypotheses,
    selected_hypotheses = vSelectedHypotheses,
    dropped_flag = lState$dropped_hypotheses,
    planned_sample_allocation = lState$planned_sample_allocation,
    adapted_sample_allocation = lState$adapted_sample_allocation,
    adapted_covariance = mAdaptedCovariance,
    structured_cer_pcer = lState$results$stage1$cer_pcer$table
  ) )
}

#' Run a CER regression scenario through the non-interactive analysis API
#'
#' @param lScenario A scenario returned by `BuildCerRegressionScenarios()`.
#' @return A named list of fixture-compatible outputs for each completed look.
CaptureNonInteractiveCerRegressionOutputs <- function( lScenario )
{
  lDesign <- SetupDesign_CER(
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
    plotGraphs = FALSE
  )
  lState <- NULL
  lLooks <- list()

  for( iLook in seq_along( lScenario$lookInputs ) )
  {
    lLookInput <- lScenario$lookInputs[[ iLook ]]
    lArguments <- list(
      design = lDesign,
      state = lState,
      p_raw = lLookInput$p_raw,
      look = iLook,
      selection = lLookInput$selection,
      stage2_cumulative_sample_size =
        lLookInput$stage2_cumulative_sample_size,
      strategy_update = lLookInput$strategy_update,
      plotGraphs = FALSE
    )
    lState <- try( do.call( AnalyzeLook_CER, lArguments ), silent = TRUE )
    if( inherits( lState, "try-error" ) )
    {
      stop(
        paste0(
          "Non-interactive CER regression scenario ", lScenario$rowId,
          " failed at look ", iLook, ": ",
          conditionMessage( attr( lState, "condition" ) )
        ),
        call. = FALSE
      )
    }
    lLooks[[ as.character( iLook ) ]] <- ExtractCerRegressionState( lState )

    if( isTRUE( lState$trial_completed ) )
    {
      break
    }
  }

  return( lLooks )
}

#' Compare outputs that are common to both CER analysis APIs
#'
#' @param lActual A non-interactive CER analysis result.
#' @param lExpected The equivalent legacy CER regression fixture result.
#' @param strContext Scenario and look context for failure diagnostics.
#' @return The testthat expectations, invisibly.
ExpectNonInteractiveCerRegressionLookEqual <- function(
    lActual, lExpected, strContext )
{
  vCumulativePValues <- lActual$cumulative_stage2_pvalues
  vContinuingHypotheses <- names( vCumulativePValues )[ !is.na( vCumulativePValues ) ]

  testthat::expect_equal(
    lActual$stage1_boundary, lExpected$stage1_boundary,
    tolerance = 1e-8, info = strContext
  )
  testthat::expect_equal(
    lActual$stage2_boundary, lExpected$stage2_boundary,
    tolerance = 1e-8, info = strContext
  )
  testthat::expect_equal(
    vCumulativePValues[ vContinuingHypotheses ],
    lExpected$cumulative_stage2_pvalues[ vContinuingHypotheses ],
    tolerance = 1e-8, info = strContext
  )
  testthat::expect_equal(
    lActual$stage1_intersect_test, lExpected$stage1_intersect_test,
    tolerance = 1e-8, info = strContext
  )
  testthat::expect_equal(
    lActual$stage1_primary_test, lExpected$stage1_primary_test,
    tolerance = 1e-8, info = strContext
  )
  testthat::expect_equal(
    lActual$active_hypotheses, lExpected$active_hypotheses,
    info = strContext
  )
  testthat::expect_equal(
    lActual$selected_hypotheses, lExpected$selected_hypotheses,
    info = strContext
  )
  testthat::expect_equal(
    lActual$dropped_flag, lExpected$dropped_flag,
    info = strContext
  )
  testthat::expect_equal(
    lActual$planned_sample_allocation, lExpected$planned_sample_allocation,
    tolerance = 1e-8, info = strContext
  )
  testthat::expect_equal(
    lActual$adapted_sample_allocation, lExpected$adapted_sample_allocation,
    tolerance = 1e-8, info = strContext
  )
  testthat::expect_equal(
    lActual$structured_cer_pcer, lExpected$structured_cer_pcer,
    tolerance = 1e-8, info = strContext
  )
}

lCerScenarios <- BuildCerRegressionScenarios()

for( strScenarioId in names( lCerScenarios ) )
{
  testthat::test_that(
    paste0( strScenarioId, " non-interactive API matches legacy fixture outputs" ),
    {
      lActual <- CaptureNonInteractiveCerRegressionOutputs(
        lCerScenarios[[ strScenarioId ]]
      )
      lExpected <- LoadCerRegressionFixtures( strScenarioId )
      strScenarioContext <- paste0( "scenario ", strScenarioId )

      testthat::expect_equal(
        names( lActual ),
        names( lExpected ),
        info = strScenarioContext
      )

      for( strLook in names( lExpected ) )
      {
        ExpectNonInteractiveCerRegressionLookEqual(
          lActual[[ strLook ]],
          lExpected[[ strLook ]],
          paste0( strScenarioContext, ", look ", strLook )
        )
      }
    }
  )
}
