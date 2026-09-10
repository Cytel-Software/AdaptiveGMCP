# --------------------------------------------------------------------------------------------------
#
# ©2026 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

# Script to generate CER analysis regression test fixtures by capturing key outputs
# at each look using adaptGMCP_CER() in non-interactive mode with mocked bindings.
#
# Key outputs captured for regression testing:
#   - Stage1_Boundary
#   - Stage2_Boundary
#   - Stage2_Test_Procedure
#   - Adjusted_Boundary
#   - Final_Rejection_Status (at each look)

suppressPackageStartupMessages({
  library(devtools)
})

load_all(quiet = TRUE)

#------ -------- -------- -------- -------- -------- -------- -------- --------
# CaptureCERRegressionOutputs
#------ -------- -------- -------- -------- -------- -------- -------- --------
# Captures key regression test outputs at each look by mocking trial functions
#
# Key outputs captured:
#   - Stage1_Boundary: mcpObj$Stage1Obj$plan_Bdry$Stage1Bdry
#   - Stage2_Boundary: mcpObj$Stage1Obj$plan_Bdry$Stage2Bdry
#   - Cumulative_Stage2_PValues: mcpObj$Stage2CumPValues
#   - Adjusted_Boundary: mcpObj$AdaptObj$Stage2AdjBdry
#   - Final_Rejection_Status: mcpObj$rej_flag_Curr
#
CaptureCERRegressionOutputs <- function( lScenario )
{
  nPlannedLooks <- length( lScenario$lookInputs )
  eCapture <- new.env( parent = emptyenv() )
  eCapture$outputs <- list()

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
      nLook <- as.integer( mcpObj$CurrentLook )

      vStage1Boundary <- NA
      vStage2Boundary <- NA
      vCumulativeStage2PValues <- NA
      vAdjustedBoundary <- NA

      if( is.list( mcpObj$Stage1Obj ) && is.list( mcpObj$Stage1Obj$plan_Bdry ) ) {
        if( !is.null( mcpObj$Stage1Obj$plan_Bdry$Stage1Bdry ) ) {
          vStage1Boundary <- mcpObj$Stage1Obj$plan_Bdry$Stage1Bdry
        }
        if( !is.null( mcpObj$Stage1Obj$plan_Bdry$Stage2Bdry ) ) {
          vStage2Boundary <- mcpObj$Stage1Obj$plan_Bdry$Stage2Bdry
        }
      }

      if( is.list( mcpObj$AdaptObj ) && !is.null( mcpObj$AdaptObj$Stage2AdjBdry ) ) {
        vAdjustedBoundary <- mcpObj$AdaptObj$Stage2AdjBdry
      }

      if( !is.null( mcpObj$Stage2CumPValues ) ) {
        vCumulativeStage2PValues <- mcpObj$Stage2CumPValues
      }

      vAdaptedCovariance <- NA
      if( is.list( mcpObj$AdaptObj ) && !is.null( mcpObj$AdaptObj$Stage2Sigma ) ) {
        vAdaptedCovariance <- mcpObj$AdaptObj$Stage2Sigma
      }

      eCapture$outputs[[ as.character( nLook ) ]] <- list(
        stage1_boundary = vStage1Boundary,
        stage2_boundary = vStage2Boundary,
        cumulative_stage2_pvalues = vCumulativeStage2PValues,
        adjusted_boundary = vAdjustedBoundary,
        final_rejection_status = mcpObj$rej_flag_Curr,
        stage1_intersect_test = mcpObj$Stage1Obj$Stage1Analysis$IntersectHypoTest,
        stage1_primary_test = mcpObj$Stage1Obj$Stage1Analysis$PrimaryHypoTest,
        active_hypotheses = mcpObj$IndexSet,
        selected_hypotheses = mcpObj$SelectedIndex,
        dropped_flag = mcpObj$DroppedFlag,
        planned_sample_allocation = mcpObj$AllocSampleSize,
        adapted_sample_allocation = mcpObj$Stage2AllocSampleSize,
        adapted_covariance = vAdaptedCovariance,
        structured_cer_pcer = mcpObj$CERTab
      )

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
      nNextLook <- as.integer( length( na.omit( unique( c( names( eCapture$outputs ) ) ) ) ) ) + 1L
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
      nNextLook <- as.integer( length( na.omit( unique( c( names( eCapture$outputs ) ) ) ) ) ) + 1L
      if( nNextLook > nPlannedLooks ) {
        return( list(
          newAllocSampleSize = AllocSampleSize,
          newallocRatio = allocRatio
        ) )
      }

      vStage2SampleSize <- lScenario$lookInputs[[ nNextLook ]]$stage2_cumulative_sample_size
      if( is.null( vStage2SampleSize ) ) {
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
  
  return( eCapture$outputs )
}

#------ -------- -------- -------- -------- -------- -------- -------- --------
# SaveCERRegressionFixtures
#------ -------- -------- -------- -------- -------- -------- -------- --------
# Generates regression fixtures for a scenario by running adaptGMCP_CER()
# with mocked bindings and saving key outputs to RDS files at each look
SaveCERRegressionFixtures <- function( lScenario, strOutDir )
{
  cat( "Generating CER regression fixtures for scenario: ", lScenario$rowId, "\n", sep = "" )

  tryCatch({
    lLooks <- CaptureCERRegressionOutputs( lScenario )
    cat( "  Captured ", length( lLooks ), " looks\n", sep = "" )

    for( strLookName in names( lLooks ) )
    {
      nLook <- as.integer( strLookName )
      strFile <- file.path( strOutDir, paste0( lScenario$rowId, ".regression.l", nLook, ".rds" ) )
      saveRDS( lLooks[[ strLookName ]], file = strFile, compress = "xz" )
    }
  }, error = function( e ){
    cat( "  ERROR: ", conditionMessage( e ), "\n" )
  })

  return( invisible( TRUE ) )
}

#------ -------- -------- -------- -------- -------- -------- -------- --------
# GenerateCERRegressionFixtures
#------ -------- -------- -------- -------- -------- -------- -------- --------
GenerateCERRegressionFixtures <- function()
{
  strOutDir <- file.path( "tests", "testthat" )
  if( !dir.exists( strOutDir ) ) dir.create( strOutDir, recursive = TRUE )

  # CER Scenario 1: 3-arm, 1-endpoint Parametric design
  # Look 1: H1=0.1, H2=0.2; Look 2: H1=0.0001, H2=0.2
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

  # CER Scenario 2: 3-arm, 1-endpoint Parametric design with selection at look 2
  # Look 1: H1=0.1, H2=0.2; Look 2: select H2 only
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

  # CER Scenario 3: full documented scenario with stage-1 rejection, stage-2
  # sample-size increase, and look-2 confirmation on H2 only.
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

  # CER Scenario 7: GS_GMCP_Example.R EXAMPLE 4, 1st adaptGMCP_CER() call.
  # Look 1: H1=0.00045 (rejected), H2=0.0952, H3=0.0225, H4=0.1104.
  # Look 2: selection retains H2,H4 (drops H3); sample-size change for
  # Control/Treatment2; real strategy update to a 2-node fully-connected graph.
  lScenario7 <- list(
    rowId = "CER-GSExample4-01",
    nArms = 3,
    nEps = 2,
    sampleSize = 210,
    epType = list( "EP1" = "Continuous", "EP2" = "Continuous" ),
    sigma = list( "EP1" = c( 1, 1, 1 ), "EP2" = c( 1, 1, 1 ) ),
    CommonStdDev = FALSE,
    prop.ctr = list( "EP1" = NA, "EP2" = NA ),
    allocRatio = c( 1, 1, 1 ),
    WI = c( 0.5, 0.5, 0, 0 ),
    G = matrix( c( 0, 1/2, 1/2, 0, 1/2, 0, 0, 1/2, 0, 1, 0, 0, 1, 0, 0, 0 ), nrow = 4, byrow = TRUE ),
    testType = "Parametric",
    alpha = 0.025,
    infoFrac = c( 0.5, 1.0 ),
    typeOfDesign = "asOF",
    lookInputs = list(
      list( p_raw = c( H1 = 0.00045, H2 = 0.0952, H3 = 0.0225, H4 = 0.1104 ) ),
      list(
        p_raw = c( H2 = 0.0299, H4 = 0.0586 ),
        selection = c( "H2", "H4" ),
        stage2_cumulative_sample_size = c( Control = 88, Treatment2 = 87 ),
        strategy_update = list(
          new_weights = c( H2 = 0.5, H4 = 0.5 ),
          new_G = matrix( c( 0, 1, 1, 0 ), nrow = 2, byrow = TRUE )
        )
      )
    )
  )

  # CER Scenario 8: GS_GMCP_Example.R EXAMPLE 4, 2nd adaptGMCP_CER() call.
  # Same design and Look 1 as scenario 7. Look 2: all of H2,H3,H4 retained (no
  # drop), no sample-size change (no-op adaptation path), real strategy update
  # to a 3-node graph with unequal weights.
  lScenario8 <- list(
    rowId = "CER-GSExample4-02",
    nArms = 3,
    nEps = 2,
    sampleSize = 210,
    epType = list( "EP1" = "Continuous", "EP2" = "Continuous" ),
    sigma = list( "EP1" = c( 1, 1, 1 ), "EP2" = c( 1, 1, 1 ) ),
    CommonStdDev = FALSE,
    prop.ctr = list( "EP1" = NA, "EP2" = NA ),
    allocRatio = c( 1, 1, 1 ),
    WI = c( 0.5, 0.5, 0, 0 ),
    G = matrix( c( 0, 1/2, 1/2, 0, 1/2, 0, 0, 1/2, 0, 1, 0, 0, 1, 0, 0, 0 ), nrow = 4, byrow = TRUE ),
    testType = "Parametric",
    alpha = 0.025,
    infoFrac = c( 0.5, 1.0 ),
    typeOfDesign = "asOF",
    lookInputs = list(
      list( p_raw = c( H1 = 0.00045, H2 = 0.0952, H3 = 0.0225, H4 = 0.1104 ) ),
      list(
        p_raw = c( H2 = 0.0299, H3 = 0.0225, H4 = 0.0586 ),
        strategy_update = list(
          new_weights = c( H2 = 0.5, H3 = 0.25, H4 = 0.25 ),
          new_G = matrix( c( 0, 1/3, 2/3, 1, 0, 0, 1/2, 1/2, 0 ), nrow = 3, byrow = TRUE )
        )
      )
    )
  )

  # CER Scenario 9: GS_GMCP_Example.R EXAMPLE 2 (Cumulative MAMS). Stage 1
  # analysis only, per explicit scope decision (Stage 2 intentionally ignored
  # because Look 2 inputs are not documented anywhere in the source file).
  lScenario9 <- list(
    rowId = "CER-GSExample2-01",
    nArms = 3,
    nEps = 1,
    sampleSize = 210,
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
      list( p_raw = c( H1 = 0.0294, H2 = 0.0463 ) )
    )
  )

  # CER Scenario 10: AdaptGMCP_CER_Analysis_Example.R Example 2. Stage 1
  # analysis only, per explicit scope decision. Look 1 p-values are
  # reasonable placeholders (not documented in source) because the source
  # only says to follow interactive console prompts.
  lScenario10 <- list(
    rowId = "CER-AdaptExample2-01",
    nArms = 3,
    nEps = 2,
    sampleSize = 400,
    epType = list( "EP1" = "Continuous", "EP2" = "Continuous" ),
    sigma = list( "EP1" = c( 1, 1, 1 ), "EP2" = c( 1, 1, 1 ) ),
    CommonStdDev = FALSE,
    prop.ctr = list( "EP1" = NA, "EP2" = NA ),
    allocRatio = c( 1, 1, 1 ),
    WI = c( 0.5, 0.5, 0, 0 ),
    G = matrix( c( 0, 1/2, 1/2, 0, 1/2, 0, 0, 1/2, 0, 1, 0, 0, 1, 0, 0, 0 ), nrow = 4, byrow = TRUE ),
    testType = "Parametric",
    alpha = 0.025,
    infoFrac = c( 0.5, 1.0 ),
    typeOfDesign = "asOF",
    lookInputs = list(
      list( p_raw = c( H1 = 0.01, H2 = 0.05, H3 = 0.02, H4 = 0.08 ) )
    )
  )

  lScenarios <- list(
    lScenario1, lScenario2, lScenario3, lScenario4, lScenario5, lScenario6,
    lScenario7, lScenario8, lScenario9, lScenario10
  )

  for( lScenario in lScenarios )
  {
    SaveCERRegressionFixtures( lScenario, strOutDir )
  }

  cat( "\nCER regression fixture generation complete.\n" )
  return( invisible( TRUE ) )
}

# IMPORTANT: Uncomment and execute the following line only to regenerate the fixtures.
# Not otherwise as the fixtures are already checked into the repository and should not be changed unless necessary.
# GenerateCERRegressionFixtures()
