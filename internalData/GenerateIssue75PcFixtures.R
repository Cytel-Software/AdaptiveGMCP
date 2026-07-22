# --------------------------------------------------------------------------------------------------
#
# ©2026 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

# Script to generate issue-75 legacy-equivalence fixtures using adaptGMCP_PC() in
# non-interactive mode by mocking interactive hooks.

suppressPackageStartupMessages({
  library(devtools)
})

load_all(quiet = TRUE)

#------------------------------------------------------ -
# CaptureLegacyLooks
#------------------------------------------------------ -
CaptureLegacyLooks <- function( lScenario )
{
  eCaptured <- new.env( parent = emptyenv() )
  eCaptured$lLooks <- list()
  nPlannedLooks <- length( lScenario$lookInputs )

  bSelection <- any( sapply( lScenario$lookInputs, function( lLook ) !is.null( lLook$selection ) ) )
  bUpdateStrategy <- any( sapply( lScenario$lookInputs, function( lLook ) {
    !is.null( lLook$new_weights ) || !is.null( lLook$new_G )
  } ) )

  testthat::with_mocked_bindings(
    adaptGMCP_PC(
      WI = lScenario$WI,
      G = lScenario$G,
      test.type = lScenario$testType,
      alpha = lScenario$alpha,
      info_frac = lScenario$infoFrac,
      typeOfDesign = lScenario$typeOfDesign,
      deltaWT = lScenario$deltaWT,
      deltaPT1 = lScenario$deltaPT1,
      gammaA = lScenario$gammaA,
      userAlphaSpending = lScenario$userAlphaSpending,
      Correlation = lScenario$correlation,
      MultipleWinners = lScenario$multipleWinners,
      Selection = bSelection,
      UpdateStrategy = bUpdateStrategy,
      plotGraphs = FALSE
    ),
    getRawPValues = function( mcpObj )
    {
      return( lScenario$lookInputs[[ mcpObj$CurrentLook ]]$p_raw )
    },
    trialContinuationDecision = function( mcpObj )
    {
      if( StopTrial( mcpObj ) ) return( "n" )
      if( mcpObj$CurrentLook < nPlannedLooks ) return( "y" )
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
      nNextLook <- as.integer( mcpObj$CurrentLook + 1L )
      if( nNextLook > nPlannedLooks ) return( mcpObj )

      vNewWeights <- lScenario$lookInputs[[ nNextLook ]]$new_weights
      mNewG <- lScenario$lookInputs[[ nNextLook ]]$new_G

      if( is.null( vNewWeights ) && is.null( mNewG ) ) return( mcpObj )

      mcpObj$ModificationLook <- c( mcpObj$ModificationLook, as.integer( mcpObj$CurrentLook ) )
      return( applyStrategyUpdate( mcpObj, new_weights = vNewWeights, new_G = mNewG ) )
    },
    do_modifyCorrelation = function( mcpObj )
    {
      nNextLook <- as.integer( mcpObj$CurrentLook + 1L )
      if( nNextLook > nPlannedLooks ) return( mcpObj )

      mNewCorr <- lScenario$lookInputs[[ nNextLook ]]$new_correlation
      if( is.null( mNewCorr ) ) return( mcpObj )

      return( applyCorrelationUpdate( mcpObj, new_correlation = mNewCorr ) )
    },
    ShowResults = function( mcpObj )
    {
      eCaptured$lLooks[[ as.character( mcpObj$CurrentLook ) ]] <- mcpObj
      invisible( NULL )
    }
  )

  return( eCaptured$lLooks )
}

#------------------------------------------------------ -
# SaveScenarioFixtures
#------------------------------------------------------ -
SaveScenarioFixtures <- function( lScenario, strOutDir )
{
  cat( "Generating fixtures for ", lScenario$rowId, "...\n", sep = "" )
  lLooks <- CaptureLegacyLooks( lScenario )

  for( strLookName in names( lLooks ) )
  {
    nLook <- as.integer( strLookName )
    strFile <- file.path( strOutDir, paste0( lScenario$rowId, ".l", nLook, ".mcpObj.rds" ) )
    saveRDS( lLooks[[ strLookName ]], file = strFile, compress = "xz" )
  }

  return( invisible( lLooks ) )
}

#------------------------------------------------------ -
# Main
#------------------------------------------------------ -
GenerateIssue75Fixtures <- function()
{
  strOutDir <- file.path( "tests", "testthat" )
  if( !dir.exists( strOutDir ) ) dir.create( strOutDir, recursive = TRUE )

  mGraph4 <- matrix(
    c(
      0, 1 / 2, 1 / 2, 0,
      1 / 2, 0, 0, 1 / 2,
      0, 1, 0, 0,
      1, 0, 0, 0
    ),
    byrow = TRUE,
    nrow = 4
  )

  mCorrDefault <- matrix(
    c(
      1, 0.5, NA, NA,
      0.5, 1, NA, NA,
      NA, NA, 1, 0.5,
      NA, NA, 0.5, 1
    ),
    byrow = TRUE,
    nrow = 4
  )

  mCorrUpdated <- matrix(
    c(
      1, 0.3, NA, NA,
      0.3, 1, NA, NA,
      NA, NA, 1, 0.4,
      NA, NA, 0.4, 1
    ),
    byrow = TRUE,
    nrow = 4
  )

  lScenarios <- list(
    list(
      rowId = "M07",
      WI = c( 0.5, 0.5, 0, 0 ),
      G = mGraph4,
      testType = "Dunnett",
      alpha = 0.025,
      infoFrac = c( 0.5, 1.0 ),
      typeOfDesign = "asP",
      deltaWT = 0,
      deltaPT1 = 0,
      gammaA = 2,
      userAlphaSpending = NULL,
      correlation = mCorrDefault,
      multipleWinners = TRUE,
      lookInputs = list(
        list( p_raw = c( H1 = 0.30, H2 = 0.35, H3 = 0.40, H4 = 0.45 ) ),
        list( p_raw = c( H1 = 0.08, H2 = 0.09, H3 = 0.12, H4 = 0.15 ) )
      )
    ),
    list(
      rowId = "M11",
      WI = c( 0.5, 0.5, 0, 0 ),
      G = mGraph4,
      testType = "Dunnett",
      alpha = 0.025,
      infoFrac = c( 0.5, 1.0 ),
      typeOfDesign = "asHSD",
      deltaWT = 0,
      deltaPT1 = 0,
      gammaA = 2.5,
      userAlphaSpending = NULL,
      correlation = mCorrDefault,
      multipleWinners = TRUE,
      lookInputs = list(
        list( p_raw = c( H1 = 0.25, H2 = 0.30, H3 = 0.35, H4 = 0.40 ) ),
        list( p_raw = c( H1 = 0.07, H2 = 0.09, H3 = 0.10, H4 = 0.12 ) )
      )
    ),
    list(
      rowId = "M12",
      WI = c( 0.5, 0.5, 0, 0 ),
      G = mGraph4,
      testType = "Dunnett",
      alpha = 0.025,
      infoFrac = c( 0.5, 1.0 ),
      typeOfDesign = "asKD",
      deltaWT = 0,
      deltaPT1 = 0,
      gammaA = 2.5,
      userAlphaSpending = NULL,
      correlation = mCorrDefault,
      multipleWinners = TRUE,
      lookInputs = list(
        list( p_raw = c( H1 = 0.28, H2 = 0.31, H3 = 0.33, H4 = 0.37 ) ),
        list( p_raw = c( H1 = 0.06, H2 = 0.09, H3 = 0.11, H4 = 0.13 ) )
      )
    ),
    list(
      rowId = "M19",
      WI = c( 0.5, 0.5, 0, 0 ),
      G = mGraph4,
      testType = "Dunnett",
      alpha = 0.025,
      infoFrac = c( 0.5, 1.0 ),
      typeOfDesign = "asOF",
      deltaWT = 0,
      deltaPT1 = 0,
      gammaA = 2,
      userAlphaSpending = NULL,
      correlation = mCorrDefault,
      multipleWinners = TRUE,
      lookInputs = list(
        list( p_raw = c( H1 = 0.26, H2 = 0.29, H3 = 0.34, H4 = 0.38 ) ),
        list(
          p_raw = c( H1 = 0.07, H2 = 0.08, H3 = 0.11, H4 = 0.14 ),
          new_correlation = mCorrUpdated
        )
      )
    ),
    list(
      rowId = "M22",
      WI = c( 0.5, 0.5, 0, 0 ),
      G = mGraph4,
      testType = "Dunnett",
      alpha = 0.025,
      infoFrac = c( 0.5, 1.0 ),
      typeOfDesign = "asOF",
      deltaWT = 0,
      deltaPT1 = 0,
      gammaA = 2,
      userAlphaSpending = NULL,
      correlation = mCorrDefault,
      multipleWinners = FALSE,
      lookInputs = list(
        list( p_raw = c( H1 = 1e-4, H2 = 0.40, H3 = 0.45, H4 = 0.50 ) ),
        list( p_raw = c( H2 = 0.20, H3 = 0.30, H4 = 0.40 ) )
      )
    ),
    list(
      rowId = "M24",
      WI = c( 0.5, 0.5, 0, 0 ),
      G = mGraph4,
      testType = "Dunnett",
      alpha = 0.025,
      infoFrac = c( 0.5, 1.0 ),
      typeOfDesign = "asOF",
      deltaWT = 0,
      deltaPT1 = 0,
      gammaA = 2,
      userAlphaSpending = NULL,
      correlation = mCorrDefault,
      multipleWinners = FALSE,
      lookInputs = list(
        list( p_raw = c( H1 = 0.30, H2 = 0.35, H3 = 0.38, H4 = 0.42 ) ),
        list( p_raw = c( H1 = 0.02, H2 = 0.06, H3 = 0.10, H4 = 0.20 ) )
      )
    )
  )

  for( lScenario in lScenarios )
  {
    SaveScenarioFixtures( lScenario, strOutDir )
  }

  cat( "Fixture generation complete.\n" )
  return( invisible( TRUE ) )
}

GenerateIssue75Fixtures()
