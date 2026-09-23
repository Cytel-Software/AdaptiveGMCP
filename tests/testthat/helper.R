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

# --------------------------------------------------------------------------------------------------
# Issue #68 (Phase 0): Reusable PC API test fixtures and assertion helpers.
#
# These helpers are shared across tests/testthat/test-PcAnalysisApi*.R files so common
# 2-hypothesis / 4-hypothesis setups and repeated assertion patterns do not need to be
# duplicated in every test_that() block. Adding tests here does not change behavior of
# any existing passing test.
# --------------------------------------------------------------------------------------------------

# Calls AnalyzeLook_PC() via do.call() so validation/error tests can pass malformed
# arguments (e.g. non-numeric `look`) without tripping R's own argument matching first.
AnalyzeLook_PC_TestWrapper <- function( state, ... )
{
  lArgs <- list( ... )
  lArgs$state <- state
  return( do.call( AnalyzeLook_PC, lArgs ) )
}

# Standard 2-hypothesis parallel graph (H1, H2 fully independent, equal weights) used
# across many PC API tests that only need a minimal, fast-to-analyze state.
pc_fixture_2h <- function(test.type = "Bonf",
                           alpha = 0.025,
                           planned_info_frac = c(0.5, 1.0),
                           ...) {
  SetupAnalysis_PC(
    WI = c(0.5, 0.5),
    G = matrix(c(0, 1, 1, 0), byrow = TRUE, nrow = 2),
    test.type = test.type,
    alpha = alpha,
    planned_info_frac = planned_info_frac,
    plotGraphs = FALSE,
    ...
  )
}

# Standard 2x2 correlation matrix for the pc_fixture_2h() graph (Dunnett/Partly-Parametric).
pc_fixture_2h_correlation <- function(rho = 0.5) {
  matrix(c(1, rho, rho, 1), byrow = TRUE, nrow = 2)
}

# Standard 4-hypothesis hierarchical gatekeeping graph (H1/H2 primary, H3/H4 secondary)
# matching the setup reused by Test 1/2/3 in test-PcAnalysisApi.R.
pc_fixture_4h <- function(test.type = "Partly-Parametric",
                           alpha = 0.025,
                           planned_info_frac = c(0.5, 0.7, 1),
                           typeOfDesign = "asOF",
                           ...) {
  SetupAnalysis_PC(
    WI = c(0.5, 0.5, 0, 0),
    G = matrix(c(
      0, 0.5, 0.5, 0,
      0.5, 0, 0, 0.5,
      0, 1, 0, 0,
      1, 0, 0, 0
    ), byrow = TRUE, nrow = 4),
    test.type = test.type,
    alpha = alpha,
    planned_info_frac = planned_info_frac,
    typeOfDesign = typeOfDesign,
    plotGraphs = FALSE,
    ...
  )
}

# Standard 4x4 correlation matrix for the pc_fixture_4h() graph (Dunnett/Partly-Parametric),
# with NA entries for hypothesis pairs that are never jointly tested under any intersection.
pc_fixture_4h_correlation <- function() {
  matrix(c(
    1, 0.5, 0.5, NA,
    0.5, 1, NA, 0.5,
    0.5, NA, 1, 0.5,
    NA, 0.5, 0.5, 1
  ), byrow = TRUE, nrow = 4)
}

# Asserts that SetupAnalysis_PC(), called with the supplied arguments, raises an error
# whose message matches `regexp`. Compact wrapper to avoid repeating the
# `plotGraphs = FALSE` boilerplate in setup-validation tests.
expect_pc_setup_error <- function(regexp, ...) {
  testthat::expect_error(SetupAnalysis_PC(..., plotGraphs = FALSE), regexp = regexp)
}

# Asserts that advancing `state` by one look with the supplied arguments raises an error
# whose message matches `regexp`. Compact wrapper around AnalyzeLook_PC_TestWrapper().
expect_pc_look_error <- function(state, regexp, ...) {
  testthat::expect_error(AnalyzeLook_PC_TestWrapper(state, ..., plotGraphs = FALSE), regexp = regexp)
}

# Asserts the common structural invariants that should hold for any PCAnalysisState:
# correct S3 class, completed_looks count, and trial_completed flag. When the trial has
# not concluded via "all_hypotheses_dropped", IndexSet is also asserted non-empty.
expect_pc_state_invariants <- function(state, completed_looks, trial_completed = FALSE) {
  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(state$completed_looks, completed_looks)
  testthat::expect_equal(state$trial_completed, trial_completed)
  if (!identical(state$completion_reason, "all_hypotheses_dropped")) {
    testthat::expect_true(length(state$mcpObj$IndexSet) > 0)
  }
}
