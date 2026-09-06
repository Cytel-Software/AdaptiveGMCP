############################################################################
# Issue #72 (Phase 5): Design and mvtnorm algorithm branch coverage
#
# Uses the shared PC fixture/assertion helpers defined in tests/testthat/helper.R
# (Issue #68). Assertions are contract-level (state creation, boundary table
# dimensions, mvtnorm algorithm class) rather than deep numerical checks, which
# are already covered by the legacy-equivalence fixtures added for Issue #75.
############################################################################

testthat::test_that("typeOfDesign = 'WT' produces a valid boundary table", {
  state <- pc_fixture_2h(
    planned_info_frac = c(0.5, 1.0),
    typeOfDesign = "WT",
    deltaWT = 0.25
  )

  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(nrow(state$mcpObj$bdryTab), 2L)
  testthat::expect_length(state$thresholds, 2L)
  testthat::expect_true(all(is.finite(state$mcpObj$bdryTab$ZScale_Eff_Bbry)))
})

testthat::test_that("typeOfDesign = 'PT' currently requires deltaPT0, which SetupAnalysis_PC does not expose", {
  # Tracked as M10 (N/A - deferred) in Issue #75's equivalence matrix: rpact::getDesignGroupSequential()
  # requires 'deltaPT0' for typeOfDesign = "PT", but SetupAnalysis_PC()/.pc_get_design_group_sequential()
  # only forwards deltaPT1. This test pins the current explicit-error contract so any future exposure
  # of deltaPT0 is a deliberate, visible change rather than a silent behavior shift.
  testthat::expect_error(
    pc_fixture_2h(
      planned_info_frac = c(0.5, 1.0),
      typeOfDesign = "PT",
      deltaPT1 = 0.5
    ),
    regexp = "deltaPT0"
  )
})

testthat::test_that("typeOfDesign = 'asHSD' produces a valid boundary table", {
  state <- pc_fixture_2h(
    planned_info_frac = c(0.5, 1.0),
    typeOfDesign = "asHSD",
    gammaA = 2.5
  )

  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(nrow(state$mcpObj$bdryTab), 2L)
  testthat::expect_true(all(is.finite(state$mcpObj$bdryTab$ZScale_Eff_Bbry)))
})

testthat::test_that("typeOfDesign = 'asKD' produces a valid boundary table", {
  state <- pc_fixture_2h(
    planned_info_frac = c(0.5, 1.0),
    typeOfDesign = "asKD",
    gammaA = 2.5
  )

  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(nrow(state$mcpObj$bdryTab), 2L)
  testthat::expect_true(all(is.finite(state$mcpObj$bdryTab$ZScale_Eff_Bbry)))
})

testthat::test_that("typeOfDesign = 'asUser' produces a valid boundary table with user-specified spending", {
  state <- pc_fixture_2h(
    planned_info_frac = c(0.5, 1.0),
    typeOfDesign = "asUser",
    userAlphaSpending = c(0.010, 0.025)
  )

  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(nrow(state$mcpObj$bdryTab), 2L)
  testthat::expect_equal(state$mcpObj$bdryTab$Incr_alpha_spent[1], 0.010, tolerance = 1e-6)
  testthat::expect_equal(sum(state$mcpObj$bdryTab$Incr_alpha_spent), 0.025, tolerance = 1e-6)
})

############################################################################
# chooseMVTAlgo() selection: d <= 20 vs. d > 20
#
# chooseMVTAlgo() is unit-tested directly at the dimension boundary because
# routing a d > 20 hypothesis set through the full SetupAnalysis_PC() /
# genWeights() pipeline is combinatorial (2^d intersection hypotheses) and is
# not runtime-acceptable in a test suite (d = 21 alone did not complete within
# several minutes). The d <= 20 path is additionally verified through the
# public API using the existing 4-hypothesis fixture, which is fast.
############################################################################

testthat::test_that("chooseMVTAlgo: d <= 20 selects Miwa", {
  testthat::expect_s3_class(AdaptGMCP:::chooseMVTAlgo(1), "Miwa")
  testthat::expect_s3_class(AdaptGMCP:::chooseMVTAlgo(20), "Miwa")
})

testthat::test_that("chooseMVTAlgo: d > 20 selects GenzBretz", {
  testthat::expect_s3_class(AdaptGMCP:::chooseMVTAlgo(21), "GenzBretz")
  testthat::expect_s3_class(AdaptGMCP:::chooseMVTAlgo(100), "GenzBretz")
})

testthat::test_that("SetupAnalysis_PC wires chooseMVTAlgo's d <= 20 selection into the analysis state", {
  state <- pc_fixture_4h()
  testthat::expect_s3_class(state$mvtnorm_algo, "Miwa")
})
