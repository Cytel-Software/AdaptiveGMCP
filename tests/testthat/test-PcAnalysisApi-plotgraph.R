############################################################################
# Issue #73 (Phase 6): PlotAnalysisGraph() edge-case and stage-validation tests
#
# Uses the shared PC fixture/assertion helpers defined in tests/testthat/helper.R
# (Issue #68). PlotAnalysisGraph() returns a non-NULL htmlwidget; these tests stay
# non-interactive (no plotGraphs side effects) and assert on the widget's presence
# plus the documented stage-selection and validation contract.
############################################################################

testthat::test_that("PlotAnalysisGraph: stage = NULL with completed_looks = 0 returns the initial graph", {
  state <- pc_fixture_2h(planned_info_frac = c(0.5, 1.0))
  testthat::expect_equal(state$completed_looks, 0L)

  widget <- PlotAnalysisGraph(state)
  testthat::expect_true(!is.null(widget))

  widget_explicit_initial <- PlotAnalysisGraph(state, stage = "initial")
  testthat::expect_true(!is.null(widget_explicit_initial))
})

testthat::test_that("PlotAnalysisGraph: stage = NULL after looks are analyzed returns the latest graph", {
  state <- pc_fixture_2h(planned_info_frac = c(0.5, 1.0))
  state <- AnalyzeLook_PC_TestWrapper(state, p_raw = c(H1 = 0.30, H2 = 0.35), plotGraphs = FALSE)

  widget <- PlotAnalysisGraph(state)
  testthat::expect_true(!is.null(widget))
})

testthat::test_that("PlotAnalysisGraph: stage = 0 and stage = 'initial' both return the initial graph", {
  state <- pc_fixture_2h(planned_info_frac = c(0.5, 1.0))
  state <- AnalyzeLook_PC_TestWrapper(state, p_raw = c(H1 = 0.30, H2 = 0.35), plotGraphs = FALSE)

  widget_zero_int <- PlotAnalysisGraph(state, stage = 0L)
  widget_zero_dbl <- PlotAnalysisGraph(state, stage = 0)
  widget_initial <- PlotAnalysisGraph(state, stage = "initial")

  testthat::expect_true(!is.null(widget_zero_int))
  testthat::expect_true(!is.null(widget_zero_dbl))
  testthat::expect_true(!is.null(widget_initial))
})

testthat::test_that("PlotAnalysisGraph: positive integer stage returns the snapshot for that look", {
  state <- pc_fixture_2h(planned_info_frac = c(0.5, 0.75, 1.0))
  state <- AnalyzeLook_PC_TestWrapper(state, p_raw = c(H1 = 0.30, H2 = 0.35), plotGraphs = FALSE)
  state <- AnalyzeLook_PC_TestWrapper(state, p_raw = c(H1 = 0.28, H2 = 0.32), plotGraphs = FALSE)

  testthat::expect_true(!is.null(PlotAnalysisGraph(state, stage = 1)))
  testthat::expect_true(!is.null(PlotAnalysisGraph(state, stage = 2)))
})

testthat::test_that("PlotAnalysisGraph: non-PCAnalysisState input is rejected", {
  testthat::expect_error(PlotAnalysisGraph(list()), regexp = "PCAnalysisState")
  testthat::expect_error(PlotAnalysisGraph("not_a_state"), regexp = "PCAnalysisState")
})

testthat::test_that("PlotAnalysisGraph: stage validation rejects a non-integer numeric value", {
  state <- pc_fixture_2h(planned_info_frac = c(0.5, 1.0))
  state <- AnalyzeLook_PC_TestWrapper(state, p_raw = c(H1 = 0.30, H2 = 0.35), plotGraphs = FALSE)

  testthat::expect_error(
    PlotAnalysisGraph(state, stage = 1.5),
    regexp = "whole number"
  )
})

testthat::test_that("PlotAnalysisGraph: stage validation rejects an out-of-range stage", {
  state <- pc_fixture_2h(planned_info_frac = c(0.5, 1.0))
  state <- AnalyzeLook_PC_TestWrapper(state, p_raw = c(H1 = 0.30, H2 = 0.35), plotGraphs = FALSE)
  # Only look 1 has been analyzed; completed_looks == 1.

  testthat::expect_error(
    PlotAnalysisGraph(state, stage = 2),
    regexp = "between 1 and completed_looks"
  )

  testthat::expect_error(
    PlotAnalysisGraph(state, stage = -1),
    regexp = "between 1 and completed_looks"
  )

  # stage = 0 is a documented special case (initial graph), not an out-of-range error,
  # even when no looks have been analyzed yet.
  state_fresh <- pc_fixture_2h(planned_info_frac = c(0.5, 1.0))
  testthat::expect_no_error(PlotAnalysisGraph(state_fresh, stage = 0))
})

testthat::test_that("PlotAnalysisGraph: stage validation rejects an unsupported type", {
  state <- pc_fixture_2h(planned_info_frac = c(0.5, 1.0))
  state <- AnalyzeLook_PC_TestWrapper(state, p_raw = c(H1 = 0.30, H2 = 0.35), plotGraphs = FALSE)

  testthat::expect_error(
    PlotAnalysisGraph(state, stage = TRUE),
    regexp = "Unsupported stage value"
  )

  testthat::expect_error(
    PlotAnalysisGraph(state, stage = list(1)),
    regexp = "Unsupported stage value"
  )

  testthat::expect_error(
    PlotAnalysisGraph(state, stage = c(1, 2)),
    regexp = "Unsupported stage value"
  )
})
