############################################################################
# Issue #71 (Phase 4): State-transition and completion-semantics tests
#
# Uses the shared PC fixture/assertion helpers defined in tests/testthat/helper.R
# (Issue #68): AnalyzeLook_PC_TestWrapper(), pc_fixture_2h(), expect_pc_state_invariants().
############################################################################

testthat::test_that("completion_reason: final_look when all planned looks are analyzed without early stop", {
  # 3-look, 2-hypothesis fixed-effect design with p-values too weak to reject at any look.
  state <- pc_fixture_2h(planned_info_frac = c(0.5, 0.75, 1.0))

  state <- AnalyzeLook_PC_TestWrapper(state, p_raw = c(H1 = 0.40, H2 = 0.40), plotGraphs = FALSE)
  expect_pc_state_invariants(state, completed_looks = 1L, trial_completed = FALSE)
  testthat::expect_null(state$completion_reason)

  state <- AnalyzeLook_PC_TestWrapper(state, p_raw = c(H1 = 0.40, H2 = 0.40), plotGraphs = FALSE)
  expect_pc_state_invariants(state, completed_looks = 2L, trial_completed = FALSE)
  testthat::expect_null(state$completion_reason)

  state <- AnalyzeLook_PC_TestWrapper(state, p_raw = c(H1 = 0.40, H2 = 0.40), plotGraphs = FALSE)
  expect_pc_state_invariants(state, completed_looks = 3L, trial_completed = TRUE)
  testthat::expect_identical(state$completion_reason, "final_look")
})

testthat::test_that("completion_reason: early_stop_efficacy when a hypothesis is rejected before the final look", {
  # 2-look design, MultipleWinners = FALSE: stopping as soon as any hypothesis is rejected.
  state <- pc_fixture_2h(planned_info_frac = c(0.5, 1.0), MultipleWinners = FALSE)

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.0001, H2 = 0.40),
    plotGraphs = FALSE
  )

  expect_pc_state_invariants(state, completed_looks = 1L, trial_completed = TRUE)
  testthat::expect_identical(state$completion_reason, "early_stop_efficacy")
  # The trial concluded before the last planned look.
  testthat::expect_lt(state$completed_looks, state$mcpObj$LastLook)
})

testthat::test_that("completion_reason: all_hypotheses_dropped short-circuits and does not advance completed_looks", {
  # AnalyzeLook_PC() short-circuits with completion_reason = "all_hypotheses_dropped"
  # whenever it observes an empty active IndexSet on entry (see PcAnalysisApi.R). Since
  # applySelection() always requires retaining at least one hypothesis, this path is
  # exercised here the same way Test 4 ("look argument validation") simulates an
  # already-concluded trial: by directly constructing that state on the object.
  state <- pc_fixture_2h(planned_info_frac = c(0.5, 1.0))
  state <- AnalyzeLook_PC_TestWrapper(state, p_raw = c(H1 = 0.40, H2 = 0.40), plotGraphs = FALSE)
  testthat::expect_false(state$trial_completed)

  state_dropped <- state
  state_dropped$mcpObj$IndexSet <- character(0)

  state_dropped <- AnalyzeLook_PC_TestWrapper(
    state_dropped,
    p_raw = c(H1 = 0.40, H2 = 0.40),
    plotGraphs = FALSE
  )

  testthat::expect_s3_class(state_dropped, "PCAnalysisState")
  testthat::expect_true(state_dropped$trial_completed)
  testthat::expect_identical(state_dropped$completion_reason, "all_hypotheses_dropped")
  testthat::expect_length(state_dropped$mcpObj$IndexSet, 0)
  # The short-circuit returns before completed_looks/look_history are advanced.
  testthat::expect_equal(state_dropped$completed_looks, state$completed_looks)
  testthat::expect_length(state_dropped$look_history, length(state$look_history))
})

testthat::test_that("look_history: a snapshot is stored for every completed look with matching call-time inputs", {
  state <- pc_fixture_2h(planned_info_frac = c(0.5, 0.75, 1.0))

  p_raw_l1 <- c(H1 = 0.30, H2 = 0.35)
  p_raw_l2 <- c(H1 = 0.28, H2 = 0.32)
  p_raw_l3 <- c(H1 = 0.20, H2 = 0.45)

  state <- AnalyzeLook_PC_TestWrapper(state, p_raw = p_raw_l1, plotGraphs = FALSE)
  state <- AnalyzeLook_PC_TestWrapper(state, p_raw = p_raw_l2, plotGraphs = FALSE)
  state <- AnalyzeLook_PC_TestWrapper(state, p_raw = p_raw_l3, plotGraphs = FALSE)

  testthat::expect_true(state$trial_completed)
  testthat::expect_length(state$look_history, 3L)

  for (k in seq_len(3L)) {
    snap <- state$look_history[[k]]
    testthat::expect_false(is.null(snap))
    testthat::expect_true(is.list(snap$mcpObj))
    testthat::expect_equal(snap$mcpObj$CurrentLook, k)
    testthat::expect_identical(snap$is_final_look, k == 3L)
  }

  # Stored inputs match the exact p_raw supplied at call time for each look.
  testthat::expect_equal(state$look_history[[1]]$inputs$p_raw, p_raw_l1)
  testthat::expect_equal(state$look_history[[2]]$inputs$p_raw, p_raw_l2)
  testthat::expect_equal(state$look_history[[3]]$inputs$p_raw, p_raw_l3)
})

testthat::test_that("look_history: selection/strategy/correlation inputs are recorded verbatim for the look applied", {
  state <- pc_fixture_4h(planned_info_frac = c(0.5, 1.0))
  corr <- pc_fixture_4h_correlation()

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.01, H2 = 0.20, H3 = 0.15, H4 = 0.30),
    Correlation = corr,
    plotGraphs = FALSE
  )

  selection <- c("H1", "H2", "H4")
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.02, H2 = 0.10, H4 = 0.40),
    selection = selection,
    plotGraphs = FALSE
  )

  testthat::expect_equal(state$look_history[[2]]$inputs$selection, selection)
  testthat::expect_null(state$look_history[[2]]$inputs$new_weights)
  testthat::expect_null(state$look_history[[2]]$inputs$new_G)
  testthat::expect_equal(state$look_history[[1]]$inputs$Correlation, corr)
})

testthat::test_that("post-completion behavior: 'Trial already concluded' error is stable across every completion pathway", {
  # Pathway 1: final_look
  state_final <- pc_fixture_2h(planned_info_frac = c(0.5, 1.0))
  state_final <- AnalyzeLook_PC_TestWrapper(state_final, p_raw = c(H1 = 0.40, H2 = 0.40), plotGraphs = FALSE)
  state_final <- AnalyzeLook_PC_TestWrapper(state_final, p_raw = c(H1 = 0.40, H2 = 0.40), plotGraphs = FALSE)
  testthat::expect_identical(state_final$completion_reason, "final_look")
  expect_pc_look_error(state_final, "Trial already concluded", p_raw = c(H1 = 0.40, H2 = 0.40))

  # Pathway 2: early_stop_efficacy
  state_early <- pc_fixture_2h(planned_info_frac = c(0.5, 1.0), MultipleWinners = FALSE)
  state_early <- AnalyzeLook_PC_TestWrapper(state_early, p_raw = c(H1 = 0.0001, H2 = 0.40), plotGraphs = FALSE)
  testthat::expect_identical(state_early$completion_reason, "early_stop_efficacy")
  expect_pc_look_error(state_early, "Trial already concluded", p_raw = c(H1 = 0.40, H2 = 0.40))

  # Pathway 3: all_hypotheses_dropped (simulated empty IndexSet, as above)
  state_dropped <- pc_fixture_2h(planned_info_frac = c(0.5, 1.0))
  state_dropped <- AnalyzeLook_PC_TestWrapper(state_dropped, p_raw = c(H1 = 0.40, H2 = 0.40), plotGraphs = FALSE)
  state_dropped$mcpObj$IndexSet <- character(0)
  state_dropped <- AnalyzeLook_PC_TestWrapper(state_dropped, p_raw = c(H1 = 0.40, H2 = 0.40), plotGraphs = FALSE)
  testthat::expect_identical(state_dropped$completion_reason, "all_hypotheses_dropped")
  expect_pc_look_error(state_dropped, "Trial already concluded", p_raw = c(H1 = 0.40, H2 = 0.40))
})
