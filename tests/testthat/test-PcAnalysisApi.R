# AnalyzeLook_PC_TestWrapper() and shared PC fixture/assertion helpers now live in
# tests/testthat/helper.R (Issue #68) so they can be reused across all
# test-PcAnalysisApi*.R files.

############
# Test1 # COMPLETED
testthat::test_that("Test 1: PC analysis API scaffolds", {
  wi <- c(1 / 2, 1 / 2, 0, 0)
  g <- matrix(c(
    0, 1 / 2, 1 / 2, 0,
    1 / 2, 0, 0, 1 / 2,
    0, 1, 0, 0,
    1, 0, 0, 0
  ), byrow = TRUE, nrow = 4)

  t <- c(0.5, 0.7, 1)
  alp <- 0.025

  corr <- matrix(c(
    1, 0.5, 0.5, NA,
    0.5, 1, NA, 0.5,
    0.5, NA, 1, 0.5,
    NA, 0.5, 0.5, 1
  ), byrow = TRUE, nrow = 4)

  tt <- "Partly-Parametric"
  des <- "asOF"

  state <- SetupAnalysisPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = tt,
    dAlpha = alp,
    vInfoFracts = t,
    strDesignType = des,
    bPlotGraphs = FALSE
  )

  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(state$completed_looks, 0L)

  # Look 1
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.01, H2 = 0.20, H3 = 0.15, H4 = 0.30),
    mCorrelation = corr,
    bPlotGraphs = FALSE
  )

  testthat::expect_equal(state$completed_looks, 1L)

  exp_out <- readRDS(testthat::test_path("t1.l1.AdjPValues.rds"))
  expected_cols_look1 <- colnames(exp_out)
  testthat::expect_true(all(expected_cols_look1 %in% colnames(state$mcpObj$AdjPValues)))
  testthat::expect_equal(state$mcpObj$AdjPValues, exp_out)

  exp_out <- readRDS(testthat::test_path("t1.l1.RejFlagCurr.rds"))
  testthat::expect_equal(state$mcpObj$rej_flag_Curr, exp_out)

  # Look 2 with selection
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.02, H2 = 0.10, H4 = 0.40),
    vSelectHypo = c("H1", "H2", "H4"),
    bPlotGraphs = FALSE
  )

  testthat::expect_equal(state$completed_looks, 2L)

  exp_out <- readRDS(testthat::test_path("t1.l2.AdjPValues.rds"))
  expected_cols_look2 <- colnames(exp_out)
  testthat::expect_true(all(expected_cols_look2 %in% colnames(state$mcpObj$AdjPValues)))
  testthat::expect_equal(state$mcpObj$AdjPValues, exp_out)

  exp_out <- readRDS(testthat::test_path("t1.l2.IndexSet.rds"))
  testthat::expect_equal(state$mcpObj$IndexSet, exp_out)

  # Look 3
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H2 = 0.05, H4 = 0.10),
    bPlotGraphs = FALSE
  )

  testthat::expect_equal(state$completed_looks, 3L)

  exp_out <- readRDS(testthat::test_path("t1.l3.AdjPValues.rds"))
  expected_cols_look3 <- colnames(exp_out)
  testthat::expect_true(all(expected_cols_look3 %in% colnames(state$mcpObj$AdjPValues)))
  testthat::expect_equal(state$mcpObj$AdjPValues, exp_out)

  testthat::expect_true(state$trial_completed)

  # Plot helper should run
  g <- PlotAnalysisGraph(state, stage = 1)
  testthat::expect_true(!is.null(g))

  # print() should not error
  testthat::expect_no_error(print(state))
})

testthat::test_that("print.PCAnalysisState reports setup and current-look results", {
  state <- pc_fixture_2h(planned_info_frac = c(0.5, 1.0))

  setup_output <- capture.output(print(state))
  testthat::expect_true(any(grepl("Design Boundary:", setup_output, fixed = TRUE)))
  testthat::expect_true(any(grepl("Inverse Normal Weights:", setup_output, fixed = TRUE)))
  testthat::expect_true(any(grepl("No look-wise analysis has been performed yet.", setup_output, fixed = TRUE)))
  testthat::expect_false(any(grepl("Adj P-values", setup_output, fixed = TRUE)))

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.10, H2 = 0.20),
    bPlotGraphs = FALSE
  )
  look_one_output <- capture.output(print(state))
  testthat::expect_true(any(grepl("Adj P-values for the intersection hypotheses at Look: 1", look_one_output, fixed = TRUE)))
  testthat::expect_true(any(grepl("Final rejection status of primary hypotheses at Look: 1", look_one_output, fixed = TRUE)))
  testthat::expect_false(any(grepl("Combined P-values", look_one_output, fixed = TRUE)))
  testthat::expect_false(any(grepl("Analysis results for Look", look_one_output, fixed = TRUE)))
  testthat::expect_false(any(grepl("Design Boundary:", look_one_output, fixed = TRUE)))

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.08, H2 = 0.12),
    bPlotGraphs = FALSE
  )
  look_two_output <- capture.output(print(state))
  testthat::expect_true(any(grepl("Combined P-values for the intersection hypotheses at Look: 2", look_two_output, fixed = TRUE)))
  testthat::expect_true(any(grepl("Final rejection status of primary hypotheses at Look: 2", look_two_output, fixed = TRUE)))
})

############
# Test2 # COMPLETED
testthat::test_that("Test 2: PC analysis API scaffolds (strategy modification)", {
  wi <- c(1 / 2, 1 / 2, 0, 0)
  g <- matrix(c(
    0, 1 / 2, 1 / 2, 0,
    1 / 2, 0, 0, 1 / 2,
    0, 1, 0, 0,
    1, 0, 0, 0
  ), byrow = TRUE, nrow = 4)

  t <- c(0.5, 0.7, 1)
  alp <- 0.025

  corr <- matrix(c(
    1, 0.5, 0.5, NA,
    0.5, 1, NA, 0.5,
    0.5, NA, 1, 0.5,
    NA, 0.5, 0.5, 1
  ), byrow = TRUE, nrow = 4)

  tt <- "Partly-Parametric"
  des <- "asOF"

  state <- SetupAnalysisPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = tt,
    dAlpha = alp,
    vInfoFracts = t,
    strDesignType = des,
    bPlotGraphs = FALSE,
    bMultipleWinners = FALSE
  )

  testthat::expect_identical(
    state$mcpObj$HypoMap,
    data.frame(Hypothesis = paste0("H", 1:4))
  )

  # Look 1
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.03, H2 = 0.20, H3 = 0.10, H4 = 0.25),
    mCorrelation = corr,
    bPlotGraphs = FALSE
  )

  exp_out <- readRDS(testthat::test_path("t2.l1.AdjPValues.rds"))
  testthat::expect_equal(state$mcpObj$AdjPValues, exp_out)

  # Apply a strategy modification before analyzing look 2
  new_weights <- c(H1 = 0.5, H2 = 0.25, H3 = 0.25, H4 = 0)
  new_G <- matrix(c(
    0, 0.5, 0.5, 0,
    0.5, 0, 0.5, 0,
    0.5, 0.5, 0, 0,
    1, 0, 0, 0
  ), byrow = TRUE, nrow = 4)

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.02, H2 = 0.12, H3 = 0.08, H4 = 0.20),
    vWeights = new_weights,
    mGMatrix = new_G,
    bPlotGraphs = FALSE
  )

  testthat::expect_equal(state$completed_looks, 2L)

  exp_out <- readRDS(testthat::test_path("t2.l2.AdjPValues.rds"))
  testthat::expect_equal(state$mcpObj$AdjPValues, exp_out)

  exp_out <- readRDS(testthat::test_path("t2.l2.IndexSet.rds"))
  testthat::expect_equal(state$mcpObj$IndexSet, exp_out)

  # Plot helper should run
  g <- PlotAnalysisGraph(state, stage = 2)
  testthat::expect_true(!is.null(g))
})

############
# Test3 # COMPLETED
testthat::test_that("Test 3: PC analysis API scaffolds (full transition at look 2)", {
  wi <- c(1 / 2, 1 / 2, 0, 0)
  g <- matrix(c(
    0, 1 / 2, 1 / 2, 0,
    1 / 2, 0, 0, 1 / 2,
    0, 1, 0, 0,
    1, 0, 0, 0
  ), byrow = TRUE, nrow = 4)

  t <- c(0.5, 0.7, 1)
  alp <- 0.025

  corr <- matrix(c(
    1, 0.5, 0.5, NA,
    0.5, 1, NA, 0.5,
    0.5, NA, 1, 0.5,
    NA, 0.5, 0.5, 1
  ), byrow = TRUE, nrow = 4)

  tt <- "Partly-Parametric"
  des <- "asOF"

  state <- SetupAnalysisPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = tt,
    dAlpha = alp,
    vInfoFracts = t,
    strDesignType = des,
    bPlotGraphs = FALSE,
    bMultipleWinners = TRUE
  )

  exp_out <- readRDS(testthat::test_path("t3.bdryTab.rds"))
  testthat::expect_equal(state$mcpObj$bdryTab, exp_out)

  # Look 1
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.04, H2 = 0.18, H3 = 0.12, H4 = 0.22),
    mCorrelation = corr,
    bPlotGraphs = FALSE
  )

  exp_out <- readRDS(testthat::test_path("t3.l1.AdjPValues.rds"))
  testthat::expect_equal(state$mcpObj$AdjPValues, exp_out)

  # Full transition at look 2:
  # 1) selection (drop H4)
  selection <- c("H1", "H2", "H3")

  # 2) strategy modification for continuing hypotheses
  new_weights <- c(H1 = 0.5, H2 = 0.5, H3 = 0)
  new_G <- matrix(c(
    0, 1, 0,
    0.5, 0, 0.5,
    1, 0, 0
  ), byrow = TRUE, nrow = 3)

  # 3) correlation update for the active hypotheses H1, H2, H3 only
  Correlation <- corr[1:3, 1:3]
  Correlation[1, 2] <- Correlation[2, 1] <- 0.3
  Correlation[1, 3] <- Correlation[3, 1] <- 0.4

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.03, H2 = 0.10, H3 = 0.08),
    vSelectHypo = selection,
    vWeights = new_weights,
    mGMatrix = new_G,
    mCorrelation = Correlation,
    bPlotGraphs = FALSE
  )

  testthat::expect_equal(state$completed_looks, 2L)

  exp_out <- readRDS(testthat::test_path("t3.l2.AdjPValues.rds"))
  testthat::expect_equal(state$mcpObj$AdjPValues, exp_out)

  g <- PlotAnalysisGraph(state, stage = 2)
  testthat::expect_true(!is.null(g))
})

############
# Test 4: look argument validation # COMPLETED
testthat::test_that("AnalyzeLookPC: look argument validation and error handling", {
  # Minimal 2-hypothesis, 2-look setup (no option guard needed — tests error conditions and
  # completed_looks only, not specific computed output values)
  wi <- c(0.5, 0.5)
  g <- matrix(c(0, 1, 1, 0), byrow = TRUE, nrow = 2)
  corr <- matrix(c(1, 0.5, 0.5, 1), byrow = TRUE, nrow = 2)

  state0 <- SetupAnalysisPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = "Bonf",
    dAlpha = 0.025,
    vInfoFracts = c(0.5, 1.0),
    bPlotGraphs = FALSE
  )

  # --- Pure validation errors (fired before any computation) ---

  # look mismatch: state expects look 1, user passes look = 2
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(state0, vRawPVals = c(H1 = 0.10, H2 = 0.20), nLook = 2L, bPlotGraphs = FALSE),
    regexp = "does not match the expected next look"
  )

  # look is non-numeric
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(state0, vRawPVals = c(H1 = 0.10, H2 = 0.20), nLook = "a", bPlotGraphs = FALSE),
    regexp = "single positive integer"
  )

  # look is non-integer numeric (1.5)
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(state0, vRawPVals = c(H1 = 0.10, H2 = 0.20), nLook = 1.5, bPlotGraphs = FALSE),
    regexp = "single positive integer"
  )

  # look = 0 (not positive)
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(state0, vRawPVals = c(H1 = 0.10, H2 = 0.20), nLook = 0L, bPlotGraphs = FALSE),
    regexp = "single positive integer"
  )

  # --- Correct usage ---

  # look = NULL (default): proceeds normally and completed_looks advances
  state1_null <- AnalyzeLook_PC_TestWrapper(state0, vRawPVals = c(H1 = 0.10, H2 = 0.20), nLook = NULL, bPlotGraphs = FALSE)
  testthat::expect_equal(state1_null$completed_looks, 1L)

  # look = 1L (correct explicit value): same result as omitting look
  state1_expl <- AnalyzeLook_PC_TestWrapper(state0, vRawPVals = c(H1 = 0.10, H2 = 0.20), nLook = 1L, bPlotGraphs = FALSE)
  testthat::expect_equal(state1_expl$completed_looks, 1L)

  testthat::expect_equal(state1_null, state1_expl)

  # --- After final look: completed-trial error ---

  # Run look 2 to complete the trial
  state2 <- AnalyzeLook_PC_TestWrapper(state1_null, vRawPVals = c(H1 = 0.10, H2 = 0.20), nLook = 2L, bPlotGraphs = FALSE)
  testthat::expect_true(state2$trial_completed)

  # Calling again after final look must report that the trial has concluded.
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(state2, vRawPVals = c(H1 = 0.10, H2 = 0.20), bPlotGraphs = FALSE),
    regexp = "Trial already concluded"
  )

  # --- Early stopping: trial already concluded before final look ---

  # Simulate early-stopped state (trial_completed = TRUE before the final look is reached)
  state_early_stopped <- state1_null
  state_early_stopped$trial_completed <- TRUE   # completed_looks (1) < LastLook (2)

  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(state_early_stopped, vRawPVals = c(H1 = 0.10, H2 = 0.20), bPlotGraphs = FALSE),
    regexp = "Trial already concluded"
  )
})

############
# Test 5: Always-on structural invariant test (no skip guard, no RDS fixtures) # COMPLETED
testthat::test_that("AnalyzeLookPC: structural invariants hold across looks", {
  wi <- c(0.5, 0.5)
  g <- matrix(c(0, 1, 1, 0), byrow = TRUE, nrow = 2)
  
  state <- SetupAnalysisPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = "Bonf",
    dAlpha = 0.025,
    vInfoFracts = c(0.5, 1.0),
    bPlotGraphs = FALSE
  )

  # Initial state
  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(state$completed_looks, 0L)
  testthat::expect_false(state$trial_completed)
  testthat::expect_equal(state$mcpObj$IndexSet, c("H1", "H2"))

  # Look 1: completed_looks advances, AdjPValues is populated, trial not yet complete
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.10, H2 = 0.20),
    bPlotGraphs = FALSE
  )
  testthat::expect_equal(state$completed_looks, 1L)
  testthat::expect_false(state$trial_completed)
  testthat::expect_true(is.data.frame(state$mcpObj$AdjPValues))
  testthat::expect_true(nrow(state$mcpObj$AdjPValues) > 0)
  testthat::expect_true(length(state$mcpObj$IndexSet) > 0)

  # Look 2 (final look): trial_completed flips to TRUE, completed_looks = 2
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.10, H2 = 0.20),
    bPlotGraphs = FALSE
  )
  testthat::expect_equal(state$completed_looks, 2L)
  testthat::expect_true(state$trial_completed)

  # Calling again after trial is complete must error
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(state, vRawPVals = c(H1 = 0.10, H2 = 0.20), bPlotGraphs = FALSE)
  )
})

############################################################################
# Test 6a: Bonferroni, 1-look, 3-hypo (ref: FS_GMCP_Example.R, Example 1)
############################################################################
testthat::test_that("Test 6a: Bonferroni, fixed-sample, 3-hypo simple allocation", {
  # Source: internalData/FS_GMCP_Example.R, Example 1 (pages 4-5)
  # 3 hypotheses, equal allocation, Bonferroni test, fixed sample (1 look)

  wi <- c(1 / 3, 1 / 3, 1 / 3)
  g <- matrix(
    c(0, 0.5, 0.5, 0.5, 0, 0.5, 0.5, 0.5, 0),
    byrow = TRUE,
    nrow = 3
  )
  tt <- "Bonf"
  alp <- 0.05
  t <- c(1)  # Fixed sample: 1 look

  # Setup
  state <- SetupAnalysisPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = tt,
    dAlpha = alp,
    vInfoFracts = t,
    bPlotGraphs = FALSE
  )

  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(state$completed_looks, 0L)
  testthat::expect_equal(state$mcpObj$IndexSet, c("H1", "H2", "H3"))

  # Look 1 (final)
  # Raw p-values from file: p1=0.02, p2=0.055, p3=0.012
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.02, H2 = 0.055, H3 = 0.012),
    bPlotGraphs = FALSE
  )

  testthat::expect_equal(state$completed_looks, 1L)
  testthat::expect_true(state$trial_completed)

  exp_mcp <- readRDS(testthat::test_path("t6a.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )
})

############################################################################
# Test 6b: Bonferroni, 1-look, 4-hypo hierarchical (ref: FS_GMCP_Example.R, Ex 3)
############################################################################
testthat::test_that("Test 6b: Bonferroni, fixed-sample, 4-hypo hierarchical gatekeeping", {
  # Source: internalData/FS_GMCP_Example.R, Example 3 (pages 12-13)
  # 4 hypotheses with hierarchical gatekeeping structure (eps-transition)
  # H1, H2 primary; H3, H4 secondary (only tested if H1 AND H2 rejected)

  wi <- c(1 / 2, 1 / 2, 0, 0)
  eps <- 1e-6
  r1 <- 0.8
  r2 <- 0.2
  g <- matrix(
    c(
      0, 1, 0, 0,
      1 - eps, 0, r1 * eps, r2 * eps,
      0, 0, 0, 1,
      0, 0, 1, 0
    ),
    byrow = TRUE,
    nrow = 4
  )
  tt <- "Bonf"
  alp <- 0.05
  t <- c(1)  # Fixed sample: 1 look

  # Setup
  state <- SetupAnalysisPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = tt,
    dAlpha = alp,
    vInfoFracts = t,
    bPlotGraphs = FALSE
  )

  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(state$completed_looks, 0L)
  testthat::expect_equal(state$mcpObj$IndexSet, c("H1", "H2", "H3", "H4"))

  # Look 1 (final)
  # Raw p-values from file: p1=0.04, p2=0.01, p3=0.03, p4=0.04
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.04, H2 = 0.01, H3 = 0.03, H4 = 0.04),
    bPlotGraphs = FALSE
  )

  testthat::expect_equal(state$completed_looks, 1L)
  testthat::expect_true(state$trial_completed)

  exp_mcp <- readRDS(testthat::test_path("t6b.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )
})

############################################################################
# Test 6c: Bonferroni, 1-look, 4-hypo serial gatekeeping (FS_EastManual M3)
############################################################################
testthat::test_that("Test 6c: Bonferroni, fixed-sample, 4-hypo serial gatekeeping", {
  # Source: internalData/FS_GMCP_EastManual_Examples.R, Example M3 (page 2586+)
  # Serial gatekeeping: 2 primary (H1, H2) + 2 secondary (H3, H4)
  # Secondary only tested if both primaries rejected

  wi <- c(1 / 2, 1 / 2, 0, 0)
  eps <- 1e-6
  g <- matrix(
    c(
      0, 1 - eps, eps / 2, eps / 2,
      1 - eps, 0, eps / 2, eps / 2,
      0, 0, 0, 1,
      0, 0, 1, 0
    ),
    byrow = TRUE,
    nrow = 4
  )
  tt <- "Bonf"
  alp <- 0.05
  t <- c(1)  # Fixed sample: 1 look

  # Setup
  state <- SetupAnalysisPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = tt,
    dAlpha = alp,
    vInfoFracts = t,
    bPlotGraphs = FALSE
  )

  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(state$completed_looks, 0L)

  # Look 1 (final)
  # Raw p-values from file: p1=0.076, p2=0.035, p3=0.563, p4=0.407
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.076, H2 = 0.035, H3 = 0.563, H4 = 0.407),
    bPlotGraphs = FALSE
  )

  testthat::expect_equal(state$completed_looks, 1L)
  testthat::expect_true(state$trial_completed)

  exp_mcp <- readRDS(testthat::test_path("t6c.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )
})

############################################################################
# Test 7: Dunnett, 2-look GS, no selection/update (ref: GS_GMCP_Example.R, Ex 3)
############################################################################
testthat::test_that("Test 7: Dunnett, group-sequential, 2-look, no selection/update", {
  # Source: internalData/GS_GMCP_Example.R, Example 3 (pages 5-20)
  # 4 hypotheses, Dunnett test, group-sequential with 2 looks
  # No selection or strategy modification; all hypotheses carried through

  wi <- c(1 / 2, 1 / 2, 0, 0)
  g <- matrix(
    c(
      0, 1 / 2, 1 / 2, 0,
      1 / 2, 0, 0, 1 / 2,
      0, 1, 0, 0,
      1, 0, 0, 0
    ),
    byrow = TRUE,
    nrow = 4
  )
  tt <- "Dunnett"
  alp <- 0.025
  t <- c(0.5, 1)  # Group-sequential: 2 looks at 50% and 100% information
  des <- "asOF"  # O'Brien-Fleming alpha spending

  # Default correlation from adaptGMCP_PC(): partial block-diagonal structure
  # This is the correlation used when adaptGMCP_PC() is called without a Correlation argument
  corr <- matrix(
    c(1, 0.5, NA, NA, 0.5, 1, NA, NA, NA, NA, 1, 0.5, NA, NA, 0.5, 1),
    nrow = 4
  )

  # Setup
  state <- SetupAnalysisPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = tt,
    dAlpha = alp,
    vInfoFracts = t,
    strDesignType = des,
    bMultipleWinners = TRUE,
    bPlotGraphs = FALSE
  )

  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(state$completed_looks, 0L)
  testthat::expect_equal(state$mcpObj$IndexSet, c("H1", "H2", "H3", "H4"))

  # Look 1
  # Raw p-values from file: p1=0.00045, p2=0.0952, p3=0.0225, p4=0.1104
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.00045, H2 = 0.0952, H3 = 0.0225, H4 = 0.1104),
    mCorrelation = corr,
    bPlotGraphs = FALSE
  )

  testthat::expect_equal(state$completed_looks, 1L)
  testthat::expect_false(state$trial_completed)
  # H1 is rejected at look 1; PerLookMCPAnalysis() removes it from IndexSet
  testthat::expect_equal(state$mcpObj$IndexSet, c("H2", "H3", "H4"))

  exp_mcp <- readRDS(testthat::test_path("t7.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )

  # Look 2 (final)
  # Raw p-values from file: p2=0.1121, p3=0.0112, p4=0.1153
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H2 = 0.1121, H3 = 0.0112, H4 = 0.1153),
    bPlotGraphs = FALSE
  )

  testthat::expect_equal(state$completed_looks, 2L)
  testthat::expect_true(state$trial_completed)

  exp_mcp <- readRDS(testthat::test_path("t7.l2.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )
})

############################################################################
# Test 8: Dunnett, 2-look GS, weight modification at look 2 (GS_GMCP Ex 3)
############################################################################
testthat::test_that("Test 8: Dunnett, group-sequential, 2-look, weight modification", {
  # Source: internalData/GS_GMCP_Example.R, Example 3 variant (page 15)
  # Same setup as Test 7, but with strategy modification (weight + graph update) at look 2

  wi <- c(1 / 2, 1 / 2, 0, 0)
  g <- matrix(
    c(
      0, 1 / 2, 1 / 2, 0,
      1 / 2, 0, 0, 1 / 2,
      0, 1, 0, 0,
      1, 0, 0, 0
    ),
    byrow = TRUE,
    nrow = 4
  )
  tt <- "Dunnett"
  alp <- 0.025
  t <- c(0.5, 1)
  des <- "asOF"

  # Default correlation from adaptGMCP_PC(): partial block-diagonal structure
  corr <- matrix(
    c(1, 0.5, NA, NA, 0.5, 1, NA, NA, NA, NA, 1, 0.5, NA, NA, 0.5, 1),
    nrow = 4
  )

  # Setup
  state <- SetupAnalysisPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = tt,
    dAlpha = alp,
    vInfoFracts = t,
    strDesignType = des,
    bMultipleWinners = TRUE,
    bPlotGraphs = FALSE
  )

  # Look 1 (same as Test 7)
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.00045, H2 = 0.0952, H3 = 0.0225, H4 = 0.1104),
    mCorrelation = corr,
    bPlotGraphs = FALSE
  )

  testthat::expect_equal(state$completed_looks, 1L)

  exp_mcp <- readRDS(testthat::test_path("t8.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )

  # Look 2 with strategy modification
  # New weights and transition matrix for H2, H3, H4 per paper (page 15)
  # From paper: modified weights allocate probability differently among remaining hypo
  new_weights <- c(H2 = 0.5, H3 = 0.25, H4 = 0.25)  # Example; adjust per paper
  new_G <- matrix(
    c(
      0, 1 / 3, 2 / 3,
      1, 0, 0,
      1 / 2, 1 / 2, 0
    ),
    byrow = TRUE,
    nrow = 3
  )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H2 = 0.0299, H3 = 0.0225, H4 = 0.0586),
    vWeights = new_weights,
    mGMatrix = new_G,
    bPlotGraphs = FALSE
  )

  testthat::expect_equal(state$completed_looks, 2L)
  testthat::expect_true(state$trial_completed)

  exp_mcp <- readRDS(testthat::test_path("t8.l2.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )
})

############################################################################
# Test 9: Simes test, 1-look (ref: FS_GMCP_Example.R, Example 8)
############################################################################
testthat::test_that("Test 9: Simes test, fixed-sample, 4-hypo", {
  # Source: internalData/FS_GMCP_Example.R, Example 8 (page 14)
  # Same setup as Example 6, but with Simes test instead of Bonf

  wi <- c(0.5, 0.5, 0, 0)
  g <- matrix(
    c(
      0, 0, 1, 0,
      0, 0, 0, 1,
      0, 1, 0, 0,
      1, 0, 0, 0
    ),
    byrow = TRUE,
    nrow = 4
  )
  tt <- "Simes"
  alp <- 0.025
  t <- c(1)  # Fixed sample: 1 look

  # Setup
  state <- SetupAnalysisPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = tt,
    dAlpha = alp,
    vInfoFracts = t,
    bPlotGraphs = FALSE
  )

  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(state$completed_looks, 0L)

  # Look 1 (final)
  # Raw p-values from file: p1=0.01, p2=0.005, p3=0.015, p4=0.022
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.01, H2 = 0.005, H3 = 0.015, H4 = 0.022),
    bPlotGraphs = FALSE
  )

  testthat::expect_equal(state$completed_looks, 1L)
  testthat::expect_true(state$trial_completed)

  exp_mcp <- readRDS(testthat::test_path("t9.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )
})

############################################################################
# Test 10: Dunnett, 1-look, with correlation matrix (FS_GMCP_Example.R Ex 7)
############################################################################
testthat::test_that("Test 10: Dunnett, fixed-sample, 4-hypo, full correlation", {
  # Source: internalData/FS_GMCP_Example.R, Example 7 (page 10)
  # Dunnett test with non-default correlation structure

  wi <- c(0.5, 0.5, 0, 0)
  g <- matrix(
    c(
      0, 0, 1, 0,
      0, 0, 0, 1,
      0, 1, 0, 0,
      1, 0, 0, 0
    ),
    byrow = TRUE,
    nrow = 4
  )
  tt <- "Dunnett"
  alp <- 0.025
  t <- c(1)  # Fixed sample: 1 look

  # Correlation matrix from file: non-default structure
  corr <- matrix(
    c(
      1, 0.5, 1, 0.5,
      0.5, 1, 0.5, 1,
      1, 0.5, 1, 0.5,
      0.5, 1, 0.5, 1
    ),
    byrow = TRUE,
    nrow = 4
  )

  # Setup
  state <- SetupAnalysisPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = tt,
    dAlpha = alp,
    vInfoFracts = t,
    bPlotGraphs = FALSE
  )

  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(state$completed_looks, 0L)

  # Look 1 (final)
  # Raw p-values from file: p1=0.01, p2=0.02, p3=0.005, p4=0.5
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.01, H2 = 0.02, H3 = 0.005, H4 = 0.5),
    mCorrelation = corr,
    bPlotGraphs = FALSE
  )

  testthat::expect_equal(state$completed_looks, 1L)
  testthat::expect_true(state$trial_completed)

  exp_mcp <- readRDS(testthat::test_path("t10.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )
})

############################################################################
# Test 11: Dunnett, 1-look, equal weights (FS_GMCP_EastManual M1)
############################################################################
testthat::test_that("Test 11: Dunnett, fixed-sample, 4-hypo, equal weights", {
  # Source: internalData/FS_GMCP_EastManual_Examples.R, Example M1 (page 2550)
  # Dunnett test with equal weights (0.25 each) instead of concentrated weights

  wi <- c(0.25, 0.25, 0.25, 0.25)  # Equal weights, all non-zero
  g <- matrix(
    c(
      0, 1 / 3, 1 / 3, 1 / 3,
      1 / 3, 0, 1 / 3, 1 / 3,
      1 / 3, 1 / 3, 0, 1 / 3,
      1 / 3, 1 / 3, 1 / 3, 0
    ),
    byrow = TRUE,
    nrow = 4
  )
  tt <- "Dunnett"
  alp <- 0.025
  t <- c(1)  # Fixed sample: 1 look

  # Correlation matrix (default: off-diag = 0.5)
  corr <- matrix(
    c(
      1, 0.5, 0.5, 0.5,
      0.5, 1, 0.5, 0.5,
      0.5, 0.5, 1, 0.5,
      0.5, 0.5, 0.5, 1
    ),
    byrow = TRUE,
    nrow = 4
  )

  # Setup
  state <- SetupAnalysisPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = tt,
    dAlpha = alp,
    vInfoFracts = t,
    bPlotGraphs = FALSE
  )

  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(state$completed_looks, 0L)
  testthat::expect_equal(state$mcpObj$IndexSet, c("H1", "H2", "H3", "H4"))

  # Look 1 (final)
  # Raw p-values from file: p1=0.638, p2=0.01, p3=0.007, p4=3.959E-4
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.638, H2 = 0.01, H3 = 0.007, H4 = 3.959e-4),
    mCorrelation = corr,
    bPlotGraphs = FALSE
  )

  testthat::expect_equal(state$completed_looks, 1L)
  testthat::expect_true(state$trial_completed)

  exp_mcp <- readRDS(testthat::test_path("t11.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )
})

############################################################################
# Issue #75 matrix row M07: asP design
############################################################################
testthat::test_that("PC equivalence M07: asP design", {
  wi <- c(1 / 2, 1 / 2, 0, 0)
  g <- matrix(
    c(
      0, 1 / 2, 1 / 2, 0,
      1 / 2, 0, 0, 1 / 2,
      0, 1, 0, 0,
      1, 0, 0, 0
    ),
    byrow = TRUE,
    nrow = 4
  )

  corr <- matrix(
    c(
      1, 0.5, NA, NA,
      0.5, 1, NA, NA,
      NA, NA, 1, 0.5,
      NA, NA, 0.5, 1
    ),
    byrow = TRUE,
    nrow = 4
  )

  state <- SetupAnalysisPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = "Dunnett",
    dAlpha = 0.025,
    vInfoFracts = c(0.5, 1.0),
    strDesignType = "asP",
    bMultipleWinners = TRUE,
    bSelection = FALSE,
    bUpdateStrategy = FALSE,
    bPlotGraphs = FALSE
  )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.30, H2 = 0.35, H3 = 0.40, H4 = 0.45),
    mCorrelation = corr,
    bPlotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M07.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.08, H2 = 0.09, H3 = 0.12, H4 = 0.15),
    bPlotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M07.l2.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )
})

############################################################################
# Issue #75 matrix row M11: asHSD design with gammaA
############################################################################
testthat::test_that("PC equivalence M11: asHSD design", {
  wi <- c(1 / 2, 1 / 2, 0, 0)
  g <- matrix(
    c(
      0, 1 / 2, 1 / 2, 0,
      1 / 2, 0, 0, 1 / 2,
      0, 1, 0, 0,
      1, 0, 0, 0
    ),
    byrow = TRUE,
    nrow = 4
  )

  corr <- matrix(
    c(
      1, 0.5, NA, NA,
      0.5, 1, NA, NA,
      NA, NA, 1, 0.5,
      NA, NA, 0.5, 1
    ),
    byrow = TRUE,
    nrow = 4
  )

  state <- SetupAnalysisPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = "Dunnett",
    dAlpha = 0.025,
    vInfoFracts = c(0.5, 1.0),
    strDesignType = "asHSD",
    dGammaA = 2.5,
    bMultipleWinners = TRUE,
    bSelection = FALSE,
    bUpdateStrategy = FALSE,
    bPlotGraphs = FALSE
  )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.25, H2 = 0.30, H3 = 0.35, H4 = 0.40),
    mCorrelation = corr,
    bPlotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M11.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.07, H2 = 0.09, H3 = 0.10, H4 = 0.12),
    bPlotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M11.l2.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )
})

############################################################################
# Issue #75 matrix row M12: asKD design with gammaA
############################################################################
testthat::test_that("PC equivalence M12: asKD design", {
  wi <- c(1 / 2, 1 / 2, 0, 0)
  g <- matrix(
    c(
      0, 1 / 2, 1 / 2, 0,
      1 / 2, 0, 0, 1 / 2,
      0, 1, 0, 0,
      1, 0, 0, 0
    ),
    byrow = TRUE,
    nrow = 4
  )

  corr <- matrix(
    c(
      1, 0.5, NA, NA,
      0.5, 1, NA, NA,
      NA, NA, 1, 0.5,
      NA, NA, 0.5, 1
    ),
    byrow = TRUE,
    nrow = 4
  )

  state <- SetupAnalysisPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = "Dunnett",
    dAlpha = 0.025,
    vInfoFracts = c(0.5, 1.0),
    strDesignType = "asKD",
    dGammaA = 2.5,
    bMultipleWinners = TRUE,
    bSelection = FALSE,
    bUpdateStrategy = FALSE,
    bPlotGraphs = FALSE
  )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.28, H2 = 0.31, H3 = 0.33, H4 = 0.37),
    mCorrelation = corr,
    bPlotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M12.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.06, H2 = 0.09, H3 = 0.11, H4 = 0.13),
    bPlotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M12.l2.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )
})

############################################################################
# Issue #75 matrix row M19: correlation-only adaptation
############################################################################
testthat::test_that("PC equivalence M19: correlation-only adaptation", {
  wi <- c(1 / 2, 1 / 2, 0, 0)
  g <- matrix(
    c(
      0, 1 / 2, 1 / 2, 0,
      1 / 2, 0, 0, 1 / 2,
      0, 1, 0, 0,
      1, 0, 0, 0
    ),
    byrow = TRUE,
    nrow = 4
  )

  corr <- matrix(
    c(
      1, 0.5, NA, NA,
      0.5, 1, NA, NA,
      NA, NA, 1, 0.5,
      NA, NA, 0.5, 1
    ),
    byrow = TRUE,
    nrow = 4
  )

  new_corr <- matrix(
    c(
      1, 0.3, NA, NA,
      0.3, 1, NA, NA,
      NA, NA, 1, 0.4,
      NA, NA, 0.4, 1
    ),
    byrow = TRUE,
    nrow = 4
  )

  state <- SetupAnalysisPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = "Dunnett",
    dAlpha = 0.025,
    vInfoFracts = c(0.5, 1.0),
    strDesignType = "asOF",
    bMultipleWinners = TRUE,
    bSelection = FALSE,
    bUpdateStrategy = FALSE,
    bPlotGraphs = FALSE
  )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.26, H2 = 0.29, H3 = 0.34, H4 = 0.38),
    mCorrelation = corr,
    bPlotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M19.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.07, H2 = 0.08, H3 = 0.11, H4 = 0.14),
    mCorrelation = new_corr,
    bPlotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M19.l2.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )
})

############################################################################
# Issue #75 matrix row M22: early-stop efficacy path
############################################################################
testthat::test_that("PC equivalence M22: early-stop efficacy path", {
  wi <- c(1 / 2, 1 / 2, 0, 0)
  g <- matrix(
    c(
      0, 1 / 2, 1 / 2, 0,
      1 / 2, 0, 0, 1 / 2,
      0, 1, 0, 0,
      1, 0, 0, 0
    ),
    byrow = TRUE,
    nrow = 4
  )

  corr <- matrix(
    c(
      1, 0.5, NA, NA,
      0.5, 1, NA, NA,
      NA, NA, 1, 0.5,
      NA, NA, 0.5, 1
    ),
    byrow = TRUE,
    nrow = 4
  )

  state <- SetupAnalysisPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = "Dunnett",
    dAlpha = 0.025,
    vInfoFracts = c(0.5, 1.0),
    strDesignType = "asOF",
    bMultipleWinners = FALSE,
    bSelection = FALSE,
    bUpdateStrategy = FALSE,
    bPlotGraphs = FALSE
  )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 1e-4, H2 = 0.40, H3 = 0.45, H4 = 0.50),
    mCorrelation = corr,
    bPlotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M22.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )
  testthat::expect_true(state$trial_completed)
  testthat::expect_equal(state$completion_reason, "early_stop_efficacy")
})

############################################################################
# Issue #75 matrix row M24: MultipleWinners = FALSE behavior
############################################################################
testthat::test_that("PC equivalence M24: MultipleWinners FALSE", {
  wi <- c(1 / 2, 1 / 2, 0, 0)
  g <- matrix(
    c(
      0, 1 / 2, 1 / 2, 0,
      1 / 2, 0, 0, 1 / 2,
      0, 1, 0, 0,
      1, 0, 0, 0
    ),
    byrow = TRUE,
    nrow = 4
  )

  corr <- matrix(
    c(
      1, 0.5, NA, NA,
      0.5, 1, NA, NA,
      NA, NA, 1, 0.5,
      NA, NA, 0.5, 1
    ),
    byrow = TRUE,
    nrow = 4
  )

  state <- SetupAnalysisPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = "Dunnett",
    dAlpha = 0.025,
    vInfoFracts = c(0.5, 1.0),
    strDesignType = "asOF",
    bMultipleWinners = FALSE,
    bSelection = FALSE,
    bUpdateStrategy = FALSE,
    bPlotGraphs = FALSE
  )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.30, H2 = 0.35, H3 = 0.38, H4 = 0.42),
    mCorrelation = corr,
    bPlotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M24.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.02, H2 = 0.06, H3 = 0.10, H4 = 0.20),
    bPlotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M24.l2.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )
})

############################################################################
# Issue #75 matrix row M09: WT design with deltaWT parameter
############################################################################
testthat::test_that("PC equivalence M09: WT design", {
  wi <- c(1 / 2, 1 / 2, 0, 0)
  g <- matrix(
    c(
      0, 1 / 2, 1 / 2, 0,
      1 / 2, 0, 0, 1 / 2,
      0, 1, 0, 0,
      1, 0, 0, 0
    ),
    byrow = TRUE,
    nrow = 4
  )

  corr <- matrix(
    c(
      1, 0.5, NA, NA,
      0.5, 1, NA, NA,
      NA, NA, 1, 0.5,
      NA, NA, 0.5, 1
    ),
    byrow = TRUE,
    nrow = 4
  )

  state <- SetupAnalysisPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = "Dunnett",
    dAlpha = 0.025,
    vInfoFracts = c(0.5, 1.0),
    strDesignType = "WT",
    dDeltaWT = 0.25,
    bMultipleWinners = TRUE,
    bSelection = FALSE,
    bUpdateStrategy = FALSE,
    bPlotGraphs = FALSE
  )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.28, H2 = 0.32, H3 = 0.36, H4 = 0.40),
    mCorrelation = corr,
    bPlotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M09.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.07, H2 = 0.10, H3 = 0.13, H4 = 0.16),
    bPlotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M09.l2.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )
})

############################################################################
# Issue #75 matrix row M13: asUser design with userAlphaSpending
############################################################################
testthat::test_that("PC equivalence M13: asUser design", {
  wi <- c(1 / 2, 1 / 2, 0, 0)
  g <- matrix(
    c(
      0, 1 / 2, 1 / 2, 0,
      1 / 2, 0, 0, 1 / 2,
      0, 1, 0, 0,
      1, 0, 0, 0
    ),
    byrow = TRUE,
    nrow = 4
  )

  corr <- matrix(
    c(
      1, 0.5, NA, NA,
      0.5, 1, NA, NA,
      NA, NA, 1, 0.5,
      NA, NA, 0.5, 1
    ),
    byrow = TRUE,
    nrow = 4
  )

  state <- SetupAnalysisPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = "Dunnett",
    dAlpha = 0.025,
    vInfoFracts = c(0.5, 1.0),
    strDesignType = "asUser",
    vUserAlphaSpending = c(0.008, 0.025),
    bMultipleWinners = TRUE,
    bSelection = FALSE,
    bUpdateStrategy = FALSE,
    bPlotGraphs = FALSE
  )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.29, H2 = 0.33, H3 = 0.37, H4 = 0.41),
    mCorrelation = corr,
    bPlotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M13.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.07, H2 = 0.09, H3 = 0.12, H4 = 0.15),
    bPlotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M13.l2.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )
})

############################################################################
# Issue #75 matrix row M17: selection-only adaptation
############################################################################
testthat::test_that("PC equivalence M17: selection-only adaptation", {
  wi <- c(1 / 2, 1 / 2, 0, 0)
  g <- matrix(
    c(
      0, 1 / 2, 1 / 2, 0,
      1 / 2, 0, 0, 1 / 2,
      0, 1, 0, 0,
      1, 0, 0, 0
    ),
    byrow = TRUE,
    nrow = 4
  )

  corr <- matrix(
    c(
      1, 0.5, NA, NA,
      0.5, 1, NA, NA,
      NA, NA, 1, 0.5,
      NA, NA, 0.5, 1
    ),
    byrow = TRUE,
    nrow = 4
  )

  state <- SetupAnalysisPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = "Dunnett",
    dAlpha = 0.025,
    vInfoFracts = c(0.5, 1.0),
    strDesignType = "asOF",
    bMultipleWinners = TRUE,
    bSelection = TRUE,
    bUpdateStrategy = FALSE,
    bPlotGraphs = FALSE
  )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.21, H2 = 0.24, H3 = 0.30, H4 = 0.35),
    mCorrelation = corr,
    bPlotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M17.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.05, H2 = 0.07, H4 = 0.11),
    vSelectHypo = c("H1", "H2", "H4"),
    bPlotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M17.l2.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )
})

############################################################################
# Issue #75 matrix row M08: noEarlyEfficacy design
############################################################################
testthat::test_that("PC equivalence M08: noEarlyEfficacy design", {
  wi <- c(1 / 2, 1 / 2, 0, 0)
  g <- matrix(
    c(
      0, 1 / 2, 1 / 2, 0,
      1 / 2, 0, 0, 1 / 2,
      0, 1, 0, 0,
      1, 0, 0, 0
    ),
    byrow = TRUE,
    nrow = 4
  )

  corr <- matrix(
    c(
      1, 0.5, NA, NA,
      0.5, 1, NA, NA,
      NA, NA, 1, 0.5,
      NA, NA, 0.5, 1
    ),
    byrow = TRUE,
    nrow = 4
  )

  state <- SetupAnalysisPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = "Dunnett",
    dAlpha = 0.025,
    vInfoFracts = c(0.5, 1.0),
    strDesignType = "noEarlyEfficacy",
    bMultipleWinners = TRUE,
    bSelection = FALSE,
    bUpdateStrategy = FALSE,
    bPlotGraphs = FALSE
  )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.31, H2 = 0.34, H3 = 0.38, H4 = 0.43),
    mCorrelation = corr,
    bPlotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M08.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.08, H2 = 0.10, H3 = 0.12, H4 = 0.16),
    bPlotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M08.l2.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )
})

############################################################################
# Issue #75 matrix row M02: Sidak baseline fixed-sample
############################################################################
testthat::test_that("PC equivalence M02: Sidak baseline", {
  wi <- c(1 / 2, 1 / 2, 0, 0)
  g <- matrix(
    c(
      0, 1 / 2, 1 / 2, 0,
      1 / 2, 0, 0, 1 / 2,
      0, 1, 0, 0,
      1, 0, 0, 0
    ),
    byrow = TRUE,
    nrow = 4
  )

  state <- SetupAnalysisPC(
    vWeights = wi,
    mGMatrix = g,
    strTestType = "Sidak",
    dAlpha = 0.025,
    vInfoFracts = c(1.0),
    strDesignType = "asOF",
    bMultipleWinners = TRUE,
    bSelection = FALSE,
    bUpdateStrategy = FALSE,
    bPlotGraphs = FALSE
  )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.01, H2 = 0.02, H3 = 0.03, H4 = 0.04),
    bPlotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M02.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )
  testthat::expect_true(state$trial_completed)
})

############################################################################
# Issue #75 matrix row M10: PT design — DEFERRED (N/A for this pass)
# rpact requires deltaPT0 which is not yet exposed by SetupAnalysisPC() or
# adaptGMCP_PC(). This row is marked N/A for this pass. Add a fixture-backed
# equivalence test once deltaPT0 parameter support is added to the API.
############################################################################

############################################################################
# Issue #67 Phase 1: SetupAnalysisPC() contract-validation tests
############################################################################

testthat::test_that("SetupAnalysisPC: WI validation - type, NA, negativity, and sum", {
  wi <- c(0.5, 0.5, 0, 0)
  g <- matrix(
    c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0),
    byrow = TRUE, nrow = 4
  )

  # Non-numeric WI
  testthat::expect_error(
    SetupAnalysisPC(
      vWeights = c("a", "b", "c", "d"), mGMatrix = g, strTestType = "Bonf",
      dAlpha = 0.025, vInfoFracts = c(1), bPlotGraphs = FALSE
    ),
    regexp = "numeric, non-negative, and non-NA"
  )

  # WI with NA
  testthat::expect_error(
    SetupAnalysisPC(
      vWeights = c(0.5, NA, 0, 0), mGMatrix = g, strTestType = "Bonf",
      dAlpha = 0.025, vInfoFracts = c(1), bPlotGraphs = FALSE
    ),
    regexp = "numeric, non-negative, and non-NA"
  )

  # WI with negative value
  testthat::expect_error(
    SetupAnalysisPC(
      vWeights = c(0.5, -0.1, 0, 0), mGMatrix = g, strTestType = "Bonf",
      dAlpha = 0.025, vInfoFracts = c(1), bPlotGraphs = FALSE
    ),
    regexp = "numeric, non-negative, and non-NA"
  )

  # WI summing to > 1
  testthat::expect_error(
    SetupAnalysisPC(
      vWeights = c(0.6, 0.6, 0, 0), mGMatrix = g, strTestType = "Bonf",
      dAlpha = 0.025, vInfoFracts = c(1), bPlotGraphs = FALSE
    ),
    regexp = "Sum of vWeights"
  )

  # WI dimension mismatch with G: WI has 2 elements, G is 4 x 4
  testthat::expect_error(
    SetupAnalysisPC(
      vWeights = c(0.5, 0.5), mGMatrix = g, strTestType = "Bonf",
      dAlpha = 0.025, vInfoFracts = c(1), bPlotGraphs = FALSE
    ),
    regexp = "mGMatrix"
  )
})

testthat::test_that("SetupAnalysisPC: G validation - matrix requirement and dimensions", {
  wi <- c(0.5, 0.5, 0, 0)

  # G is not a matrix (vector instead)
  testthat::expect_error(
    SetupAnalysisPC(
      vWeights = wi, mGMatrix = c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0),
      strTestType = "Bonf", dAlpha = 0.025, vInfoFracts = c(1), bPlotGraphs = FALSE
    ),
    regexp = "mGMatrix"
  )

  # G has wrong dimensions (3 x 3 instead of 4 x 4)
  gBad <- matrix(c(0, 0.5, 0.5, 0.5, 0, 0.5, 0.5, 0.5, 0), byrow = TRUE, nrow = 3)
  testthat::expect_error(
    SetupAnalysisPC(
      vWeights = wi, mGMatrix = gBad, strTestType = "Bonf",
      dAlpha = 0.025, vInfoFracts = c(1), bPlotGraphs = FALSE
    ),
    regexp = "mGMatrix"
  )
})

testthat::test_that("SetupAnalysisPC: vInfoFracts validation - length, bounds, and monotonicity", {
  wi <- c(0.5, 0.5, 0, 0)
  g <- matrix(
    c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0),
    byrow = TRUE, nrow = 4
  )

  # Empty info_frac
  testthat::expect_error(
    SetupAnalysisPC(
      vWeights = wi, mGMatrix = g, strTestType = "Bonf",
      dAlpha = 0.025, vInfoFracts = numeric(0), bPlotGraphs = FALSE
    ),
    regexp = "vInfoFracts must have length"
  )

  # info_frac contains zero (not in (0, 1])
  testthat::expect_error(
    SetupAnalysisPC(
      vWeights = wi, mGMatrix = g, strTestType = "Bonf",
      dAlpha = 0.025, vInfoFracts = c(0, 0.5, 1.0), bPlotGraphs = FALSE
    ),
    regexp = "vInfoFracts must be in"
  )

  # info_frac contains value > 1
  testthat::expect_error(
    SetupAnalysisPC(
      vWeights = wi, mGMatrix = g, strTestType = "Bonf",
      dAlpha = 0.025, vInfoFracts = c(0.5, 1.2), bPlotGraphs = FALSE
    ),
    regexp = "vInfoFracts must be in"
  )

  # info_frac not strictly increasing (repeated value)
  testthat::expect_error(
    SetupAnalysisPC(
      vWeights = wi, mGMatrix = g, strTestType = "Bonf",
      dAlpha = 0.025, vInfoFracts = c(0.5, 0.5, 1.0), bPlotGraphs = FALSE
    ),
    regexp = "strictly increasing"
  )

  # info_frac not strictly increasing (decreasing segment)
  testthat::expect_error(
    SetupAnalysisPC(
      vWeights = wi, mGMatrix = g, strTestType = "Bonf",
      dAlpha = 0.025, vInfoFracts = c(0.7, 0.5, 1.0), bPlotGraphs = FALSE
    ),
    regexp = "strictly increasing"
  )
})

testthat::test_that("SetupAnalysisPC: unsupported test.type is rejected", {
  wi <- c(0.5, 0.5, 0, 0)
  g <- matrix(
    c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0),
    byrow = TRUE, nrow = 4
  )

  testthat::expect_error(
    SetupAnalysisPC(
      vWeights = wi, mGMatrix = g, strTestType = "Unknown",
      dAlpha = 0.025, vInfoFracts = c(1), bPlotGraphs = FALSE
    ),
    regexp = "Unsupported strTestType"
  )
})

testthat::test_that("SetupAnalysisPC: correlation policy by test.type", {
  wi <- c(0.5, 0.5, 0, 0)
  g <- matrix(
    c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0),
    byrow = TRUE, nrow = 4
  )
  # Dunnett and Partly-Parametric defer Correlation to AnalyzeLookPC
  stateDunnett <- SetupAnalysisPC(
    vWeights = wi, mGMatrix = g, strTestType = "Dunnett",
    dAlpha = 0.025, vInfoFracts = c(1), bPlotGraphs = FALSE
  )
  testthat::expect_null(stateDunnett$mcpObj$Correlation)

  statePP <- SetupAnalysisPC(
    vWeights = wi, mGMatrix = g, strTestType = "Partly-Parametric",
    dAlpha = 0.025, vInfoFracts = c(1), bPlotGraphs = FALSE
  )
  testthat::expect_null(statePP$mcpObj$Correlation)

  # Bonf: overrides Correlation to identity with NA off-diagonal
  stateBonf <- SetupAnalysisPC(
    vWeights = wi, mGMatrix = g, strTestType = "Bonf",
    dAlpha = 0.025, vInfoFracts = c(1), bPlotGraphs = FALSE
  )
  corrExpBonf <- diag(4)
  corrExpBonf[corrExpBonf == 0] <- NA
  testthat::expect_equal(stateBonf$mcpObj$Correlation, corrExpBonf)

  # Sidak: sets Correlation to NA
  stateSidak <- SetupAnalysisPC(
    vWeights = wi, mGMatrix = g, strTestType = "Sidak",
    dAlpha = 0.025, vInfoFracts = c(1), bPlotGraphs = FALSE
  )
  testthat::expect_true(is.na(stateSidak$mcpObj$Correlation))

  # Simes: sets Correlation to NA
  stateSimes <- SetupAnalysisPC(
    vWeights = wi, mGMatrix = g, strTestType = "Simes",
    dAlpha = 0.025, vInfoFracts = c(1), bPlotGraphs = FALSE
  )
  testthat::expect_true(is.na(stateSimes$mcpObj$Correlation))
})

testthat::test_that("SetupAnalysisPC: asUser typeOfDesign requires userAlphaSpending", {
  wi <- c(0.5, 0.5, 0, 0)
  g <- matrix(
    c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0),
    byrow = TRUE, nrow = 4
  )
  # Missing userAlphaSpending when typeOfDesign = 'asUser'
  testthat::expect_error(
    SetupAnalysisPC(
      vWeights = wi, mGMatrix = g, strTestType = "Dunnett",
      dAlpha = 0.025, vInfoFracts = c(0.5, 1.0),
      strDesignType = "asUser",
      bPlotGraphs = FALSE
    ),
    regexp = "vUserAlphaSpending"
  )

  # Valid asUser call does not error
  testthat::expect_no_error(
    SetupAnalysisPC(
      vWeights = wi, mGMatrix = g, strTestType = "Dunnett",
      dAlpha = 0.025, vInfoFracts = c(0.5, 1.0),
      strDesignType = "asUser",
      vUserAlphaSpending = c(0.008, 0.025),
      bPlotGraphs = FALSE
    )
  )
})

############################################################################
# Issue #70 Phase 2: AnalyzeLook_PC_TestWrapper() input-validation tests
############################################################################

testthat::test_that("AnalyzeLookPC: non-PCAnalysisState input is rejected", {
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(list(), vRawPVals = c(H1 = 0.1), bPlotGraphs = FALSE),
    regexp = "PCAnalysisState"
  )

  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper("not_a_state", vRawPVals = c(H1 = 0.1), bPlotGraphs = FALSE),
    regexp = "PCAnalysisState"
  )
})

testthat::test_that("AnalyzeLookPC: p_raw contract validation", {
  wi <- c(0.5, 0.5)
  g <- matrix(c(0, 1, 1, 0), byrow = TRUE, nrow = 2)

  state0 <- SetupAnalysisPC(
    vWeights = wi, mGMatrix = g, strTestType = "Bonf",
    dAlpha = 0.025, vInfoFracts = c(0.5, 1.0),
    bPlotGraphs = FALSE
  )

  # Non-numeric p_raw
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(state0, vRawPVals = c(H1 = "a", H2 = "b"), bPlotGraphs = FALSE),
    regexp = "p_raw must be numeric"
  )

  # p_raw contains NA
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(state0, vRawPVals = c(H1 = 0.1, H2 = NA), bPlotGraphs = FALSE),
    regexp = "p_raw cannot contain NA"
  )

  # p_raw value below 0
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(state0, vRawPVals = c(H1 = -0.1, H2 = 0.2), bPlotGraphs = FALSE),
    regexp = "p_raw values must be in"
  )

  # p_raw value above 1
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(state0, vRawPVals = c(H1 = 1.1, H2 = 0.2), bPlotGraphs = FALSE),
    regexp = "p_raw values must be in"
  )

  # Unnamed p_raw with wrong length
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(state0, vRawPVals = c(0.1, 0.2, 0.3), bPlotGraphs = FALSE),
    regexp = "Unnamed p_raw must have length"
  )

  # Named p_raw with names not matching current IndexSet
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(state0, vRawPVals = c(X1 = 0.1, X2 = 0.2), bPlotGraphs = FALSE),
    regexp = "p_raw names must match"
  )

  # Unnamed p_raw with correct length succeeds
  testthat::expect_no_error(
    AnalyzeLook_PC_TestWrapper(state0, vRawPVals = c(0.1, 0.2), bPlotGraphs = FALSE)
  )
})

testthat::test_that("AnalyzeLookPC: look 1 adaptations are ignored with a warning", {
  wi <- c(0.5, 0.5, 0, 0)
  g <- matrix(
    c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0),
    byrow = TRUE, nrow = 4
  )
  corr <- matrix(
    c(1, 0.5, NA, NA, 0.5, 1, NA, NA, NA, NA, 1, 0.5, NA, NA, 0.5, 1),
    nrow = 4
  )

  state0 <- SetupAnalysisPC(
    vWeights = wi, mGMatrix = g, strTestType = "Partly-Parametric",
    dAlpha = 0.025, vInfoFracts = c(0.5, 1.0),
    bPlotGraphs = FALSE
  )

  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state0, vRawPVals = c(H1 = 0.1, H2 = 0.2, H3 = 0.3, H4 = 0.4),
      bPlotGraphs = FALSE
    ),
    regexp = "mCorrelation must be provided at look 1"
  )

  testthat::expect_warning(
    AnalyzeLook_PC_TestWrapper(
      state0, vRawPVals = c(H1 = 0.1, H2 = 0.2, H3 = 0.3, H4 = 0.4),
      mCorrelation = corr,
      vSelectHypo = c("H1", "H2"), bPlotGraphs = FALSE
    ),
    regexp = "Look 1 uses the initial design.*vSelectHypo.*ignored"
  )
  state_selection <- suppressWarnings(
    AnalyzeLook_PC_TestWrapper(
      state0, vRawPVals = c(H1 = 0.1, H2 = 0.2, H3 = 0.3, H4 = 0.4),
      mCorrelation = corr,
      vSelectHypo = c("H1", "H2"), bPlotGraphs = FALSE
    )
  )
  testthat::expect_identical(state_selection$mcpObj$IndexSet, state0$mcpObj$IndexSet)
  testthat::expect_length(state_selection$mcpObj$SelectionLook, 0)

  testthat::expect_warning(
    AnalyzeLook_PC_TestWrapper(
      state0, vRawPVals = c(H1 = 0.1, H2 = 0.2, H3 = 0.3, H4 = 0.4),
      mCorrelation = corr,
      vWeights = c(H1 = 0.5, H2 = 0.5, H3 = 0, H4 = 0),
      bPlotGraphs = FALSE
    ),
    regexp = "Look 1 uses the initial design.*vWeights.*ignored"
  )
  state_weights <- suppressWarnings(
    AnalyzeLook_PC_TestWrapper(
      state0, vRawPVals = c(H1 = 0.1, H2 = 0.2, H3 = 0.3, H4 = 0.4),
      mCorrelation = corr,
      vWeights = c(H1 = 0.5, H2 = 0.5, H3 = 0, H4 = 0),
      bPlotGraphs = FALSE
    )
  )
  testthat::expect_false(state_weights$mcpObj$Modify)
  testthat::expect_equal(state_weights$mcpObj$newWeights, state0$mcpObj$newWeights)

  testthat::expect_warning(
    AnalyzeLook_PC_TestWrapper(
      state0, vRawPVals = c(H1 = 0.1, H2 = 0.2, H3 = 0.3, H4 = 0.4),
      mCorrelation = corr,
      mGMatrix = g, bPlotGraphs = FALSE
    ),
    regexp = "Look 1 uses the initial design.*mGMatrix.*ignored"
  )
  state_graph <- suppressWarnings(
    AnalyzeLook_PC_TestWrapper(
      state0, vRawPVals = c(H1 = 0.1, H2 = 0.2, H3 = 0.3, H4 = 0.4),
      mCorrelation = corr,
      mGMatrix = g, bPlotGraphs = FALSE
    )
  )
  testthat::expect_false(state_graph$mcpObj$Modify)
  testthat::expect_equal(state_graph$mcpObj$newG, state0$mcpObj$newG)

  testthat::expect_warning(
    AnalyzeLook_PC_TestWrapper(
      state0, vRawPVals = c(H1 = 0.1, H2 = 0.2, H3 = 0.3, H4 = 0.4),
      mCorrelation = corr,
      vSelectHypo = c("H1", "H2"),
      vWeights = c(H1 = 0.5, H2 = 0.5, H3 = 0, H4 = 0),
      mGMatrix = g, bPlotGraphs = FALSE
    ),
    regexp = "vSelectHypo, vWeights, mGMatrix will be ignored"
  )

  # Correlation at look 1 is valid
  testthat::expect_no_error(
    AnalyzeLook_PC_TestWrapper(
      state0, vRawPVals = c(H1 = 0.1, H2 = 0.2, H3 = 0.3, H4 = 0.4),
      mCorrelation = corr, bPlotGraphs = FALSE
    )
  )
})

############################################################################
# Issue #69 Phase 3: look > 1 adaptation gate and mutation-validation tests
############################################################################

testthat::test_that("AnalyzeLookPC: feature-flag gates at look > 1", {
  wi <- c(0.5, 0.5, 0, 0)
  g <- matrix(
    c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0),
    byrow = TRUE, nrow = 4
  )
  corr <- matrix(
    c(1, 0.5, NA, NA, 0.5, 1, NA, NA, NA, NA, 1, 0.5, NA, NA, 0.5, 1),
    nrow = 4
  )

  # Both feature flags disabled
  stateNoAdapt <- SetupAnalysisPC(
    vWeights = wi, mGMatrix = g, strTestType = "Dunnett",
    dAlpha = 0.025, vInfoFracts = c(0.5, 1.0),
    bSelection = FALSE, bUpdateStrategy = FALSE,
    bPlotGraphs = FALSE
  )
  stateNoAdapt <- AnalyzeLook_PC_TestWrapper(
    stateNoAdapt,
    vRawPVals = c(H1 = 0.30, H2 = 0.35, H3 = 0.40, H4 = 0.45),
    mCorrelation = corr,
    bPlotGraphs = FALSE
  )

  # Selection = FALSE: providing selection must error
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      stateNoAdapt,
      vRawPVals = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      vSelectHypo = c("H1", "H2"), bPlotGraphs = FALSE
    ),
    regexp = "selection was disabled"
  )

  # UpdateStrategy = FALSE: providing new_weights/new_G must error
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      stateNoAdapt,
      vRawPVals = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      vWeights = c(H1 = 0.5, H2 = 0.5, H3 = 0, H4 = 0),
      mGMatrix = g, bPlotGraphs = FALSE
    ),
    regexp = "strategy updates were disabled"
  )
})

testthat::test_that("AnalyzeLookPC: selection validity at look > 1", {
  wi <- c(0.5, 0.5, 0, 0)
  g <- matrix(
    c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0),
    byrow = TRUE, nrow = 4
  )
  corr <- matrix(
    c(1, 0.5, NA, NA, 0.5, 1, NA, NA, NA, NA, 1, 0.5, NA, NA, 0.5, 1),
    nrow = 4
  )

  state1 <- SetupAnalysisPC(
    vWeights = wi, mGMatrix = g, strTestType = "Dunnett",
    dAlpha = 0.025, vInfoFracts = c(0.5, 1.0),
    bSelection = TRUE, bUpdateStrategy = FALSE,
    bPlotGraphs = FALSE
  )
  state1 <- AnalyzeLook_PC_TestWrapper(
    state1,
    vRawPVals = c(H1 = 0.30, H2 = 0.35, H3 = 0.40, H4 = 0.45),
    mCorrelation = corr,
    bPlotGraphs = FALSE
  )

  # Non-character selection
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      vRawPVals = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      vSelectHypo = c(1L, 2L), bPlotGraphs = FALSE
    ),
    regexp = "selection must be a character"
  )

  # selection not a subset of active IndexSet
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      vRawPVals = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      vSelectHypo = c("H1", "H5"), bPlotGraphs = FALSE
    ),
    regexp = "subset of current IndexSet"
  )

  # Empty selection vector
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      vRawPVals = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      vSelectHypo = character(0), bPlotGraphs = FALSE
    ),
    regexp = "retain at least one hypothesis"
  )

  # Valid selection proceeds without error
  testthat::expect_no_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      vRawPVals = c(H1 = 0.10, H2 = 0.15),
      vSelectHypo = c("H1", "H2"), bPlotGraphs = FALSE
    )
  )
})

testthat::test_that("AnalyzeLookPC: strategy update validity at look > 1", {
  wi <- c(0.5, 0.5, 0, 0)
  g <- matrix(
    c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0),
    byrow = TRUE, nrow = 4
  )
  corr <- matrix(
    c(1, 0.5, NA, NA, 0.5, 1, NA, NA, NA, NA, 1, 0.5, NA, NA, 0.5, 1),
    nrow = 4
  )

  state1 <- SetupAnalysisPC(
    vWeights = wi, mGMatrix = g, strTestType = "Dunnett",
    dAlpha = 0.025, vInfoFracts = c(0.5, 1.0),
    bSelection = FALSE, bUpdateStrategy = TRUE,
    bPlotGraphs = FALSE
  )
  state1 <- AnalyzeLook_PC_TestWrapper(
    state1,
    vRawPVals = c(H1 = 0.30, H2 = 0.35, H3 = 0.40, H4 = 0.45),
    mCorrelation = corr,
    bPlotGraphs = FALSE
  )

  vValidNewW <- c(H1 = 0.5, H2 = 0.5, H3 = 0, H4 = 0)
  gValidNew <- matrix(
    c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0),
    byrow = TRUE, nrow = 4
  )

  # Only new_weights without new_G must error
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      vRawPVals = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      vWeights = vValidNewW, bPlotGraphs = FALSE
    ),
    regexp = "Both new_weights and new_G"
  )

  # Only new_G without new_weights must error
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      vRawPVals = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      mGMatrix = gValidNew, bPlotGraphs = FALSE
    ),
    regexp = "Both new_weights and new_G"
  )

  # Non-numeric new_weights must error
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      vRawPVals = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      vWeights = c(H1 = "a", H2 = "b", H3 = "c", H4 = "d"),
      mGMatrix = gValidNew, bPlotGraphs = FALSE
    ),
    regexp = "new_weights must be numeric"
  )

  # Unnamed new_weights with wrong length must error
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      vRawPVals = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      vWeights = c(0.5, 0.5),
      mGMatrix = gValidNew, bPlotGraphs = FALSE
    ),
    regexp = "Unnamed new_weights must have length"
  )

  # new_weights with names not matching IndexSet must error
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      vRawPVals = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      vWeights = c(X1 = 0.5, X2 = 0.5, X3 = 0, X4 = 0),
      mGMatrix = gValidNew, bPlotGraphs = FALSE
    ),
    regexp = "new_weights names must match"
  )

  # new_weights summing to > 1 must error
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      vRawPVals = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      vWeights = c(H1 = 0.7, H2 = 0.7, H3 = 0, H4 = 0),
      mGMatrix = gValidNew, bPlotGraphs = FALSE
    ),
    regexp = "new_weights must sum"
  )

  # new_G not a matrix must error
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      vRawPVals = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      vWeights = vValidNewW,
      mGMatrix = c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0),
      bPlotGraphs = FALSE
    ),
    regexp = "new_G must be a matrix"
  )

  # new_G with wrong dimensions must error
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      vRawPVals = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      vWeights = vValidNewW,
      mGMatrix = matrix(c(0, 0.5, 0.5, 0.5, 0, 0.5, 0.5, 0.5, 0), byrow = TRUE, nrow = 3),
      bPlotGraphs = FALSE
    ),
    regexp = "square matrix of size"
  )

  # new_G with negative entries must error
  gNeg <- gValidNew
  gNeg[1, 2] <- -0.1
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      vRawPVals = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      vWeights = vValidNewW,
      mGMatrix = gNeg, bPlotGraphs = FALSE
    ),
    regexp = "new_G must be non-negative"
  )

  # new_G with row sum > 1 must error
  gBadRowSum <- matrix(
    c(0, 0.8, 0.8, 0, 0.5, 0, 0, 0.5, 0, 0, 0, 1, 0, 0, 1, 0),
    byrow = TRUE, nrow = 4
  )
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      vRawPVals = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      vWeights = vValidNewW,
      mGMatrix = gBadRowSum, bPlotGraphs = FALSE
    ),
    regexp = "row sum"
  )
})

testthat::test_that("AnalyzeLookPC: correlation update validity at look > 1", {
  wi <- c(0.5, 0.5, 0, 0)
  g <- matrix(
    c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0),
    byrow = TRUE, nrow = 4
  )
  corr <- matrix(
    c(1, 0.5, 0.2, 0.2, 0.5, 1, 0.2, 0.2, 0.2, 0.2, 1, 0.5, 0.2, 0.2, 0.5, 1),
    nrow = 4
  )

  state1 <- SetupAnalysisPC(
    vWeights = wi, mGMatrix = g, strTestType = "Dunnett",
    dAlpha = 0.025, vInfoFracts = c(0.5, 1.0),
    bSelection = FALSE, bUpdateStrategy = FALSE,
    bPlotGraphs = FALSE
  )
  state1 <- AnalyzeLook_PC_TestWrapper(
    state1,
    vRawPVals = c(H1 = 0.30, H2 = 0.35, H3 = 0.40, H4 = 0.45),
    mCorrelation = corr,
    bPlotGraphs = FALSE
  )

  # Correlation not a matrix must error
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      vRawPVals = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      mCorrelation = c(1, 0.5, 0.5, 1), bPlotGraphs = FALSE
    ),
    regexp = "Correlation must be a matrix"
  )

  # Wrong dimensions (2 x 2 instead of 4 x 4) must error
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      vRawPVals = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      mCorrelation = matrix(c(1, 0.5, 0.5, 1), nrow = 2),
      bPlotGraphs = FALSE
    ),
    regexp = "Correlation must be a"
  )

  # Non-symmetric Correlation must error
  corrAsym <- corr
  corrAsym[1, 2] <- 0.3  # differs from corrAsym[2, 1] = 0.5
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      vRawPVals = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      mCorrelation = corrAsym, bPlotGraphs = FALSE
    ),
    regexp = "Correlation must be symmetric"
  )

  # Diagonal not equal to 1 must error
  corrBadDiag <- corr
  corrBadDiag[1, 1] <- 0.9
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      vRawPVals = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      mCorrelation = corrBadDiag, bPlotGraphs = FALSE
    ),
    regexp = "diagonal must be 1"
  )

  # Entry outside [-1, 1] must error
  corrOutOfRange <- corr
  corrOutOfRange[1, 2] <- corrOutOfRange[2, 1] <- 1.5
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      vRawPVals = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      mCorrelation = corrOutOfRange, bPlotGraphs = FALSE
    ),
    regexp = "entries must be in"
  )

  # Valid Correlation proceeds without error
  corrValid <- matrix(
    c(1, 0.3, NA, NA, 0.3, 1, NA, NA, NA, NA, 1, 0.4, NA, NA, 0.4, 1),
    nrow = 4
  )
  testthat::expect_warning(
    state2 <- AnalyzeLook_PC_TestWrapper(
      state1,
      vRawPVals = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      mCorrelation = corrValid, bPlotGraphs = FALSE
    ),
    regexp = "converted to 'Partly-Parametric'"
  )
  testthat::expect_identical(state2$mcpObj$test.type, "Partly-Parametric")
  testthat::expect_identical(state2$design_params$test.type, "Partly-Parametric")
})

testthat::test_that("AnalyzeLookPC: look-level sized correlation update maps to initial hypothesis universe", {
  wi <- c(0.5, 0.5, 0, 0)
  g <- matrix(
    c(0, 1, 0, 0,
      1, 0, 0, 0,
      0, 0, 0, 1,
      0, 0, 1, 0),
    byrow = TRUE, nrow = 4
  )

  corr_l1 <- matrix(
    c(1, 0.5, 0.2, 0.2,
      0.5, 1, 0.2, 0.2,
      0.2, 0.2, 1, 0.5,
      0.2, 0.2, 0.5, 1),
    byrow = TRUE, nrow = 4
  )

  state <- SetupAnalysisPC(
    vWeights = wi, mGMatrix = g, strTestType = "Dunnett",
    dAlpha = 0.025, vInfoFracts = c(0.5, 1.0),
    bSelection = TRUE, bUpdateStrategy = TRUE,
    bPlotGraphs = FALSE
  )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.30, H2 = 0.35, H3 = 0.40, H4 = 0.45),
    mCorrelation = corr_l1,
    bPlotGraphs = FALSE
  )

  new_weights <- c(H1 = 0.5, H2 = 0.5, H4 = 0)
  new_G <- matrix(
    c(0, 1, 0,
      1, 0, 0,
      1, 0, 0),
    byrow = TRUE, nrow = 3
  )

  corr_l2_subset <- matrix(
    c(1, 0.3, 0.25,
      0.3, 1, 0.35,
      0.25, 0.35, 1),
    byrow = TRUE, nrow = 3
  )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    vRawPVals = c(H1 = 0.10, H2 = 0.15, H4 = 0.20),
    vSelectHypo = c("H1", "H2", "H4"),
    vWeights = new_weights,
    mGMatrix = new_G,
    mCorrelation = corr_l2_subset,
    bPlotGraphs = FALSE
  )

  testthat::expect_equal(dim(state$mcpObj$Correlation), c(4, 4))
  
  # Set dimnames to match what extraction returns
  dimnames(corr_l2_subset) <- list(c("H1", "H2", "H4"), c("H1", "H2", "H4"))
  testthat::expect_equal(
    state$mcpObj$Correlation[c("H1", "H2", "H4"), c("H1", "H2", "H4")],
    corr_l2_subset
  )
  testthat::expect_equal(state$completed_looks, 2L)
})

testthat::test_that("AnalyzeLookPC: Dunnett stays Dunnett when updated correlation is fully specified", {
  wi <- c(0.5, 0.5, 0, 0)
  g <- matrix(
    c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0),
    byrow = TRUE, nrow = 4
  )
  corr <- matrix(
    c(1, 0.5, 0.2, 0.2, 0.5, 1, 0.2, 0.2, 0.2, 0.2, 1, 0.5, 0.2, 0.2, 0.5, 1),
    nrow = 4
  )

  state1 <- SetupAnalysisPC(
    vWeights = wi, mGMatrix = g, strTestType = "Dunnett",
    dAlpha = 0.025, vInfoFracts = c(0.5, 1.0),
    bSelection = FALSE, bUpdateStrategy = FALSE,
    bPlotGraphs = FALSE
  )
  state1 <- AnalyzeLook_PC_TestWrapper(
    state1,
    vRawPVals = c(H1 = 0.30, H2 = 0.35, H3 = 0.40, H4 = 0.45),
    mCorrelation = corr,
    bPlotGraphs = FALSE
  )

  corrValid <- matrix(
    c(1, 0.3, 0.2, 0.2, 0.3, 1, 0.2, 0.2, 0.2, 0.2, 1, 0.4, 0.2, 0.2, 0.4, 1),
    nrow = 4
  )
  state2 <- testthat::expect_no_warning(
    AnalyzeLook_PC_TestWrapper(
      state1,
      vRawPVals = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      mCorrelation = corrValid, bPlotGraphs = FALSE
    )
  )
  testthat::expect_identical(state2$mcpObj$test.type, "Dunnett")
  testthat::expect_identical(state2$design_params$test.type, "Dunnett")
})

testthat::test_that("AnalyzeLookPC output validation", {
    G <- matrix(c(0,0.5,0.5,0,
                0.5,0,0,0.5,
                0,1,0,0,
                1,0,0,0),
                nrow = 4, byrow = TRUE)

    corr <- matrix(c(1,0.5,NA,NA,
                0.5,1,NA,NA,
                NA,NA,1,0.5,
                NA,NA,0.5,1),
                byrow=TRUE, nrow= 4)

    # Setting up the design
    state <- SetupAnalysisPC(vWeights = c(1/2,1/2,0,0), mGMatrix = G, strTestType = "Partly-Parametric",
                dAlpha = 0.025, vInfoFracts = c(0.5,0.7,1), strDesignType = "asOF",
                bPlotGraphs = FALSE)

    # print("Initial design:")
    # print(state)

    # Look 1 Analysis
    state <- AnalyzeLookPC(state, nLook = 1,
                vRawPVals = c(H1 = 0.01, H2 = 0.20, H3 = 0.15, H4 = 0.30),
                mCorrelation  = corr, bPlotGraphs = FALSE)

    act_out <- state$mcpObj

    # print("Look 1 actual output:")
    # print(act_out)

    exp_out <- readRDS(testthat::test_path("PC-test-01.l1.mcpObj.rds"))
    
    # print("Look 1 expected output:")
    # print(exp_out)

    testthat::expect_true( CompareImportantMcpMembers( act_out, exp_out ) )

    # Look 2 Analysis
    state <- AnalyzeLookPC(state, nLook = 2,
                vRawPVals = c(H1 = 0.02, H2 = 0.10, H4 = 0.40),
                vSelectHypo = c("H1", "H2", "H4"), bPlotGraphs = FALSE)

    exp_out <- readRDS(testthat::test_path("PC-test-01.l2.mcpObj.rds"))
    act_out <- state$mcpObj

    testthat::expect_true( CompareImportantMcpMembers( act_out, exp_out ) )

    # Look 3 Analysis
    state <- AnalyzeLookPC(state, nLook = 3,
                vRawPVals = c(H2 = 0.005, H4 = 0.10), bPlotGraphs = FALSE)

    exp_out <- readRDS(testthat::test_path("PC-test-01.l3.mcpObj.rds"))
    act_out <- state$mcpObj

    testthat::expect_true( CompareImportantMcpMembers( act_out, exp_out ) )
})

testthat::test_that("AnalyzeLookPC preserves design-time testing quantities", {
  vWi <- c(0.5, 0.5)
  mG <- matrix(c(0, 1, 1, 0), nrow = 2, byrow = TRUE)

  state <- SetupAnalysisPC(
    vWeights = vWi,
    mGMatrix = mG,
    strTestType = "Sidak",
    dAlpha = 0.025,
    vInfoFracts = c(0.5, 1.0),
    bPlotGraphs = FALSE
  )

  expectedInvNormWeights <- state$mcpObj$InvNormWeights
  expectedWNorm <- state$mcpObj$W_Norm
  expectedBdryTab <- state$mcpObj$bdryTab
  expectedThresholds <- state$thresholds

  state <- AnalyzeLookPC(
    cState = state,
    vRawPVals = c(H1 = 0.20, H2 = 0.25),
    bPlotGraphs = FALSE
  )

  testthat::expect_equal(state$mcpObj$CutOff, expectedThresholds[1])
  testthat::expect_equal(state$mcpObj$InvNormWeights, expectedInvNormWeights)
  testthat::expect_equal(state$mcpObj$W_Norm, expectedWNorm)
  testthat::expect_equal(state$mcpObj$bdryTab, expectedBdryTab)
  testthat::expect_equal(state$mcpObj$LastLook, 2)
  testthat::expect_false(state$trial_completed)

  state <- AnalyzeLookPC(
    cState = state,
    vRawPVals = c(H1 = 0.20, H2 = 0.25),
    bPlotGraphs = FALSE
  )

  testthat::expect_equal(state$mcpObj$CutOff, expectedThresholds[2])
  testthat::expect_equal(state$mcpObj$InvNormWeights, expectedInvNormWeights)
  testthat::expect_equal(state$mcpObj$W_Norm, expectedWNorm)
  testthat::expect_equal(state$mcpObj$bdryTab, expectedBdryTab)
  testthat::expect_true(state$trial_completed)
  testthat::expect_identical(state$completion_reason, "final_look")
})
