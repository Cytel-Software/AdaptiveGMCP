AnalyzeLook_PC_TestWrapper <- function( state, ... )
{
  lArgs <- list( ... )
  if( inherits( state, "PCAnalysisState" ) && is.null( lArgs$info_frac_cur ) )
  {
    nNextLook <- state$completed_looks + 1L
    nPlannedLooks <- length( state$design_params$info_frac )
    lArgs$info_frac_cur <- state$design_params$info_frac[ min( nNextLook, nPlannedLooks ) ]
  }

  lArgs$state <- state
  return( do.call( AnalyzeLook_PC, lArgs ) )
}

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

  state <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = tt,
    alpha = alp,
    info_frac = t,
    typeOfDesign = des,
    plotGraphs = FALSE
  )

  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(state$completed_looks, 0L)

  # Look 1
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.01, H2 = 0.20, H3 = 0.15, H4 = 0.30),
    Correlation = corr,
    plotGraphs = FALSE
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
    p_raw = c(H1 = 0.02, H2 = 0.10, H4 = 0.40),
    selection = c("H1", "H2", "H4"),
    plotGraphs = FALSE
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
    p_raw = c(H2 = 0.05, H4 = 0.10),
    plotGraphs = FALSE
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

  state <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = tt,
    alpha = alp,
    info_frac = t,
    typeOfDesign = des,
    plotGraphs = FALSE,
    MultipleWinners = FALSE
  )

  # Look 1
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.03, H2 = 0.20, H3 = 0.10, H4 = 0.25),
    Correlation = corr,
    plotGraphs = FALSE
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
    p_raw = c(H1 = 0.02, H2 = 0.12, H3 = 0.08, H4 = 0.20),
    new_weights = new_weights,
    new_G = new_G,
    plotGraphs = FALSE
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

  state <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = tt,
    alpha = alp,
    info_frac = t,
    typeOfDesign = des,
    plotGraphs = FALSE,
    MultipleWinners = TRUE
  )

  exp_out <- readRDS(testthat::test_path("t3.bdryTab.rds"))
  testthat::expect_equal(state$mcpObj$bdryTab, exp_out)

  # Look 1
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.04, H2 = 0.18, H3 = 0.12, H4 = 0.22),
    Correlation = corr,
    plotGraphs = FALSE
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
    p_raw = c(H1 = 0.03, H2 = 0.10, H3 = 0.08),
    selection = selection,
    new_weights = new_weights,
    new_G = new_G,
    Correlation = Correlation,
    plotGraphs = FALSE
  )

  testthat::expect_equal(state$completed_looks, 2L)

  exp_out <- readRDS(testthat::test_path("t3.l2.AdjPValues.rds"))
  testthat::expect_equal(state$mcpObj$AdjPValues, exp_out)

  g <- PlotAnalysisGraph(state, stage = 2)
  testthat::expect_true(!is.null(g))
})

############
# Test 4: look argument validation # COMPLETED
testthat::test_that("AnalyzeLook_PC: look argument validation and error handling", {
  # Minimal 2-hypothesis, 2-look setup (no option guard needed — tests error conditions and
  # completed_looks only, not specific computed output values)
  wi <- c(0.5, 0.5)
  g <- matrix(c(0, 1, 1, 0), byrow = TRUE, nrow = 2)
  corr <- matrix(c(1, 0.5, 0.5, 1), byrow = TRUE, nrow = 2)

  state0 <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = "Bonf",
    alpha = 0.025,
    info_frac = c(0.5, 1.0),
    plotGraphs = FALSE
  )

  # --- Pure validation errors (fired before any computation) ---

  # look mismatch: state expects look 1, user passes look = 2
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(state0, p_raw = c(H1 = 0.10, H2 = 0.20), look = 2L, plotGraphs = FALSE),
    regexp = "does not match the expected next look"
  )

  # look is non-numeric
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(state0, p_raw = c(H1 = 0.10, H2 = 0.20), look = "a", plotGraphs = FALSE),
    regexp = "single positive integer"
  )

  # look is non-integer numeric (1.5)
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(state0, p_raw = c(H1 = 0.10, H2 = 0.20), look = 1.5, plotGraphs = FALSE),
    regexp = "single positive integer"
  )

  # look = 0 (not positive)
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(state0, p_raw = c(H1 = 0.10, H2 = 0.20), look = 0L, plotGraphs = FALSE),
    regexp = "single positive integer"
  )

  # --- Correct usage ---

  # look = NULL (default): proceeds normally and completed_looks advances
  state1_null <- AnalyzeLook_PC_TestWrapper(state0, p_raw = c(H1 = 0.10, H2 = 0.20), look = NULL, plotGraphs = FALSE)
  testthat::expect_equal(state1_null$completed_looks, 1L)

  # look = 1L (correct explicit value): same result as omitting look
  state1_expl <- AnalyzeLook_PC_TestWrapper(state0, p_raw = c(H1 = 0.10, H2 = 0.20), look = 1L, plotGraphs = FALSE)
  testthat::expect_equal(state1_expl$completed_looks, 1L)

  testthat::expect_equal(state1_null, state1_expl)

  # --- After final look: specific "final look" error ---

  # Run look 2 to complete the trial
  state2 <- AnalyzeLook_PC_TestWrapper(state1_null, p_raw = c(H1 = 0.10, H2 = 0.20), look = 2L, plotGraphs = FALSE)
  testthat::expect_true(state2$trial_completed)

  # Calling again after final look must name the final look in the error message
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(state2, p_raw = c(H1 = 0.10, H2 = 0.20), plotGraphs = FALSE),
    regexp = "was the final look"
  )

  # --- Early stopping: trial already concluded before final look ---

  # Simulate early-stopped state (trial_completed = TRUE before the final look is reached)
  state_early_stopped <- state1_null
  state_early_stopped$trial_completed <- TRUE   # completed_looks (1) < LastLook (2)

  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(state_early_stopped, p_raw = c(H1 = 0.10, H2 = 0.20), plotGraphs = FALSE),
    regexp = "Trial already concluded"
  )
})

############
# Test 5: Always-on structural invariant test (no skip guard, no RDS fixtures) # COMPLETED
testthat::test_that("AnalyzeLook_PC: structural invariants hold across looks", {
  wi <- c(0.5, 0.5)
  g <- matrix(c(0, 1, 1, 0), byrow = TRUE, nrow = 2)
  corr <- matrix(c(1, 0.5, 0.5, 1), byrow = TRUE, nrow = 2)

  state <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = "Bonf",
    alpha = 0.025,
    info_frac = c(0.5, 1.0),
    plotGraphs = FALSE
  )

  # Initial state
  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(state$completed_looks, 0L)
  testthat::expect_false(state$trial_completed)
  testthat::expect_equal(state$mcpObj$IndexSet, c("H1", "H2"))

  # Look 1: completed_looks advances, AdjPValues is populated, trial not yet complete
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.10, H2 = 0.20),
    plotGraphs = FALSE
  )
  testthat::expect_equal(state$completed_looks, 1L)
  testthat::expect_false(state$trial_completed)
  testthat::expect_true(is.data.frame(state$mcpObj$AdjPValues))
  testthat::expect_true(nrow(state$mcpObj$AdjPValues) > 0)
  testthat::expect_true(length(state$mcpObj$IndexSet) > 0)

  # Look 2 (final look): trial_completed flips to TRUE, completed_looks = 2
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.10, H2 = 0.20),
    plotGraphs = FALSE
  )
  testthat::expect_equal(state$completed_looks, 2L)
  testthat::expect_true(state$trial_completed)

  # Calling again after trial is complete must error
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(state, p_raw = c(H1 = 0.10, H2 = 0.20), plotGraphs = FALSE)
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
  state <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = tt,
    alpha = alp,
    info_frac = t,
    plotGraphs = FALSE
  )

  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(state$completed_looks, 0L)
  testthat::expect_equal(state$mcpObj$IndexSet, c("H1", "H2", "H3"))

  # Look 1 (final)
  # Raw p-values from file: p1=0.02, p2=0.055, p3=0.012
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.02, H2 = 0.055, H3 = 0.012),
    plotGraphs = FALSE
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
  state <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = tt,
    alpha = alp,
    info_frac = t,
    plotGraphs = FALSE
  )

  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(state$completed_looks, 0L)
  testthat::expect_equal(state$mcpObj$IndexSet, c("H1", "H2", "H3", "H4"))

  # Look 1 (final)
  # Raw p-values from file: p1=0.04, p2=0.01, p3=0.03, p4=0.04
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.04, H2 = 0.01, H3 = 0.03, H4 = 0.04),
    plotGraphs = FALSE
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
  state <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = tt,
    alpha = alp,
    info_frac = t,
    plotGraphs = FALSE
  )

  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(state$completed_looks, 0L)

  # Look 1 (final)
  # Raw p-values from file: p1=0.076, p2=0.035, p3=0.563, p4=0.407
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.076, H2 = 0.035, H3 = 0.563, H4 = 0.407),
    plotGraphs = FALSE
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
  state <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = tt,
    alpha = alp,
    info_frac = t,
    typeOfDesign = des,
    MultipleWinners = TRUE,
    plotGraphs = FALSE
  )

  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(state$completed_looks, 0L)
  testthat::expect_equal(state$mcpObj$IndexSet, c("H1", "H2", "H3", "H4"))

  # Look 1
  # Raw p-values from file: p1=0.00045, p2=0.0952, p3=0.0225, p4=0.1104
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.00045, H2 = 0.0952, H3 = 0.0225, H4 = 0.1104),
    Correlation = corr,
    plotGraphs = FALSE
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
    p_raw = c(H2 = 0.1121, H3 = 0.0112, H4 = 0.1153),
    plotGraphs = FALSE
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
  state <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = tt,
    alpha = alp,
    info_frac = t,
    typeOfDesign = des,
    MultipleWinners = TRUE,
    plotGraphs = FALSE
  )

  # Look 1 (same as Test 7)
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.00045, H2 = 0.0952, H3 = 0.0225, H4 = 0.1104),
    Correlation = corr,
    plotGraphs = FALSE
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
    p_raw = c(H2 = 0.0299, H3 = 0.0225, H4 = 0.0586),
    new_weights = new_weights,
    new_G = new_G,
    plotGraphs = FALSE
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
  state <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = tt,
    alpha = alp,
    info_frac = t,
    plotGraphs = FALSE
  )

  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(state$completed_looks, 0L)

  # Look 1 (final)
  # Raw p-values from file: p1=0.01, p2=0.005, p3=0.015, p4=0.022
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.01, H2 = 0.005, H3 = 0.015, H4 = 0.022),
    plotGraphs = FALSE
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
  state <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = tt,
    alpha = alp,
    info_frac = t,
    plotGraphs = FALSE
  )

  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(state$completed_looks, 0L)

  # Look 1 (final)
  # Raw p-values from file: p1=0.01, p2=0.02, p3=0.005, p4=0.5
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.01, H2 = 0.02, H3 = 0.005, H4 = 0.5),
    Correlation = corr,
    plotGraphs = FALSE
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
  state <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = tt,
    alpha = alp,
    info_frac = t,
    plotGraphs = FALSE
  )

  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(state$completed_looks, 0L)
  testthat::expect_equal(state$mcpObj$IndexSet, c("H1", "H2", "H3", "H4"))

  # Look 1 (final)
  # Raw p-values from file: p1=0.638, p2=0.01, p3=0.007, p4=3.959E-4
  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.638, H2 = 0.01, H3 = 0.007, H4 = 3.959e-4),
    Correlation = corr,
    plotGraphs = FALSE
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

  state <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = "Dunnett",
    alpha = 0.025,
    info_frac = c(0.5, 1.0),
    typeOfDesign = "asP",
    MultipleWinners = TRUE,
    Selection = FALSE,
    UpdateStrategy = FALSE,
    plotGraphs = FALSE
  )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.30, H2 = 0.35, H3 = 0.40, H4 = 0.45),
    Correlation = corr,
    plotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M07.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.08, H2 = 0.09, H3 = 0.12, H4 = 0.15),
    plotGraphs = FALSE
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

  state <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = "Dunnett",
    alpha = 0.025,
    info_frac = c(0.5, 1.0),
    typeOfDesign = "asHSD",
    gammaA = 2.5,
    MultipleWinners = TRUE,
    Selection = FALSE,
    UpdateStrategy = FALSE,
    plotGraphs = FALSE
  )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.25, H2 = 0.30, H3 = 0.35, H4 = 0.40),
    Correlation = corr,
    plotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M11.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.07, H2 = 0.09, H3 = 0.10, H4 = 0.12),
    plotGraphs = FALSE
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

  state <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = "Dunnett",
    alpha = 0.025,
    info_frac = c(0.5, 1.0),
    typeOfDesign = "asKD",
    gammaA = 2.5,
    MultipleWinners = TRUE,
    Selection = FALSE,
    UpdateStrategy = FALSE,
    plotGraphs = FALSE
  )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.28, H2 = 0.31, H3 = 0.33, H4 = 0.37),
    Correlation = corr,
    plotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M12.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.06, H2 = 0.09, H3 = 0.11, H4 = 0.13),
    plotGraphs = FALSE
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

  state <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = "Dunnett",
    alpha = 0.025,
    info_frac = c(0.5, 1.0),
    typeOfDesign = "asOF",
    MultipleWinners = TRUE,
    Selection = FALSE,
    UpdateStrategy = FALSE,
    plotGraphs = FALSE
  )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.26, H2 = 0.29, H3 = 0.34, H4 = 0.38),
    Correlation = corr,
    plotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M19.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.07, H2 = 0.08, H3 = 0.11, H4 = 0.14),
    Correlation = new_corr,
    plotGraphs = FALSE
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

  state <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = "Dunnett",
    alpha = 0.025,
    info_frac = c(0.5, 1.0),
    typeOfDesign = "asOF",
    MultipleWinners = FALSE,
    Selection = FALSE,
    UpdateStrategy = FALSE,
    plotGraphs = FALSE
  )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 1e-4, H2 = 0.40, H3 = 0.45, H4 = 0.50),
    Correlation = corr,
    plotGraphs = FALSE
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

  state <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = "Dunnett",
    alpha = 0.025,
    info_frac = c(0.5, 1.0),
    typeOfDesign = "asOF",
    MultipleWinners = FALSE,
    Selection = FALSE,
    UpdateStrategy = FALSE,
    plotGraphs = FALSE
  )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.30, H2 = 0.35, H3 = 0.38, H4 = 0.42),
    Correlation = corr,
    plotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M24.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.02, H2 = 0.06, H3 = 0.10, H4 = 0.20),
    plotGraphs = FALSE
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

  state <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = "Dunnett",
    alpha = 0.025,
    info_frac = c(0.5, 1.0),
    typeOfDesign = "WT",
    deltaWT = 0.25,
    MultipleWinners = TRUE,
    Selection = FALSE,
    UpdateStrategy = FALSE,
    plotGraphs = FALSE
  )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.28, H2 = 0.32, H3 = 0.36, H4 = 0.40),
    Correlation = corr,
    plotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M09.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.07, H2 = 0.10, H3 = 0.13, H4 = 0.16),
    plotGraphs = FALSE
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

  state <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = "Dunnett",
    alpha = 0.025,
    info_frac = c(0.5, 1.0),
    typeOfDesign = "asUser",
    userAlphaSpending = c(0.008, 0.025),
    MultipleWinners = TRUE,
    Selection = FALSE,
    UpdateStrategy = FALSE,
    plotGraphs = FALSE
  )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.29, H2 = 0.33, H3 = 0.37, H4 = 0.41),
    Correlation = corr,
    plotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M13.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.07, H2 = 0.09, H3 = 0.12, H4 = 0.15),
    plotGraphs = FALSE
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

  state <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = "Dunnett",
    alpha = 0.025,
    info_frac = c(0.5, 1.0),
    typeOfDesign = "asOF",
    MultipleWinners = TRUE,
    Selection = TRUE,
    UpdateStrategy = FALSE,
    plotGraphs = FALSE
  )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.21, H2 = 0.24, H3 = 0.30, H4 = 0.35),
    Correlation = corr,
    plotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M17.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.05, H2 = 0.07, H4 = 0.11),
    selection = c("H1", "H2", "H4"),
    plotGraphs = FALSE
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

  state <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = "Dunnett",
    alpha = 0.025,
    info_frac = c(0.5, 1.0),
    typeOfDesign = "noEarlyEfficacy",
    MultipleWinners = TRUE,
    Selection = FALSE,
    UpdateStrategy = FALSE,
    plotGraphs = FALSE
  )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.31, H2 = 0.34, H3 = 0.38, H4 = 0.43),
    Correlation = corr,
    plotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M08.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.08, H2 = 0.10, H3 = 0.12, H4 = 0.16),
    plotGraphs = FALSE
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

  state <- SetupAnalysis_PC(
    WI = wi,
    G = g,
    test.type = "Sidak",
    alpha = 0.025,
    info_frac = c(1.0),
    typeOfDesign = "asOF",
    MultipleWinners = TRUE,
    Selection = FALSE,
    UpdateStrategy = FALSE,
    plotGraphs = FALSE
  )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.01, H2 = 0.02, H3 = 0.03, H4 = 0.04),
    plotGraphs = FALSE
  )

  exp_mcp <- readRDS(testthat::test_path("M02.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )
  testthat::expect_true(state$trial_completed)
})

############################################################################
# Issue #75 matrix row M10: PT design — DEFERRED (N/A for this pass)
# rpact requires deltaPT0 which is not yet exposed by SetupAnalysis_PC() or
# adaptGMCP_PC(). This row is marked N/A for this pass. Add a fixture-backed
# equivalence test once deltaPT0 parameter support is added to the API.
############################################################################

############################################################################
# Issue #67 Phase 1: SetupAnalysis_PC() contract-validation tests
############################################################################

testthat::test_that("SetupAnalysis_PC: WI validation - type, NA, negativity, and sum", {
  wi <- c(0.5, 0.5, 0, 0)
  g <- matrix(
    c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0),
    byrow = TRUE, nrow = 4
  )

  # Non-numeric WI
  testthat::expect_error(
    SetupAnalysis_PC(
      WI = c("a", "b", "c", "d"), G = g, test.type = "Bonf",
      alpha = 0.025, info_frac = c(1), plotGraphs = FALSE
    ),
    regexp = "numeric, non-negative, and non-NA"
  )

  # WI with NA
  testthat::expect_error(
    SetupAnalysis_PC(
      WI = c(0.5, NA, 0, 0), G = g, test.type = "Bonf",
      alpha = 0.025, info_frac = c(1), plotGraphs = FALSE
    ),
    regexp = "numeric, non-negative, and non-NA"
  )

  # WI with negative value
  testthat::expect_error(
    SetupAnalysis_PC(
      WI = c(0.5, -0.1, 0, 0), G = g, test.type = "Bonf",
      alpha = 0.025, info_frac = c(1), plotGraphs = FALSE
    ),
    regexp = "numeric, non-negative, and non-NA"
  )

  # WI summing to > 1
  testthat::expect_error(
    SetupAnalysis_PC(
      WI = c(0.6, 0.6, 0, 0), G = g, test.type = "Bonf",
      alpha = 0.025, info_frac = c(1), plotGraphs = FALSE
    ),
    regexp = "Sum of WI"
  )

  # WI dimension mismatch with G: WI has 2 elements, G is 4 x 4
  testthat::expect_error(
    SetupAnalysis_PC(
      WI = c(0.5, 0.5), G = g, test.type = "Bonf",
      alpha = 0.025, info_frac = c(1), plotGraphs = FALSE
    ),
    regexp = "transition matrix"
  )
})

testthat::test_that("SetupAnalysis_PC: G validation - matrix requirement and dimensions", {
  wi <- c(0.5, 0.5, 0, 0)

  # G is not a matrix (vector instead)
  testthat::expect_error(
    SetupAnalysis_PC(
      WI = wi, G = c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0),
      test.type = "Bonf", alpha = 0.025, info_frac = c(1), plotGraphs = FALSE
    ),
    regexp = "transition matrix"
  )

  # G has wrong dimensions (3 x 3 instead of 4 x 4)
  gBad <- matrix(c(0, 0.5, 0.5, 0.5, 0, 0.5, 0.5, 0.5, 0), byrow = TRUE, nrow = 3)
  testthat::expect_error(
    SetupAnalysis_PC(
      WI = wi, G = gBad, test.type = "Bonf",
      alpha = 0.025, info_frac = c(1), plotGraphs = FALSE
    ),
    regexp = "transition matrix"
  )
})

testthat::test_that("SetupAnalysis_PC: info_frac validation - length, bounds, and monotonicity", {
  wi <- c(0.5, 0.5, 0, 0)
  g <- matrix(
    c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0),
    byrow = TRUE, nrow = 4
  )

  # Empty info_frac
  testthat::expect_error(
    SetupAnalysis_PC(
      WI = wi, G = g, test.type = "Bonf",
      alpha = 0.025, info_frac = numeric(0), plotGraphs = FALSE
    ),
    regexp = "info_frac must have length"
  )

  # info_frac contains zero (not in (0, 1])
  testthat::expect_error(
    SetupAnalysis_PC(
      WI = wi, G = g, test.type = "Bonf",
      alpha = 0.025, info_frac = c(0, 0.5, 1.0), plotGraphs = FALSE
    ),
    regexp = "info_frac must be in"
  )

  # info_frac contains value > 1
  testthat::expect_error(
    SetupAnalysis_PC(
      WI = wi, G = g, test.type = "Bonf",
      alpha = 0.025, info_frac = c(0.5, 1.2), plotGraphs = FALSE
    ),
    regexp = "info_frac must be in"
  )

  # info_frac not strictly increasing (repeated value)
  testthat::expect_error(
    SetupAnalysis_PC(
      WI = wi, G = g, test.type = "Bonf",
      alpha = 0.025, info_frac = c(0.5, 0.5, 1.0), plotGraphs = FALSE
    ),
    regexp = "strictly increasing"
  )

  # info_frac not strictly increasing (decreasing segment)
  testthat::expect_error(
    SetupAnalysis_PC(
      WI = wi, G = g, test.type = "Bonf",
      alpha = 0.025, info_frac = c(0.7, 0.5, 1.0), plotGraphs = FALSE
    ),
    regexp = "strictly increasing"
  )
})

testthat::test_that("SetupAnalysis_PC: unsupported test.type is rejected", {
  wi <- c(0.5, 0.5, 0, 0)
  g <- matrix(
    c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0),
    byrow = TRUE, nrow = 4
  )

  testthat::expect_error(
    SetupAnalysis_PC(
      WI = wi, G = g, test.type = "Unknown",
      alpha = 0.025, info_frac = c(1), plotGraphs = FALSE
    ),
    regexp = "Unsupported test.type"
  )
})

testthat::test_that("SetupAnalysis_PC: correlation policy by test.type", {
  wi <- c(0.5, 0.5, 0, 0)
  g <- matrix(
    c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0),
    byrow = TRUE, nrow = 4
  )
  # Dunnett and Partly-Parametric defer Correlation to AnalyzeLook_PC
  stateDunnett <- SetupAnalysis_PC(
    WI = wi, G = g, test.type = "Dunnett",
    alpha = 0.025, info_frac = c(1), plotGraphs = FALSE
  )
  testthat::expect_null(stateDunnett$mcpObj$Correlation)

  statePP <- SetupAnalysis_PC(
    WI = wi, G = g, test.type = "Partly-Parametric",
    alpha = 0.025, info_frac = c(1), plotGraphs = FALSE
  )
  testthat::expect_null(statePP$mcpObj$Correlation)

  # Bonf: overrides Correlation to identity with NA off-diagonal
  stateBonf <- SetupAnalysis_PC(
    WI = wi, G = g, test.type = "Bonf",
    alpha = 0.025, info_frac = c(1), plotGraphs = FALSE
  )
  corrExpBonf <- diag(4)
  corrExpBonf[corrExpBonf == 0] <- NA
  testthat::expect_equal(stateBonf$mcpObj$Correlation, corrExpBonf)

  # Sidak: sets Correlation to NA
  stateSidak <- SetupAnalysis_PC(
    WI = wi, G = g, test.type = "Sidak",
    alpha = 0.025, info_frac = c(1), plotGraphs = FALSE
  )
  testthat::expect_true(is.na(stateSidak$mcpObj$Correlation))

  # Simes: sets Correlation to NA
  stateSimes <- SetupAnalysis_PC(
    WI = wi, G = g, test.type = "Simes",
    alpha = 0.025, info_frac = c(1), plotGraphs = FALSE
  )
  testthat::expect_true(is.na(stateSimes$mcpObj$Correlation))
})

testthat::test_that("SetupAnalysis_PC: asUser typeOfDesign requires userAlphaSpending", {
  wi <- c(0.5, 0.5, 0, 0)
  g <- matrix(
    c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0),
    byrow = TRUE, nrow = 4
  )
  # Missing userAlphaSpending when typeOfDesign = 'asUser'
  testthat::expect_error(
    SetupAnalysis_PC(
      WI = wi, G = g, test.type = "Dunnett",
      alpha = 0.025, info_frac = c(0.5, 1.0),
      typeOfDesign = "asUser",
      plotGraphs = FALSE
    ),
    regexp = "userAlphaSpending"
  )

  # Valid asUser call does not error
  testthat::expect_no_error(
    SetupAnalysis_PC(
      WI = wi, G = g, test.type = "Dunnett",
      alpha = 0.025, info_frac = c(0.5, 1.0),
      typeOfDesign = "asUser",
      userAlphaSpending = c(0.008, 0.025),
      plotGraphs = FALSE
    )
  )
})

############################################################################
# Issue #70 Phase 2: AnalyzeLook_PC_TestWrapper() input-validation tests
############################################################################

testthat::test_that("AnalyzeLook_PC: non-PCAnalysisState input is rejected", {
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(list(), p_raw = c(H1 = 0.1), plotGraphs = FALSE),
    regexp = "PCAnalysisState"
  )

  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper("not_a_state", p_raw = c(H1 = 0.1), plotGraphs = FALSE),
    regexp = "PCAnalysisState"
  )
})

testthat::test_that("AnalyzeLook_PC: p_raw contract validation", {
  wi <- c(0.5, 0.5)
  g <- matrix(c(0, 1, 1, 0), byrow = TRUE, nrow = 2)

  state0 <- SetupAnalysis_PC(
    WI = wi, G = g, test.type = "Bonf",
    alpha = 0.025, info_frac = c(0.5, 1.0),
    plotGraphs = FALSE
  )

  # Non-numeric p_raw
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(state0, p_raw = c(H1 = "a", H2 = "b"), plotGraphs = FALSE),
    regexp = "p_raw must be numeric"
  )

  # p_raw contains NA
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(state0, p_raw = c(H1 = 0.1, H2 = NA), plotGraphs = FALSE),
    regexp = "p_raw cannot contain NA"
  )

  # p_raw value below 0
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(state0, p_raw = c(H1 = -0.1, H2 = 0.2), plotGraphs = FALSE),
    regexp = "p_raw values must be in"
  )

  # p_raw value above 1
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(state0, p_raw = c(H1 = 1.1, H2 = 0.2), plotGraphs = FALSE),
    regexp = "p_raw values must be in"
  )

  # Unnamed p_raw with wrong length
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(state0, p_raw = c(0.1, 0.2, 0.3), plotGraphs = FALSE),
    regexp = "Unnamed p_raw must have length"
  )

  # Named p_raw with names not matching current IndexSet
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(state0, p_raw = c(X1 = 0.1, X2 = 0.2), plotGraphs = FALSE),
    regexp = "p_raw names must match"
  )

  # Unnamed p_raw with correct length succeeds
  testthat::expect_no_error(
    AnalyzeLook_PC_TestWrapper(state0, p_raw = c(0.1, 0.2), plotGraphs = FALSE)
  )
})

testthat::test_that("AnalyzeLook_PC: look 1 adaptation prohibition and correlation requirement", {
  wi <- c(0.5, 0.5, 0, 0)
  g <- matrix(
    c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0),
    byrow = TRUE, nrow = 4
  )
  corr <- matrix(
    c(1, 0.5, NA, NA, 0.5, 1, NA, NA, NA, NA, 1, 0.5, NA, NA, 0.5, 1),
    nrow = 4
  )

  state0 <- SetupAnalysis_PC(
    WI = wi, G = g, test.type = "Partly-Parametric",
    alpha = 0.025, info_frac = c(0.5, 1.0),
    plotGraphs = FALSE
  )

  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state0, p_raw = c(H1 = 0.1, H2 = 0.2, H3 = 0.3, H4 = 0.4),
      plotGraphs = FALSE
    ),
    regexp = "Correlation must be provided at look 1"
  )

  # selection at look 1 must error
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state0, p_raw = c(H1 = 0.1, H2 = 0.2, H3 = 0.3, H4 = 0.4),
      Correlation = corr,
      selection = c("H1", "H2"), plotGraphs = FALSE
    ),
    regexp = "selection cannot be applied at look 1"
  )

  # new_weights/new_G at look 1 must error
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state0, p_raw = c(H1 = 0.1, H2 = 0.2, H3 = 0.3, H4 = 0.4),
      Correlation = corr,
      new_weights = c(H1 = 0.5, H2 = 0.5, H3 = 0, H4 = 0),
      new_G = g, plotGraphs = FALSE
    ),
    regexp = "new_weights/new_G cannot be applied at look 1"
  )

  # Correlation at look 1 is valid
  testthat::expect_no_error(
    AnalyzeLook_PC_TestWrapper(
      state0, p_raw = c(H1 = 0.1, H2 = 0.2, H3 = 0.3, H4 = 0.4),
      Correlation = corr, plotGraphs = FALSE
    )
  )
})

############################################################################
# Issue #69 Phase 3: look > 1 adaptation gate and mutation-validation tests
############################################################################

testthat::test_that("AnalyzeLook_PC: feature-flag gates at look > 1", {
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
  stateNoAdapt <- SetupAnalysis_PC(
    WI = wi, G = g, test.type = "Dunnett",
    alpha = 0.025, info_frac = c(0.5, 1.0),
    Selection = FALSE, UpdateStrategy = FALSE,
    plotGraphs = FALSE
  )
  stateNoAdapt <- AnalyzeLook_PC_TestWrapper(
    stateNoAdapt,
    p_raw = c(H1 = 0.30, H2 = 0.35, H3 = 0.40, H4 = 0.45),
    Correlation = corr,
    plotGraphs = FALSE
  )

  # Selection = FALSE: providing selection must error
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      stateNoAdapt,
      p_raw = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      selection = c("H1", "H2"), plotGraphs = FALSE
    ),
    regexp = "Selection was disabled"
  )

  # UpdateStrategy = FALSE: providing new_weights/new_G must error
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      stateNoAdapt,
      p_raw = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      new_weights = c(H1 = 0.5, H2 = 0.5, H3 = 0, H4 = 0),
      new_G = g, plotGraphs = FALSE
    ),
    regexp = "UpdateStrategy was disabled"
  )
})

testthat::test_that("AnalyzeLook_PC: selection validity at look > 1", {
  wi <- c(0.5, 0.5, 0, 0)
  g <- matrix(
    c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0),
    byrow = TRUE, nrow = 4
  )
  corr <- matrix(
    c(1, 0.5, NA, NA, 0.5, 1, NA, NA, NA, NA, 1, 0.5, NA, NA, 0.5, 1),
    nrow = 4
  )

  state1 <- SetupAnalysis_PC(
    WI = wi, G = g, test.type = "Dunnett",
    alpha = 0.025, info_frac = c(0.5, 1.0),
    Selection = TRUE, UpdateStrategy = FALSE,
    plotGraphs = FALSE
  )
  state1 <- AnalyzeLook_PC_TestWrapper(
    state1,
    p_raw = c(H1 = 0.30, H2 = 0.35, H3 = 0.40, H4 = 0.45),
    Correlation = corr,
    plotGraphs = FALSE
  )

  # Non-character selection
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      p_raw = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      selection = c(1L, 2L), plotGraphs = FALSE
    ),
    regexp = "selection must be a character"
  )

  # selection not a subset of active IndexSet
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      p_raw = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      selection = c("H1", "H5"), plotGraphs = FALSE
    ),
    regexp = "subset of current IndexSet"
  )

  # Empty selection vector
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      p_raw = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      selection = character(0), plotGraphs = FALSE
    ),
    regexp = "retain at least one hypothesis"
  )

  # Valid selection proceeds without error
  testthat::expect_no_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      p_raw = c(H1 = 0.10, H2 = 0.15),
      selection = c("H1", "H2"), plotGraphs = FALSE
    )
  )
})

testthat::test_that("AnalyzeLook_PC: strategy update validity at look > 1", {
  wi <- c(0.5, 0.5, 0, 0)
  g <- matrix(
    c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0),
    byrow = TRUE, nrow = 4
  )
  corr <- matrix(
    c(1, 0.5, NA, NA, 0.5, 1, NA, NA, NA, NA, 1, 0.5, NA, NA, 0.5, 1),
    nrow = 4
  )

  state1 <- SetupAnalysis_PC(
    WI = wi, G = g, test.type = "Dunnett",
    alpha = 0.025, info_frac = c(0.5, 1.0),
    Selection = FALSE, UpdateStrategy = TRUE,
    plotGraphs = FALSE
  )
  state1 <- AnalyzeLook_PC_TestWrapper(
    state1,
    p_raw = c(H1 = 0.30, H2 = 0.35, H3 = 0.40, H4 = 0.45),
    Correlation = corr,
    plotGraphs = FALSE
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
      p_raw = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      new_weights = vValidNewW, plotGraphs = FALSE
    ),
    regexp = "Both new_weights and new_G"
  )

  # Only new_G without new_weights must error
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      p_raw = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      new_G = gValidNew, plotGraphs = FALSE
    ),
    regexp = "Both new_weights and new_G"
  )

  # Non-numeric new_weights must error
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      p_raw = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      new_weights = c(H1 = "a", H2 = "b", H3 = "c", H4 = "d"),
      new_G = gValidNew, plotGraphs = FALSE
    ),
    regexp = "new_weights must be numeric"
  )

  # Unnamed new_weights with wrong length must error
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      p_raw = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      new_weights = c(0.5, 0.5),
      new_G = gValidNew, plotGraphs = FALSE
    ),
    regexp = "Unnamed new_weights must have length"
  )

  # new_weights with names not matching IndexSet must error
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      p_raw = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      new_weights = c(X1 = 0.5, X2 = 0.5, X3 = 0, X4 = 0),
      new_G = gValidNew, plotGraphs = FALSE
    ),
    regexp = "new_weights names must match"
  )

  # new_weights summing to > 1 must error
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      p_raw = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      new_weights = c(H1 = 0.7, H2 = 0.7, H3 = 0, H4 = 0),
      new_G = gValidNew, plotGraphs = FALSE
    ),
    regexp = "new_weights must sum"
  )

  # new_G not a matrix must error
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      p_raw = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      new_weights = vValidNewW,
      new_G = c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0),
      plotGraphs = FALSE
    ),
    regexp = "new_G must be a matrix"
  )

  # new_G with wrong dimensions must error
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      p_raw = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      new_weights = vValidNewW,
      new_G = matrix(c(0, 0.5, 0.5, 0.5, 0, 0.5, 0.5, 0.5, 0), byrow = TRUE, nrow = 3),
      plotGraphs = FALSE
    ),
    regexp = "square matrix of size"
  )

  # new_G with negative entries must error
  gNeg <- gValidNew
  gNeg[1, 2] <- -0.1
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      p_raw = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      new_weights = vValidNewW,
      new_G = gNeg, plotGraphs = FALSE
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
      p_raw = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      new_weights = vValidNewW,
      new_G = gBadRowSum, plotGraphs = FALSE
    ),
    regexp = "row sum"
  )
})

testthat::test_that("AnalyzeLook_PC: correlation update validity at look > 1", {
  wi <- c(0.5, 0.5, 0, 0)
  g <- matrix(
    c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0),
    byrow = TRUE, nrow = 4
  )
  corr <- matrix(
    c(1, 0.5, 0.2, 0.2, 0.5, 1, 0.2, 0.2, 0.2, 0.2, 1, 0.5, 0.2, 0.2, 0.5, 1),
    nrow = 4
  )

  state1 <- SetupAnalysis_PC(
    WI = wi, G = g, test.type = "Dunnett",
    alpha = 0.025, info_frac = c(0.5, 1.0),
    Selection = FALSE, UpdateStrategy = FALSE,
    plotGraphs = FALSE
  )
  state1 <- AnalyzeLook_PC_TestWrapper(
    state1,
    p_raw = c(H1 = 0.30, H2 = 0.35, H3 = 0.40, H4 = 0.45),
    Correlation = corr,
    plotGraphs = FALSE
  )

  # Correlation not a matrix must error
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      p_raw = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      Correlation = c(1, 0.5, 0.5, 1), plotGraphs = FALSE
    ),
    regexp = "Correlation must be a matrix"
  )

  # Wrong dimensions (2 x 2 instead of 4 x 4) must error
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      p_raw = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      Correlation = matrix(c(1, 0.5, 0.5, 1), nrow = 2),
      plotGraphs = FALSE
    ),
    regexp = "Correlation must be a"
  )

  # Non-symmetric Correlation must error
  corrAsym <- corr
  corrAsym[1, 2] <- 0.3  # differs from corrAsym[2, 1] = 0.5
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      p_raw = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      Correlation = corrAsym, plotGraphs = FALSE
    ),
    regexp = "Correlation must be symmetric"
  )

  # Diagonal not equal to 1 must error
  corrBadDiag <- corr
  corrBadDiag[1, 1] <- 0.9
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      p_raw = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      Correlation = corrBadDiag, plotGraphs = FALSE
    ),
    regexp = "diagonal must be 1"
  )

  # Entry outside [-1, 1] must error
  corrOutOfRange <- corr
  corrOutOfRange[1, 2] <- corrOutOfRange[2, 1] <- 1.5
  testthat::expect_error(
    AnalyzeLook_PC_TestWrapper(
      state1,
      p_raw = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      Correlation = corrOutOfRange, plotGraphs = FALSE
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
      p_raw = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      Correlation = corrValid, plotGraphs = FALSE
    ),
    regexp = "converted to 'Partly-Parametric'"
  )
  testthat::expect_identical(state2$mcpObj$test.type, "Partly-Parametric")
  testthat::expect_identical(state2$design_params$test.type, "Partly-Parametric")
})

testthat::test_that("AnalyzeLook_PC: look-level sized correlation update maps to initial hypothesis universe", {
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

  state <- SetupAnalysis_PC(
    WI = wi, G = g, test.type = "Dunnett",
    alpha = 0.025, info_frac = c(0.5, 1.0),
    Selection = TRUE, UpdateStrategy = TRUE,
    plotGraphs = FALSE
  )

  state <- AnalyzeLook_PC_TestWrapper(
    state,
    p_raw = c(H1 = 0.30, H2 = 0.35, H3 = 0.40, H4 = 0.45),
    Correlation = corr_l1,
    plotGraphs = FALSE
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
    p_raw = c(H1 = 0.10, H2 = 0.15, H4 = 0.20),
    selection = c("H1", "H2", "H4"),
    new_weights = new_weights,
    new_G = new_G,
    Correlation = corr_l2_subset,
    plotGraphs = FALSE
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

testthat::test_that("AnalyzeLook_PC: Dunnett stays Dunnett when updated correlation is fully specified", {
  wi <- c(0.5, 0.5, 0, 0)
  g <- matrix(
    c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0),
    byrow = TRUE, nrow = 4
  )
  corr <- matrix(
    c(1, 0.5, 0.2, 0.2, 0.5, 1, 0.2, 0.2, 0.2, 0.2, 1, 0.5, 0.2, 0.2, 0.5, 1),
    nrow = 4
  )

  state1 <- SetupAnalysis_PC(
    WI = wi, G = g, test.type = "Dunnett",
    alpha = 0.025, info_frac = c(0.5, 1.0),
    Selection = FALSE, UpdateStrategy = FALSE,
    plotGraphs = FALSE
  )
  state1 <- AnalyzeLook_PC_TestWrapper(
    state1,
    p_raw = c(H1 = 0.30, H2 = 0.35, H3 = 0.40, H4 = 0.45),
    Correlation = corr,
    plotGraphs = FALSE
  )

  corrValid <- matrix(
    c(1, 0.3, 0.2, 0.2, 0.3, 1, 0.2, 0.2, 0.2, 0.2, 1, 0.4, 0.2, 0.2, 0.4, 1),
    nrow = 4
  )
  state2 <- testthat::expect_no_warning(
    AnalyzeLook_PC_TestWrapper(
      state1,
      p_raw = c(H1 = 0.10, H2 = 0.15, H3 = 0.20, H4 = 0.25),
      Correlation = corrValid, plotGraphs = FALSE
    )
  )
  testthat::expect_identical(state2$mcpObj$test.type, "Dunnett")
  testthat::expect_identical(state2$design_params$test.type, "Dunnett")
})


