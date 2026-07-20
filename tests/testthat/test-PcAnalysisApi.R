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
    Correlation = corr,
    plotGraphs = FALSE
  )

  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(state$completed_looks, 0L)

  # Look 1
  state <- AnalyzeLook_PC(
    state,
    p_raw = c(H1 = 0.01, H2 = 0.20, H3 = 0.15, H4 = 0.30),
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
  state <- AnalyzeLook_PC(
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
  state <- AnalyzeLook_PC(
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
    Correlation = corr,
    plotGraphs = FALSE,
    MultipleWinners = FALSE
  )

  # Look 1
  state <- AnalyzeLook_PC(
    state,
    p_raw = c(H1 = 0.03, H2 = 0.20, H3 = 0.10, H4 = 0.25),
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

  state <- AnalyzeLook_PC(
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
    Correlation = corr,
    plotGraphs = FALSE,
    MultipleWinners = TRUE
  )

  exp_out <- readRDS(testthat::test_path("t3.bdryTab.rds"))
  testthat::expect_equal(state$mcpObj$bdryTab, exp_out)

  # Look 1
  state <- AnalyzeLook_PC(
    state,
    p_raw = c(H1 = 0.04, H2 = 0.18, H3 = 0.12, H4 = 0.22),
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

  # 3) correlation update (example: tweak H1-H2 and H1-H3)
  new_correlation <- corr
  new_correlation[1, 2] <- new_correlation[2, 1] <- 0.3
  new_correlation[1, 3] <- new_correlation[3, 1] <- 0.4

  state <- AnalyzeLook_PC(
    state,
    p_raw = c(H1 = 0.03, H2 = 0.10, H3 = 0.08),
    selection = selection,
    new_weights = new_weights,
    new_G = new_G,
    new_correlation = new_correlation,
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
    test.type = "Partly-Parametric",
    alpha = 0.025,
    info_frac = c(0.5, 1.0),
    Correlation = corr,
    plotGraphs = FALSE
  )

  # --- Pure validation errors (fired before any computation) ---

  # look mismatch: state expects look 1, user passes look = 2
  testthat::expect_error(
    AnalyzeLook_PC(state0, p_raw = c(H1 = 0.10, H2 = 0.20), look = 2L, plotGraphs = FALSE),
    regexp = "does not match the expected next look"
  )

  # look is non-numeric
  testthat::expect_error(
    AnalyzeLook_PC(state0, p_raw = c(H1 = 0.10, H2 = 0.20), look = "a", plotGraphs = FALSE),
    regexp = "single positive integer"
  )

  # look is non-integer numeric (1.5)
  testthat::expect_error(
    AnalyzeLook_PC(state0, p_raw = c(H1 = 0.10, H2 = 0.20), look = 1.5, plotGraphs = FALSE),
    regexp = "single positive integer"
  )

  # look = 0 (not positive)
  testthat::expect_error(
    AnalyzeLook_PC(state0, p_raw = c(H1 = 0.10, H2 = 0.20), look = 0L, plotGraphs = FALSE),
    regexp = "single positive integer"
  )

  # --- Correct usage ---

  # look = NULL (default): proceeds normally and completed_looks advances
  state1_null <- AnalyzeLook_PC(state0, p_raw = c(H1 = 0.10, H2 = 0.20), look = NULL, plotGraphs = FALSE)
  testthat::expect_equal(state1_null$completed_looks, 1L)

  # look = 1L (correct explicit value): same result as omitting look
  state1_expl <- AnalyzeLook_PC(state0, p_raw = c(H1 = 0.10, H2 = 0.20), look = 1L, plotGraphs = FALSE)
  testthat::expect_equal(state1_expl$completed_looks, 1L)

  testthat::expect_equal(state1_null, state1_expl)

  # --- After final look: specific "final look" error ---

  # Run look 2 to complete the trial
  state2 <- AnalyzeLook_PC(state1_null, p_raw = c(H1 = 0.10, H2 = 0.20), look = 2L, plotGraphs = FALSE)
  testthat::expect_true(state2$trial_completed)

  # Calling again after final look must name the final look in the error message
  testthat::expect_error(
    AnalyzeLook_PC(state2, p_raw = c(H1 = 0.10, H2 = 0.20), plotGraphs = FALSE),
    regexp = "was the final look"
  )

  # --- Early stopping: trial already concluded before final look ---

  # Simulate early-stopped state (trial_completed = TRUE before the final look is reached)
  state_early_stopped <- state1_null
  state_early_stopped$trial_completed <- TRUE   # completed_looks (1) < LastLook (2)

  testthat::expect_error(
    AnalyzeLook_PC(state_early_stopped, p_raw = c(H1 = 0.10, H2 = 0.20), plotGraphs = FALSE),
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
    test.type = "Partly-Parametric",
    alpha = 0.025,
    info_frac = c(0.5, 1.0),
    Correlation = corr,
    plotGraphs = FALSE
  )

  # Initial state
  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(state$completed_looks, 0L)
  testthat::expect_false(state$trial_completed)
  testthat::expect_equal(state$mcpObj$IndexSet, c("H1", "H2"))

  # Look 1: completed_looks advances, AdjPValues is populated, trial not yet complete
  state <- AnalyzeLook_PC(
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
  state <- AnalyzeLook_PC(
    state,
    p_raw = c(H1 = 0.10, H2 = 0.20),
    plotGraphs = FALSE
  )
  testthat::expect_equal(state$completed_looks, 2L)
  testthat::expect_true(state$trial_completed)

  # Calling again after trial is complete must error
  testthat::expect_error(
    AnalyzeLook_PC(state, p_raw = c(H1 = 0.10, H2 = 0.20), plotGraphs = FALSE)
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
  state <- AnalyzeLook_PC(
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
  state <- AnalyzeLook_PC(
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
  state <- AnalyzeLook_PC(
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
    Correlation = corr,
    MultipleWinners = TRUE,
    plotGraphs = FALSE
  )

  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(state$completed_looks, 0L)
  testthat::expect_equal(state$mcpObj$IndexSet, c("H1", "H2", "H3", "H4"))

  # Look 1
  # Raw p-values from file: p1=0.00045, p2=0.0952, p3=0.0225, p4=0.1104
  state <- AnalyzeLook_PC(
    state,
    p_raw = c(H1 = 0.00045, H2 = 0.0952, H3 = 0.0225, H4 = 0.1104),
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
  state <- AnalyzeLook_PC(
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
    Correlation = corr,
    MultipleWinners = TRUE,
    plotGraphs = FALSE
  )

  # Look 1 (same as Test 7)
  state <- AnalyzeLook_PC(
    state,
    p_raw = c(H1 = 0.00045, H2 = 0.0952, H3 = 0.0225, H4 = 0.1104),
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

  state <- AnalyzeLook_PC(
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
  state <- AnalyzeLook_PC(
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
    Correlation = corr,
    plotGraphs = FALSE
  )

  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(state$completed_looks, 0L)

  # Look 1 (final)
  # Raw p-values from file: p1=0.01, p2=0.02, p3=0.005, p4=0.5
  state <- AnalyzeLook_PC(
    state,
    p_raw = c(H1 = 0.01, H2 = 0.02, H3 = 0.005, H4 = 0.5),
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
    Correlation = corr,
    plotGraphs = FALSE
  )

  testthat::expect_s3_class(state, "PCAnalysisState")
  testthat::expect_equal(state$completed_looks, 0L)
  testthat::expect_equal(state$mcpObj$IndexSet, c("H1", "H2", "H3", "H4"))

  # Look 1 (final)
  # Raw p-values from file: p1=0.638, p2=0.01, p3=0.007, p4=3.959E-4
  state <- AnalyzeLook_PC(
    state,
    p_raw = c(H1 = 0.638, H2 = 0.01, H3 = 0.007, H4 = 3.959e-4),
    plotGraphs = FALSE
  )

  testthat::expect_equal(state$completed_looks, 1L)
  testthat::expect_true(state$trial_completed)

  exp_mcp <- readRDS(testthat::test_path("t11.l1.mcpObj.rds"))
  testthat::expect_true( CompareImportantMcpMembers( state$mcpObj, exp_mcp ) )
})

