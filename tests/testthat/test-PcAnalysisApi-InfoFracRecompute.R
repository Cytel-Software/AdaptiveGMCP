testthat::test_that("AnalyzeLook_PC preserves design-time testing quantities", {
	vWi <- c(0.5, 0.5)
	mG <- matrix(c(0, 1, 1, 0), nrow = 2, byrow = TRUE)

	state <- SetupAnalysis_PC(
		WI = vWi,
		G = mG,
		test.type = "Sidak",
		alpha = 0.025,
		planned_info_frac = c(0.5, 1.0),
		plotGraphs = FALSE
	)

	expectedInvNormWeights <- state$mcpObj$InvNormWeights
	expectedWNorm <- state$mcpObj$W_Norm
	expectedBdryTab <- state$mcpObj$bdryTab
	expectedThresholds <- state$thresholds

	state <- AnalyzeLook_PC(
		state = state,
		p_raw = c(H1 = 0.20, H2 = 0.25),
		plotGraphs = FALSE
	)

	testthat::expect_equal(state$mcpObj$CutOff, expectedThresholds[1])
	testthat::expect_equal(state$mcpObj$InvNormWeights, expectedInvNormWeights)
	testthat::expect_equal(state$mcpObj$W_Norm, expectedWNorm)
	testthat::expect_equal(state$mcpObj$bdryTab, expectedBdryTab)
	testthat::expect_equal(state$mcpObj$LastLook, 2)
	testthat::expect_false(state$trial_completed)

	state <- AnalyzeLook_PC(
		state = state,
		p_raw = c(H1 = 0.20, H2 = 0.25),
		plotGraphs = FALSE
	)

	testthat::expect_equal(state$mcpObj$CutOff, expectedThresholds[2])
	testthat::expect_equal(state$mcpObj$InvNormWeights, expectedInvNormWeights)
	testthat::expect_equal(state$mcpObj$W_Norm, expectedWNorm)
	testthat::expect_equal(state$mcpObj$bdryTab, expectedBdryTab)
	testthat::expect_true(state$trial_completed)
	testthat::expect_identical(state$completion_reason, "final_look")
})

testthat::test_that("AnalyzeLook_PC rejects a look beyond the planned schedule", {
	state <- SetupAnalysis_PC(
		WI = c(0.5, 0.5),
		G = matrix(c(0, 1, 1, 0), nrow = 2, byrow = TRUE),
		test.type = "Sidak",
		planned_info_frac = c(1),
		plotGraphs = FALSE
	)

	state$completed_looks <- 1L
	state$trial_completed <- FALSE

	testthat::expect_error(
		AnalyzeLook_PC(
			state = state,
			p_raw = c(H1 = 0.20, H2 = 0.25),
			plotGraphs = FALSE
		),
		"All planned looks have been analyzed"
	)
})
