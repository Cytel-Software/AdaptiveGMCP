# This file contains tests for validating the numerical output of the AnalyzeLook_PC() function.

testthat::test_that("AnalyzeLook_PC example 1 validation", {
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
    state <- SetupAnalysis_PC(WI = c(1/2,1/2,0,0), G = G, test.type = "Partly-Parametric",
                alpha = 0.025, info_frac = c(0.5,0.7,1), typeOfDesign = "asOF",
                plotGraphs = FALSE)

    # print("Initial design:")
    # print(state)

    # Look 1 Analysis
    state <- AnalyzeLook_PC(state, look = 1, info_frac_cur = 0.5,
                p_raw = c(H1 = 0.01, H2 = 0.20, H3 = 0.15, H4 = 0.30),
                Correlation  = corr, plotGraphs = FALSE)

    act_out <- state$mcpObj

    # print("Look 1 actual output:")
    # print(act_out)

    exp_out <- readRDS(testthat::test_path("PC-test-01.l1.mcpObj.rds"))
    
    # print("Look 1 expected output:")
    # print(exp_out)

    testthat::expect_true( CompareImportantMcpMembers( act_out, exp_out ) )

    # Look 2 Analysis
    state <- AnalyzeLook_PC(state, look = 2, info_frac_cur = 0.7,
                p_raw = c(H1 = 0.02, H2 = 0.10, H4 = 0.40),
                selection = c("H1", "H2", "H4"), plotGraphs = FALSE)

    exp_out <- readRDS(testthat::test_path("PC-test-01.l2.mcpObj.rds"))
    act_out <- state$mcpObj

    testthat::expect_true( CompareImportantMcpMembers( act_out, exp_out ) )

    # Look 3 Analysis
    state <- AnalyzeLook_PC(state, look = 3, info_frac_cur = 1,
                p_raw = c(H2 = 0.005, H4 = 0.10), plotGraphs = FALSE)

    exp_out <- readRDS(testthat::test_path("PC-test-01.l3.mcpObj.rds"))
    act_out <- state$mcpObj

    testthat::expect_true( CompareImportantMcpMembers( act_out, exp_out ) )
})



####################################################################################################
# testthat::test_that("AnalyzeLook_PC example 2 validation", {})
