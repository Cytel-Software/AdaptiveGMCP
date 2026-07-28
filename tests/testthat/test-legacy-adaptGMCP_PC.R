testthat::test_that("adaptGMCP_PC: Dunnett with NA correlation warns and converts to Partly-Parametric", {
  wi <- c(0.5, 0.5, 0, 0)
  g <- matrix(
    c(0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0),
    byrow = TRUE, nrow = 4
  )
  corr <- matrix(
    c(1, 0.5, NA, NA, 0.5, 1, NA, NA, NA, NA, 1, 0.5, NA, NA, 0.5, 1),
    nrow = 4
  )

  testthat::local_mocked_bindings(
    getRawPValues = function(mcpObj) {
      stats::setNames(c(0.30, 0.35, 0.40, 0.45), mcpObj$IndexSet)
    },
    trialContinuationDecision = function(mcpObj) "n",
    ShowResults = function(mcpObj) NULL
  )

  testthat::expect_warning(
    adaptGMCP_PC(
      WI = wi,
      G = g,
      test.type = "Dunnett",
      alpha = 0.025,
      info_frac = c(0.5, 1),
      Correlation = corr,
      Selection = FALSE,
      UpdateStrategy = FALSE,
      plotGraphs = FALSE
    ),
    regexp = "converted to 'Partly-Parametric'"
  )
})
