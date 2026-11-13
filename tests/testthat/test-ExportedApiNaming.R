test_that("renamed exported APIs follow the R naming conventions", {
  exported_names <- getNamespaceExports("AdaptGMCP")
  expected_names <- c(
    "AnalyzeLookCER",
    "AnalyzeLookPC",
    "AnalyzeLookPEPC",
    "PlotAnalysisGraph",
    "PlotGraphCER",
    "SetupAnalysisPC",
    "SetupAnalysisPEPC",
    "SetupAnalysisCER",
    "GeneratePowerTablePlots",
    "SimulateMAMSMEPWrapper"
  )

  expect_true(all(expected_names %in% exported_names))
  expect_false(any(c(
    "AnalyzeLook_CER",
    "AnalyzeLook_PC",
    "AnalyzeLook_PE_PC",
    "PlotGraph_CER",
    "SetupAnalysis_PC",
    "SetupAnalysis_PE_PC",
    "SetupDesignCER",
    "SetupDesign_CER",
    "genPowerTablePlots",
    "simMAMSMEP_Wrapper"
  ) %in% exported_names))
  expect_true(all(c(
    "adaptGMCP_CER",
    "adaptGMCP_PC",
    "simMAMSMEP",
    "plotGraph"
  ) %in% exported_names))

  expect_true(all(grepl("^[A-Z][A-Za-z0-9]*$", expected_names)))

  for (function_name in expected_names) {
    parameter_names <- names(formals(getExportedValue("AdaptGMCP", function_name)))
    conforms <- grepl("^(n|d|b|v|m|df|l|c|str)[A-Z][A-Za-z0-9]*$", parameter_names)
    expect_true(all(conforms), info = function_name)
  }
})
