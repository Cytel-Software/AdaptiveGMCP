# Section 1: simMAMSMEP positive contract cases.
# Parameter values below are hard-coded from selected scenarios in:
#   - internalData/BatchInput_FS_GMCP_Sim_Bin.csv (ModelID 1)
#   - internalData/CER_Inp_1ep5arms - Continuous.csv (ModelID 2)
#   - internalData/Mixed-2OrMoreEPs.csv (ModelID 164)
# nSimulation (and nSimulation_Stage2 for CER) and Parallel are overridden for fast, serial runs.

ExpectValidSimContract <- function(out)
{
  testthat::expect_type(out, "list")

  requiredTopLevel <- c("Overall_Powers_df", "stagewiseRejections", "Seed")
  testthat::expect_true(all(requiredTopLevel %in% names(out)))

  testthat::expect_s3_class(out$Overall_Powers_df, "data.frame")
  testthat::expect_gt(nrow(out$Overall_Powers_df), 0)
  testthat::expect_true(all(c("Overall_Powers", "Values") %in% names(out$Overall_Powers_df)))

  testthat::expect_true(is.list(out$stagewiseRejections))
  testthat::expect_true(all(c("Count", "Percentage") %in% names(out$stagewiseRejections)))
}

testthat::test_that("Section 1 positive contract: simMAMSMEP runs binary CombPValue scenario", {
  lEpType <- list(EP1 = "Binary")
  Arms.Prop <- list(EP1 = c(0.1, 0.1, 0.1, 0.1, 0.1))
  alloc <- c(1, 1, 1, 1, 1)
  wi <- rep(1 / 4, 4)
  g <- matrix(rep(0, 16), byrow = TRUE, nrow = 4)

  out <- simMAMSMEP(
    Method = "CombPValue", alpha = 0.025, SampleSize = 500,
    TestStatBin = "UnPooled", FWERControl = "CombinationTest",
    nArms = 5, nEps = 1, lEpType = lEpType,
    Arms.Prop = Arms.Prop, Arms.alloc.ratio = alloc, EP.Corr = matrix(1),
    WI = wi, G = g, test.type = "Bonf", info_frac = 1,
    typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = FALSE,
    ImplicitSSR = "None", nSimulation = 10, Seed = 1234,
    SummaryStat = TRUE, plotGraphs = FALSE, Parallel = FALSE, Verbose = FALSE
  )

  ExpectValidSimContract(out = out)
})

testthat::test_that("Section 1 positive contract: simMAMSMEP runs continuous CER scenario", {
  lEpType <- list(EP1 = "Continuous")
  Arms.Mean <- list(EP1 = c(0, 0, 0, 0, 0.25))
  Arms.std.dev <- list(EP1 = c(1, 1, 1, 1, 1))
  alloc <- c(1, 0.5, 0.5, 0.5, 0.5)
  wi <- rep(1 / 4, 4)
  g <- rbind(
    H1 = c(0, 1 / 3, 1 / 3, 1 / 3),
    H2 = c(1 / 3, 0, 1 / 3, 1 / 3),
    H3 = c(1 / 3, 1 / 3, 0, 1 / 3),
    H4 = c(1 / 3, 1 / 3, 1 / 3, 0)
  )

  out <- simMAMSMEP(
    Method = "CER", alpha = 0.025, SampleSize = 500,
    TestStatCont = "t-equal", CommonStdDev = FALSE, FWERControl = "None",
    nArms = 5, nEps = 1, lEpType = lEpType,
    Arms.Mean = Arms.Mean, Arms.std.dev = Arms.std.dev,
    Arms.alloc.ratio = alloc, EP.Corr = matrix(1),
    WI = wi, G = g, test.type = "Parametric", info_frac = c(1 / 2, 1),
    typeOfDesign = "asOF", MultipleWinners = TRUE, Selection = FALSE,
    ImplicitSSR = "None", nSimulation = 10, nSimulation_Stage2 = 10, Seed = 1234,
    SummaryStat = TRUE, plotGraphs = FALSE, Parallel = FALSE, Verbose = FALSE
  )

  ExpectValidSimContract(out = out)
})

testthat::test_that("Section 1 positive contract: simMAMSMEP runs mixed endpoint CombPValue scenario", {
  lEpType <- list(EP1 = "Continuous", EP2 = "Binary")
  Arms.Mean <- list(EP1 = c(0, 0, 0, 0, 0), EP2 = NA)
  Arms.std.dev <- list(EP1 = c(1, 1, 1, 1, 1), EP2 = NA)
  Arms.Prop <- list(EP1 = NA, EP2 = c(0.1, 0.1, 0.1, 0.1, 0.1))
  alloc <- c(1, 1, 1, 1, 1)
  epCorr <- matrix(c(1, 0, 0, 1), nrow = 2)
  wi <- c(0.25, 0.25, 0.25, 0.25, 0, 0, 0, 0)
  g <- matrix(
    c(
      0, 1 / 12, 1 / 12, 1 / 12, 3 / 4, 0, 0, 0,
      1 / 12, 0, 1 / 12, 1 / 12, 0, 3 / 4, 0, 0,
      1 / 12, 1 / 12, 0, 1 / 12, 0, 0, 3 / 4, 0,
      1 / 12, 1 / 12, 1 / 12, 0, 0, 0, 0, 3 / 4,
      0, 1 / 3, 1 / 3, 1 / 3, 0, 0, 0, 0,
      1 / 3, 0, 1 / 3, 1 / 3, 0, 0, 0, 0,
      1 / 3, 1 / 3, 0, 1 / 3, 0, 0, 0, 0,
      1 / 3, 1 / 3, 1 / 3, 0, 0, 0, 0, 0
    ),
    nrow = 8, byrow = TRUE
  )

  out <- simMAMSMEP(
    Method = "CombPValue", alpha = 0.025, SampleSize = 500,
    TestStatCont = "t-equal", TestStatBin = "Pooled", FWERControl = "CombinationTest",
    nArms = 5, nEps = 2, lEpType = lEpType,
    Arms.Mean = Arms.Mean, Arms.std.dev = Arms.std.dev, Arms.Prop = Arms.Prop,
    Arms.alloc.ratio = alloc, EP.Corr = epCorr,
    WI = wi, G = g, test.type = "Partly-Parametric", info_frac = c(1 / 2, 1),
    typeOfDesign = "asOF", MultipleWinners = TRUE, Selection = TRUE,
    SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue",
    SelectionCriterion = "threshold", SelectionParameter = 0.75,
    KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection",
    nSimulation = 10, Seed = 7481,
    SummaryStat = FALSE, plotGraphs = FALSE, Parallel = FALSE, Verbose = FALSE
  )

  ExpectValidSimContract(out = out)
})

# Section 2: simMAMSMEP negative/validation cases.
# simMAMSMEP validates inputs and, on failure, returns a list of "Invalid argument in '<field>'"
# messages instead of throwing. Each test induces one specific invalidity and asserts that message.

ExpectInvalidInput <- function(out, field)
{
  testthat::expect_true(is.list(out))
  msgs <- unlist(out)
  testthat::expect_true(any(grepl("Invalid", msgs)))
  testthat::expect_true(any(grepl(field, msgs, fixed = TRUE)))
}

testthat::test_that("Section 2 negative: inconsistent nEps vs lEpType is rejected", {
  # nEps = 1 but lEpType has length 2 (malformed endpoint definition).
  lEpType <- list(EP1 = "Binary", EP2 = "Binary")
  Arms.Prop <- list(EP1 = c(0.1, 0.1, 0.1, 0.1, 0.1))
  alloc <- c(1, 1, 1, 1, 1)
  wi <- rep(1 / 4, 4)
  g <- matrix(rep(0, 16), byrow = TRUE, nrow = 4)

  out <- simMAMSMEP(
    Method = "CombPValue", alpha = 0.025, SampleSize = 500,
    TestStatBin = "UnPooled", FWERControl = "CombinationTest",
    nArms = 5, nEps = 1, lEpType = lEpType,
    Arms.Prop = Arms.Prop, Arms.alloc.ratio = alloc, EP.Corr = matrix(1),
    WI = wi, G = g, test.type = "Bonf", info_frac = 1,
    typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = FALSE,
    ImplicitSSR = "None", nSimulation = 10, Seed = 1234,
    SummaryStat = TRUE, plotGraphs = FALSE, Parallel = FALSE, Verbose = FALSE
  )

  ExpectInvalidInput(out = out, field = "'lEpType'")
})

testthat::test_that("Section 2 negative: CER with more than two looks is rejected", {
  # CER supports at most two looks; three-look info_frac is invalid.
  lEpType <- list(EP1 = "Continuous")
  Arms.Mean <- list(EP1 = c(0, 0, 0, 0, 0.25))
  Arms.std.dev <- list(EP1 = c(1, 1, 1, 1, 1))
  alloc <- c(1, 0.5, 0.5, 0.5, 0.5)
  wi <- rep(1 / 4, 4)
  g <- rbind(
    H1 = c(0, 1 / 3, 1 / 3, 1 / 3),
    H2 = c(1 / 3, 0, 1 / 3, 1 / 3),
    H3 = c(1 / 3, 1 / 3, 0, 1 / 3),
    H4 = c(1 / 3, 1 / 3, 1 / 3, 0)
  )

  out <- simMAMSMEP(
    Method = "CER", alpha = 0.025, SampleSize = 500,
    TestStatCont = "t-equal", CommonStdDev = FALSE, FWERControl = "None",
    nArms = 5, nEps = 1, lEpType = lEpType,
    Arms.Mean = Arms.Mean, Arms.std.dev = Arms.std.dev,
    Arms.alloc.ratio = alloc, EP.Corr = matrix(1),
    WI = wi, G = g, test.type = "Parametric", info_frac = c(0.3, 0.6, 1),
    typeOfDesign = "asOF", MultipleWinners = TRUE, Selection = FALSE,
    ImplicitSSR = "None", nSimulation = 10, nSimulation_Stage2 = 10, Seed = 1234,
    SummaryStat = TRUE, plotGraphs = FALSE, Parallel = FALSE, Verbose = FALSE
  )

  ExpectInvalidInput(out = out, field = "'info_frac'")
})

testthat::test_that("Section 2 negative: invalid SelectionCriterion is rejected", {
  # Two-look design with Selection = TRUE but an unsupported SelectionCriterion.
  lEpType <- list(EP1 = "Continuous")
  Arms.Mean <- list(EP1 = c(0, 0, 0, 0, 0.25))
  Arms.std.dev <- list(EP1 = c(1, 1, 1, 1, 1))
  alloc <- c(1, 0.5, 0.5, 0.5, 0.5)
  wi <- rep(1 / 4, 4)
  g <- rbind(
    H1 = c(0, 1 / 3, 1 / 3, 1 / 3),
    H2 = c(1 / 3, 0, 1 / 3, 1 / 3),
    H3 = c(1 / 3, 1 / 3, 0, 1 / 3),
    H4 = c(1 / 3, 1 / 3, 1 / 3, 0)
  )

  out <- simMAMSMEP(
    Method = "CER", alpha = 0.025, SampleSize = 500,
    TestStatCont = "t-equal", CommonStdDev = FALSE, FWERControl = "None",
    nArms = 5, nEps = 1, lEpType = lEpType,
    Arms.Mean = Arms.Mean, Arms.std.dev = Arms.std.dev,
    Arms.alloc.ratio = alloc, EP.Corr = matrix(1),
    WI = wi, G = g, test.type = "Parametric", info_frac = c(1 / 2, 1),
    typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = TRUE,
    SelectionLook = 1, SelectEndPoint = 1, SelectionScale = "pvalue",
    SelectionCriterion = "bogus", SelectionParameter = 1,
    KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection",
    nSimulation = 10, nSimulation_Stage2 = 10, Seed = 1234,
    SummaryStat = TRUE, plotGraphs = FALSE, Parallel = FALSE, Verbose = FALSE
  )

  ExpectInvalidInput(out = out, field = "'SelectionCriterion'")
})

testthat::test_that("Section 2 negative: WI length mismatch is rejected", {
  # WI must have length nEps * (nArms - 1) = 4; length 3 is invalid.
  lEpType <- list(EP1 = "Binary")
  Arms.Prop <- list(EP1 = c(0.1, 0.1, 0.1, 0.1, 0.1))
  alloc <- c(1, 1, 1, 1, 1)
  wi <- rep(1 / 3, 3)
  g <- matrix(rep(0, 16), byrow = TRUE, nrow = 4)

  out <- simMAMSMEP(
    Method = "CombPValue", alpha = 0.025, SampleSize = 500,
    TestStatBin = "UnPooled", FWERControl = "CombinationTest",
    nArms = 5, nEps = 1, lEpType = lEpType,
    Arms.Prop = Arms.Prop, Arms.alloc.ratio = alloc, EP.Corr = matrix(1),
    WI = wi, G = g, test.type = "Bonf", info_frac = 1,
    typeOfDesign = "asOF", MultipleWinners = FALSE, Selection = FALSE,
    ImplicitSSR = "None", nSimulation = 10, Seed = 1234,
    SummaryStat = TRUE, plotGraphs = FALSE, Parallel = FALSE, Verbose = FALSE
  )

  ExpectInvalidInput(out = out, field = "'WI'")
})

# Section 3: simMAMSMEP_Wrapper positive contract cases.
# The wrapper consumes a data frame of scenario rows (CSV-style) and maps each row to simMAMSMEP
# via run1TestCase. Input rows are built inline with hard-coded values (expression columns as
# R-code strings). nSimulation / nSimulation_Stage2 / Parallel are set to fast, serial values.

MakeWrapperInputDF <- function(ModelID, Scenario, Method, SampleSize, alpha,
                               TestStatCont, CommonStdDev, TestStatBin, UseCC,
                               FWERControl, nArms, nEps, lEpType, Arms.Mean,
                               Arms.std.dev, Arms.Prop, Arms.alloc.ratio, EP.Corr,
                               WI, G, test.type, info_frac, typeOfDesign,
                               MultipleWinners, Selection, SelectionLook,
                               SelectEndPoint, SelectionScale, SelectionCriterion,
                               SelectionParameter, KeepAssociatedHypo, ImplicitSSR,
                               nSimulation, nSimulation_Stage2, Seed, SummaryStat,
                               plotGraphs, Parallel)
{
  return(data.frame(
    ModelID = ModelID, Scenario = Scenario, Method = Method,
    SampleSize = SampleSize, alpha = alpha, TestStatCont = TestStatCont,
    CommonStdDev = CommonStdDev, TestStatBin = TestStatBin, UseCC = UseCC,
    FWERControl = FWERControl, nArms = nArms, nEps = nEps, lEpType = lEpType,
    Arms.Mean = Arms.Mean, Arms.std.dev = Arms.std.dev, Arms.Prop = Arms.Prop,
    Arms.alloc.ratio = Arms.alloc.ratio, EP.Corr = EP.Corr, WI = WI, G = G,
    test.type = test.type, info_frac = info_frac, typeOfDesign = typeOfDesign,
    MultipleWinners = MultipleWinners, Selection = Selection,
    SelectionLook = SelectionLook, SelectEndPoint = SelectEndPoint,
    SelectionScale = SelectionScale, SelectionCriterion = SelectionCriterion,
    SelectionParameter = SelectionParameter, KeepAssociatedHypo = KeepAssociatedHypo,
    ImplicitSSR = ImplicitSSR, nSimulation = nSimulation,
    nSimulation_Stage2 = nSimulation_Stage2, Seed = Seed, SummaryStat = SummaryStat,
    plotGraphs = plotGraphs, Parallel = Parallel,
    stringsAsFactors = FALSE, check.names = FALSE
  ))
}

ExpectValidWrapperOutput <- function(dfOut, expectedModelIds)
{
  testthat::expect_s3_class(dfOut, "data.frame")
  testthat::expect_equal(nrow(dfOut), length(expectedModelIds))

  requiredCols <- c("ModelID", "seed", "StagewiseRejection_Count",
                    "StagewiseRejection_Percentage", "HoursTaken", "Method")
  testthat::expect_true(all(requiredCols %in% names(dfOut)))

  testthat::expect_setequal(dfOut$ModelID, expectedModelIds)
}

testthat::test_that("Section 3 positive contract: wrapper runs binary CombPValue scenario", {
  dfInput <- MakeWrapperInputDF(
    ModelID = 1, Scenario = "Binary CombPValue", Method = "CombPValue",
    SampleSize = 500, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    TestStatBin = "UnPooled", UseCC = FALSE, FWERControl = "CombinationTest",
    nArms = 5, nEps = 1, lEpType = 'list(EP1 = "Binary")',
    Arms.Mean = "NA", Arms.std.dev = "NA",
    Arms.Prop = "list(EP1 = c(0.1, 0.1, 0.1, 0.1, 0.1))",
    Arms.alloc.ratio = "c(1, 1, 1, 1, 1)", EP.Corr = "matrix(1)",
    WI = "rep(1/4, 4)", G = "matrix(rep(0, 16), byrow = TRUE, nrow = 4)",
    test.type = "Bonf", info_frac = "1", typeOfDesign = "asOF",
    MultipleWinners = FALSE, Selection = FALSE, SelectionLook = NA,
    SelectEndPoint = NA, SelectionScale = NA, SelectionCriterion = NA,
    SelectionParameter = NA, KeepAssociatedHypo = NA, ImplicitSSR = "None",
    nSimulation = 10, nSimulation_Stage2 = 1, Seed = 1234, SummaryStat = TRUE,
    plotGraphs = FALSE, Parallel = FALSE
  )

  dfOut <- simMAMSMEP_Wrapper(InputDF = dfInput, sOutPath = tempfile(fileext = ".csv"))

  ExpectValidWrapperOutput(dfOut = dfOut, expectedModelIds = 1)
  testthat::expect_equal(dfOut$Method, "CombPValue")
})

testthat::test_that("Section 3 positive contract: wrapper runs continuous CER scenario", {
  dfInput <- MakeWrapperInputDF(
    ModelID = 2, Scenario = "Continuous CER", Method = "CER",
    SampleSize = 500, alpha = 0.025, TestStatCont = "t-equal", CommonStdDev = FALSE,
    TestStatBin = NA, UseCC = FALSE, FWERControl = "None",
    nArms = 5, nEps = 1, lEpType = "list(EP1 = 'Continuous')",
    Arms.Mean = "list(EP1 = c(0, 0, 0, 0, 0.25))",
    Arms.std.dev = "list(EP1 = c(1, 1, 1, 1, 1))", Arms.Prop = "NA",
    Arms.alloc.ratio = "c(1, 0.5, 0.5, 0.5, 0.5)", EP.Corr = "matrix(1)",
    WI = "rep(1/4, 4)",
    G = "rbind(c(0,1/3,1/3,1/3), c(1/3,0,1/3,1/3), c(1/3,1/3,0,1/3), c(1/3,1/3,1/3,0))",
    test.type = "Parametric", info_frac = "c(1/2, 1)", typeOfDesign = "asOF",
    MultipleWinners = TRUE, Selection = FALSE, SelectionLook = NA,
    SelectEndPoint = NA, SelectionScale = NA, SelectionCriterion = NA,
    SelectionParameter = NA, KeepAssociatedHypo = NA, ImplicitSSR = "None",
    nSimulation = 10, nSimulation_Stage2 = 10, Seed = 1234, SummaryStat = TRUE,
    plotGraphs = FALSE, Parallel = FALSE
  )

  dfOut <- simMAMSMEP_Wrapper(InputDF = dfInput, sOutPath = tempfile(fileext = ".csv"))

  ExpectValidWrapperOutput(dfOut = dfOut, expectedModelIds = 2)
  testthat::expect_equal(dfOut$Method, "CER")
})

testthat::test_that("Section 3 positive contract: wrapper runs mixed endpoint CombPValue scenario", {
  gMixed <- paste0(
    "matrix(c(0,1/12,1/12,1/12,3/4,0,0,0, ",
    "1/12,0,1/12,1/12,0,3/4,0,0, ",
    "1/12,1/12,0,1/12,0,0,3/4,0, ",
    "1/12,1/12,1/12,0,0,0,0,3/4, ",
    "0,1/3,1/3,1/3,0,0,0,0, ",
    "1/3,0,1/3,1/3,0,0,0,0, ",
    "1/3,1/3,0,1/3,0,0,0,0, ",
    "1/3,1/3,1/3,0,0,0,0,0), nrow = 8, byrow = TRUE)"
  )

  dfInput <- MakeWrapperInputDF(
    ModelID = 164, Scenario = "Mixed CombPValue", Method = "CombPValue",
    SampleSize = 500, alpha = 0.025, TestStatCont = "t-equal", CommonStdDev = NA,
    TestStatBin = "Pooled", UseCC = FALSE, FWERControl = "CombinationTest",
    nArms = 5, nEps = 2, lEpType = 'list(EP1 = "Continuous", EP2 = "Binary")',
    Arms.Mean = "list(EP1 = c(0, 0, 0, 0, 0), EP2 = NA)",
    Arms.std.dev = "list(EP1 = c(1, 1, 1, 1, 1), EP2 = NA)",
    Arms.Prop = "list(EP1 = NA, EP2 = c(0.1, 0.1, 0.1, 0.1, 0.1))",
    Arms.alloc.ratio = "c(1, 1, 1, 1, 1)", EP.Corr = "matrix(c(1, 0, 0, 1), nrow = 2)",
    WI = "c(0.25, 0.25, 0.25, 0.25, 0, 0, 0, 0)", G = gMixed,
    test.type = "Partly-Parametric", info_frac = "c(1/2, 1)", typeOfDesign = "asOF",
    MultipleWinners = TRUE, Selection = TRUE, SelectionLook = 1,
    SelectEndPoint = 1, SelectionScale = "pvalue", SelectionCriterion = "threshold",
    SelectionParameter = 0.75, KeepAssociatedHypo = TRUE, ImplicitSSR = "Selection",
    nSimulation = 10, nSimulation_Stage2 = 1, Seed = 7481, SummaryStat = FALSE,
    plotGraphs = FALSE, Parallel = FALSE
  )

  dfOut <- simMAMSMEP_Wrapper(InputDF = dfInput, sOutPath = tempfile(fileext = ".csv"))

  ExpectValidWrapperOutput(dfOut = dfOut, expectedModelIds = 164)
  testthat::expect_equal(dfOut$Method, "CombPValue")
})

# Section 4: simMAMSMEP_Wrapper negative/validation cases.
# The wrapper skips rows whose scenario fails; when no row succeeds it has no results to
# assemble and errors while finalizing output. Each test induces one bad row and asserts the
# wrapper surfaces a failure rather than returning a valid result.

MakeValidBinaryWrapperInput <- function()
{
  return(MakeWrapperInputDF(
    ModelID = 1, Scenario = "Binary CombPValue", Method = "CombPValue",
    SampleSize = 500, alpha = 0.025, TestStatCont = NA, CommonStdDev = NA,
    TestStatBin = "UnPooled", UseCC = FALSE, FWERControl = "CombinationTest",
    nArms = 5, nEps = 1, lEpType = 'list(EP1 = "Binary")',
    Arms.Mean = "NA", Arms.std.dev = "NA",
    Arms.Prop = "list(EP1 = c(0.1, 0.1, 0.1, 0.1, 0.1))",
    Arms.alloc.ratio = "c(1, 1, 1, 1, 1)", EP.Corr = "matrix(1)",
    WI = "rep(1/4, 4)", G = "matrix(rep(0, 16), byrow = TRUE, nrow = 4)",
    test.type = "Bonf", info_frac = "1", typeOfDesign = "asOF",
    MultipleWinners = FALSE, Selection = FALSE, SelectionLook = NA,
    SelectEndPoint = NA, SelectionScale = NA, SelectionCriterion = NA,
    SelectionParameter = NA, KeepAssociatedHypo = NA, ImplicitSSR = "None",
    nSimulation = 10, nSimulation_Stage2 = 1, Seed = 1234, SummaryStat = TRUE,
    plotGraphs = FALSE, Parallel = FALSE
  ))
}

testthat::test_that("Section 4 negative: wrapper fails when a required column is missing", {
  # Drop a directly-read column; an expression column would trigger parse(text = NULL),
  # which reads from stdin and blocks in an interactive session.
  dfInput <- MakeValidBinaryWrapperInput()
  dfInput$Method <- NULL

  testthat::expect_error(
    simMAMSMEP_Wrapper(InputDF = dfInput, sOutPath = tempfile(fileext = ".csv"))
  )
})

testthat::test_that("Section 4 negative: wrapper fails on an unparseable expression column", {
  dfInput <- MakeValidBinaryWrapperInput()
  dfInput$WI <- "c(1/4, 1/4, 1/4,"

  testthat::expect_error(
    simMAMSMEP_Wrapper(InputDF = dfInput, sOutPath = tempfile(fileext = ".csv"))
  )
})

testthat::test_that("Section 4 negative: wrapper fails on inconsistent per-row dimensions", {
  # Arms.alloc.ratio has length 3 but nArms = 5.
  dfInput <- MakeValidBinaryWrapperInput()
  dfInput$Arms.alloc.ratio <- "c(1, 1, 1)"

  testthat::expect_error(
    simMAMSMEP_Wrapper(InputDF = dfInput, sOutPath = tempfile(fileext = ".csv"))
  )
})
