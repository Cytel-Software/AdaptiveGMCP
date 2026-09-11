# --------------------------------------------------------------------------------------------------
#
# ©2025 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

test_that("Test Computations of per-look Summary Statistics Computations", {
  # Test Case1: Verify Look1 summary statistics with Excel Benchmarks(from the subject data)
  SimSeed <- 100
  simID <- 1
  lookID <- 1
  Arms.Mean <- list("EP1" = c(0, 0.1, 0.4))
  Arms.std.dev <- list("EP1" = c(1, 1, 1))
  Arms.Prop <- NA
  Arms.alloc.ratio <- c(1, 1, 1)
  Arms.SS <- c(56, 56, 56)
  ArmsPresent <- c(T, T, T)
  HypoPresent <- c(T, T)
  HypoMap <- data.frame(
    "Hypothesis" = c("H1", "H2"),
    "Groups" = 1,
    "EpType" = "Continuous",
    "Control" = 1,
    "Treatment" = c(2, 3)
  )
  EPCorr <- matrix(c(1,0.5, 0.5,1), nrow = 2)
  Cumulative <- F

  Stage1Response <- genIncrLookSummary(
    SimSeed = SimSeed,
    simID = simID,
    lookID = lookID,
    Arms.Mean = Arms.Mean,
    Arms.std.dev = Arms.std.dev,
    Arms.Prop = Arms.Prop,
    UseCC = FALSE,
    Arms.alloc.ratio = Arms.alloc.ratio,
    Arms.SS = Arms.SS,
    EPCorr = EPCorr,
    ArmsPresent = ArmsPresent,
    HypoPresent = HypoPresent,
    HypoMap = HypoMap
  )

  SummStat1 <- getPerLookTestStat(
    simID = simID,
    lookID = lookID,
    TestStatCont = "t-unequal",
    TestStatBin = NA,
    Arms.std.dev = Arms.std.dev,
    IncrLookSummary= Stage1Response,
    Cumulative = Cumulative,
    HypoMap = HypoMap
  )

  delta_benchmark <- c(0.06600336, 0.2184182)
  SE_benchmark <- c(0.1901001, 0.1887724)
  TestStat_benchmark <- c(0.3472031, 1.157045)
  pValue_benchmark <- c(0.3645511, 0.1248802)

  delta <- unlist(SummStat1[, grep("Delta", names(SummStat1))])
  SE <- unlist(SummStat1[, grep("StdError", names(SummStat1))])
  TestStat <- unlist(SummStat1[, grep("TestStat", names(SummStat1))])
  pValue <- unlist(SummStat1[, grep("RawPvalues", names(SummStat1))])
  names(delta) <- names(SE) <- names(TestStat) <- names(pValue) <- NULL

  expect_equal(object = delta, expected = delta_benchmark, tolerance = 1e-4)
  expect_equal(object = SE, expected = SE_benchmark, tolerance = 1e-4)
  expect_equal(object = TestStat, expected = TestStat_benchmark, tolerance = 1e-4)
  expect_equal(object = pValue, expected = pValue_benchmark, tolerance = 1e-4)
  #-----------------------------------------------------------------------------------------------

  # Test Case2: Verify Look2 summary statistics(Incr.) with Excel Benchmarks(from the subject data)
  lookID <- 2
  Arms.SS <- c(56, 56, 56)
  ArmsPresent <- c(T, T, T)
  HypoPresent <- c(T, T)
  Cumulative <- F

  Stage2Response <- genIncrLookSummary(
    SimSeed = SimSeed,
    simID = simID,
    lookID = lookID,
    Arms.Mean = Arms.Mean,
    Arms.std.dev = Arms.std.dev,
    Arms.Prop = Arms.Prop,
    UseCC = FALSE,
    Arms.alloc.ratio = Arms.alloc.ratio,
    Arms.SS = Arms.SS,
    EPCorr = EPCorr,
    ArmsPresent = ArmsPresent,
    HypoPresent = HypoPresent,
    HypoMap = HypoMap
  )

  SummStat2Incr <- getPerLookTestStat(
    simID = simID,
    lookID = lookID,
    TestStatCont = "t-unequal",
    TestStatBin = NA,
    Arms.std.dev = Arms.std.dev,
    IncrLookSummary= Stage2Response,
    Cumulative = Cumulative,
    HypoMap = HypoMap
  )

  delta_benchmark <- c(0.1887818, 0.3287579)
  SE_benchmark <- c(0.1943976, 0.1824913)
  TestStat_benchmark <- c(0.9711117, 1.8015)
  pValue_benchmark <- c(0.1668622, 0.03736549)

  delta <- unlist(SummStat2Incr[, grep("Delta", names(SummStat2Incr))])
  SE <- unlist(SummStat2Incr[, grep("StdError", names(SummStat2Incr))])
  TestStat <- unlist(SummStat2Incr[, grep("TestStat", names(SummStat2Incr))])
  pValue <- unlist(SummStat2Incr[, grep("RawPvalues", names(SummStat2Incr))])
  names(delta) <- names(SE) <- names(TestStat) <- names(pValue) <- NULL

  expect_equal(object = delta, expected = delta_benchmark, tolerance = 1e-4)
  expect_equal(object = SE, expected = SE_benchmark, tolerance = 1e-4)
  expect_equal(object = TestStat, expected = TestStat_benchmark, tolerance = 1e-4)
  expect_equal(object = pValue, expected = pValue_benchmark, tolerance = 1e-4)

  #-----------------------------------------------------------------------------

  # Test Case3: Verify Look2 summary statistics(Cum.) with Excel Benchmarks(from the subject data)
  Cumulative <- T

  SummStat2Cum <- getPerLookTestStat(
    simID = simID,
    lookID = lookID,
    TestStatCont = "t-unequal",
    TestStatBin = NA,
    Arms.std.dev = Arms.std.dev,
    IncrLookSummary= Stage2Response,
    IncrLookSummaryPrev = Stage1Response,
    Cumulative = Cumulative,
    HypoMap = HypoMap
  )

  delta_benchmark <- c(0.1273926, 0.273588)
  SE_benchmark <- c(0.1359877, 0.1313916)
  TestStat_benchmark <- c(0.9367949, 2.082234)
  pValue_benchmark <- c(0.1749456, 0.01925215)

  delta <- unlist(SummStat2Cum[, grep("Delta", names(SummStat2Cum))])
  SE <- unlist(SummStat2Cum[, grep("StdError", names(SummStat2Cum))])
  TestStat <- unlist(SummStat2Cum[, grep("TestStat", names(SummStat2Cum))])
  pValue <- unlist(SummStat2Cum[, grep("RawPvalues", names(SummStat2Cum))])
  names(delta) <- names(SE) <- names(TestStat) <- names(pValue) <- NULL

  expect_equal(object = delta, expected = delta_benchmark, tolerance = 1e-4)
  expect_equal(object = SE, expected = SE_benchmark, tolerance = 1e-4)
  expect_equal(object = TestStat, expected = TestStat_benchmark, tolerance = 1e-4)
  expect_equal(object = pValue, expected = pValue_benchmark, tolerance = 1e-4)
  #------------------------------------------------------------------------------
})
