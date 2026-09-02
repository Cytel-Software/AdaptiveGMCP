# Section 5: genPowerTablePlots positive contract cases.
# genPowerTablePlots reshapes wrapper-style power output into wide/long tables. It requires a
# template (ModelID, Level1, Level2, Method) with exactly two Method values and a dfOut carrying
# the requested PowerType column. Inputs are built inline; ggplot2/gridExtra are Suggests, so the
# function's library() calls are guarded with skip_if_not_installed.

MakePlotTemplateDF <- function()
{
  return(data.frame(
    ModelID = c(1, 2, 3, 4),
    Level1 = c("S1", "S1", "S2", "S2"),
    Level2 = c("Normal", "Normal", "Normal", "Normal"),
    Method = c("CER", "CombPValue", "CER", "CombPValue"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  ))
}

MakePlotDfOut <- function()
{
  return(data.frame(
    ModelID = c(1, 2, 3, 4),
    Global.Power = c(0.80, 0.75, 0.60, 0.55),
    Conjunctive.Power = c(0.40, 0.35, 0.30, 0.25),
    Disjunctive.Power = c(0.85, 0.80, 0.65, 0.60),
    FWER = c(0.024, 0.023, 0.022, 0.021),
    stringsAsFactors = FALSE,
    check.names = FALSE
  ))
}

ExpectValidPowerTableContract <- function(res, expectedScenarioRows)
{
  testthat::expect_type(res, "list")
  testthat::expect_true(all(c("TableWide", "TableLong") %in% names(res)))

  testthat::expect_s3_class(res$TableWide, "data.frame")
  testthat::expect_equal(nrow(res$TableWide), expectedScenarioRows)
  testthat::expect_true(all(c("Scenario", "Treatment Selection Rule", "Difference") %in%
                             names(res$TableWide)))

  testthat::expect_s3_class(res$TableLong, "data.frame")
  testthat::expect_true(all(c("Scenario", "Treatment Selection Rule", "MAMS", "value") %in%
                             names(res$TableLong)))
  testthat::expect_setequal(unique(res$TableLong$MAMS), c("CER", "CombPValue"))
}

testthat::test_that("Section 5 positive contract: genPowerTablePlots builds tables for Global.Power", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("gridExtra")

  res <- genPowerTablePlots(
    PowerType = "Global.Power",
    dfOut = MakePlotDfOut(),
    TableTemDF = MakePlotTemplateDF()
  )

  ExpectValidPowerTableContract(res = res, expectedScenarioRows = 2)
})

testthat::test_that("Section 5 positive contract: genPowerTablePlots builds tables for Conjunctive.Power", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("gridExtra")

  res <- genPowerTablePlots(
    PowerType = "Conjunctive.Power",
    dfOut = MakePlotDfOut(),
    TableTemDF = MakePlotTemplateDF()
  )

  ExpectValidPowerTableContract(res = res, expectedScenarioRows = 2)
})
