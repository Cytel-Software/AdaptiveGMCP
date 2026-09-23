#!/usr/bin/env Rscript

# Fast PR test subset.
# Tests in this list are reserved for the exhaustive validation lane.
exhaustive_tests <- c(
  "test-overall_powers_df_snapshot_CER.R",
  "test-overall_powers_df_snapshot_pvaluecomb.R",
  "test-overall_powers_df_snapshot_pvaluecomb_5arm2ep.R",
  "test-CERSimulationEquivalence.R"
)

all_tests <- list.files(file.path("tests", "testthat"), pattern = "^test-.*\\.R$", full.names = FALSE)
missing_tests <- setdiff(exhaustive_tests, all_tests)

if( length(missing_tests) > 0 )
{
  stop(
    sprintf(
      "Exhaustive test list includes files not found under tests/testthat: %s",
      toString(sort(missing_tests))
    )
  )
}

fast_tests <- setdiff(all_tests, exhaustive_tests)

if( length(fast_tests) == 0 )
{
  stop("No fast tests selected. Check test lane configuration.")
}

escape_regex <- function(x)
{
  return(gsub("([][{}()+*^$|\\\\?.])", "\\\\\\\\\\1", x, perl = TRUE))
}

test_stems <- sub("\\.R$", "", sub("^test-", "", fast_tests))
filter_regex <- paste(vapply(test_stems, escape_regex, character(1)), collapse = "|")

message("Running fast tests (excluding exhaustive validation subset):")
for( test_file in sort(exhaustive_tests) )
{
  message("  - excluded: ", test_file)
}
for( test_file in sort(fast_tests) )
{
  message("  - included: ", test_file)
}

devtools::test(filter = filter_regex, reporter = "summary", stop_on_failure = TRUE)
