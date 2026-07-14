#!/usr/bin/env Rscript

# Exhaustive validation test subset.
# This lane covers the heavier regression scenarios (including simulation-heavy
# and snapshot-based tests) that are not suitable for per-PR fast feedback.
exhaustive_tests <- c(
  "test-overall_powers_df_snapshot_CER.R",
  "test-overall_powers_df_snapshot_pvaluecomb.R",
  "test-overall_powers_df_snapshot_pvaluecomb_5arm2ep.R"
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

if( length(exhaustive_tests) == 0 )
{
  stop("No exhaustive tests configured. Check test lane configuration.")
}

escape_regex <- function(x)
{
  return(gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x, perl = TRUE))
}

test_stems <- sub("\\.R$", "", sub("^test-", "", exhaustive_tests))
filter_regex <- paste(vapply(test_stems, escape_regex, character(1)), collapse = "|")

message("Running exhaustive tests (simulation/analysis heavy subset):")
for( test_file in sort(exhaustive_tests) )
{
  message("  - included: ", test_file)
}

devtools::test(filter = filter_regex, reporter = "summary", stop_on_failure = TRUE)
