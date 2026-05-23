#!/usr/bin/env Rscript

# Fast PR test subset.
# Keep this list aligned with heavy tests that should run in nightly/release lanes.
excluded_tests <- c(
  "test-overall_powers_df_snapshot_CER.R",
  "test-overall_powers_df_snapshot_pvaluecomb.R",
  "test-overall_powers_df_snapshot_pvaluecomb_5arm2ep.R"
)

all_tests <- list.files("tests/testthat", pattern = "^test-.*\\.R$", full.names = FALSE)
fast_tests <- setdiff(all_tests, excluded_tests)

if( length(fast_tests) == 0 )
{
  stop("No fast tests selected. Check tools/ci/run_fast_tests.R configuration.")
}

escape_regex <- function(x)
{
  return(gsub("([][{}()+*^$|\\\\?.])", "\\\\\\\\\\1", x, perl = TRUE))
}

test_stems <- sub("\\.R$", "", sub("^test-", "", fast_tests))
filter_regex <- paste(vapply(test_stems, escape_regex, character(1)), collapse = "|")

message("Running fast tests (excluded heavy tests):")
for( test_file in sort(excluded_tests) )
{
  message("  - excluded: ", test_file)
}
for( test_file in sort(fast_tests) )
{
  message("  - included: ", test_file)
}

devtools::test(filter = filter_regex, reporter = "summary", stop_on_failure = TRUE)
