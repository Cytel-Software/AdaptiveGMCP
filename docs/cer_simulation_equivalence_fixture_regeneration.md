# Regenerating CER Simulation-Equivalence Fixtures

## Purpose

The tests in `tests/testthat/test-CERSimulationEquivalence.R` compare the
non-interactive CER analysis API with output captured from individual CER
simulation runs. The captured output is stored in:

```text
tests/testthat/sim-anal-equiv-fixtures/
```

Regenerate a fixture only when an intentional simulation change makes the
captured result stale. Do not update a fixture merely to make a failing test
pass. First confirm that a fresh simulation changes only fields expected to be
affected by the implementation change.

This guide applies to every CER simulation-equivalence fixture in the suite.

## Fixture Inputs and Reproducibility

The simulation inputs are stored in:

```text
tests/testthat/sim-anal-equiv-fixtures/Mixed-2OrMoreEPs.csv
```

Each row contains a `ModelID` and `Seed`. The checked-in fixtures represent one
simulation iteration, even when the source CSV row configures a larger power
simulation. For fixture generation:

- Select exactly one row by `ModelID`.
- Preserve the row's `Seed`.
- Set `nSimulation` to `1`.
- Set `nSimulation_Stage2` to `1`.
- Set `Parallel` to `FALSE` for a simple deterministic local run.

## Prerequisites

From the repository root, restore the locked package environment:

```powershell
Rscript -e "source('rebuild_package.R'); renv_restore()"
```

The simulation output dump hook is currently disabled in the public function
signatures and in `R/singleSimulation.R`. The procedure below uses `trace()` to
capture the same output structure in memory without temporarily editing package
source files.

## Regenerate a Fixture in a Temporary Location

Set the model ID and existing fixture filename, then run the regeneration from
the repository root:

```powershell
$env:CER_MODEL_ID = "<model-id>"
$env:CER_FIXTURE_NAME = "<existing-fixture-filename>"

@'
devtools::load_all(quiet = TRUE)

model_id <- as.integer(Sys.getenv("CER_MODEL_ID"))
fixture_name <- Sys.getenv("CER_FIXTURE_NAME")
stopifnot(
  length(model_id) == 1L,
  !is.na(model_id),
  nzchar(fixture_name)
)

fixture_model_id <- as.integer(
  sub(
    "^SimOutDump_([0-9]+)_1_[0-9]{8}_[0-9]{6}\\.rds$",
    "\\1",
    fixture_name
  )
)
stopifnot(identical(model_id, fixture_model_id))

fixture_directory <- file.path(
  "tests",
  "testthat",
  "sim-anal-equiv-fixtures"
)
existing_fixture <- file.path(fixture_directory, fixture_name)
staging_directory <- file.path(
  Sys.getenv("TEMP"),
  "AdaptiveGMCP-CER-fixtures"
)
dir.create(staging_directory, recursive = TRUE, showWarnings = FALSE)
temporary_fixture <- file.path(staging_directory, fixture_name)

trace(
  "SingleSimCER",
  exit = substitute(
    {
      fixture <- list(
        PlannedStage1Bdry = mcpObj$plan_Bdry$Stage1Bdry,
        PlannedStage2Bdry = mcpObj$plan_Bdry$Stage2Bdry,
        IncrRawPVals = mcpObj$rawpvalues,
        SummStatDF = mcpObj$SummStatDF,
        AdaptStage2 = if (is.null(mcpObj$AdaptStage2)) {
          NA
        } else {
          mcpObj$AdaptStage2
        },
        SelectedIndex = if (is.null(mcpObj$SelectedIndex)) {
          NA
        } else {
          mcpObj$SelectedIndex
        },
        AdjStage2Bdry = if (
          !is.null(mcpObj$AdaptStage2) &&
            mcpObj$AdaptStage2
        ) {
          mcpObj$AdaptObj$Stage2AdjBdry
        } else {
          NA
        },
        CER_PCER = if (
          !is.null(mcpObj$AdaptStage2) &&
            mcpObj$AdaptStage2
        ) {
          mcpObj$AdaptObj$Stage2Tables$Stage2_Test_Procedure
        } else {
          NA
        },
        Stage2SampleSize = if (
          !is.null(mcpObj$AdaptStage2) &&
            mcpObj$AdaptStage2
        ) {
          mcpObj$Stage2AllocSampleSize
        } else {
          NA
        }
      )
      saveRDS(fixture, file = output_path, compress = "xz")
    },
    list(output_path = temporary_fixture)
  ),
  print = FALSE,
  where = asNamespace("AdaptGMCP")
)

input_data <- utils::read.csv(
  file.path(fixture_directory, "Mixed-2OrMoreEPs.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
input_row <- input_data[
  input_data$ModelID == model_id,
  ,
  drop = FALSE
]
stopifnot(nrow(input_row) == 1L)

input_row$nSimulation <- 1L
input_row$nSimulation_Stage2 <- 1L
input_row$Parallel <- FALSE

tryCatch(
  AdaptGMCP:::run1TestCase(input_row, ModelID = model_id),
  finally = untrace(
    "SingleSimCER",
    where = asNamespace("AdaptGMCP")
  )
)
stopifnot(file.exists(temporary_fixture))

old_fixture <- readRDS(existing_fixture)
new_fixture <- readRDS(temporary_fixture)

comparison <- vapply(
  names(old_fixture),
  function(field) {
    isTRUE(
      all.equal(
        old_fixture[[field]],
        new_fixture[[field]],
        tolerance = 1e-12,
        check.attributes = TRUE
      )
    )
  },
  logical(1)
)
print(comparison)
cat("Temporary fixture:", temporary_fixture, "\n")
'@ | Rscript -
```

The script prints a logical comparison for every fixture field. Review every
changed field before replacing the checked-in fixture. It uses a persistent
operating-system temporary directory so that the staged fixture remains
available after `Rscript` exits.

Investigate each reported difference and confirm that it is a direct,
understood consequence of the implementation change. An unexpected field
difference may indicate a regression, nondeterministic simulation behavior, or
an incorrect input row and should block fixture replacement.

## Regenerate the Complete Fixture Set

For a change that may affect multiple scenarios, apply the same operation to
every file matching:

```text
^SimOutDump_[0-9]+_1_[0-9]{8}_[0-9]{6}\.rds$
```

Extract the model ID from each filename and select the corresponding input row.
Each regeneration must run in an independent R process or worker because
`devtools::load_all()`, `trace()`, and the simulation code modify process-level
state. Do not run multiple traced simulations concurrently in the same R
process.

Stage all results outside the repository before replacing any checked-in file.
For every staged fixture:

1. Require exactly one matching input row.
2. Require the new and old fixtures to have identical field names in the same
   order.
3. Compare every field with `all.equal()` and record the changed field names.
4. Treat any simulation error or structural difference as a batch failure.
5. Replace the checked-in fixtures only after the entire batch succeeds.

Semantically unchanged fixtures may still have different serialized bytes
because the R or compression-library version can encode the same objects
differently. Use the field comparison, not file hashes or binary diffs, to
determine whether fixture content changed.

## Replace the Checked-In Fixture

Copy the temporary file over the existing fixture only after reviewing the
field comparison:

```powershell
$fixtureDirectory = "tests\testthat\sim-anal-equiv-fixtures"
$stagingDirectory = Join-Path $env:TEMP "AdaptiveGMCP-CER-fixtures"

Copy-Item `
  -LiteralPath (Join-Path $stagingDirectory $env:CER_FIXTURE_NAME) `
  -Destination (Join-Path $fixtureDirectory $env:CER_FIXTURE_NAME) `
  -Force
```

Keep the existing fixture filename unless the test reference is intentionally
being changed. The timestamp in the filename identifies the original fixture;
the deterministic model ID and seed provide reproducibility.

## Validate the Fixture

Run the package-aware simulation-equivalence test suite:

```powershell
Rscript -e "devtools::test(filter = 'CERSimulationEquivalence')"
```

To diagnose one model specifically, keep `CER_MODEL_ID` and
`CER_FIXTURE_NAME` set to the values used during regeneration. Load the helper
definitions without evaluating the test declarations and invoke the shared
assertion helper:

```powershell
@'
devtools::load_all(quiet = TRUE)

model_id <- as.integer(Sys.getenv("CER_MODEL_ID"))
fixture_name <- Sys.getenv("CER_FIXTURE_NAME")
stopifnot(
  length(model_id) == 1L,
  !is.na(model_id),
  nzchar(fixture_name)
)

expressions <- parse("tests/testthat/test-CERSimulationEquivalence.R")
test_environment <- new.env(parent = globalenv())
for (index in seq_len(4L)) {
  eval(expressions[[index]], envir = test_environment)
}

fixture_directory <- file.path(
  "tests",
  "testthat",
  "sim-anal-equiv-fixtures"
)
input_data <- utils::read.csv(
  file.path(fixture_directory, "Mixed-2OrMoreEPs.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

testthat::test_that(
  paste("CER simulation equivalence for model", model_id),
  {
    test_environment$RunCerSimulationEquivalence(
      file.path(fixture_directory, fixture_name),
      input_data
    )
  }
)
'@ | Rscript -
```

## Final Review

Before committing:

1. Confirm that only expected fixture fields changed.
2. Confirm that the model-specific test passes.
3. Run the complete `CERSimulationEquivalence` test filter.
4. Check `git diff --stat` and verify that no temporary files were added.
5. Explain in the commit or pull request why the fixture changed and which
   implementation change made the previous baseline stale.
