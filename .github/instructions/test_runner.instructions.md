---
description: This file defines the preferred test runner workflow for AdaptiveGMCP and prevents false failures from running package test files without loading the package.
---

# Test Runner Guidance

Use package-aware test runners by default.

## Rule

- Do not use `testthat::test_file()` as the first choice for package tests in `tests/testthat/`.
- Prefer `devtools::test(filter = '<pattern>')` for targeted validation.
- If a whole-file run is needed, prefer `testthat::test_local(filter = '<pattern>')` or first load the package with `devtools::load_all()` before using `testthat::test_file()`.

## Why

- `testthat::test_file()` can run a test file without the normal package-loading context.
- In this repository that can produce false failures such as `could not find function "SetupAnalysis_PE_PC"` even when the implementation and tests are correct.

## Expected Workflow

1. For targeted package validation, run:
   - `Rscript -e "devtools::test(filter = '<pattern>')"`
2. Only use `testthat::test_file()` when there is a specific reason and the package has already been loaded.
3. If `testthat::test_file()` fails with missing package functions, treat that as a test-runner context problem first, not as immediate evidence of a code defect.