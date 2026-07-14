---
description: This file describes the build and development workflow for the AdaptiveGMCP package, including helper functions for common tasks, documentation practices, testing, linting, and environment management with renv.
---

# Build & Dev Workflow — AdaptiveGMCP

## Helper Script

All common tasks are exposed through [`rebuild_package.R`](../../rebuild_package.R). Source it at the start of any dev session:

```r
source('rebuild_package.R')
```

## Common Workflows

### Quick dev loop (most common)
```r
source('rebuild_package.R'); quick_reinstall(); load_pkg()
```

### Full rebuild (document → check → reinstall)
```r
source('rebuild_package.R'); full_rebuild()
```

### Load without reinstalling (for iterative development)
```r
source('rebuild_package.R'); load_pkg()
# equivalent to:
devtools::load_all()
```

## Individual Tasks

| Task | Helper function | Equivalent |
|------|----------------|------------|
| Reinstall (remove old first) | `reinstall()` | `devtools::install(upgrade = "never")` |
| Quick reinstall | `quick_reinstall()` | — |
| Load for development | `load_pkg()` | `devtools::load_all()` |
| Build tarball | `build_pkg()` | `devtools::build()` |
| R CMD check | `check_pkg()` | `devtools::check()` |
| Run tests | `test_pkg()` | `devtools::test()` |
| Refresh docs & NAMESPACE | `document_pkg()` | `devtools::document()` |
| Install dependencies | `install_deps()` | `devtools::install_deps(dependencies = TRUE)` |
| Lint package | `lint_pkg()` | `lintr::lint_package()` |

## Documentation (Roxygen2)

- Functions are documented inline with roxygen2 comments.
- Run `document_pkg()` to refresh man pages and `NAMESPACE` after any roxygen change.
- Never manually edit `NAMESPACE` — it is fully managed by roxygen2.

## Tests

- Test harness: [`tests/testthat.R`](../../tests/testthat.R) with cases in [`tests/testthat/`](../../tests/testthat/).
- Snapshot files live under [`tests/testthat/_snaps/`](../../tests/testthat/_snaps/).
- Run tests:
  ```r
  source('rebuild_package.R'); test_pkg()
  # or
  devtools::test()
  ```
- Run a specific test file:
  ```r
  testthat::test_file('tests/testthat/test-<name>.R')
  ```
- Run a targeted test suite (matching file names):
  ```r
  devtools::test(filter = '<pattern>')
  ```

## Linting & Quality

- Lintr script: [`internalData/LintThisPkg.R`](../../internalData/LintThisPkg.R) shows typical usage and exclusions.
- Run lint:
  ```r
  source('rebuild_package.R'); lint_pkg()
  # or target the R directory directly:
  lintr::lint_dir('R')
  ```

## renv (Reproducible Environments)

Always run `renv_restore()` before running tests locally to ensure packages match the lockfile.

| Task | Function |
|------|----------|
| Check sync status | `renv_status()` |
| Restore to lockfile | `renv_restore()` |
| Update lockfile to current state | `renv_snapshot()` |
| Install a new package via renv | `renv_install('pkg')` |
| Update all packages | `renv_update()` |
| Clean unused packages | `renv_clean()` |
| Full hygiene check with suggested fixes | `renv_hygiene()` |
| Snapshot + sync in one step | `renv_sync()` |

> After `renv_snapshot()` or `renv_sync()`, always commit `renv.lock` to git.
