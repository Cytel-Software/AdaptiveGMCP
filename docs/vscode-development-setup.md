# VS Code Development Setup (AdaptiveGMCP)

## Purpose

Use this guide to get a new developer productive quickly with the same VS Code + R workflow used in this repo.

## Prerequisites

- VS Code
- R (recommended: 4.5.x)
- Git

Install these VS Code extensions:

- R (`REditorSupport.r`)
- R Debugger (`RDebugger.r-debugger`)
- Quarto (`quarto.quarto`) for `.qmd` work
- GitHub Pull Requests (`GitHub.vscode-pull-request-github`) (optional but useful)

## One-Time Setup

1. Clone the repository and open it in VS Code.
2. Ensure VS Code uses your local R executable (User Settings):
   `r.rpath.windows` and `r.rterm.windows`

   Open Command Palette (`Ctrl+Shift+P`) -> `Preferences: Open User Settings (JSON)`.

   Add/update the following values (adjust version/path if needed):

```json
"r.rpath.windows": "C:\\Program Files\\R\\R-4.5.1\\bin\\x64\\R.exe",
"r.rterm.windows": "C:\\Program Files\\R\\R-4.5.1\\bin\\x64\\Rterm.exe"
```

   Save settings and reload VS Code window (`Ctrl+Shift+P` -> `Developer: Reload Window`).

1. Restore package environment from lockfile:

```r
source("rebuild_package.R")
renv_restore()
```

## Workspace Defaults Already Included

- `.Rprofile` auto-activates `renv`.
- `.Rprofile` includes a VS Code language-server timeout workaround for Windows/OneDrive environments.
- `.vscode/settings.json` excludes `renv/library/**` from search and increases R debugger startup timeout.
- `.vscode/launch.json` includes ready-to-use debug profiles (workspace, file, function, package, attach).

## Daily Workflow

Most common loop:

```r
source("rebuild_package.R")
quick_reinstall()
load_pkg()
```

Run tests:

```r
source("rebuild_package.R")
test_pkg()
```

Run checks/lint when needed:

```r
source("rebuild_package.R")
check_pkg()
lint_pkg()
```

## Dependency Rules

- Use `renv_install("pkg")` instead of `install.packages()`.
- After dependency changes, run `renv_snapshot()` and commit `renv.lock`.
- Before running tests locally, run `renv_restore()`.

## Troubleshooting

### R language server fails or is unstable

- Confirm `r.lsp.args` does not include `--vanilla`.
- Keep `.Rprofile` as-is (it patches `languageserver` session timeout for VS Code).
- If needed, increase timeout with env var `VSCR_CALLR_WAIT_TIMEOUT_MS`.

## Quick Reference

- Build/dev workflow details: `.github/instructions/build.instructions.md`
- R style guide used in this repo: `.github/instructions/r_coding_conventions.instructions.md`
- Architecture/dependency notes: `docs/architecture.md`
