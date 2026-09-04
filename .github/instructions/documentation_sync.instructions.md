---
description: This file defines mandatory documentation checks and updates for Copilot-implemented changes, including man pages and project documentation such as the living plan.
---

# Documentation Sync Gate

Apply this gate for any task where Copilot implements code, behavior, API, workflow, or governance changes.

## Required Documentation Review

Before marking work complete, review and update all relevant documentation touched by the change:

1. R function documentation in source roxygen blocks.
2. Generated man pages under `man/`.
3. `NAMESPACE` exports when roxygen changes affect exported functions.
4. Project documentation under `docs/` (including the relevant living plan document, if one exists, when status, scope, or acceptance progress changes).
5. User-facing references that can become stale (for example `README.md`, vignettes, examples, and workflow notes).
6. Release metadata for any user-visible package change, including new or changed exported APIs, bug fixes, behavior changes, and compatibility changes:
   - `DESCRIPTION`: decide whether the version number must be incremented for the completed work.
   - `NEWS.md`: add or update the unreleased/next-release entry with a concise description of the completed change.

## Required Actions

1. Update roxygen comments in `R/*.R` for any changed function signature, argument behavior, defaults, return value, errors, or details.
2. Regenerate documentation with:
   - `Rscript -e "devtools::document()"`
3. Verify generated man pages reflect the implementation and contain no stale arguments or stale constraints.
4. When issue state/progress changed as part of the task, update the relevant living plan row and changelog entry if a living plan document exists for that feature. If no living plan document exists, there is nothing to update.
5. Before marking a user-visible package change complete, review `DESCRIPTION` and `NEWS.md`. Update both when a release version has been assigned; otherwise, add the completed change to the pending next-release section of `NEWS.md`. Do not defer these updates until release preparation.
6. If documentation or release metadata is intentionally not updated, explicitly state why in the final task report.

## Validation

1. Run at least one targeted validation command relevant to the implemented change (for example `devtools::test(filter = '<pattern>')`), unless the user asks not to run tests.
2. Report the validation command and result in the final response.

## Enforcement

- A Copilot task is incomplete if implementation changes are made without corresponding documentation sync checks.
- A user-visible package change is incomplete until its `DESCRIPTION` version decision and `NEWS.md` entry have been reviewed and recorded.
- Do not close or mark a task complete until all applicable documentation updates are done and reported.
