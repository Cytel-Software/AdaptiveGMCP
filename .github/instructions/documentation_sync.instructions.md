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

## Required Actions

1. Update roxygen comments in `R/*.R` for any changed function signature, argument behavior, defaults, return value, errors, or details.
2. Regenerate documentation with:
   - `Rscript -e "devtools::document()"`
3. Verify generated man pages reflect the implementation and contain no stale arguments or stale constraints.
4. When issue state/progress changed as part of the task, update the relevant living plan row and changelog entry if a living plan document exists for that feature. If no living plan document exists, there is nothing to update.
5. If documentation is intentionally not updated, explicitly state why in the final task report.

## Validation

1. Run at least one targeted validation command relevant to the implemented change (for example `devtools::test(filter = '<pattern>')`), unless the user asks not to run tests.
2. Report the validation command and result in the final response.

## Enforcement

- A Copilot task is incomplete if implementation changes are made without corresponding documentation sync checks.
- Do not close or mark a task complete until all applicable documentation updates are done and reported.
