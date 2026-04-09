---
description: "Quick reinstall the AdaptiveGMCP package and run all tests. Use when: rebuilding after code changes, verifying tests pass, doing a dev cycle rebuild."
agent: "agent"
---

Perform a quick reinstall of the AdaptiveGMCP package followed by running all tests. Follow these steps exactly, in order:

## Step 1 — Quick Reinstall

Run the following command in the terminal:

```
Rscript -e "source('rebuild_package.R'); quick_reinstall()"
```

Wait for it to complete. If it fails with an error (not a warning), stop and report the error clearly — do not proceed to testing.

## Step 2 — Run Tests

Run the following command in the terminal:

```
Rscript -e "source('rebuild_package.R'); test_pkg()"
```

Wait for it to complete.

## Step 3 — Summarize Results

After both commands finish, provide a concise summary structured as follows:

**Reinstall**: Pass / Fail (with error message if failed)

**Tests**:
- Total: X passed, Y failed, Z skipped
- For each failed test: test file name, test description, and the assertion that failed
- For each warning raised during tests: brief description

If everything passed, say so clearly in one line.
Do not repeat raw terminal output — only the structured summary above.
