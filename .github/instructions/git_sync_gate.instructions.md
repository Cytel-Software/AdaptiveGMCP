---
description: This file defines a mandatory Git sync gate that must run before any repo-changing task.
---

# Mandatory Git Sync Gate

Before starting any new task, run a repository state check first.

## Step 0: Repository State Check (Always First)

Run:

1. `git status --porcelain=v1 --branch`

Then evaluate and report:

- Uncommitted changes
- Untracked files
- Pending commits (local commits not pushed to upstream)

If any of the above are present:

- Stop before making any new changes.
- Suggest the user commit or stash first so new work is not mixed with older changes.
- Encourage small, regular commits as the default working pattern.
- Continue only after explicit user confirmation to proceed.

## Step 1: Sync With `origin/master`

Before any file edit, code generation, refactor, test run, or commit-related action:

1. Run: `git fetch origin master`
2. Run: `git status --porcelain`
3. If working tree is not clean, stop and ask user to commit/stash or explicitly approve proceeding.
4. Run: `git merge --ff-only origin/master`
5. If fast-forward merge fails or conflicts occur, stop and ask user to resolve before continuing.

## Enforcement

- Do not make any repository changes until all steps above pass.
- If any gate step fails, report the exact failure and wait for user direction.
