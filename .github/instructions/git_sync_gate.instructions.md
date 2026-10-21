---
description: This file defines when the mandatory Git sync gate applies before local repository changes.
---

# Mandatory Git Sync Gate

Run this gate only when the task will change the local repository or Git state.

## Applicability

Run the gate before:

- Editing, creating, deleting, generating, or reformatting repository files.
- Refactoring code or updating dependencies, generated artifacts, configuration, tests, or documentation.
- Creating commits, changing branches, rebasing, merging, or performing another operation that changes local Git state.
- Continuing repo-changing work when the working tree is already known to be dirty.

Do not run the gate for:

- Read-only code investigation, explanation, review, or repository searches.
- Tests, builds, linting, or other validation commands that are not expected to update files.
- GitHub-only operations such as searching, creating, commenting on, or updating issues and pull requests.
- Other tasks that neither modify local files nor change local Git state.

## Step 0: Repository State Check

For an applicable task, run this before making changes:

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

After Step 0 confirms that the working tree is clean:

1. Run: `git fetch origin master`
2. Run: `git status --porcelain`
3. If working tree is not clean, stop and ask user to commit/stash or explicitly approve proceeding.
4. Run: `git merge --ff-only origin/master`
5. If fast-forward merge fails or conflicts occur, stop and ask user to resolve before continuing.

## Enforcement

- Do not make local repository or Git-state changes until all applicable steps above pass.
- Do not invoke this gate solely because a task reads repository content or changes remote GitHub metadata.
- If any gate step fails, report the exact failure and wait for user direction.
