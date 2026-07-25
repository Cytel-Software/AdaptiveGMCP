# AI Coding Agent Instructions — AdaptiveGMCP

These instructions make AI agents immediately productive in this R package. They document actual patterns and workflows observed in the repo.

## Overview
- Purpose: Graph-based, adaptive multi-arm, multi-endpoint clinical trial methods with simulation and analysis.
- Key exports: `simMAMSMEP`, `adaptGMCP_CER`, `adaptGMCP_PC`, `plotGraph`, `genPowerTablePlots`, `AdaptGMCPSimApp` (Shiny).
- See exports in [NAMESPACE](../NAMESPACE) and package metadata in [DESCRIPTION](../DESCRIPTION).

## Architecture & Data Conventions
See [docs/architecture.md](../docs/architecture.md) for the full architecture overview and data/hypothesis conventions.

## Build & Dev Workflow
See [instructions/build.instructions.md](instructions/build.instructions.md) for the full build, test, lint, and renv workflow.

## Mandatory Git Sync Gate
Before any file edit, code generation, refactor, test run, or commit-related action, follow [instructions/git_sync_gate.instructions.md](instructions/git_sync_gate.instructions.md).

## Debugging
See [docs/debugging.md](../docs/debugging.md) for the package debugging workflow in VS Code.

## External Dependencies
See [docs/architecture.md](../docs/architecture.md) for the full dependency list and rationale.

## Issue Management
When creating, updating, or reviewing issues and epics, follow [instructions/issue_management.instructions.md](instructions/issue_management.instructions.md).

## Agent Tips
- Preserve exported signatures and documented defaults; they are referenced by examples in [internalData](../internalData).

## Style Guide
- Always follow the R style guide described in [instructions/r_coding_conventions.instructions.md](instructions/r_coding_conventions.instructions.md) while generating new code or refactoring / changing existing code for any purpose. This includes naming conventions, indentation, and commenting practices to maintain consistency across the codebase.

## Governance
- Follow all governance rules defined in copilot-instructions.md and all referenced instruction files as mandatory constraints.
- Before finishing, run and report a Governance Checklist proving each applicable rule was applied, including issue type, labels, hierarchy, state updates, and required docs updates.
- Task is incomplete unless governance checklist is reported and all applicable governance actions are done.