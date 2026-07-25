# Population Enrichment Implementation Plan (PC Analysis First)

This is a living plan document. Update this file as work progresses.

## Scope

- In scope now:
  - Population enrichment analysis using the p-value combination method.
  - Simplest case first: 1 treatment, 1 endpoint, 2 populations.
  - Non-interactive analysis first, then interactive interface.
  - Normal endpoint first, then binomial endpoint.
- Out of scope now:
  - Simulation changes for population enrichment.
  - CER method (analysis/simulation) changes.
  - Survival endpoint work.

## Canonical Hypothesis Ordering

- Follow numbering and ordering in:
  - docs/pop enrich/pop enrich - page 3.png
- Design, implementation, examples, and tests must use the same ordering consistently.

## Delivery Sequence

1. Governance updates
- Update issue management instructions:
  - Epic issues must have epic label.
  - New issues must set Type (Feature, Task, Bug).
  - Issue states must be updated correctly as work progresses.

2. Issue setup first
- Create one epic for population enrichment (PC analysis) and apply epic label.
- Create sub-issues before implementation starts:
  - Design and logic documentation.
  - Living plan maintenance (this file).
  - EAST example preparation and numerical validation fixture creation.
  - Non-interactive normal endpoint implementation.
  - Non-interactive binomial endpoint implementation.
  - Interactive normal endpoint implementation.
  - Interactive binomial endpoint implementation.
  - Regression/TDD test tasks.
  - README and package-documentation updates.

3. Regression gate and readiness checks
- Re-confirm current PC API regression baseline.
- Review open work under issue #30 and issue #66 and triage any blockers for enrichment.
- Ensure baseline tests are green before starting implementation.

4. Design documentation before code changes
- Document population enrichment logic for simple case.
- Document stage-1 alpha split behavior for full vs subgroup families.
- Document data assumptions and expected outputs.

5. TDD implementation flow
- For each scoped issue, follow:
  1. Write failing tests.
  2. Make minimal changes to pass tests.
  3. Refactor safely.
  4. Re-run regression tests.
- Implementation order:
  - Non-interactive normal.
  - Non-interactive binomial.
  - Interactive normal.
  - Interactive binomial.

6. Numerical validation using EAST
- Build high-quality reference examples in EAST.
- Convert EAST outputs to reproducible validation fixtures.
- Compare AdaptiveGMCP outputs against EAST references and document agreement criteria/tolerances.

7. Final documentation and package updates
- Update README.md.
- Update roxygen docs and regenerate man/NAMESPACE.
- Update NEWS.md and any relevant architecture/developer docs.

## Progress Tracker

Update status as work progresses.

| ID | Task | Owner | Status | Linked Issue | Notes |
|---|---|---|---|---|---|
| G1 | Update issue management instructions | TBD | Done | #92 | Closed after adding epic label, issue Type, and issue-state rules |
| G2 | Create living plan document under docs | TBD | Done | #93 | Closed after creating this living plan file |
| I1 | Create enrichment epic (PC analysis) | TBD | Done | #87 | Epic created with epic label |
| I2 | Create sub-issues and set Type | TBD | Done | #93-#102 | Sub-issues created with explicit Type values |
| R1 | Regression gate review (#30, #66) | TBD | Done | #94 | Completed: baseline green; #31 parallel; merge-gate coverage #67/#70/#69 now implemented and closed |
| D1 | Document simple-case enrichment logic | TBD | Not Started | #97 | Include stage-1 alpha split explanation |
| V1 | EAST reference example creation | User/TBD | Not Started | #95 | Source of numerical truth cases |
| V2 | EAST fixture integration and tolerance rules | TBD | Not Started | #96 | Reproducible comparison fixtures |
| T1 | Non-interactive normal endpoint (TDD) | TBD | Not Started | #98 | First implementation slice |
| T2 | Non-interactive binomial endpoint (TDD) | TBD | Not Started | #99 | After T1 is complete |
| U1 | Interactive normal endpoint (TDD) | TBD | Not Started | #100 | After T1 and baseline stability |
| U2 | Interactive binomial endpoint (TDD) | TBD | Not Started | #101 | After T2 and U1 |
| P1 | README + package documentation refresh | TBD | Not Started | #102 | Include examples and release notes |

## Change Log

- 2026-07-25: Initial living plan created.
- 2026-07-25: Created epic #87 and sub-issues #92-#102; closed #92 and #93 after completion in this phase.
- 2026-07-25: Completed regression gate review (#94), posted blocker/parallel triage, and closed #94.
- 2026-07-25: Verified progress for #67/#69/#70, posted completion evidence comments, and closed all three issues.
