# Population Enrichment Implementation Plan (PC Analysis First)

This is a living plan document. Update this file as work progresses.

## Scope

- In scope now:
  - Population enrichment analysis using the p-value combination method.
  - Simplest case first: 1 treatment, 1 endpoint, 2 populations.
  - Non-interactive analysis first, then interactive interface.
  - Endpoint-agnostic analysis with user-provided raw p-values.
  - Look-wise correlation updates derived from per-look sample-size inputs.
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
  - Non-interactive endpoint-agnostic implementation.
  - Interactive implementation tasks (tracked under the interactive epic).
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
  - Non-interactive endpoint-agnostic analysis.
  - Interactive implementation tasks under the interactive epic.

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
| I2 | Create sub-issues and set Type | TBD | Done | #88-#108 | Sub-issues created with explicit Type values |
| R1 | Regression gate review (#30, #66) | TBD | Done | #94 | Completed: baseline green; #31 parallel; merge-gate coverage #67/#70/#69 now implemented and closed |
| D1 | Document simple-case enrichment logic | TBD | Done | #97 | Final D1 sign-off draft completed with explicit acceptance thresholds and decision log |
| V1 | EAST reference example creation | User/TBD | Not Started | #95 | Source of numerical truth cases |
| V2 | EAST fixture integration and tolerance rules | TBD | Not Started | #96 | Reproducible comparison fixtures |
| T1 | Non-interactive endpoint-agnostic analysis (TDD) | TBD | In Progress | #98 | Core PE wrapper and look-level correlation update flow implemented; final sign-off pending |
| T2 | Retire binomial-specific non-interactive track | TBD | Done | #99 | Closed as not planned; scope absorbed by T1 |
| U1 | Interactive implementation track | TBD | Not Started | #111 | Interactive work managed under dedicated epic |
| P1 | README + package documentation refresh | TBD | Not Started | #102 | Include examples and release notes |

## Change Log

- 2026-07-25: Initial living plan created.
- 2026-07-25: Created epic #87 and sub-issues #92-#102; closed #92 and #93 after completion in this phase.
- 2026-07-25: Completed regression gate review (#94), posted blocker/parallel triage, and closed #94.
- 2026-07-25: Verified progress for #67/#69/#70, posted completion evidence comments, and closed all three issues.
- 2026-07-25: Started D1 (#97); added draft design spec at docs/pop_enrichment_simple_case_pc_design.md and set D1 status to In Progress.
- 2026-07-25: Finalized D1 sign-off content for #97 by adding explicit acceptance thresholds and decision logs in docs/pop_enrichment_simple_case_pc_design.md; updated D1 status to Done.
- 2026-08-04: Updated scope to endpoint-agnostic non-interactive analysis, retired #99 as not planned, and aligned tracker entries to #98 and #111.
- 2026-08-05: Reconciled epic #87 child-issue coverage in this plan; updated I2 linked-issue range to reflect current child issue set.
- 2026-08-06: Updated T1 (#98) status to In Progress after implementing PE wrapper/correlation update changes and targeted tests.
