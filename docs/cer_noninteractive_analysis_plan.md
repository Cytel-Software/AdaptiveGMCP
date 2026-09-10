# Epic 54: Non-Interactive CER Design and Analysis API

## Status

- Epic: [#54](https://github.com/Cytel-Software/AdaptiveGMCP/issues/54)
- Branch: `54-CER-non-interactive-analysis-interface`
- Status: Phase 0 and Phase 1 complete (2026-09-10); awaiting explicit approval
  at Review Checkpoint 1 before Phase 2 begins. No child issue's implementation
  scope has started yet — see "Phase 1 Results" below.
- Delivery model: Incremental implementation with mandatory user review checkpoints

## Objective

Add a deterministic, non-interactive API for the Conditional Error Rate (CER)
method while preserving the calculations and ordering of `adaptGMCP_CER()`.

The API will separate the immutable planned trial design from the evolving
analysis state:

```r
design <- SetupDesign_CER(...)

state <- AnalyzeLook_CER(
  design = design,
  state = NULL,
  p_raw = look1_pvalues
)

state <- AnalyzeLook_CER(
  design = design,
  state = state,
  p_raw = look2_pvalues,
  selection = ...,
  new_sample_size = ...
)
```

The new API is restricted to two-look CER trials. It must not prompt for input,
depend on hidden session state, or require console output for downstream use.

## Architecture

### CERDesign

`SetupDesign_CER()` will return an S3 object with class
`CERDesign`. It will contain only normalized planning inputs and
precomputed design artifacts:

- Clinical design inputs such as arms, endpoints, endpoint types, planned
  sample size, variances, binary control proportions, and allocation ratios
- Multiplicity inputs such as initial weights and the transition matrix
- Test type, alpha, information fractions, and spending-function parameters
- Hypothesis map and initial graph metadata
- All planned intersection hypotheses and propagated intersection weights
- Planned cumulative sample allocations
- Planned covariance information
- Planned Stage 1 and Stage 2 CER boundaries
- The selected multivariate-normal integration algorithm

It will not contain observed p-values, look counters, selection decisions,
rejection decisions, adaptations, completion status, or analysis history.

The object is conceptually immutable. The same planned design can initialize
multiple independent analyses.

### CERAnalysisState

`AnalyzeLook_CER()` will return an S3 object with class
`CERAnalysisState`. It will contain only information for one evolving analysis:

- Number of completed looks
- Current active hypotheses and continuing arms
- Current and previous rejection flags
- Dropped hypotheses and selection decisions
- Current intersection weights and graph after any adaptation
- Observed Stage 1 and incremental Stage 2 p-values
- Structured Stage 1, CER/PCER, adaptation, and Stage 2 results
- Adapted sample sizes, covariance, and Stage 2 boundaries
- Trial completion status and reason
- Append-only look history containing inputs, decisions, and result snapshots

It will not contain a copy of `CERDesign`, the original design-parameter
list, or a setup-state snapshot.

No public empty-state constructor is planned. For Look 1,
`AnalyzeLook_CER(design, state = NULL, ...)` creates the initial state. Look 2
requires the state returned by Look 1.

### Internal Execution Context

Existing CER computational helpers expect a combined `mcpObj`. To avoid a
risky rewrite, each call to `AnalyzeLook_CER()` may construct a temporary
internal execution context from:

1. Immutable planned artifacts from `design`
2. Evolving fields from `state`
3. Inputs for the current look

Existing helpers can operate on this temporary context. Only evolving analysis
data and structured results are copied back to `CERAnalysisState`; the combined
context is not part of the public API.

### Design and State Compatibility

Each analysis call will validate structural compatibility between `design` and
`state`, including hypothesis names and counts, planned look count, and relevant
matrix dimensions.

A persistent design identifier or signature is intentionally deferred. This
means structural validation will not detect every same-shaped but statistically
different design. This is a documented residual risk and can be revisited if
the initial implementation shows that stronger binding is necessary.

### Decoupled Plotting

`PlotGraph_CER(design, state = NULL)` reconstructs and plots the
intersection-hypothesis graph for a given point in the analysis: the initial
planned graph when `state` is `NULL`, or the graph as of the most recently
completed look otherwise. It is a thin wrapper around the existing
`plotGraph()` helper and has no effect on any analysis result. This lets a
caller regenerate any graph on demand instead of relying solely on the inline
`plotGraphs` argument of `AnalyzeLook_CER()`.

## Compatibility Requirements

- `adaptGMCP_CER()` remains public and behavior-compatible; it is never
  modified by this epic. No other existing code is modified unless strictly
  required for implementing the new non-interactive API. If such a
  modification is required, it is explained to the user along with the
  rationale and approved before it is made.
- The interactive API will not be rewritten around the new API until numerical
  parity is proven and such a refactor is demonstrably low risk.
- Existing statistical helpers should be reused where practical.
- Interactive prompting, printing, and plotting must remain outside the core
  calculations used by the new API.
- Stage 2 inputs are incremental p-values, matching `adaptGMCP_CER()`.
- Adapted cumulative Stage 2 p-values are computed internally.
- Plotting is optional and must not affect analysis results.
- Caller-owned design objects and arguments must not be mutated.

## Scenario Coverage Contract

The epic acceptance criterion governs coverage: every scenario currently
runnable through `adaptGMCP_CER()` must be reproducible through the new API.
The scenarios completed under #56 are a baseline, not a coverage limit.

Mandatory coverage includes active CER calls in `internalData/`, including the
distinct CER variants in `GS_GMCP_Example.R` and `PresentationExample.R`.
Duplicate designs are separate scenarios only when their analysis decisions or
expected results differ.

Each scenario specification must record:

- Design parameters
- Look 1 and Look 2 p-values
- Continuation decision
- Hypothesis selection
- Cumulative Stage 2 sample size
- Updated weights and transition matrix
- Expected planned and adapted results

The commented mixed continuous/binary example and the plain-text extreme
scenario are optional follow-up coverage because they are not currently
runnable calls. They must remain listed in the coverage ledger.

If the intended console decisions for a runnable example cannot be established
from source comments or existing expected-paper comparisons, implementation
will pause for clarification rather than inventing inputs.

## Implementation Phases

### Phase 0: Repository and Baseline Gate — **Complete (2026-09-09)**

1. Run the mandatory Git state and synchronization checks. Done; pre-existing
   unrelated dirty files were flagged (`.github/copilot-instructions.md`,
   `.github/instructions/development.instructions.md`) and left untouched.
2. Run the existing CER regression tests and record the baseline. Done —
   baseline: `devtools::test(filter = 'CER')` → 75 passed, 0 failed.
3. Keep this document updated as the living scope and progress ledger. Ongoing.

### Phase 1: Equivalence Baseline and Scenario Coverage — **Steps 1-7 complete (2026-09-10); steps 8-9 deferred**

1. Inventory every active `adaptGMCP_CER()` call. Done.
2. Convert each console workflow into a complete deterministic scenario. Done —
   see "Phase 1 Results" below for the full scenario matrix.
3. Extend the fixture harness to support real selection, sample-size changes,
   and strategy updates instead of no-op mocks where required. Done —
   `do_modifyStrategy` now reuses PC's existing `applyStrategyUpdate()` helper
   instead of a no-op.
4. Expand fixture outputs to include:
   - Planned Stage 1 and Stage 2 boundaries — already captured; unchanged.
   - Stage 1 intersection and elementary decisions — added.
   - Structured CER and PCER results — added (required exposing `mcpObj$CERTab`
     in `R/cerAdaptGMCP_Analysis.R`; see "Known Risks" item 6 and "Planned
     Files").
   - Active and dropped hypotheses — added.
   - Planned and adapted sample allocations — added.
   - Adapted covariance and Stage 2 boundaries — added.
   - Cumulative Stage 2 p-values — already captured; unchanged.
   - Final rejection decisions — already captured; unchanged.
5. Preserve existing fixtures unless a verified defect requires an intentional
   fixture update. Done — the 6 pre-existing fixtures' original field values
   are unchanged; only the new fields from step 4 were added to them.
6. Review any existing tests for `adaptGMCP_CER()` that already exist and do not duplicate them while doing 1 to 5. Done — extended rather than duplicated.
7. Add tests for `adaptGMCP_CER()` for any new scenarios discovered while doing 1 to 5. Done — 4 new scenarios and `test_that()` blocks added.
8. Add corresponding tests for the new non-interactive CER analysis API using the same scenarios as for `adaptGMCP_CER()`. Deferred — the new API does not
   exist until Phase 3+. Revisit per-scenario once `SetupDesign_CER()` and
   `AnalyzeLook_CER()` exist (Phase 4/5 already require comparing against
   these same fixtures).
9. Make sure that the new non-interactive api gives the same output as `adaptGMCP_CER()` for the same scenario. Deferred for the same reason as step 8.

#### Phase 1 Results: Scenario Matrix

All 10 scenarios live in `internalData/GenerateCERRegressionFixtures.R` and
`tests/testthat/test-CERRegressionApi.R`.

| rowId | Source | Notes |
|---|---|---|
| `CER-regression-01/02/03` | Synthetic (pre-existing, #56) | Basic continuation, selection, sample-size change |
| `CER-Examp-AdaptGMCP-01` | `AdaptGMCP_CER_Analysis_Example.R` Ex. 1 (pre-existing, #56) | |
| `CER-Examp-3arm-1ep` | `CER Analysis 3arm-1ep.R` (pre-existing, #56) | |
| `CER-Examp-2ep` | `CER.Analysis.2primary-2secondary.R` (pre-existing, #56) | |
| `CER-GSExample4-01` | `GS_GMCP_Example.R` EXAMPLE 4, 1st call (new) | Real selection (drops H3) + sample-size change + real strategy update |
| `CER-GSExample4-02` | `GS_GMCP_Example.R` EXAMPLE 4, 2nd call (new) | No selection drop, no-op sample-size adaptation, real strategy update |
| `CER-GSExample2-01` | `GS_GMCP_Example.R` EXAMPLE 2 (new) | Stage-1-only by explicit decision (Look 2 undocumented in source) |
| `CER-AdaptExample2-01` | `AdaptGMCP_CER_Analysis_Example.R` Example 2 (new) | Stage-1-only, placeholder Look-1 p-values, by explicit decision (source undocumented; `nEps=2`/`Parametric` kept as-is) |

**Excluded / deferred coverage (per Scenario Coverage Contract):** the commented-out
`AdaptGMCP_CER_Analysis_NormBin_Example.R` call and the plain-text
`internalData/TestCases/Extreme CER Scenario.txt` scenario remain optional
follow-up coverage — not currently runnable, left in the ledger only.

**Validation:** full package suite after all Phase 1 changes — 762 passed, 1
failed (pre-existing, unrelated: a `Parallel = TRUE` PC-simulation test failing
in this dev environment because the package is only `load_all()`-loaded, not
installed, for parallel workers), 37 warnings, 3 skipped.

**Review checkpoint 1:** Reached, pending your explicit approval to begin
Phase 2. Steps 8-9 above are intentionally deferred rather than skipped.

### Phase 2: Object Contracts and TDD Red

Issues: #55 and #57.

1. Define internal constructors and print methods for `CERDesign` and
   `CERAnalysisState`.
2. Validate that the planned-design object contains no analysis progress and
   that the analysis-state object contains no copied design.
3. Extract the pure planned-design calculations from `PerformStage1Test()` so
   planned sample allocations, covariance, and boundaries can be computed
   without p-values.
4. Route the interactive implementation through the extracted helper without
   changing its numerical behavior.
5. Write failing `SetupDesign_CER()` tests for every fixture design and for:
   - Invalid arm and endpoint counts
   - Invalid endpoint-specific `sigma` and `prop.ctr`
   - Invalid sample sizes and allocation ratios
   - Invalid graph weights and transition matrices
   - Invalid alpha and spending-function inputs
   - Information fractions that are not exactly two strictly increasing values
     ending at 1
   - Invalid or incompatible test types and endpoint structures
6. Run the new tests to demonstrate the expected red state and rerun the legacy
   CER regression suite.

**Review checkpoint 2:** Stop with the object contracts, extracted planning
helper, and failing design-setup specification. Do not implement the setup
function without explicit approval.

### Phase 3: Planned Design TDD Green

Issue: #60.

1. Implement `SetupDesign_CER()`.
2. Validate and normalize all planning inputs.
3. Build the hypothesis map and all intersection graphs and weights.
4. Compute the planned sample allocation, covariance, and CER boundaries.
5. Select the multivariate-normal integration algorithm once.
6. Optionally plot the initial design graph.
7. Return only `CERDesign`.
8. Prove that setup is non-interactive and does not mutate caller inputs.
9. Make the setup tests green and rerun the CER regression suite.
10. Confirm issue wording still matches `SetupDesign_CER()` naming (already
    aligned per Known Risk #6); correct any newly discovered drift before the
    affected issues are closed.

**Review checkpoint 3:** Stop with a reusable planned-design API and report its
shape, numerical parity, validation behavior, and test results.

### Phase 4: Look 1 TDD Red and Green

Issues: #58 and #61.

1. Write failing tests for `AnalyzeLook_CER(design, state = NULL, ...)`.
2. Cover look sequencing, named p-values, Stage 1 closed testing, active-set
   updates, retained intersections, CER/PCER results, history, graph state, and
   early efficacy stopping.
3. Initialize `CERAnalysisState` from the design's immutable artifacts.
4. Run Stage 1 using existing closed-testing and CER helpers.
5. Store structured results rather than relying on printed `kable` output.
6. Preserve full initial-hypothesis alignment using `NA` for inactive values.
7. Apply deterministic completion precedence:
   - Empty active set: `all_hypotheses_dropped`
   - Final look: `final_look`
   - Otherwise, efficacy stop: `early_stop_efficacy`
8. Reject analysis after completion.
9. Compare every mandatory scenario with Look 1 fixtures.
10. Prove that one `CERDesign` can initialize multiple independent
    analyses without mutation.

**Review checkpoint 4:** Stop when Look 1 is independently usable and
fixture-equivalent. Do not begin Stage 2 work without explicit approval.

### Phase 5: Look 2 TDD Red and Green

Issues: #59 and #62.

1. Write failing tests for `AnalyzeLook_CER(design, state, ...)` covering:
   - As-planned and adapted Stage 2 paths
   - No-op adaptation
   - Selection and empty selection
   - Cumulative sample-size changes
   - Paired weight and graph changes
   - Combinations of adaptations
   - P-value names after selection
   - Structurally incompatible design/state pairs
   - Duplicate or out-of-order calls
   - Final completion
2. Apply adaptations in the same order as the interactive workflow:

   `selection -> continuing arms -> sample size -> strategy -> boundary -> test`

3. Reuse selection and strategy helpers only after verifying that their state
   semantics match CER.
4. Replace sample-size prompts with a pure validator and application helper.
5. Validate cumulative sizes against Stage 1 accrual, active arms, names,
   dimensions, finiteness, and resulting allocation ratios.
6. For adapted Stage 2, combine planned artifacts from `design` with current
   decisions from `state`, preserve conditional error, compute cumulative
   p-values, and test against adjusted boundaries.
7. For as-planned Stage 2, use the matching planned boundary rows directly.
8. Record compact inputs and result snapshots in `look_history[[2]]` without
   embedding a duplicate planned design.
9. Compare both looks for every mandatory scenario against fixtures.

**Review checkpoint 5:** Stop with complete two-look parity and report all
numerical tolerances or behavior differences for approval.

### Phase 6: Refactor Without Behavioral Change

Issue: #64.

1. Confirm the "Compatibility Requirements" rule on not modifying existing
   code was honored throughout Phases 1-5; `adaptGMCP_CER()` itself must
   remain untouched.
2. Harden validation and verify state/history invariants for the new API.
3. Complete print methods for both new classes.
4. Implement `PlotGraph_CER(design, state = NULL)` for initial, Look 1, and
   Look 2 graph reconstruction (see "Decoupled Plotting" in Architecture),
   without coupling plots to analysis.
5. Run targeted CER tests after every refactor.
6. PC analysis code or tests are not expected to change due to the non-interactive CER analysis API. However, if this assumption turns out to be false, flag it clearly so that the user (the human developer) can review it and take appropriate decision.
7. Run changed-file lint and the broader package test suite.

**Review checkpoint 6:** Stop with before-and-after behavior evidence and a
focused refactor summary.

### Phase 7: Documentation, Release Metadata, and Governance

Issues: #63 and #54.

1. Complete roxygen documentation and examples for:
   - `SetupDesign_CER()`
   - `AnalyzeLook_CER()`
   - `print.CERDesign()`
   - `print.CERAnalysisState()`
   - `PlotGraph_CER()`
2. Explain design/state separation, `state = NULL` at Look 1, incremental Stage
   2 p-values, two-look enforcement, adaptation timing, history, and errors.
3. Add a fully non-interactive `internalData/CER_Analysis_NonInteractive_Example.R` showing
   design creation and a complete adapted analysis.
4. Update user-facing documentation where needed.
5. Update `NEWS.md` and record the `DESCRIPTION` version decision.
6. Regenerate `NAMESPACE` and `man/` only through `devtools::document()`.
7. Run targeted tests, full tests, documentation checks, changed-file lint, and
   package check with no new warnings or notes.
8. Verify issue types, labels, parent-child links, project membership, issue
   states, and this living plan.

**Final review checkpoint:** Present the implementation summary,
scenario-by-scenario equivalence, validation results, documentation and version
decisions, residual risks, and proposed issue closures. Do not close the epic
or remaining child issues without explicit approval.

## Validation Strategy

At each phase, run the narrowest relevant package-aware test first:

```r
Rscript -e "devtools::test(filter = 'CERAnalysisApi')"
Rscript -e "devtools::test(filter = 'CERRegressionApi')"
```

After changes to shared helpers, run both CER filters and the relevant existing
PC filters. Before completion, also run:

- Full package tests
- Changed-file lint
- `devtools::document()` and generated-file drift review
- Documentation checks
- `devtools::check()` or equivalent `R CMD check`
- The non-interactive worked example with plotting disabled

The final equivalence evidence must show that every mandatory runnable
interactive scenario produces matching structured results within a documented
numerical tolerance.

## Planned Files

### New files

- `R/CERDesign.R`: design constructor and S3 methods
- `R/CERAnalysisState.R`: analysis-state constructor and S3 methods
- `R/CERAnalysisApi.R`: `SetupDesign_CER()`, `AnalyzeLook_CER()`, and `PlotGraph_CER()`
- `tests/testthat/test-CERAnalysisApi.R`: setup and analysis tests
- `internalData/CER_Analysis_NonInteractive_Example.R`: worked example

### Existing files to modify or extend

- `R/cerStage1Analysis.R`: already contains `PerformStage1Test()`; extract the
  shared pure planned-design computation from it.
- `R/cerStage2Analysis.R`: already contains `PerformStage2Test()`; add
  prompt-free Stage 2 sample-size handling.
- `R/cerAdaptBoundary.R`: already contains conditional-error recalibration
  logic; reuse as-is unless a defect fix is required.
- `R/cerComputation.R`: already contains `getCER()`; extend to expose
  structured CER/PCER results alongside the existing `kable` output.
- `tests/testthat/test-CERRegressionApi.R`: already contains the interactive
  `adaptGMCP_CER()` regression baseline from issue #56; extend with any newly
  discovered scenarios and with equivalence tests against the new API.
- `internalData/GenerateCERRegressionFixtures.R`: already generates the
  existing regression fixtures; extend for any newly discovered scenarios.
- `R/cerAdaptGMCP_Analysis.R`: already contains `adaptGMCP_CER()`; a single
  additive line (`mcpObj$CERTab <- CERTab`) was added during Phase 1 to expose
  the CER/PCER table for fixture capture (approved 2026-09-09; see Known Risks
  item 6). No other behavior was changed.
- `NEWS.md`, `DESCRIPTION`, `NAMESPACE`, and generated `man/` files.

Test files may be divided into setup, lifecycle, adaptation, and plotting files
if a single file becomes difficult to review.

## Known Risks and Open Considerations

1. `getCER()` currently returns a formatted `kable`, not analysis-grade
   structured data. The implementation must expose structured values while
   preserving legacy display behavior.
2. `getArmsFromHypo()` appears to refer to a `Hypo` column while
   `getHypoMap2()` creates `Hypothesis`. Selection parity tests must determine
   whether a narrowly scoped defect fix is required.
3. Stage 1 boundary row subsetting must preserve matrix dimensions with
   `drop = FALSE` for single-row cases.
4. Fixture harnesses must mock prompt-based Stage 2 sample-size handling until
   the pure helper exists.
5. Deferring a persistent design signature allows a same-shaped but different
   design to pass structural compatibility checks. Revisit this if testing or
   usage demonstrates a practical risk.
6. Resolved 2026-09-09: issues #54, #55, #57–#64 were rewritten to match this
   living plan's `CERDesign`/`CERAnalysisState` split and `SetupDesign_CER()`
   naming (previously they described a single combined state object and
   `SetupAnalysis_CER()`, mirrored from the PC method). Issue #56 was left
   unchanged because it is closed and only concerns `adaptGMCP_CER()`
   regression fixtures, which are unaffected by the split.
7. Investigated 2026-09-10: the item 2 `getArmsFromHypo()` defect was confirmed
   to have no effect on the regression fixtures, because `mcpObj$ArmsPresent`
   (the field corrupted by the bug) is only consumed by the real, non-mocked
   `do_ModifyStage2Sample()`, which no regression scenario calls. The defect
   itself is still unresolved and remains a risk once the new API accepts real
   (non-mocked) sample-size adaptation.
8. Resolved 2026-09-09: `mcpObj$CERTab` is now exposed (see "Planned Files" /
   `R/cerAdaptGMCP_Analysis.R`), closing the gap that previously blocked
   capturing "structured CER and PCER results" per item 1.

## Governance Checklist

Before any checkpoint is marked complete, verify and report the applicable
items:

- [ ] Git sync gate completed
- [ ] Existing user changes preserved
- [ ] Epic #54 remains a Feature with the `epic` label
- [ ] Child issues remain Task issues linked to epic #54
- [ ] Epic and child issues share project membership
- [ ] Relevant issue states reflect actual progress
- [ ] This living plan reflects current scope and status
- [ ] Roxygen, generated man pages, and `NAMESPACE` are synchronized when needed
- [ ] `NEWS.md` and the `DESCRIPTION` version decision are reviewed
- [ ] Targeted validation is recorded
- [ ] No later implementation phase started without explicit user approval
