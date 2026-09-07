# Epic 54: Non-Interactive CER Design and Analysis API

## Status

- Epic: [#54](https://github.com/Cytel-Software/AdaptiveGMCP/issues/54)
- Branch: `54-CER-non-interactive-analysis-interface`
- Status: Planning; implementation has not started
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

## Compatibility Requirements

- `adaptGMCP_CER()` remains public and behavior-compatible.
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

### Phase 0: Repository and Baseline Gate

1. Run the mandatory Git state and synchronization checks.
2. Restore the `renv` environment.
3. Run the existing CER regression tests and record the baseline.
4. Keep this document updated as the living scope and progress ledger.

### Phase 1: Equivalence Baseline and Scenario Coverage

1. Inventory every active `adaptGMCP_CER()` call.
2. Convert each console workflow into a complete deterministic scenario.
3. Extend the fixture harness to support real selection, sample-size changes,
   and strategy updates instead of no-op mocks where required.
4. Expand fixture outputs to include:
   - Planned Stage 1 and Stage 2 boundaries
   - Stage 1 intersection and elementary decisions
   - Structured CER and PCER results
   - Active and dropped hypotheses
   - Planned and adapted sample allocations
   - Adapted covariance and Stage 2 boundaries
   - Cumulative Stage 2 p-values
   - Final rejection decisions
5. Preserve existing fixtures unless a verified defect requires an intentional
   fixture update.
6. Run the targeted CER regression suite.

**Review checkpoint 1:** Stop and present the complete scenario matrix,
fixtures, exclusions, ambiguities, and regression results. Do not begin Phase 2
without explicit approval.

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
10. Update issue wording that still refers to `SetupAnalysis_CER()` before the
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

1. Extract shared pure logic between the legacy and new APIs only after parity
   tests are green.
2. Keep console prompting and display behavior in the legacy API.
3. Harden validation and verify state/history invariants.
4. Complete print methods for both new classes.
5. Add initial, Look 1, and Look 2 graph reconstruction without coupling plots
   to analysis.
6. Run targeted CER tests after every refactor and run affected PC tests when a
   shared helper changes.
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
   - Any exported plotting helper
2. Explain design/state separation, `state = NULL` at Look 1, incremental Stage
   2 p-values, two-look enforcement, adaptation timing, history, and errors.
3. Add a fully non-interactive `internalData/SetupDesign_CER_Example.R` showing
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

- `R/CerDesign.R`: design constructor and S3 methods
- `R/CerAnalysisState.R`: analysis-state constructor and S3 methods
- `R/CerAnalysisApi.R`: `SetupDesign_CER()` and `AnalyzeLook_CER()`
- `R/cerStage1Analysis.R`: shared pure planned-design computation
- `R/cerStage2Analysis.R`: prompt-free Stage 2 sample-size handling
- `R/cerAdaptBoundary.R`: reused conditional-error recalibration
- `R/cerComputation.R`: structured CER/PCER results
- `tests/testthat/test-CerAnalysisApi.R`: setup and analysis tests
- `tests/testthat/test-CERRegressionApi.R`: complete compatibility baseline
- `internalData/GenerateCERRegressionFixtures.R`: deterministic fixtures
- `internalData/SetupDesign_CER_Example.R`: worked example
- `NEWS.md`, `DESCRIPTION`, `NAMESPACE`, and generated `man/` files

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
6. Existing issue titles and acceptance criteria refer to
   `SetupAnalysis_CER()`. They must be revised to `SetupDesign_CER()` before
   implementation issues are completed.

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
