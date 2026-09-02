# Issue 39 Plan: Contract Tests for Remaining Exported APIs

## Status
- Planning only.
- No test implementation and no test execution in this step.

## Objective
Add contract tests for the remaining exported APIs not covered by the existing PC and PE analysis API contract tests:
1. `simMAMSMEP`
2. `simMAMSMEP_Wrapper`
3. `genPowerTablePlots`

## Agreed Constraints
1. Do not introduce slow-test tagging (no `skip_if(Sys.getenv("SKIP_SLOW_TESTS") == "1")` and no custom slow marker tags).
2. Keep tests in the default fast lane unless they are explicitly moved later by file-level CI lane curation.
3. For all newly added tests, set `Parallel = FALSE`.
4. For `simMAMSMEP` positive tests, do not use examples from `.R` script examples as primary sources.
5. Positive `simMAMSMEP` scenarios must be derived from CSV-driven scenarios.
6. Use low simulation counts to keep tests fast:
- `nSimulation = 10`
- `nSimulation_Stage2 = 10` for CER tests

## CSV Sources for Positive Scenarios
Use these files as the source of positive contract scenarios:
1. `internalData/Mixed-2OrMoreEPs.csv`
2. `internalData/BatchInput_FS_GMCP_Sim_Bin.csv`
3. `internalData/CER_Inp_1ep5arms - Continuous.csv`

These sources jointly cover:
- Binary endpoints
- Continuous endpoints
- Mixed endpoint combinations
- CER and p-value combination methods

## API-by-API Test Plan

### 1) `simMAMSMEP` Contract Tests

#### Positive contract cases
1. Build test inputs from selected rows in the three CSV files above.
2. Map CSV columns to `simMAMSMEP` arguments using the same semantics as `run1TestCase` in `R/simMAMSMEP_Wrapper.R`.
3. Override runtime controls in test fixtures:
- `nSimulation = 10`
- `nSimulation_Stage2 = 10` for CER rows
- `Parallel = FALSE`
4. Validate stable contract-level invariants, not fragile exact power values:
- Return object is a list with expected top-level components.
- Output includes power summary structure expected by downstream wrappers/plot helpers.
- Key fields are present and non-empty for valid scenarios.

#### Negative/validation cases
1. Reject malformed endpoint definitions (for example, inconsistent `nEps` vs endpoint inputs).
2. Reject invalid look structure when two-look assumptions are required by scenario.
3. Reject invalid or inconsistent selection and SSR parameter combinations.
4. Reject malformed graph/weight inputs (`WI`, `G`) or mismatched dimensions.

### 2) `simMAMSMEP_Wrapper` Contract Tests

#### Positive contract cases
1. Use a filtered data frame from each CSV source as wrapper input.
2. Confirm wrapper executes scenario rows and returns data frame output with required columns.
3. Validate wrapper applies mapping behavior consistent with `run1TestCase` argument conversion.
4. Ensure tests set or override:
- `nSimulation = 10`
- `nSimulation_Stage2 = 10` for CER scenarios
- `Parallel = FALSE`

#### Negative/validation cases
1. Missing required columns in input data frame should fail clearly.
2. Invalid expression-like columns (that cannot be parsed/evaluated) should fail with actionable errors.
3. Inconsistent per-row dimensions (for example between endpoint definitions and arm-level vectors) should fail.

### 3) `genPowerTablePlots` Contract Tests

#### Positive contract cases
1. Construct minimal representative `dfOut` and `TableTemDF` fixtures from wrapper-style output.
2. Run for both supported power modes used by current workflow.
3. Validate returned object contract:
- Contains `TableWide` and `TableLong`.
- Both are data frames with expected core columns.
- Row counts and key identifiers are internally consistent.

#### Negative/validation cases
1. Unsupported `PowerType` should error clearly.
2. Missing required columns in `dfOut` or template should error clearly.
3. Non-matching template and output keys should fail deterministically.

## Input-Parameter Coverage Strategy
Across the selected CSV scenarios and targeted negatives, cover:
1. Method variants: CER and combination p-value routes.
2. Endpoint configurations: binary-only, continuous-only, and mixed.
3. Two-look information fractions and look-dependent settings.
4. Selection combinations (`best`, `threshold`, keep-associated-hypothesis variants).
5. SSR options and no-SSR baseline.
6. Graph and weight definitions for multiple hypotheses.

## Proposed Test Organization
Add dedicated files under `tests/testthat/`:
1. `test-SimMAMSMEPApi.R`
2. `test-SimMAMSMEPWrapperApi.R`
3. `test-GenPowerTablePlotsApi.R`

Keep helper builders local to each file unless shared helpers are clearly reusable.

## Incremental Implementation and Approval Gate
Execution must proceed in small gated sections, not as one bulk implementation.

For each section:
1. Implement only that section.
2. Run only the targeted tests for that section.
3. Fix any failures or contract mismatches found in those tests.
4. Share results for review.
5. Wait for explicit user approval before starting the next section.

Section order:
1. `simMAMSMEP` positive contract cases.
2. `simMAMSMEP` negative/validation cases.
3. `simMAMSMEP_Wrapper` positive contract cases.
4. `simMAMSMEP_Wrapper` negative/validation cases.
5. `genPowerTablePlots` positive contract cases.
6. `genPowerTablePlots` negative/validation cases.

## Execution and Validation Plan (Deferred)
When implementation starts (not in this step):
1. Follow the section order and approval gate in "Incremental Implementation and Approval Gate".
2. After each section, run targeted tests only for the section just implemented.
3. Resolve section-local failures before requesting approval to continue.
4. If runtime exceeds fast-lane expectations, optimize fixtures first before any lane reclassification.

## Done Criteria
1. Contract tests exist for all three remaining APIs.
2. Positive tests are sourced from the three agreed CSV files.
3. All new tests use `Parallel = FALSE`.
4. All new simulation-based tests use `nSimulation = 10`, and CER tests use `nSimulation_Stage2 = 10`.
5. Tests assert API contract shape and validation behavior without brittle full-output snapshots.
6. No new slow-test tag mechanism is introduced.
7. Work is delivered and approved section-by-section before moving to the next section.
