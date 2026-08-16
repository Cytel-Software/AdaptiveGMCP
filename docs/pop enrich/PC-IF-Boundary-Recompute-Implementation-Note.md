# PC Analysis Interface: IF-Driven Boundary Recomputation Implementation Note

## Purpose
Capture the full technical context, decisions, and implementation details for Issue #127 so the non-interactive PC analysis interface can be updated safely and completely.

## Primary objective
Implement adaptive boundary recomputation for p-value combination (PC) analysis using actual per-look information fraction (IF), instead of using only setup-time planned IF boundaries.

## Why this change is needed
Current behavior computes boundaries once at setup using planned IF and then uses static cutoffs by look index. This is not robust when trial execution differs from plan.

Real-world scenarios that require adaptation:
- Unscheduled looks
- Off-schedule information timing
- Final look over-recruitment where IF may exceed 1.0

## In-scope and out-of-scope

In scope:
- Non-interactive PC API in `PcAnalysisApi.R`
- SetupAnalysis_PC
- AnalyzeLook_PC
- Supporting state/history updates needed by these APIs

Out of scope:
- Legacy interactive PC API in `pValueAdaptGMCP_Analysis.R`, planned for deprecation
- PE-PC implementation changes (to be done in follow-up issue after PC foundation is complete)

## Current baseline behavior (non-interactive API)
In `PcAnalysisApi.R`:
- SetupAnalysis_PC validates planned info_frac and builds design once with rpact getDesignGroupSequential
- SetupAnalysis_PC stores:
  - thresholds = des stageLevels
  - inverse-normal weights matrix derived from planned IF
- AnalyzeLook_PC currently sets current cutoff as thresholds[next_look]
- No per-look boundary recomputation using actual IF currently occurs

## External dependency behavior and constraint
Package rpact is used for group sequential design.
Function used: getDesignGroupSequential.

Important constraint:
- No direct single-look boundary update API exists for this use case.
- Therefore full design recomputation per look is the required pattern:
  - completed looks use actual IF
  - remaining looks use planned IF

This is acceptable in practice because typical look counts are small (often 2-3), so runtime overhead is negligible.

## Agreed design decisions

1. Actual per-look IF is mandatory in AnalyzeLook_PC
- Prevents inconsistent state where analysis proceeds with stale/planned-only timing assumptions.
- Makes look timing explicit and auditable.

2. Add configurable IF matching tolerance at setup
- New setup parameter: info_frac_tolerance
- Default: 0.05 absolute
- Stored in state design params

3. Finality determination is IF-driven, not look-count-driven
- is_final_look is true when info_frac_cur >= (1.0 - tolerance)
- This decouples completion semantics from planned look count and supports flexible execution patterns.

4. Interim IF rule is strict
- Interim if and only if info_frac_cur < (1.0 - tolerance)
- Interim IF must satisfy 0 < info_frac_cur < (1.0 - tolerance)
- If info_frac_cur is equal to or above (1.0 - tolerance), treat as final look

5. Final IF upper bound
- Final look must satisfy info_frac_cur <= (1.0 + tolerance)
- This permits practical over-recruitment near final look

6. Monotonic IF requirement for all looks
- info_frac_cur must be strictly increasing across analyzed looks
- For look > 1, info_frac_cur must be strictly greater than previous IF

7. Final-look IF above 1.0 is handled by standard rpact spending
- If current look is final and info_frac_cur > 1.0, keep using actual info_frac_cur for history and inverse-normal weights.
- For rpact compatibility, cap only the rpact information rate input at 1.0 for that final look.
- Use the resulting rpact boundaries directly (no separate remaining-alpha override call).

## IF mapping and matching logic
Match actual IF to planned IF using absolute tolerance:
- If |actual_info_frac - planned_info_frac[i]| <= tolerance, candidate match exists
- If multiple candidates exist, choose closest planned IF (minimum absolute distance)
- If no match exists, treat as unplanned timing event and continue with adaptive recomputation

Why this matters:
- Supports off-schedule timing while still aligning to intended structure when close
- Preserves deterministic mapping logic for downstream bookkeeping

## Boundary recomputation pattern per look
At each AnalyzeLook_PC call:

1. Collect IF history
- actual IF from completed looks
- current look IF from input

2. Reconstruct full IF trajectory for rpact call
- first segment: actual IF values for completed/current looks
- remaining segment: planned IF values for not-yet-observed looks
- do not force total length to the original planned kMax; use a dynamic reconstructed length
- set rpact kMax to length(reconstructed_if)

Pseudo-code for dynamic reconstruction:

```r
observed_info_frac <- c(previous_info_frac, info_frac_cur)
is_final_look <- info_frac_cur >= (1.0 - tolerance)

if (is_final_look) {
  # Final look: design ends at current observed trajectory.
  reconstructed_info_frac <- observed_info_frac
} else {
  # Not final: first match info_frac_cur to planned_info_frac using tolerance.
  matched_idx <- which(abs(planned_info_frac - info_frac_cur) <= tolerance)

  if (length(matched_idx) > 0L) {
    # Tie-break by closest planned IF.
    mapped_idx <- matched_idx[which.min(abs(planned_info_frac[matched_idx] - info_frac_cur))]
    if (mapped_idx < length(planned_info_frac)) {
      planned_future_info_frac <- planned_info_frac[seq.int(mapped_idx + 1L, length(planned_info_frac))]
    } else {
      planned_future_info_frac <- numeric(0)
    }
  } else {
    # No tolerant match: keep genuinely future planned looks.
    planned_future_info_frac <- planned_info_frac[planned_info_frac > info_frac_cur]
  }

  if (length(planned_future_info_frac) > 0L) {
    reconstructed_info_frac <- c(observed_info_frac, planned_future_info_frac)
  } else {
    stop(
      "No future planned IF remains in a non-final look. ",
      "Check IF finality logic or planned IF vector."
    )
  }
}

k_reconstructed <- length(reconstructed_info_frac)
rpact_info_frac <- reconstructed_info_frac
if (is_final_look && info_frac_cur > 1.0) {
  rpact_info_frac[length(rpact_info_frac)] <- 1.0
}
```

3. Recompute design
- call rpact getDesignGroupSequential with reconstructed IF, kMax = length(reconstructed_info_frac),
  and the same design options as setup
- if current look is final and info_frac_cur > 1.0, cap only the rpact IF input for that final look at 1.0

4. Refresh look-dependent analysis components
- stage-wise p-value boundary for current look
- inverse-normal combination weights derived from reconstructed IF increments

5. Continue PerLookMCPAnalysis using updated:
- mcpObj CutOff
- mcpObj W_Norm

## Validation logic (agreed simplified version)
Use the following conceptual flow:

- Set tolerance from state design params.
- Compute is_final_look as info_frac_cur >= (1.0 - tolerance).

If not final:
- Require 0 < info_frac_cur < (1.0 - tolerance)
- Error otherwise

If final:
- Require info_frac_cur <= (1.0 + tolerance)
- Error otherwise

Always:
- If look > 1, require info_frac_cur > previous_info_frac
- Error otherwise

Note:
- This avoids redundant lower-bound checks in final branch because final status already encodes lower-threshold crossing.

## Detailed implementation checklist (non-interactive API)

SetupAnalysis_PC in `PcAnalysisApi.R`:
- Add parameter info_frac_tolerance with default 0.05
- Add argument validation for tolerance (numeric scalar, non-missing, strictly > 0 and < 1)
- Add roxygen entry documenting semantics
- Persist tolerance in design_params

AnalyzeLook_PC in `PcAnalysisApi.R`:
- Add mandatory parameter info_frac_cur
- Add roxygen entry documenting required per-look IF input
- Validate info_frac_cur type and length
- Apply strict interim/final validation logic
- Apply monotonicity validation against prior IF
- Maintain actual IF history in state look_history
- Reconstruct IF vector for current call
- Recompute rpact design per look using reconstructed IF
- If final look has info_frac_cur > 1.0, cap only rpact IF input at that final look to 1.0
- Update current boundary and inverse-normal weights in mcpObj
- Ensure state updates persist recomputed quantities and IF history

State/history expectations:
- look_history entry for each analyzed look should include:
  - actual IF used
  - any mapped/planned look metadata if recorded
  - inputs used at that look
  - snapshot mcpObj as needed by existing plot/history behavior

## Edge cases to cover in tests

IF classification:
- info_frac_cur exactly equal to (1.0 - tolerance) must be final
- info_frac_cur just below (1.0 - tolerance) must be interim
- info_frac_cur above (1.0 + tolerance) must fail final upper-bound validation
- info_frac_cur <= 0 must fail interim validation
- when final info_frac_cur > 1.0, boundary must match rpact output evaluated at capped final IF input (1.0)

Monotonicity:
- equal IF across consecutive looks must fail
- decreasing IF must fail

Mapping behavior:
- single planned match within tolerance
- multiple planned matches within tolerance, closest wins
- no planned match, treated as unplanned timing

Recomputation behavior:
- thresholds change appropriately when actual IF deviates from plan
- inverse-normal weights recomputed from reconstructed IF
- consistency with static behavior when actual IF equals planned IF exactly

Trial progression behavior:
- finality triggered by IF crossing, not only planned look index
- supports over-recruitment final IF up to upper tolerance

## Documentation updates to include during implementation
- Roxygen for both updated functions in `PcAnalysisApi.R`
- Any relevant architecture/process notes in `architecture.md` if project governance requires
- Ensure issue linkage and changelog/update notes follow repository process

## Risks and safeguards
Risk:
- Recomputed IF trajectory length or ordering mistakes can silently alter boundaries.

Safeguards:
- Strict validation on IF monotonicity and branch classification
- Unit tests for boundary/weight recomputation determinism
- Compare against baseline planned-equals-actual scenarios

## Summary for implementation order
1. Add setup tolerance parameter and storage.
2. Add mandatory per-look IF parameter to AnalyzeLook_PC.
3. Implement strict interim/final validation and monotonicity checks.
4. Implement IF history tracking and reconstructed IF rpact recomputation.
5. Update cutoff and inverse-normal weights per look.
6. Add comprehensive tests for edge cases and recomputation correctness.
7. Keep interactive API unchanged (deprecation path).
