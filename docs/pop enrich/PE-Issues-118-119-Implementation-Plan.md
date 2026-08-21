## Plan: PE Issues #118 + #119 Implementation

Implement PE-side IF computation and look-wise boundary recomputation by deriving `info_frac_cur` internally from cumulative full-population control-arm sample size versus planned full-population control-arm baseline, then passing that computed IF into the updated PC API.

**Steps**
1. Phase 1: PE Contract + State (Issue #118)
2. Update `SetupAnalysis_PE_PC()` in `PeAnalysisApi.R` to accept planned baseline vectors mirroring analyze-time sample-size shapes.
3. Add `planned_fullpop_sample_sizes` and `planned_subpop_sample_sizes` as setup parameters.
4. Validate planned vectors: numeric, finite, positive, equal lengths, control plus at least one treatment, and `planned_subpop_sample_sizes <= planned_fullpop_sample_sizes` elementwise.
5. Persist planned baseline vectors in state for downstream looks.
6. Update roxygen for setup function and regenerate man pages.
7. Phase 2: PE IF Computation Utility + Planned-vs-Actual Gates (Issue #118)
8. Add helper in `PeAnalysisApi.R` to compute `info_frac_cur` from:
9. Numerator: cumulative full-population control-arm sample size at current look.
10. Denominator: planned full-population control-arm baseline sample size.
11. Do not use subgroup sample sizes for IF calculation.
12. Add analyze-time validation that cumulative look-wise sample sizes are <= corresponding planned baseline sample sizes (full and subpopulation vectors, arm-by-arm).
13. Add guardrails: denominator > 0, finite ratio, monotone IF progression checks, compatibility with PC tolerance/finality checks.
14. Store computed IF and denominator components in look history for audit traceability.
15. Phase 3: Wire into PE Analyze (Issue #119)
16. Remove `info_frac_cur` from external `AnalyzeLook_PE_PC()` parameters.
17. Compute `info_frac_cur` internally each look.
18. Pass computed IF to `AnalyzeLook_PC(..., info_frac_cur = computed_if, ...)`.
19. Remove planned-IF fallback logic from PE analyze path.
20. Keep correlation derivation and selection sequencing unchanged.
21. Confirm tolerance-consistent terminal behavior for control-arm-near-planned scenarios.
22. Phase 4: Tests
23. Extend `test-PeAnalysisApi.R` for:
24. valid planned baseline + valid look inputs,
25. missing/malformed planned baseline failures,
26. failures when cumulative sample size exceeds corresponding planned sample size,
27. multi-arm control-arm IF behavior,
28. sequential persistence of planned fields, computed IF, and denominator components,
29. delayed-look and early-look scenarios with boundary change,
30. control-arm-near-planned scenario triggering PC final-look behavior via tolerance,
31. planned-equals-actual scenario preserving outputs.
32. Keep PC regression green in `test-PcAnalysisApi.R` and `test-PcAnalysisApi-InfoFracRecompute.R`.
33. Phase 5: Docs + Examples + Validation
34. Update roxygen in `PeAnalysisApi.R` and regenerate `SetupAnalysis_PE_PC.Rd` and `AnalyzeLook_PE_PC.Rd`.
35. Update PE example script(s) in `internalData` with planned baseline setup inputs and internal IF behavior.
36. Run package-aware targeted PE/PC validation.

**Decisions Applied**
- Remove `info_frac_cur` from external PE API; compute internally.
- Compute IF only from full-population control-arm cumulative vs planned baseline control-arm sample size.
- Add planned baseline setup inputs mirroring analyze-time sample-size vectors.
- Enforce cumulative look-wise sample sizes <= planned sample sizes.
- Store computed IF plus denominator components in PE look history for full audit traceability.

## Implementation Status Update

Implemented in code:
- `SetupAnalysis_PE_PC()` now requires `planned_fullpop_sample_sizes` and `planned_subpop_sample_sizes`.
- Planned baseline vectors are validated during setup and stored under `state$design_params`.
- `AnalyzeLook_PE_PC()` no longer accepts external `info_frac_cur`.
- `AnalyzeLook_PE_PC()` computes IF internally from control-arm cumulative/planned full-population sample size.
- Analyze-time validation now enforces cumulative look sample sizes are not greater than planned vectors.
- Computed IF and denominator components are persisted in both `state$pe_sample_history[[look]]` and `state$look_history[[look]]$pe_info_frac`.
- `internalData/PopEnrich_Analysis_Ex.R` was updated so all `SetupAnalysis_PE_PC()` calls include planned sample-size vectors.
- `tests/testthat/test-PeAnalysisApi.R` was updated for the new setup/analyze contract and IF-history checks.

Pending validation/documentation regeneration:
- Regenerate Rd docs with `devtools::document()`.
- Run targeted package-aware tests for PE API (and PC regressions if needed).

## Issue #120 Scope Update (PE Two-Look Guardrails)

Decision applied:
- PE analysis path supports one-look and two-look workflows.
- PE analysis requests with more than two looks are rejected at setup with clear guidance.
- PC API remains unconstrained and continues to support generic multi-look workflows.

Implementation updates:
- `SetupAnalysis_PE_PC()` now validates `planned_info_frac` length for PE scope (allowed: 1 or 2).
- `test-PeAnalysisApi.R` includes a positive one-look setup test and a negative >2-look setup test.
- `internalData/PopEnrich_Analysis_Ex.R` PE examples were aligned to one- or two-look workflows only.
