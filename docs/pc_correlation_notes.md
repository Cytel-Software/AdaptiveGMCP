# PC Analysis Correlation Notes

This note explains the role of `Correlation` in `SetupAnalysis_PC()` and `new_correlation` in `AnalyzeLook_PC()`.

## Summary

- `Correlation` is the baseline correlation matrix for the hypothesis test statistics.
- `new_correlation` is an optional replacement matrix that can be supplied before analyzing a later look.
- These matrices affect the multiplicity-adjusted intersection p-values used by the p-value combination analysis.
- They do not directly change graph weights, transition rules, alpha-spending boundaries, or selection decisions.

## `Correlation` in `SetupAnalysis_PC()`

`SetupAnalysis_PC()` stores the initial correlation structure in `mcpObj$Correlation`.

Behavior depends on `test.type`:

- `"Dunnett"` and `"Partly-Parametric"`:
  - `Correlation` must be provided as a `d x d` matrix.
  - It is treated as the planned dependence structure among the test statistics.
  - If `test.type = "Dunnett"` and `Correlation` contains any `NA`, the code issues a warning and converts `test.type` to `"Partly-Parametric"`.
  - Aside from that warning-and-convert step, the new API does not apply distinct computational logic to these two labels at setup.
- `"Bonf"`:
  - The code replaces `Correlation` with a diagonal/`NA` matrix.
  - This effectively removes parametric dependence borrowing across different hypotheses.
- `"Sidak"` and `"Simes"`:
  - `Correlation` is set to `NA` and is not meaningfully used in the adjusted p-value calculation.

Key code:

- `R/PcAnalysisApi.R`: correlation setup and storage in `mcpObj$Correlation`
- `R/pValueAdaptGmcpHelper.R`: downstream use in adjusted p-value calculations

## `new_correlation` in `AnalyzeLook_PC()`

`AnalyzeLook_PC()` allows `new_correlation` only for looks after look 1.

- If provided, it is validated by `applyCorrelationUpdate()`.
- It must be a symmetric `d x d` matrix with diagonal equal to 1.
- Entries must lie in `[-1, 1]` or be `NA`.
- Once accepted, it replaces `mcpObj$Correlation`.
- If the current `test.type` is `"Dunnett"` and `new_correlation` contains any `NA`, the code issues a warning and converts `test.type` to `"Partly-Parametric"` before storing the replacement matrix.

Important detail:

- `new_correlation` is a full replacement, not an incremental patch.
- The matrix dimension stays tied to the full initial hypothesis set, not only the currently active hypotheses.

Key code:

- `R/PcAnalysisApi.R`: applies `new_correlation` before per-look analysis
- `R/PcAnalysisHelpers.R`: `applyCorrelationUpdate()`

## How Correlation Is Used in the Analysis

At each look, `PerLookMCPAnalysis()` passes `mcpObj$Correlation` into `compute_adjP()` for each intersection hypothesis.

- `compute_adjP()` uses the correlation matrix for `"Partly-Parametric"`, `"Dunnett"`, and `"Bonf"`.
- `comb.test()` then decides how to evaluate the intersection:
  - If a subset has known correlations, it uses a parametric multivariate normal probability calculation via `mvtnorm::pmvnorm()`.
  - If some pairwise correlations are unknown (`NA`), the code partitions the hypotheses into cliques of mutually known correlations.
  - Singletons or disconnected pieces fall back to Bonferroni-like handling for those pieces.
- After that conversion step, a `Dunnett` analysis with `NA` entries is effectively handled as mixed / partly-parametric for the affected intersections rather than using a distinct Dunnett-only rule.
- More generally, once the warning/normalization step is past, the downstream adjusted p-value computation does not distinguish between `"Dunnett"` and `"Partly-Parametric"`; the actual behavior is driven by the supplied correlation matrix and where it contains known values versus `NA`.

This means the correlation matrix controls how much parametric dependence information is used when computing adjusted p-values for intersection hypotheses.

## Role in the Full PC Analysis Flow

The p-value combination method works in two layers:

1. At each look, compute adjusted p-values for each relevant intersection hypothesis.
2. From look 2 onward, combine look-wise adjusted p-values across looks using the inverse-normal method.

`Correlation` and `new_correlation` matter in step 1:

- They determine the adjusted p-values produced at the current look.
- Those adjusted p-values are then carried forward into the inverse-normal combination step.
- Because of that, a correlation update can affect both the current rejection decision and all later combined p-values.

By contrast, these inputs do not directly control:

- graph structure updates (`new_weights`, `new_G`)
- hypothesis selection (`selection`)
- group-sequential boundaries from `rpact`
- inverse-normal weights derived from `info_frac`

## Practical Interpretation

Use `Correlation` to encode the planned dependence structure of the test statistics at setup.

Use `new_correlation` when that dependence structure should change between looks, for example because the analysis assumptions or the effective testing structure changed and the current look should use a different full correlation matrix.
