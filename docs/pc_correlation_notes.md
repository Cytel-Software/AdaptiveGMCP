# PC Analysis Correlation Notes

This note explains the role of `Correlation` in the stateless PC analysis API.

## Summary

- `SetupAnalysis_PC()` no longer accepts a `Correlation` argument.
- `AnalyzeLook_PC()` accepts `Correlation` as a look-level input.
- For `test.type = "Dunnett"` or `"Partly-Parametric"`, `Correlation` is mandatory at look 1.
- For later looks, `Correlation` is optional; if omitted, the most recently stored matrix is reused.
- These matrices affect the multiplicity-adjusted intersection p-values used by the p-value combination analysis.
- They do not directly change graph weights, transition rules, alpha-spending boundaries, or selection decisions.

## `Correlation` in `SetupAnalysis_PC()`

`SetupAnalysis_PC()` does not accept user-supplied correlation input.

Behavior depends on `test.type`:

- `"Dunnett"` and `"Partly-Parametric"`:
  - `mcpObj$Correlation` is initialized to `NULL`.
  - The required matrix is supplied at look 1 through `AnalyzeLook_PC(Correlation = ...)`.
- `"Bonf"`:
  - `mcpObj$Correlation` is initialized to a diagonal/`NA` matrix.
  - This effectively removes parametric dependence borrowing across different hypotheses.
- `"Sidak"` and `"Simes"`:
  - `mcpObj$Correlation` is initialized to `NA` and is not meaningfully used in the adjusted p-value calculation.

Key code:

- `R/PcAnalysisApi.R`: correlation setup and storage in `mcpObj$Correlation`
- `R/pValueAdaptGmcpHelper.R`: downstream use in adjusted p-value calculations

## `Correlation` in `AnalyzeLook_PC()`

`AnalyzeLook_PC()` accepts `Correlation` as a look-level argument.

- At look 1, `Correlation` is required for `test.type = "Dunnett"` and `"Partly-Parametric"`.
- At look > 1, `Correlation` is optional for those test types.
- If supplied, it is validated by `applyCorrelationUpdate()` and replaces the stored matrix.
- If omitted at look > 1, the previously stored matrix remains in effect.
- It must be a symmetric `d x d` matrix with diagonal equal to 1.
- Entries must lie in `[-1, 1]` or be `NA`.
- If the current `test.type` is `"Dunnett"` and `Correlation` contains any `NA`, the code issues a warning and converts `test.type` to `"Partly-Parametric"` before storing the replacement matrix.

Important detail:

- `Correlation` is a full replacement when provided, not an incremental patch.
- The matrix dimension stays tied to the full initial hypothesis set, not only the currently active hypotheses.

Key code:

- `R/PcAnalysisApi.R`: applies `Correlation` before per-look analysis/adaptation
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

`Correlation` matters in step 1:

- They determine the adjusted p-values produced at the current look.
- Those adjusted p-values are then carried forward into the inverse-normal combination step.
- Because of that, a correlation update can affect both the current rejection decision and all later combined p-values.

By contrast, these inputs do not directly control:

- graph structure updates (`new_weights`, `new_G`)
- hypothesis selection (`selection`)
- group-sequential boundaries from `rpact`
- inverse-normal weights derived from `info_frac`

## Practical Interpretation

For `"Dunnett"` and `"Partly-Parametric"`, supply `Correlation` at look 1.

At later looks, supply `Correlation` only when the dependence structure needs to change; otherwise omit it to keep using the current stored matrix.
