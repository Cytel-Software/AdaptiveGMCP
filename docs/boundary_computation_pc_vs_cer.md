# Stopping Boundary: `adaptGMCP_PC()` vs `adaptGMCP_CER()`

This note compares how the two analysis functions compute the stopping boundary
when a parametric test is requested (`test.type = "Dunnett"` in the PC method,
`test.type = "Parametric"` in the CER method). Both names refer to the same
weighted-Dunnett parametric test, but the two functions use the correlation at
opposite ends of the decision rule.

## Summary

- `adaptGMCP_PC()`: the stopping boundary is **independent of `test.type`**. It is
  a pure group-sequential alpha-spending boundary from `rpact`. The correlation
  enters only when raw p-values are turned into adjusted/combined p-values.
- `adaptGMCP_CER()`: the stopping boundary **depends on `test.type` and the
  correlation**. The critical value is obtained by numerically inverting a
  multivariate-normal boundary-crossing probability, so the parametric correlation
  is baked into the boundary itself.
- Consequently the two computations are **not** the same for parametric tests.

## `adaptGMCP_PC()` (p-value combination / inverse normal)

- The boundary comes entirely from `rpact::getDesignGroupSequential()` using
  `alpha`, `informationRates = info_frac`, and `typeOfDesign`.
- `Threshold <- des$stageLevels` (p-value-scale boundary) and
  `des$criticalValues` (Z-scale) are the same for every `test.type`
  (`Bonf`, `Sidak`, `Simes`, `Dunnett`, `Partly-Parametric`).
- `test.type` / `Correlation` affect only the multiplicity-adjusted (combined)
  p-values, which are then compared against the common per-look cutoff
  (`CutOff = stageLevels[look]`).
- Net effect: the correlation moves the **test-statistic** side of the inequality,
  not the boundary side.

Key code:

- `R/pValueAdaptGMCP_Analysis.R`: boundary built from `rpact` before `test.type`
  is inspected; stored in `bdryTab`.
- `R/PcAnalysisApi.R`: same `stageLevels` boundary recomputed per look.
- `R/pValueAdaptGmcpHelper.R`: adjusted p-values compared to `CutOff`.

## `adaptGMCP_CER()` (conditional error rate)

- `rpact` is used **only** to obtain the stage-1 spent alpha (`alpha1`).
- The actual stage-wise critical values `cJ1` (stage 1) and `cJ2` (stage 2) are
  found with `uniroot()`, solving for the value at which the total
  boundary-crossing probability under the null equals the allocated alpha.
- For a parametric (Dunnett) connected subset, the crossing probability is a
  **multivariate-normal** exit probability computed via `mvtnorm::pmvnorm()` using
  the correlation. Non-parametric singletons use a plain `pnorm`.
- The boundary is therefore correlation-dependent and is produced as a
  per-intersection, per-hypothesis matrix (`Stage1Bdry`, `Stage2Bdry`) scaled by
  the intersection weights, rather than a single per-look sequence.

Key code:

- `R/cerPlanBoundary.R`: `uniroot` inversion for `cJ1` / `cJ2`.
- `R/cerTotalCrossingProbFuns.R`, `R/cerBoundaryParametric.R`: `pmvnorm`-based
  exit probabilities.
- `R/cerConnectedSets.R`: `connSets()` classifies parametric vs non-parametric
  subsets.

## Side-by-side

| Aspect | `adaptGMCP_PC()` | `adaptGMCP_CER()` |
|---|---|---|
| Boundary source | `rpact` group-sequential `stageLevels` / `criticalValues` | `uniroot` inversion of a crossing probability; `rpact` only supplies `alpha1` |
| Depends on `test.type`? | No | Yes |
| Depends on correlation? | No (boundary); correlation affects adjusted p-values | Yes; correlation enters the boundary via `mvtnorm::pmvnorm` |
| Boundary shape | One value per look (a vector) | Matrix: per intersection hypothesis x hypothesis, weight-scaled |
| Where multiplicity is handled | On the statistic side (adjusted p-values vs common cutoff) | On the boundary side (critical value absorbs the joint structure) |
| Parametric test label | `"Dunnett"` (or `"Partly-Parametric"`) | `"Parametric"` (weighted Dunnett) |
