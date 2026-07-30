# Simulation Endpoint Correlation Note

This note clarifies how `simMAMSMEP()` uses `EP.Corr`, especially when `nEps > 1` and the user selects:

- `Method = "CombPValue", test.type = "Dunnett"`
- `Method = "CER", test.type = "Parametric"`

## Short answer

`EP.Corr` is used to generate correlated endpoint data within each arm, but it is not propagated into a full cross-endpoint test-statistic correlation matrix for the multiplicity adjustment.

As a result, when multiple endpoints are present:

- the simulation data are correlated across endpoints according to `EP.Corr`
- the formal multiplicity adjustment is parametric within endpoint blocks
- cross-endpoint correlation is treated as unknown in the analysis layer

So in multi-endpoint simulations, `Dunnett` / `Parametric` are not fully parametric across all hypotheses. In practice they behave very similarly to `Partly-Parametric` whenever the active set spans multiple endpoints.

## How `EP.Corr` is used

In `simMAMSMEP()`, the user-supplied `EP.Corr` matrix is stored in the simulation object and passed into subject-level response generation.

During data generation:

- `singleSimulation.R` passes `EPCorr = mcpObj$EP.Corr` into `genIncrLookSummary()`
- `genIncrLookSummary()` passes that matrix to `genNormToOther2(..., mNormCorr = EPCorr, ...)`
- `genNormToOther2()` uses `mNormCorr` to construct the arm-wise multivariate covariance matrix across endpoints

Therefore `EP.Corr` affects the simulated joint endpoint outcomes and, indirectly, the realized raw p-values.

## How analysis correlation is handled

### Combining p-values method

For `Method = "CombPValue"`, the planned correlation matrix used by the GMCP analysis is built by `getPlanCorrelation()`.

That function does **not** take `EP.Corr` as input. Instead, it uses:

- sample sizes / allocation ratios
- endpoint type
- arm standard deviations for continuous endpoints
- control proportions for binary endpoints
- `test.type`

For multiple endpoints, `getPlanCorrelation()` builds separate within-endpoint correlation blocks and combines them with a block-diagonal structure. Cross-endpoint off-block entries are then set to `NA`.

So:

- within the same endpoint, treatment-vs-control test statistics may be handled parametrically
- across different endpoints, correlation is treated as unknown

### CER method

For `Method = "CER"`, the planned covariance objects are built by `getSigma()`.

That function also does **not** use `EP.Corr`. It constructs endpoint-specific information/covariance objects separately for each endpoint.

Later, the CER code uses `connSets()` to partition active hypotheses by endpoint group when `test.type` is `"Parametric"` or `"Partly-Parametric"`.

So again:

- parametric handling is endpoint-specific
- cross-endpoint dependence is not modeled parametrically in the multiplicity adjustment

## Practical implication

When `nEps > 1`, users should interpret:

- `Dunnett` in simulation as "parametric within endpoint, unknown across endpoints"
- `Parametric` in simulation as "parametric within endpoint, unknown across endpoints"

This means that in many multi-endpoint simulation settings:

- `Dunnett` versus `Partly-Parametric` for `Method = "CombPValue"`
- `Parametric` versus `Partly-Parametric` for `Method = "CER"`

will differ little or not at all in the way cross-endpoint multiplicity is handled.

## Summary

`EP.Corr` currently drives correlated endpoint data generation, not full cross-endpoint parametric multiplicity adjustment.

If a future enhancement is desired, it would require deriving and using a full hypothesis-level test-statistic correlation structure across endpoints from `EP.Corr` in the analysis layer.
