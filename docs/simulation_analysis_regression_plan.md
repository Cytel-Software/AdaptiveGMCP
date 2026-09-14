# Simulation Analysis Regression Plan

## Objective

Add an opt-in CER simulation tracing path that preserves per-simulation and
per-look analysis outputs from `SingleSimCER()` through the existing
`simMAMSMEP()` stack, then writes replay fixtures as `.rds` files named
`cer_out_<ModelID>_sim_<SimID>_seed_<Seed>.rds`.

The initial implementation focuses on CER while reserving a compatible
placeholder structure for future p-value combination replay fixtures.

## CER Implementation Steps

1. Define a CER replay schema that separates:
   - design inputs for `SetupDesign_CER()`
   - replay inputs for each `AnalyzeLook_CER()` call
   - expected look-wise outputs to assert
   - metadata such as `ModelID`, `SimID`, `SimID_Stage2`, and `Seed`
2. Preserve explicit `SimID`, `LookID`, and `SimID_Stage2` in the CER
   simulation path before any stage-2 averaging occurs.
3. Keep the current power-summary behavior unchanged by storing replay traces
   separately from the existing summary aggregation.
4. Propagate the CER replay payload upward through `modified_MAMSMEP_sim2()`
   alongside the existing summary outputs.
5. Add optional wrapper-driven `.rds` writing using the agreed filename pattern
   `cer_out_<ModelID>_sim_<SimID>_seed_<Seed>.rds`.
6. Add targeted tests that:
   - validate trace payload structure and stage-2 indexing
   - confirm a saved or in-memory CER trace can be replayed through
     `SetupDesign_CER()` and `AnalyzeLook_CER()`
7. Update the CER living plan, roxygen, generated man pages, and release notes
   as required by repo governance.

## CER Design Decisions

- One fixture file is written per top-level simulation run.
- `SimID_Stage2` is stored inside the `.rds` payload, not in the filename.
- Fixture writing is additive and opt-in so standard simulation runs do not pay
  the I/O cost.
- The replay payload stores look-2 incremental p-values as analysis inputs and
  the CER-transformed cumulative p-values as expected outputs.

## P-Value Combination Placeholder

Future PC work should reuse the same replay concepts:

1. explicit per-look identifiers
2. replay inputs for `SetupAnalysis_PC()` / `AnalyzeLook_PC()`
3. expected look-wise outputs
4. optional per-simulation `.rds` writing with a PC-specific filename prefix

No PC implementation is included in this change.