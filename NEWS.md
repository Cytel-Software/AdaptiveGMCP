# AdaptGMCP 2.1.0 (2026-04-11)

## New Features

### Non-Interactive Analysis Interface (P-Value Combination Method)
* `SetupAnalysis_PC()`: New stateless setup function that initialises a `PCAnalysisState`
  object from design parameters (graph weights, transition matrix, test type, alpha level,
  information fractions, group-sequential design, and correlation). No console prompts — all
  inputs are passed as function arguments.
* `AnalyzeLook_PC()`: Advances a `PCAnalysisState` object by exactly one look. Accepts the
  raw p-values for active hypotheses and optional adaptation inputs (selection, weight/graph
  update, correlation update). Returns the updated state object.
* `PlotAnalysisGraph()`: Plots the hypothesis graph stored in a `PCAnalysisState` object at
  any stage of the analysis (initial graph, after a specific look, or current).

These functions are designed for automated pipelines, batch processing, and scripted analyses
where interactive console input is not appropriate. They replicate the behaviour of the
existing interactive `adaptGMCP_PC()` function.

---

# AdaptGMCP 2.0.0 (2026-01-16)

## Major Release

This is the first stable production release of AdaptGMCP, transitioning from date-based versioning to semantic versioning.

## New Features

### Multi-Endpoint Support
* **Multiple Continuous Endpoints**: Full support for analysis and simulations for adaptive multi-arm trials with multiple continuous endpoints
* **Multiple Binary Endpoints**: Complete implementation of analysis and simulations for trials with multiple binary endpoints  
* **Mixed Endpoints**: Support for analysis and simulations for trials combining continuous and binary endpoints

### Analysis Functions
* `adaptGMCP_CER()`: Adaptive GMCP analysis using Conditional Error Rate (CER) method and closed testing
  - Supports continuous, binary, and mixed endpoints
  - Parametric, non-parametric, and partly-parametric test options
  - Two-stage adaptive designs with optional stage 2 adaptation
  - Multiple winner selection strategies
  - Flexible update strategies
  - Interactive graph visualization
  
* `adaptGMCP_PC()`: Adaptive GMCP analysis using Combined P-Value (PVCombo) method and closed testing
  - Supports continuous, binary, and mixed endpoints
  - Parametric, non-parametric, and partly-parametric test options
  - Two-stage adaptive designs with optional stage 2 adaptation
  - Multiple winner selection strategies
  - Flexible update strategies
  - Interactive graph visualization

### Simulation Capabilities
* `simMAMSMEP()`: Comprehensive simulation capability for Multi-Arm Multi-Endpoint trials with 2 stages
  - Supports continuous, binary, and mixed endpoints
  - Supports both PVCombo and CER methods
  - Parametric, non-parametric, and partly-parametric test options
  - Uses closed testing for strong FWER control
  - Configurable test statistics for continuous (t-test equal/unequal variance) and binary (pooled/unpooled) endpoints
  - Endpoint correlation modeling
  - Parallel processing support for large-scale simulations

## Breaking Changes

* Version numbering changed from date-based (13.6.24) to semantic versioning (2.0.0)
* This marks the transition to a stable, production-ready API
* Functions removed: simMAMSMEP_CONT(), gmcpPlot(), adaptGMCP_BIN_CER(), adaptGMCP_CONT_CER(), MAMSMEP_sim2(), mnMAMSMEP_sim(), SingleSimAnalysis(), adaptGMCP_CER()

---

# AdaptGMCP 13.6.24 (2024-12-03)

## Pre-release: MAMSMEP Binary

Initial pre-release focusing on continuous and binary endpoint functionality for multi-arm multi-stage multi-endpoint trials.

* Working and well tested implementation of analysis and simulations with multiple continuous endpoints
* Untested implementation of binary endpoints
* Comprehensive test suite added
* Documentation expanded with detailed examples
