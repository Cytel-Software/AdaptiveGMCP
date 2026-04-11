# Architecture & Data Conventions — AdaptiveGMCP

## Architecture
- Simulation flow: `simMAMSMEP` builds a `gmcpSimObj` and drives per-look simulation and selection; see [R/MAMSMEP_SIMULATION_MAIN.R](../R/MAMSMEP_SIMULATION_MAIN.R) and wrapper in [R/simMAMSMEP_Wrapper.R](../R/simMAMSMEP_Wrapper.R).
- Analysis flows:
  - CER method: [R/cerAdaptGMCP_Analysis.R](../R/cerAdaptGMCP_Analysis.R) — stage 1/2 analysis with conditional error computation and optional adaptation.
  - P‑value combination: [R/pValueAdaptGMCP_Analysis.R](../R/pValueAdaptGMCP_Analysis.R) — inverse normal combination with rpact boundaries.
- Graph visualization: [R/graphPlot.R](../R/graphPlot.R) renders visNetwork graphs of hypotheses, weights, and transitions.
- Shiny app: `AdaptGMCPSimApp` is the exported entry point; UI/server defined in [inst/shinyApps/AdaptGMCPSimApp.R](../inst/shinyApps/AdaptGMCPSimApp.R). Modules in the same folder:
  - `tabularModule.R` — tabular input/output
  - `correlationMatrixModule.R` — correlation matrix editor
  - `transitionMatrixModule.R` — graph transition matrix editor
  - `IAModule.R` — interim analysis configuration
  - `helper_inputDataCsv.R` — CSV upload helpers

## Data & Hypothesis Conventions
- Hypothesis ordering: order by endpoint, then treatment (iterate treatments within each endpoint; e.g., for 2 trts × 3 eps: H1=EP1/T1, H2=EP1/T2, H3=EP2/T1, H4=EP2/T2, H5=EP3/T1, H6=EP3/T2). See UI note in [inst/shinyApps/AdaptGMCPSimApp.R](../inst/shinyApps/AdaptGMCPSimApp.R) and `WI`/`G` usage across functions.
- Weights and transitions: `WI` (initial weights) and `G` (transition matrix) drive intersection weights via `genWeights()`; conventions are consistent across simulation and analyses.
- Info fraction and allocation: `info_frac` is cumulative per-look; `simMAMSMEP` normalizes `Arms.alloc.ratio` so control equals 1.
- Two-arm constraint: For 2 arms, parametric tests are auto-downgraded (Bonferroni / Non‑Parametric); see logic in [R/MAMSMEP_SIMULATION_MAIN.R](../R/MAMSMEP_SIMULATION_MAIN.R).

## External Dependencies
- rpact: group sequential design boundaries and alpha spending (used to derive `stageLevels` and `alphaSpent`).
- Core runtime: `data.table`, `dplyr`, `mvtnorm`, `Matrix`, `matrixcalc`, `parallel`, `stringr`.
- Visualization/UI: `visNetwork`, `ggplot2`, `gridExtra`, `shiny`, `rhandsontable`, `htmltools`, `shinycssloaders`.
- See [DESCRIPTION](../DESCRIPTION) for full `Imports`/`Suggests`.
