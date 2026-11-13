# AdaptGMCP

## Installation

You can install the AdaptGMCP package in R directly from the GitHub repository using the `remotes` package:

```R
remotes::install_github("Cytel-Software/AdaptiveGMCP", ref = "master")
```

This will download and install the latest version of the package from the master branch.

## Important Functions

The package provides APIs for interactive analyses, scripted look-by-look
analyses, simulation, and graph visualization:

- **Interactive analyses:** `adaptGMCP_CER` (Conditional Error Rate) and
  `adaptGMCP_PC` (p-value combination).
- **Non-interactive CER analysis:** `SetupAnalysisCER` initializes a design,
  `AnalyzeLookCER` analyzes each look, and `PlotGraphCER` displays its graph.
- **Non-interactive PC analysis:** `SetupAnalysisPC` initializes a design,
  `AnalyzeLookPC` analyzes each look, and `PlotAnalysisGraph` displays its
  graph. `SetupAnalysisPEPC` and `AnalyzeLookPEPC` provide the population-
  enrichment specialization.
- **Simulation:** `simMAMSMEP` runs adaptive multi-arm, multi-stage,
  multi-endpoint simulations; `GeneratePowerTablePlots` returns wide and long
  power-summary tables.
- **Graph visualization:** `plotGraph` displays a hypothesis graph.

For detailed usage and arguments of each function, please refer to the help section in R after installing the package (e.g., `?simMAMSMEP`).

## Quick Start

### `simMAMSMEP`

- **Example script:** [internalData/MAMSMEP_Simulation_Example.R](internalData/MAMSMEP_Simulation_Example.R)
- **How to use:** Open and run the script in R to see a typical workflow.
- **Power summaries:** Use `GeneratePowerTablePlots` to return wide and long
  power tables from simulation results.
- **Documentation:** For detailed documentation on all arguments, run:
  ```R
  ?simMAMSMEP
  ?GeneratePowerTablePlots
  ```

### `adaptGMCP_CER`

- **Example script:** [internalData/AdaptGMCP_CER_Analysis_Example.R](internalData/AdaptGMCP_CER_Analysis_Example.R)
- **How to use:** Open and run the script in R to perform analysis using the Conditional Error Rate method.
- **Documentation:** For detailed documentation on all arguments, run:
  ```R
  ?adaptGMCP_CER
  ```

### Non-Interactive Analysis Interface (Conditional Error Rate Method)

For scripted CER analyses without console prompts, use `SetupAnalysisCER`
to create the design state and call `AnalyzeLookCER` once for each formal
look. The returned state carries the analysis forward; `PlotGraphCER` plots
the planned or analyzed hypothesis graph. This interface supports two-look
designs.

- **Example script:** [internalData/CER_Analysis_NonInteractive_Example.R](internalData/CER_Analysis_NonInteractive_Example.R)
- **Documentation:** For detailed documentation on all arguments, run:
  ```R
  ?SetupAnalysisCER
  ?AnalyzeLookCER
  ?PlotGraphCER
  ```

### `adaptGMCP_PC`

- **Example script:** [internalData/AdaptGMCP_Analysis_Example.R](internalData/AdaptGMCP_Analysis_Example.R)
- **How to use:** Open and run the script in R to use the p-value combination method for your analysis.
- **Documentation:** For detailed documentation on all arguments, run:
  ```R
  ?adaptGMCP_PC
  ```

### Non-Interactive Analysis Interface (P-Value Combination Method)

For automated pipelines, batch processing, or scripted analyses, the non-interactive interface
avoids R console prompts by accepting all look-level inputs as function arguments.
The generic PC interface supports one, two, three, or more planned looks.
Look 1 always uses the initial design from `SetupAnalysisPC()`. If
`vSelectHypo`, `vWeights`, or `mGMatrix` is supplied at
look 1, it is ignored with a warning;
these adaptations may be applied at look 2 or later when enabled in the setup.

**Key functions:**
- `SetupAnalysisPC()`: Initialises the analysis state (design parameters, graph, boundaries).
- `AnalyzeLookPC()`: Processes one interim or final look, returning an updated state object.
- `PlotAnalysisGraph()`: Plots the hypothesis graph at any stage of the analysis.

**Example:**

```R
library(AdaptGMCP)

WI <- c(0.5, 0.5, 0, 0)
G  <- matrix(c(
  0,   0.5, 0.5, 0,
  0.5, 0,   0,   0.5,
  0,   1,   0,   0,
  1,   0,   0,   0
), byrow = TRUE, nrow = 4)

Correlation <- matrix(c(
  1,   0.5, 0.5, NA,
  0.5, 1,   NA,  0.5,
  0.5, NA,  1,   0.5,
  NA,  0.5, 0.5, 1
), byrow = TRUE, nrow = 4)

# Set up the analysis
state <- SetupAnalysisPC(
  vWeights       = WI,
  mGMatrix    = G,
  strTestType          = "Partly-Parametric",
  dAlpha               = 0.025,
  vInfoFracts = c(0.5, 0.7, 1),
  strDesignType        = "asOF",
  bPlotGraphs          = FALSE
)

# Look 1
state <- AnalyzeLookPC(
  state,
  vRawPVals = c(H1 = 0.01, H2 = 0.20, H3 = 0.15, H4 = 0.30),
  mCorrelation = Correlation,
  bPlotGraphs = FALSE
)

# Look 2 — with selection of continuing hypotheses
state <- AnalyzeLookPC(
  state,
  vRawPVals = c(H1 = 0.02, H2 = 0.10, H4 = 0.40),
  vSelectHypo = c("H1", "H2", "H4"),
  bPlotGraphs = FALSE
)

# Look 3 — final look
state <- AnalyzeLookPC(
  state,
  vRawPVals = c(H2 = 0.005, H4 = 0.10),
  bPlotGraphs = FALSE
)

# Plot the hypothesis graph after look 2
PlotAnalysisGraph(state, nStage = 2)
```

- **Example script:** [internalData/PC_Analysis_NonInteractive_Example.R](internalData/PC_Analysis_NonInteractive_Example.R)
- **Documentation:** For detailed documentation on all arguments, run:
  ```R
  ?SetupAnalysisPC
  ?AnalyzeLookPC
  ?PlotAnalysisGraph
  ```

### Non-Interactive Population-Enrichment Analysis (PE-PC)

PE-PC is a population-enrichment specialization of the generic PC interface.
Use its two wrapper functions when hypotheses are defined for a full population
and a nested subgroup. PE-PC currently supports one-look or two-look designs
and derives each look's correlation matrix from the cumulative full-population
and subgroup sample sizes supplied at that look. This two-look restriction does
not apply to the generic `SetupAnalysisPC()`/`AnalyzeLookPC()` interface.

```R
library( AdaptGMCP )

state <- SetupAnalysisPEPC(
  vWeights = c( 0.5, 0.5, 0, 0 ),
  mGMatrix = matrix(
    c( 0, 0.5, 0.5, 0,
       0.5, 0, 0, 0.5,
       0, 1, 0, 0,
       1, 0, 0, 0 ),
    byrow = TRUE,
    nrow = 4
  ),
  strTestType = "Partly-Parametric",
  vInfoFracts = c( 0.5, 1 ),
  strDesignType = "asOF",
  bPlotGraphs = FALSE
)

state <- AnalyzeLookPEPC(
  cState = state,
  nLook = 1,
  vRawPVals = c( H1 = 0.01, H2 = 0.20, H3 = 0.15, H4 = 0.30 ),
  vFullPopulationSampleSizes = c( 70, 105, 84 ),
  vSubpopulationSampleSizes = c( 35, 56, 49 ),
  bPlotGraphs = FALSE
)

state <- AnalyzeLookPEPC(
  cState = state,
  nLook = 2,
  vRawPVals = c( H1 = 0.02, H2 = 0.10, H4 = 0.40 ),
  vSelectHypo = c( "H1", "H2", "H4" ),
  vFullPopulationSampleSizes = c( 100, 140, 123 ),
  vSubpopulationSampleSizes = c( 48, 80, 70 ),
  bPlotGraphs = FALSE
)

state$mcpObj$AdjPValues
state$mcpObj$CombinedPValuesTable
state$mcpObj$rej_flag_Curr
```

`vInfoFracts` controls the planned look schedule and boundaries; the
sample-size vectors are cumulative observed counts used to derive the
look-specific correlation. Use `(control, treatment 1, treatment 2, ...)` in
every vector, keep subgroup counts no greater than full-population counts, and
keep counts non-decreasing across looks. The complete runnable workflow,
including interpretation and troubleshooting notes, is in
`internalData/PopEnrich_Analysis_Ex.R`.

- **Documentation:** For detailed documentation on the PE-PC functions, run:
  ```R
  ?SetupAnalysisPEPC
  ?AnalyzeLookPEPC
  ```

## Note

The binary and mixed endpoint engine is currently undergoing testing. We welcome your feedback to help improve the R package.

**How to report bugs or observations:**

1. Go to the [Issues](https://github.com/Cytel-Software/AdaptiveGMCP/issues) section of the GitHub repository.
2. Click on the "New issue" button.
3. Provide a clear and descriptive title for your issue.
4. In the description, include as much detail as possible, such as:
   - Steps to reproduce the problem
   - Expected and actual behavior
   - Any error messages or warnings
   - Relevant code snippets or data (if applicable)
   - Your R version and operating system
5. Submit the issue.

Your input is valuable and will help us address problems and improve the package for all users.
