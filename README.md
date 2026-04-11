# AdaptGMCP

## Installation

You can install the AdaptGMCP package in R directly from the GitHub repository using the `remotes` package:

```R
remotes::install_github("Cytel-Software/AdaptiveGMCP", ref = "master")
```

This will download and install the latest version of the package from the master branch.

## Important Functions

The following are some of the key functions provided by the AdaptGMCP package:

- `simMAMSMEP`: Runs simulations for adaptive group sequential multiple testing procedures.
- `adaptGMCP_CER`: Performs interactive analysis using the CER (Conditional Error Rate) method.
- `adaptGMCP_PC`: Performs interactive analysis using the p-value combination method.
- `SetupAnalysis_PC`: Sets up a non-interactive analysis state for the p-value combination method.
- `AnalyzeLook_PC`: Advances an existing non-interactive PC analysis by one look.
- `PlotAnalysisGraph`: Plots a graph from a non-interactive PC analysis state object.

For detailed usage and arguments of each function, please refer to the help section in R after installing the package (e.g., `?simMAMSMEP`).

## Quick Start

### `simMAMSMEP`

- **Example script:** [internalData/MAMSMEP_Simulation_Example.R](internalData/MAMSMEP_Simulation_Example.R)
- **How to use:** Open and run the script in R to see a typical workflow.
- **Documentation:** For detailed documentation on all arguments, run:
  ```R
  ?simMAMSMEP
  ```

### `adaptGMCP_CER`

- **Example script:** [internalData/AdaptGMCP_CER_Analysis_Example.R](internalData/AdaptGMCP_CER_Analysis_Example.R)
- **How to use:** Open and run the script in R to perform analysis using the Conditional Error Rate method.
- **Documentation:** For detailed documentation on all arguments, run:
  ```R
  ?adaptGMCP_CER
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

**Key functions:**
- `SetupAnalysis_PC()`: Initialises the analysis state (design parameters, graph, boundaries).
- `AnalyzeLook_PC()`: Processes one interim or final look, returning an updated state object.
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
state <- SetupAnalysis_PC(
  WI           = WI,
  G            = G,
  test.type    = "Partly-Parametric",
  alpha        = 0.025,
  info_frac    = c(0.5, 0.7, 1),
  typeOfDesign = "asOF",
  Correlation  = Correlation,
  plotGraphs   = FALSE
)

# Look 1
state <- AnalyzeLook_PC(
  state,
  p_raw      = c(H1 = 0.01, H2 = 0.20, H3 = 0.15, H4 = 0.30),
  plotGraphs = FALSE
)

# Look 2 — with selection of continuing hypotheses
state <- AnalyzeLook_PC(
  state,
  p_raw      = c(H1 = 0.02, H2 = 0.10, H4 = 0.40),
  selection  = c("H1", "H2", "H4"),
  plotGraphs = FALSE
)

# Look 3 — final look
state <- AnalyzeLook_PC(
  state,
  p_raw = c(H2 = 0.005, H4 = 0.10)
)

# Plot the hypothesis graph after look 2
PlotAnalysisGraph(state, stage = 2)
```

- **Example script:** [internalData/PC_Analysis_NonInteractive_Example.R](internalData/PC_Analysis_NonInteractive_Example.R)
- **Documentation:** For detailed documentation on all arguments, run:
  ```R
  ?SetupAnalysis_PC
  ?AnalyzeLook_PC
  ?PlotAnalysisGraph
  ```

### `AdaptGMCPSimApp` (Shiny App)

- **How to launch:**
  ```R
  library(AdaptGMCP)
  AdaptGMCPSimApp()
  ```
- **Description:** Interactive Shiny application for configuring and running adaptive GMCP simulations without writing code. Provides UI modules for hypothesis graphs, correlation/transition matrices, and interim analysis settings.

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
