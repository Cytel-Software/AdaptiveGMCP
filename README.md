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
- `adaptGMCP_CER`: Performs analysis using the CER (Conditional Error Rate) method.
- `adaptGMCP_PC`: Performs analysis using the p-value combination method.

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
