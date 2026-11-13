# Additional Simulation Tools

This page describes the data-frame simulation wrapper and interactive
simulation app.

## Data-frame Simulation Wrapper

`SimulateMAMSMEPWrapper` runs scenarios supplied in an R data frame. Its
`dfInput` argument accepts scenario data, and `strOutputPath` specifies where
to write the output CSV. Results are written after each scenario is simulated.

For argument details, run `?SimulateMAMSMEPWrapper` after installing the
package.

## Interactive Simulation App

`AdaptGMCPSimApp()` launches the Shiny application for configuring and running
adaptive GMCP simulations without writing code. It provides UI modules for
hypothesis graphs, correlation and transition matrices, and interim analysis
settings.

```R
library(AdaptGMCP)
AdaptGMCPSimApp()
```
