# Non-interactive analysis example for the p-value combination method
#
# This mirrors internalData/AdaptGMCP_Analysis_Example.R but provides all inputs
# via function arguments (no interactive prompts).

##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## Temporary code added to save the intermediate output produced by
## adaptGMCP_PC()
  wi <- c(1 / 2, 1 / 2, 0, 0)
  g <- matrix(c(
    0, 1 / 2, 1 / 2, 0,
    1 / 2, 0, 0, 1 / 2,
    0, 1, 0, 0,
    1, 0, 0, 0
  ), byrow = TRUE, nrow = 4)

  t <- c(0.5, 0.7, 1)
  alp <- 0.025

  corr <- matrix(c(
    1, 0.5, 0.5, NA,
    0.5, 1, NA, 0.5,
    0.5, NA, 1, 0.5,
    NA, 0.5, 0.5, 1
  ), byrow = TRUE, nrow = 4)

  tt <- "Partly-Parametric"
  des <- "asOF"

  adaptGMCP_PC(WI=wi, G=g, test.type = tt, alpha = alp, info_frac = t,
               typeOfDesign = des, Correlation = corr, MultipleWinners = F,
               Selection = T, UpdateStrategy = T, plotGraphs = T)
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

devtools::load_all()
options(AdaptGMCP.run_pc_analysis_api_tests = TRUE)
testthat::test_file("tests/testthat/test-pc_analysis_api.R")

library(AdaptGMCP)

# WI <- c(1 / 2, 1 / 2, 0, 0)
# G <- matrix(c(
#   0, 1 / 2, 1 / 2, 0,
#   1 / 2, 0, 0, 1 / 2,
#   0, 1, 0, 0,
#   1, 0, 0, 0
# ), byrow = TRUE, nrow = 4)
#
# info_frac <- c(0.5, 0.7, 1) # 3 stages / looks
# alpha <- 0.025
#
# # Example correlation matrix (NA means unknown correlation)
# Correlation <- matrix(c(
#   1, 0.5, 0.5, NA,
#   0.5, 1, NA, 0.5,
#   0.5, NA, 1, 0.5,
#   NA, 0.5, 0.5, 1
# ), byrow = TRUE, nrow = 4)
#
# # Setting up analysis
# state <- SetupAnalysis_PC(
#   WI = WI,
#   G = G,
#   test.type = "Partly-Parametric",
#   alpha = alpha,
#   info_frac = info_frac,
#   typeOfDesign = "asOF",
#   Correlation = Correlation,
#   plotGraphs = TRUE
# )

state <- readRDS("state_25Mar2026.rds")

# Look 1 analysis
look1_out <- AnalyzeLook_PC(
  state,
  p_raw = c(H1 = 0.01, H2 = 0.20, H3 = 0.15, H4 = 0.30)
)
print(look1_out)
saveRDS(look1_out, "look1_out_25Mar2026.rds")

# Adaptation after look 1 and look 2 analysis
look2_out <- AnalyzeLook_PC(
  look1_out,
  p_raw = c(H1 = 0.02, H2 = 0.10, H4 = 0.40),
  selection = c("H1", "H2", "H4")
)
print(look2_out)

# Look 3 analysis (no further design changes)
look3_out <- AnalyzeLook_PC(
  look2_out,
  p_raw = c(H2 = 0.005, H4 = 0.10)
)
print(look3_out)

# Plot the graph after look 1
PlotAnalysisGraph(look3_out, stage = 1)

PlotAnalysisGraph(look3_out, stage = 2)

PlotAnalysisGraph(look3_out, stage = 3)
