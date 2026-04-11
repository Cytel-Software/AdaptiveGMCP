# --------------------------------------------------------------------------------------------------
#
# ©2025 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

#######################################################################
#####  Script to Run Non-Interactive P-Value Combination Analysis  ####
#######################################################################
#
# This script demonstrates the non-interactive analysis interface for the
# p-value combination (PC) method.  All interim inputs are supplied as
# function arguments — no console prompts are issued.
#
# Equivalent interactive example: internalData/AdaptGMCP_Analysis_Example.R

library(AdaptGMCP)

#----------------Initial Weights---------------------
# Number of hypotheses (k) equals the length of WI
WI <- c(0.5, 0.5, 0, 0)

#---------------Transition Matrix--------------------
# Dimension: k x k
G <- matrix(c(
  0,   0.5, 0.5, 0,
  0.5, 0,   0,   0.5,
  0,   1,   0,   0,
  1,   0,   0,   0
), byrow = TRUE, nrow = 4)

#----------------Information Fraction----------------
# The number of looks equals the length of info_frac
info_frac <- c(0.5, 0.7, 1)

#-----------------------Alpha------------------------
alpha <- 0.025

#-------------------Boundary Type-------------------
# O'Brien & Fleming type alpha spending (default)
typeOfDesign <- "asOF"

#-------------------Test Type------------------------
# Partly-Parametric uses the correlation matrix below
test.type <- "Partly-Parametric"

#-------------------Correlation Matrix---------------
# NA entries indicate unknown pairwise correlations
Correlation <- matrix(c(
  1,   0.5, 0.5, NA,
  0.5, 1,   NA,  0.5,
  0.5, NA,  1,   0.5,
  NA,  0.5, 0.5, 1
), byrow = TRUE, nrow = 4)

#-------------------Plot Intermediate Graphs---------
plotGraphs <- TRUE

#--------------Setup Analysis------------------------
state <- SetupAnalysis_PC(
  WI           = WI,
  G            = G,
  test.type    = test.type,
  alpha        = alpha,
  info_frac    = info_frac,
  typeOfDesign = typeOfDesign,
  Correlation  = Correlation,
  plotGraphs   = plotGraphs
)

#--------------Look 1 Analysis-----------------------
state <- AnalyzeLook_PC(
  state,
  look  = 1,
  p_raw = c(H1 = 0.01, H2 = 0.20, H3 = 0.15, H4 = 0.30),
  plotGraphs = plotGraphs
)
print(state)

#--------------Look 2 Analysis (with selection)------
# H3 is dropped; remaining hypotheses are H1, H2, H4
state <- AnalyzeLook_PC(
  state,
  look      = 2,
  p_raw     = c(H1 = 0.02, H2 = 0.10, H4 = 0.40),
  selection = c("H1", "H2", "H4"),
  plotGraphs = plotGraphs
)
print(state)

#--------------Look 3 Analysis (final look)----------
state <- AnalyzeLook_PC(
  state,
  look  = 3,
  p_raw = c(H2 = 0.005, H4 = 0.10),
  plotGraphs = plotGraphs
)
print(state)

#--------------Plot Graphs at Each Stage-------------
PlotAnalysisGraph(state, stage = 1)
PlotAnalysisGraph(state, stage = 2)
PlotAnalysisGraph(state, stage = 3)
