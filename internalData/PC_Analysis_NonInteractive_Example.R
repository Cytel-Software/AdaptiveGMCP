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

###################################################################################
# EXAMPLE 2
# Two hypotheses (control vs two treatment arms: H1, H2) with a 3-look design.
# Demonstrates two look-1 outcomes:
#   (1) one hypothesis rejected at look 1 (its stopping boundary is crossed), and
#   (2) no rejection at look 1, followed by dropping H2 at look 2.

#----------------Shared Design Inputs----------------
# Two hypotheses => equal Bonferroni weights and a symmetric transition graph
WI2 <- c(0.5, 0.5)
G2 <- matrix(c(
  0, 1,
  1, 0
), byrow = TRUE, nrow = 2)

# Three looks: number of looks equals the length of info_frac
info_frac2 <- c(1.0/3.0, 2.0/3.0, 1)
alpha2 <- 0.025
typeOfDesign2 <- "asOF"
test.type2 <- "Partly-Parametric"

# 2 x 2 correlation between the two test statistics (both share the control arm)
Correlation2 <- matrix(c(
  1,   0.5,
  0.5, 1
), byrow = TRUE, nrow = 2)

plotGraphs2 <- TRUE

#--------------Setup Analysis------------------------
state2 <- SetupAnalysisPC(
  vWeights            = WI2,
  mGMatrix             = G2,
  strTestType     = test.type2,
  dAlpha         = alpha2,
  vInfoFracts = info_frac2,
  strDesignType  = typeOfDesign2,
  bPlotGraphs    = plotGraphs2
)

# Computed stopping boundaries from the planned design
print("EXAMPLE 2 - Computed stopping boundaries after SetupAnalysisPC():")
print(state2)

#===================================================================================
# SCENARIO (1): H1 is rejected at look 1 (its stopping boundary is crossed)
#===================================================================================
state2_s1 <- state2

#--------------Look 1 Analysis-----------------------
# p1 is far below the look-1 efficacy boundary, so H1 is rejected.
state2_s1 <- AnalyzeLookPC(
  state2_s1,
  nLook          = 1,
  vRawPVals         = c(H1 = 0.00001, H2 = 0.40),
  mCorrelation   = Correlation2,
  bPlotGraphs    = plotGraphs2
)
print("SCENARIO (1) - State after look 1 (expect H1 rejected):")
print(state2_s1)

#--------------Look 2 Analysis-----------------------
# H1 has been rejected, so only H2 remains active; supply p_raw for H2 only.
state2_s1 <- AnalyzeLookPC(
  state2_s1,
  nLook          = 2,
  vRawPVals         = c(H2 = 0.20),
  bPlotGraphs    = plotGraphs2
)
print("SCENARIO (1) - Pre-specified stopping boundaries:")
print(state2_s1)

#===================================================================================
# SCENARIO (2): No rejection at look 1; H2 dropped at look 2 (2-arm stage 2)
#===================================================================================
state2_s2 <- state2

#--------------Look 1 Analysis-----------------------
# Both p-values are above the look-1 efficacy boundary, so nothing is rejected.
state2_s2 <- AnalyzeLookPC(
  state2_s2,
  nLook          = 1,
  vRawPVals         = c(H1 = 0.10, H2 = 0.20),
  mCorrelation   = Correlation2,
  bPlotGraphs    = plotGraphs2
)
print("SCENARIO (2) - State after look 1 (expect no rejection):")
print(state2_s2)

#--------------Look 2 Analysis (with selection)------
# H2 is dropped, leaving only H1 for stage 2 (a 2-arm design going forward).
state2_s2 <- AnalyzeLookPC(
  state2_s2,
  nLook          = 2,
  vRawPVals         = c(H1 = 0.03),
  vSelectHypo     = c("H1"),
  bPlotGraphs    = plotGraphs2
)
print("SCENARIO (2) - Analysis output:")
print(state2_s2)
###################################################################################

###################################################################################
# EXAMPLE 1
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
state <- SetupAnalysisPC(
  vWeights           = WI,
  mGMatrix            = G,
  strTestType    = test.type,
  dAlpha        = alpha,
  vInfoFracts    = info_frac,
  strDesignType = typeOfDesign,
  bPlotGraphs   = plotGraphs
)

print("Initial design:")
print(state)

#--------------Look 1 Analysis-----------------------
# Look 1 always uses the initial setup design; any selection or strategy
# adaptation inputs supplied here are ignored with a warning.
state <- AnalyzeLookPC(
  state,
  nLook         = 1,
  vRawPVals        = c(H1 = 0.01, H2 = 0.20, H3 = 0.15, H4 = 0.30),
  mCorrelation  = Correlation,
  bPlotGraphs = plotGraphs
)
print(state)

#--------------Look 2 Analysis (with selection)------
# H3 is dropped; remaining hypotheses are H1, H2, H4
state <- AnalyzeLookPC(
  state,
  nLook         = 2,
  vRawPVals        = c(H1 = 0.02, H2 = 0.10, H4 = 0.40),
  vSelectHypo    = c("H1", "H2", "H4"),
  bPlotGraphs = plotGraphs
)
print(state)

#--------------Look 3 Analysis (final look)----------
state <- AnalyzeLookPC(
  state,
  nLook         = 3,
  vRawPVals        = c(H2 = 0.005, H4 = 0.10),
  bPlotGraphs = plotGraphs
)
print(state)

#--------------Plot Graphs at Each Stage-------------
PlotAnalysisGraph(state, nStage = 1)
PlotAnalysisGraph(state, nStage = 2)
PlotAnalysisGraph(state, nStage = 3)
###################################################################################
