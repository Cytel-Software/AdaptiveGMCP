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

# adaptGMCP_PC(
#   WI = WI2,
#   G = G2,
#   test.type = test.type2,
#   alpha = alpha2,
#   info_frac = info_frac2,
#   typeOfDesign = typeOfDesign2,
#   Correlation = Correlation2,
#   Selection = T,
#   UpdateStrategy = T,
#   plotGraphs = plotGraphs2
# )

#--------------Setup Analysis------------------------
state2 <- SetupAnalysis_PC(
  WI            = WI2,
  G             = G2,
  test.type     = test.type2,
  alpha         = alpha2,
  planned_info_frac = info_frac2,
  typeOfDesign  = typeOfDesign2,
  plotGraphs    = plotGraphs2
)

# Computed stopping boundaries from the planned design
print("EXAMPLE 2 - Computed stopping boundaries after SetupAnalysis_PC():")
print(state2$mcpObj$bdryTab)

# #===================================================================================
# # SCENARIO (1): H1 is rejected at look 1 (its stopping boundary is crossed)
# #===================================================================================
# state2_s1 <- state2

# #--------------Look 1 Analysis-----------------------
# # p1 is far below the look-1 efficacy boundary, so H1 is rejected.
# state2_s1 <- AnalyzeLook_PC(
#   state2_s1,
#   look          = 1,
#   p_raw         = c(H1 = 0.00001, H2 = 0.40),
#   Correlation   = Correlation2,
#   plotGraphs    = plotGraphs2
# )
# print("SCENARIO (1) - State after look 1 (expect H1 rejected):")
# print(state2_s1)

# #--------------Look 2 Analysis-----------------------
# # H1 has been rejected, so only H2 remains active; supply p_raw for H2 only.
# state2_s1 <- AnalyzeLook_PC(
#   state2_s1,
#   look          = 2,
#   p_raw         = c(H2 = 0.20),
#   plotGraphs    = plotGraphs2
# )
# print("SCENARIO (1) - Pre-specified stopping boundaries:")
# print(state2_s1$mcpObj$bdryTab)

#===================================================================================
# SCENARIO (2): No rejection at look 1; H2 dropped at look 2 (2-arm stage 2)
#===================================================================================
state2_s2 <- state2

#--------------Look 1 Analysis-----------------------
# Both p-values are above the look-1 efficacy boundary, so nothing is rejected.
state2_s2 <- AnalyzeLook_PC(
  state2_s2,
  look          = 1,
  p_raw         = c(H1 = 0.10, H2 = 0.20),
  Correlation   = Correlation2,
  plotGraphs    = plotGraphs2
)
print("SCENARIO (2) - State after look 1 (expect no rejection):")
print(state2_s2)

#--------------Look 2 Analysis (with selection)------
# H2 is dropped, leaving only H1 for stage 2 (a 2-arm design going forward).
state2_s2 <- AnalyzeLook_PC(
  state2_s2,
  look          = 2,
  p_raw         = c(H1 = 0.03),
  selection     = c("H1"),
  plotGraphs    = plotGraphs2
)
print("SCENARIO (2) - Pre-specified stopping boundaries:")
print(state2_s2$mcpObj$bdryTab)
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
state <- SetupAnalysis_PC(
  WI           = WI,
  G            = G,
  test.type    = test.type,
  alpha        = alpha,
  planned_info_frac    = info_frac,
  typeOfDesign = typeOfDesign,
  plotGraphs   = plotGraphs
)

print("Initial design:")
print(state)

#--------------Look 1 Analysis-----------------------
state <- AnalyzeLook_PC(
  state,
  look         = 1,
  p_raw        = c(H1 = 0.01, H2 = 0.20, H3 = 0.15, H4 = 0.30),
  Correlation  = Correlation,
  plotGraphs = plotGraphs
)
print(state)

#--------------Look 2 Analysis (with selection)------
# H3 is dropped; remaining hypotheses are H1, H2, H4
state <- AnalyzeLook_PC(
  state,
  look         = 2,
  p_raw        = c(H1 = 0.02, H2 = 0.10, H4 = 0.40),
  selection    = c("H1", "H2", "H4"),
  plotGraphs = plotGraphs
)
print(state)

#--------------Look 3 Analysis (final look)----------
state <- AnalyzeLook_PC(
  state,
  look         = 3,
  p_raw        = c(H2 = 0.005, H4 = 0.10),
  plotGraphs = plotGraphs
)
print(state)

#--------------Plot Graphs at Each Stage-------------
PlotAnalysisGraph(state, stage = 1)
PlotAnalysisGraph(state, stage = 2)
PlotAnalysisGraph(state, stage = 3)
###################################################################################
