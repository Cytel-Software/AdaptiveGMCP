# Examples for analysis in the population enrichment case
library(AdaptGMCP)

# #########################################################

# # EXAMPLE 1 #############################################
# # Simplest population enrichment problem with 1 treatment and 1 control arm
# # 1 endpoint, and 1 full population and 1 subpopulation
# # Fixed sample design
# # Total sample size = 200, balanced allocation between treatment and control arms
# # Subpopulation is 50% of full population

# # Setting input parameters for the function
# # Weights
# wi <- rep(0.5, 2) # Initial weights for the 2 hypotheses

# # Transition matrix
# g <- matrix(c(0, 1, 1, 0), byrow = TRUE, nrow = 2)

# # Test type
# test <- "Dunnett"

# # Type I error
# alp <- 0.025

# # Info fraction
# t <- 1

# # Setting up the design first
# design <- SetupAnalysis_PE_PC(
#   WI = wi,
#   G = g,
#   test.type = test,
#   alpha = alp,
#   planned_info_frac = t,
#   plotGraphs = FALSE)

# print(design)

# # Performing analysis
# look1_out <- AnalyzeLook_PE_PC(
#   design,
#   p_raw = c(H1 = 0.1, H2 = 0.0025),
#   fullpop_sample_sizes = c(100, 100), # Sample sizes are specified first for control and then for treatment arm
#   subpop_sample_sizes = c(50, 50)) # Sample sizes are specified first for control and then for treatment arm

# print(look1_out$mcpObj$Correlation)
# print(look1_out)
# #########################################################

# # EXAMPLE 2 #############################################
# # Same as Example 1, but with 1 interim look

# # Setting input parameters for the function
# # Weights
# wi <- rep(0.5, 2) # Initial weights for the 2 hypotheses

# # Transition matrix
# g <- matrix(c(0, 1, 1, 0), byrow = TRUE, nrow = 2)

# # Test type
# test <- "Dunnett"

# # Type I error
# alp <- 0.025

# # Info fraction
# t <- c(0.5, 1)

# # Design type
# des <- "asOF"

# # Setting up the design first
# design <- SetupAnalysis_PE_PC(
#   WI = wi,
#   G = g,
#   test.type = test,
#   alpha = alp,
#   planned_info_frac = t,
#   typeOfDesign = des,
#   plotGraphs = FALSE
#   )

# print(design)

# # Performing analysis for look 1
# look1_out <- AnalyzeLook_PE_PC(
#   design,
#   p_raw = c(H1 = 0.1, H2 = 0.075),
#   fullpop_sample_sizes = c(48, 53), # Sample sizes are specified first for control and then for treatment arm
#   subpop_sample_sizes = c(26, 24)) # Sample sizes are specified first for control and then for treatment arm

# print(look1_out$mcpObj$Correlation)
# print(look1_out)

# # Performing analysis for look 2
# look2_out <- AnalyzeLook_PE_PC(
#   look1_out,
#   p_raw = c(H1 = 0.08, H2 = 0.003),
#   fullpop_sample_sizes = c(100, 100), # Sample sizes are specified first for control and then for treatment arm
#   subpop_sample_sizes = c(53, 49)) # Sample sizes are specified first for control and then for treatment arm

# print(look2_out$mcpObj$Correlation)
# print(look2_out)
# #########################################################

# EXAMPLE 3 #############################################
# Severe oral mucositis example
# H1: high dose, full population
# H2: low dose, full population
# H3: high dose, HPV+ subgroup
# H4: low dose, HPV+ subgroup
# Single endpoint
# Total sample size = 300, balanced allocation to all arms
# Subpopulation is 50% of full population
# One interim look at 50% information fraction
# Ref: CIT\MAMS\adaptgmcp-resources\GMCP papers, etc\Cyrus' Talk - AdaptGMCP - Sep 2025.pdf

# Setting input parameters for the function
# Weights
wi <- rep(0.25, 4) # Initial weights for the 4 hypotheses

# Transition matrix
g <- matrix(c(0, 0, 1, 0,
              0, 0, 0, 1,
              0, 1, 0, 0,
              1, 0, 0, 0), byrow = TRUE, nrow = 4)

# Test type
test <- "Dunnett"

# Type I error
alp <- 0.025

# Info fraction
t <- c(0.5, 1)

# Design type
des <- "asOF"

# Setting up the design first
design <- SetupAnalysis_PE_PC(
  WI = wi,
  G = g,
  test.type = test,
  alpha = alp,
  planned_info_frac = t,
  typeOfDesign = des,
  plotGraphs = FALSE)

print(design)

# Performing analysis at look 1
look1_out <- AnalyzeLook_PE_PC(
  design,
  p_raw = c(H1 = 0.00045, H2 = 0.0952, H3 = 0.0225, H4 = 0.1104),
  fullpop_sample_sizes = c(50, 50, 50), # Sample sizes: first for control and then for treatment arms
  subpop_sample_sizes = c(25, 25, 25)) # Sample sizes: first for control and then for treatment arms

print(look1_out$mcpObj$Correlation)
print(look1_out)

# No hypotheses are rejected at look 1

# Performing analysis at look 2
look2_out <- AnalyzeLook_PE_PC(
  look1_out,
  p_raw = c(H1 = 0.00045, H2 = 0.1121, H3 = 0.0112),
  selection = c("H1", "H2", "H3"),
  fullpop_sample_sizes = c(100, 100, 100), # Sample sizes: first for control and then for treatment arms
  subpop_sample_sizes = c(50, 50, 50)) # Sample sizes: first for control and then for treatment arms

print(look2_out$mcpObj$Correlation)
print(look2_out)

# >>> Trying with higher look 1 info fract
# Setting up the design first
design <- SetupAnalysis_PE_PC(
  WI = wi,
  G = g,
  test.type = test,
  alpha = alp,
  planned_info_frac = c(0.7, 1),
  typeOfDesign = des,
  plotGraphs = FALSE)

print(design)

# Performing analysis at look 1
look1_out <- AnalyzeLook_PE_PC(
  design,
  p_raw = c(H1 = 0.00045, H2 = 0.0952, H3 = 0.0225, H4 = 0.1104),
  fullpop_sample_sizes = c(50, 50, 50), # Sample sizes: first for control and then for treatment arms
  subpop_sample_sizes = c(25, 25, 25)) # Sample sizes: first for control and then for treatment arms

print(look1_out$mcpObj$Correlation)
print(look1_out)

# H1 is rejected at look 1.

# Performing analysis at look 2
look2_out <- AnalyzeLook_PE_PC(
  look1_out,
  p_raw = c(H1 = 0.00045, H2 = 0.1121, H3 = 0.0112, H4 = 0.1153),
  fullpop_sample_sizes = c(100, 100, 100), # Sample sizes: first for control and then for treatment arms
  subpop_sample_sizes = c(50, 50, 50)) # Sample sizes: first for control and then for treatment arms

print(look2_out$mcpObj$Correlation)
print(look2_out)

#########################################################

# EXAMPLE 4 #############################################
# Problem: Full population and a subpopulation (50% of full population)
# High dose and low dose of the drug being tested
# 2 stage trial with interim look at 50% information fraction
# H1: full pop, high dose; H2: subpop, high dose;
# H3: full pop, low dose; H4: subpop, low dose

# Setting input parameters for the function

# Weights
wi <- rep(0.25, 4) # Initial weights for the 4 hypo

# Transition matrix
g <- matrix(c(0, 0.5, 0.5, 0, # H1->H2, H1->H3
              0.5, 0, 0, 0.5, # H2->H1, H2->H4
              0.5, 0, 0, 0.5, # H3->H1, H3->H4
              0, 0.5, 0.5, 0), # H4->H2, H4->H3
            byrow = TRUE, nrow = 4)

# Test type
test <- "Bonf"

# Type I error
alp <- 0.025

# Info fraction
t <- c(0.5, 1)

# Design type
des <- "asOF"

# Setting up the design first
design <- SetupAnalysis_PE_PC(
  WI = wi,
  G = g,
  test.type = test,
  alpha = alp,
  planned_info_frac = t,
  typeOfDesign = des,
  plotGraphs = FALSE)

print(design)

# Performing analysis at look 1
look1_out <- AnalyzeLook_PE_PC(
  design,
  p_raw = c(H1 = 0.00025, H2 = 0.0952, H3 = 0.0245, H4 = 0.1104),
  fullpop_sample_sizes = c(50, 50, 50), # Sample sizes: first for control and then for treatment arms
  subpop_sample_sizes = c(25, 25, 25)) # Sample sizes: first for control and then for treatment arms

print(look1_out$mcpObj$Correlation)
print(look1_out)

#########################################################

# EXAMPLE 5 #################################################
wi <- c(1 / 2, 1 / 2, 0, 0)
g <- matrix(c(
  0, 1 / 2, 1 / 2, 0,
  1 / 2, 0, 0, 1 / 2,
  0, 1, 0, 0,
  1, 0, 0, 0
), byrow = TRUE, nrow = 4)

t <- c(0.5, 1)
alp <- 0.025

tt <- "Partly-Parametric"
des <- "asOF"

# Setting up the design first
design <- SetupAnalysis_PE_PC(
  WI = wi,
  G = g,
  test.type = tt,
  alpha = alp,
  planned_info_frac = t,
  typeOfDesign = des,
  plotGraphs = FALSE)

print(design)

# Performing analysis at look 1
look1_out <- AnalyzeLook_PE_PC(
  design,
  p_raw = c(H1 = 0.03, H2 = 0.20, H3 = 0.10, H4 = 0.25),
  fullpop_sample_sizes = c(50, 75, 60), # Sample sizes: first for control and then for treatment arms
  subpop_sample_sizes = c(25, 40, 35)) # Sample sizes: first for control and then for treatment arms

print(look1_out$mcpObj$Correlation)
print(look1_out)

# Performing analysis at look 2
look2_out <- AnalyzeLook_PE_PC(
  look1_out,
  p_raw = c(H1 = 0.003, H2 = 0.10, H3 = 0.08, H4 = 0.15),
  fullpop_sample_sizes = c(100, 140, 123), # Sample sizes: first for control and then for treatment arms
  subpop_sample_sizes = c(48, 80, 70)) # Sample sizes: first for control and then for treatment arms

print(look2_out$mcpObj$Correlation)
print(look2_out)

#########################################################

# EXAMPLE 6 #############################################
wi <- c(1 / 2, 1 / 2, 0, 0)
g <- matrix(c(
  0, 1 / 2, 1 / 2, 0,
  1 / 2, 0, 0, 1 / 2,
  0, 1, 0, 0,
  1, 0, 0, 0
), byrow = TRUE, nrow = 4)

t <- c(0.7, 1)
alp <- 0.025

# Test type
test <- "Partly-Parametric"

# Design type
des <- "asOF"

# Setting up the design first
design <- SetupAnalysis_PE_PC(
  WI = wi, G = g, test.type = test, alpha = alp, planned_info_frac = t, typeOfDesign = des,
  plotGraphs = FALSE)

print(design)

# Performing analysis at look 1
look1_out <- AnalyzeLook_PE_PC(
  design,
  p_raw = c(H1 = 0.01, H2 = 0.20, H3 = 0.15, H4 = 0.30),
  fullpop_sample_sizes = c(70, 105, 84), # Sample sizes: first for control and then for treatment arms
  subpop_sample_sizes = c(35, 56, 49)) # Sample sizes: first for control and then for treatment arms

print(look1_out$mcpObj$Correlation)
print(look1_out)

# Performing analysis at look 2
look2_out <- AnalyzeLook_PE_PC(
  look1_out,
  p_raw = c(H1 = 0.02, H2 = 0.10, H4 = 0.40),
  selection = c("H1", "H2", "H4"),
  fullpop_sample_sizes = c(100, 140, 123), # Sample sizes: first for control and then for treatment arms
  subpop_sample_sizes = c(48, 80, 70)) # Sample sizes: first for control and then for treatment arms

print(look2_out$mcpObj$Correlation)
print(look2_out)
#########################################################

# EXAMPLE 7 #############################################
# Phase-3 clinical trial in severe oral mucositis
# 2 doses (low, high) compared to placebo, 2 normal endpoints (primary and secondary) evaluated
# In addition to the full population, a subgroup of patients with HPV+ status is also evaluated.
# This leads to an 8-hypothesis population enrichment problem:
# H1/H2: full population, primary endpoint, dose low/high
# H3/H4: full population, secondary endpoint, dose low/high
# H5/H6: HPV+ subgroup, primary endpoint, dose low/high
# H7/H8: HPV+ subgroup, secondary endpoint, dose low/high
# Note that the hypotheses are arranged in this order: population, endpoint, dose.

# Weights for the graph nodes (hypotheses)
wi <- c(0.35, 0.35, 0, 0, 0.15, 0.15, 0, 0)
names(wi) <- c("H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8")

# Transition matrix for the graph:
G <- matrix(
  c(
    # H1    H2    H3    H4    H5    H6    H7    H8
    0,    0.2,  0.4,  0,    0.2,  0.2,  0,    0,    # H1
    0.2,  0,    0,    0.4,  0.2,  0.2,  0,    0,    # H2
    0,    1/3,  0,    0,    1/3,  1/3,  0,    0,    # H3
    1/3,  0,    0,    0,    1/3,  1/3,  0,    0,    # H4
    0.2,  0.2,  0,    0,    0,    0.2,  0.4,  0,    # H5
    0.2,  0.2,  0,    0,    0.2,  0,    0,    0.4,  # H6
    1/3,  1/3,  0,    0,    0,    1/3,  0,    0,    # H7
    1/3,  1/3,  0,    0,    1/3,  0,    0,    0     # H8
  ),
  nrow = 8, byrow = TRUE,
  dimnames = list(
    c("H1","H2","H3","H4","H5","H6","H7","H8"),
    c("H1","H2","H3","H4","H5","H6","H7","H8")
  )
)

# Test type
test <- "Partly-Parametric"

# Type I error
alp <- 0.025

# Info fraction
t <- 1 # c(0.5, 1)

# Design type
des <- "asOF"

# Setting up the design first
design <- SetupAnalysis_PE_PC(WI = wi, G = G, test.type = test, alpha = alp, 
                              planned_info_frac = t, typeOfDesign = des,
                              plotGraphs = FALSE)

print(design)

# Performing analysis at look 1
look1_out <- AnalyzeLook_PE_PC(
  design,
  p_raw = c(0.21, 0.04, 0.01, 0.0155, 0.02, 0.01, 0.009, 0.003),
  fullpop_sample_sizes = c(100, 100, 100), # Sample sizes: first for control and then for treatment arms
  subpop_sample_sizes = c(50, 50, 50)) # Sample sizes: first for control and then for treatment arms

print(look1_out$mcpObj$Correlation)
print(look1_out)
#########################################################

# FINAL NON-INTERACTIVE PE-PC WORKFLOW ##################
#
# PE-PC is the population-enrichment specialization of the generic PC
# analysis interface. Unlike SetupAnalysis_PC()/AnalyzeLook_PC(), the PE-PC
# wrapper currently supports one-look or two-look designs only.
#
# planned_info_frac defines the planned look schedule and boundaries. The
# cumulative sample-size vectors supplied to each AnalyzeLook_PE_PC() call
# define that look's population correlation matrix and may differ from the
# nominal information fractions.

wi <- c( 0.35, 0.35, 0, 0, 0.15, 0.15, 0, 0 )
names( wi ) <- paste0( "H", seq_along( wi ) )

G <- matrix(
  c(
    0, 0.2, 0.4, 0, 0.2, 0.2, 0, 0,
    0.2, 0, 0, 0.4, 0.2, 0.2, 0, 0,
    0, 1 / 3, 0, 0, 1 / 3, 1 / 3, 0, 0,
    1 / 3, 0, 0, 0, 1 / 3, 1 / 3, 0, 0,
    0.2, 0.2, 0, 0, 0, 0.2, 0.4, 0,
    0.2, 0.2, 0, 0, 0.2, 0, 0, 0.4,
    1 / 3, 1 / 3, 0, 0, 0, 1 / 3, 0, 0,
    1 / 3, 1 / 3, 0, 0, 1 / 3, 0, 0, 0
  ),
  byrow = TRUE,
  nrow = 8
)

pe_state <- SetupAnalysis_PE_PC(
  WI = wi,
  G = G,
  test.type = "Partly-Parametric",
  alpha = 0.025,
  planned_info_frac = c( 0.5, 1.0 ),
  typeOfDesign = "asOF",
  plotGraphs = FALSE
)

# Hypotheses are endpoint-major and treatment-minor, with the full-population
# block followed by the subgroup block. Sample-size vectors use
# (control, treatment 1, treatment 2, ...).
pe_state <- AnalyzeLook_PE_PC(
  state = pe_state,
  look = 1,
  p_raw = c(
    H1 = 0.01, H2 = 0.20, H3 = 0.15, H4 = 0.30,
    H5 = 0.02, H6 = 0.25, H7 = 0.10, H8 = 0.40
  ),
  fullpop_sample_sizes = c( 70, 105, 84 ),
  subpop_sample_sizes = c( 35, 56, 49 ),
  plotGraphs = FALSE
)

print( pe_state$mcpObj$AdjPValues )

# Counts are cumulative and must be non-decreasing from look 1 to look 2.
pe_state <- AnalyzeLook_PE_PC(
  state = pe_state,
  look = 2,
  p_raw = c( H1 = 0.02, H2 = 0.10, H4 = 0.40, H5 = 0.01, H6 = 0.15 ),
  selection = c( "H1", "H2", "H4", "H5", "H6" ),
  fullpop_sample_sizes = c( 100, 140, 123 ),
  subpop_sample_sizes = c( 48, 80, 70 ),
  plotGraphs = FALSE
)

# AdjPValues contains look-specific adjusted p-values and the combined
# p-values used for decisions. rej_flag_Curr identifies current rejections.
print( pe_state$mcpObj$AdjPValues )
print( pe_state$mcpObj$CombinedPValuesTable )
print( pe_state$mcpObj$rej_flag_Curr )
print( pe_state$completion_reason )

# Troubleshooting:
# - Keep the same arm order in WI/G, p_raw, and both sample-size vectors.
# - Supply cumulative counts, not increment-only counts.
# - Subgroup counts must be <= full-population counts.
# - At look 2, selection must contain only currently active hypotheses, and
#   p_raw must contain a value for every selected hypothesis.
