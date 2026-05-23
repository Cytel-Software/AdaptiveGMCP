# Examples for analysis in the population enrichment case
library(AdaptGMCP)

# # EXAMPLE 5 #############################################
# # Setting input parameters for the function

# # Weights
# wi <- rep(0.5, 2) # Initial weights for the 2 hypotheses
# stopifnot(length(wi) == 2)

# # Transition matrix
# g <- matrix(c(0, 1, 1, 0), byrow = T, nrow = 2)

# # Test type
# test <- "Dunnett" # "Bonf" # "Partly-Parametric" #

# # Type I error
# alp <- 0.025

# # Info fraction
# t <- c(0.5, 1)

# # Design type
# des <- "asOF"

# # Correlation matrix between test stats
# corr <- matrix(c(1, 0.5, 0.5, 1), byrow = T, nrow = 2)

# x <- AdaptGMCP:::conn.comp(corr)
# print(x)

# y <- AdaptGMCP:::clique.partition(corr)
# print(y)

# # Calling the analysis function for p-value combination method
# design <- SetupAnalysis_PC(
#   WI = wi,
#   G = g,
#   test.type = test,
#   alpha = alp,
#   info_frac = t,
#   typeOfDesign = des,
#   Correlation = corr,
#   plotGraphs = TRUE,
#   MultipleWinners = TRUE
# )

# # Look 1
# look1_out <- AnalyzeLook_PC(
#   design,
#   p_raw = c(H1 = 0.025, H2 = 0.025), plotGraphs = TRUE
# )

# look1_out
# #########################################################

# EXAMPLE 4 #############################################
# Problem: Full population and a subpopulation (50% of full population)
# High dose and low dose of the drug being tested
# 2 stage trial with interim look at 50% information fraction
# H1: full pop, high dose; H2: subpop, high dose;
# H3: full pop, low dose; H4: subpop, low dose
# Correlations:
#       (1) For the same dose, test stats for full population and subpopulation
#           are correlated due to the shared patients and the correlation coefficient
#           is the same as the subpop prop. (H1 vs H2 and H3 vs H4)
#       (2) For the same population, test stats for high dose and low dose are correlated
#           due to the shared control arm and the correlation coefficient depends on
#           the allocation ratio between the control and the dose arms.
#           (H1 vs H3 and H2 vs H4)
#       (3) Correlation unknown between the remaining test stats (i.e. H1 vs H4 and H2 vs H3)

# Setting input parameters for the function

# Weights
wi <- rep(0.25, 4) # Initial weights for the 4 hypo

# Transition matrix
g <- matrix(c(0, 0.5, 0.5, 0, # H1->H2, H1->H3
              0.5, 0, 0, 0.5, # H2->H1, H2->H4
              0.5, 0, 0, 0.5, # H3->H1, H3->H4
              0, 0.5, 0.5, 0), # H4->H2, H4->H3
            byrow = T, nrow = 4)

# Test type
test <- "Dunnett" # "Bonf" # "Partly-Parametric" #

# Type I error
alp <- 0.025

# Info fraction
t <- c(0.5, 1)

# Design type
des <- "asOF"

# Correlation matrix between test stats
corr <- matrix(c(1, 0.5, 0.5, NA,
                 0.5, 1, NA, 0.5,
                 0.5, NA, 1, 0.5,
                 NA, 0.5, 0.5, 1), byrow = T, nrow = 4)
x <- AdaptGMCP:::conn.comp(corr)
print(x)

y <- AdaptGMCP:::clique.partition(corr)
print(y)

# Calling the analysis function for p-value combination method
# Use these p-values: p(H1) = 0.00025, p(H2) = 0.0952, p(H3) = 0.0245, p(H4) = 0.1104
# out <- adaptGMCP_PC(WI=wi, G=g, test.type = test, alpha = alp, info_frac = t,
#                     typeOfDesign = des, Correlation = corr,
#                     Selection = T, UpdateStrategy = T, plotGraphs = T)

design <- SetupAnalysis_PC(
  WI = wi,
  G = g,
  test.type = test,
  alpha = alp,
  info_frac = t,
  typeOfDesign = des,
  Correlation = corr,
  plotGraphs = TRUE,
  MultipleWinners = TRUE
)

# Look 1
look1_out <- AnalyzeLook_PC(
  design,
  p_raw = c(H1 = 0.00025, H2 = 0.0952, H3 = 0.0245, H4 = 0.1104),
  plotGraphs = TRUE
)

look1_out
#########################################################

# EXAMPLE 3 #################################################
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

out <- adaptGMCP_PC(WI=wi, G=g, test.type = tt, alpha = alp, info_frac = t,
                    typeOfDesign = des, Correlation = corr,
                    Selection = T, UpdateStrategy = T, plotGraphs = T)

# p_raw = c(H1 = 0.03, H2 = 0.20, H3 = 0.10, H4 = 0.25)

#########################################################


# EXAMPLE 2 #############################################
wi <- c(1 / 2, 1 / 2, 0, 0)
g <- matrix(c(
  0, 1 / 2, 1 / 2, 0,
  1 / 2, 0, 0, 1 / 2,
  0, 1, 0, 0,
  1, 0, 0, 0
), byrow = TRUE, nrow = 4)

t <- c(0.5, 0.7, 1)
alp <- 0.025

# Example correlation matrix (NA means unknown correlation)
corr <- matrix(c(
  1, 0.5, 0.5, NA,
  0.5, 1, NA, 0.5,
  0.5, NA, 1, 0.5,
  NA, 0.5, 0.5, 1
), byrow = TRUE, nrow = 4)

AdaptGMCP:::conn.comp(corr)
AdaptGMCP:::clique.partition(corr)

# Test type
test <- "Partly-Parametric"

# Design type
des <- "asOF"

# Calling the analysis function for p-value combination method
out <- adaptGMCP_PC(WI=wi, G=g, test.type = test, alpha = alp, info_frac = t,
                    typeOfDesign = des, Correlation = corr,
                    Selection = T, UpdateStrategy = T, plotGraphs = T)

# Use these p-values:
# Look 1 p_vals = c(H1 = 0.01, H2 = 0.20, H3 = 0.15, H4 = 0.30)
# Look 2:
#   selection = c("H1", "H2", "H4")
#   p-vals = c(H1 = 0.02, H2 = 0.10, H4 = 0.40)
# Look 3:
#   no selection changes
#   p-vals = c(H1 = 0.01, H2 = 0.05, H4 = 0.10)
#########################################################


# EXAMPLE 1 #############################################
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

corr <- matrix(
  c(
    # H1          H2          H3          H4          H5          H6          H7          H8
    1,          0.5,        NA,         NA,         0.6324555,  0.3162278,  NA,         NA,         # H1
    0.5,        1,          NA,         NA,         0.3162278,  0.6324555,  NA,         NA,         # H2
    NA,         NA,         1,          0.5,        NA,         NA,         0.6324555,  0.3162278,  # H3
    NA,         NA,         0.5,        1,          NA,         NA,         0.3162278,  0.6324555,  # H4
    0.6324555,  0.3162278,  NA,         NA,         1,          0.5,        NA,         NA,         # H5
    0.3162278,  0.6324555,  NA,         NA,         0.5,        1,          NA,         NA,         # H6
    NA,         NA,         0.6324555,  0.3162278,  NA,         NA,         1,          0.5,        # H7
    NA,         NA,         0.3162278,  0.6324555,  NA,         NA,         0.5,        1           # H8
  ),
  nrow = 8, byrow = TRUE,
  dimnames = list(
    paste0("H", 1:8),
    paste0("H", 1:8)
  )
)

AdaptGMCP:::conn.comp(corr)
AdaptGMCP:::clique.partition(corr)

# Test type
test <- "Partly-Parametric"

# Type I error
alp <- 0.025

# Info fraction
t <- 1 # c(0.5, 1)

# Design type
des <- "asOF"

# Calling the analysis function for p-value combination method
# Use these p-values: p =(0.21, 0.04, 0.01, 0.0155, 0.02, 0.01, 0.009, 0.003)
out <- adaptGMCP_PC(WI=wi, G=G, test.type = test, alpha = alp, info_frac = t,
                    typeOfDesign = des, Correlation = corr,
                    Selection = T, UpdateStrategy = T, plotGraphs = T)
#########################################################

