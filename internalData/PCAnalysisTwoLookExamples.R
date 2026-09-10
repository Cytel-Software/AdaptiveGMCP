# --------------------------------------------------------------------------------------------------
#
# Copyright 2026 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

library(AdaptGMCP)

#===================================================================================================
# EXAMPLE 1 - Two-look hypothesis rejection without early stopping
#===================================================================================================
# H1, H2, and H3 compare three treatment arms with a common control.  H1 is
# rejected at look 1 and H2 is rejected at look 2.  MultipleWinners = TRUE
# keeps the trial running after the first rejection.

# Setting all input parameters
wi_ex1 <- c(H1 = 1 / 3, H2 = 1 / 3, H3 = 1 / 3)
g_ex1 <- matrix(
  c(
    0, 0.5, 0.5,
    0.5, 0, 0.5,
    0.5, 0.5, 0
  ),
  byrow = TRUE,
  nrow = 3,
  dimnames = list(names(wi_ex1), names(wi_ex1))
)

corr_ex1 <- matrix(
  c(
    1, 0.5, 0.5,
    0.5, 1, 0.5,
    0.5, 0.5, 1
  ),
  byrow = TRUE,
  nrow = 3,
  dimnames = list(names(wi_ex1), names(wi_ex1))
)

# Setting up the design
state_ex1 <- SetupAnalysis_PC(
  WI = wi_ex1,
  G = g_ex1,
  test.type = "Partly-Parametric",
  alpha = 0.025,
  planned_info_frac = c(0.5, 1.0),
  typeOfDesign = "asOF",
  MultipleWinners = TRUE,
  plotGraphs = FALSE
)

print("Example 1: Design")
print(state_ex1)

state_ex1 <- AnalyzeLook_PC(
  state = state_ex1,
  look = 1,
  p_raw = c(H1 = 0.0001, H2 = 0.40, H3 = 0.40),
  Correlation = corr_ex1,
  plotGraphs = FALSE
)

print("EXAMPLE 1 - Look-1 analysis output")
print(state_ex1$mcpObj)

state_ex1 <- AnalyzeLook_PC(
  state = state_ex1,
  look = 2,
  p_raw = c(H2 = 0.0001, H3 = 0.40),
  plotGraphs = FALSE
)

print("EXAMPLE 1 - Look-2 analysis output")
print(state_ex1$mcpObj)
print(state_ex1$completion_reason)

#===================================================================================================
# EXAMPLE 2 - Select a surviving treatment arm
#===================================================================================================
# H1/H2 are endpoint 1 for treatments 1/2, and H3/H4 are endpoint 2 for
# treatments 1/2.  At look 2, treatment 2 is dropped across both endpoints.
# Setting all input parameters
wi_ex2 <- c(H1 = 0.5, H2 = 0.5, H3 = 0, H4 = 0)
g_ex2 <- matrix(
  c(
    0, 0.5, 0.5, 0,
    0.5, 0, 0, 0.5,
    0, 1, 0, 0,
    1, 0, 0, 0
  ),
  byrow = TRUE,
  nrow = 4,
  dimnames = list(names(wi_ex2), names(wi_ex2))
)

corr_ex2 <- matrix(
  c(
    1, 0.5, 0.5, NA,
    0.5, 1, NA, 0.5,
    0.5, NA, 1, 0.5,
    NA, 0.5, 0.5, 1
  ),
  byrow = TRUE,
  nrow = 4,
  dimnames = list(names(wi_ex2), names(wi_ex2))
)

# Setting up the design
state_ex2 <- SetupAnalysis_PC(
  WI = wi_ex2,
  G = g_ex2,
  test.type = "Partly-Parametric",
  alpha = 0.025,
  planned_info_frac = c(0.5, 1.0),
  typeOfDesign = "asOF",
  plotGraphs = FALSE
)

print("Example 2: Design")
print(state_ex2)

state_ex2 <- AnalyzeLook_PC(
  state = state_ex2,
  look = 1,
  p_raw = c(H1 = 0.10, H2 = 0.20, H3 = 0.15, H4 = 0.30),
  Correlation = corr_ex2,
  plotGraphs = FALSE
)

print("EXAMPLE 2 - Look-1 analysis output")
print(state_ex2$mcpObj)

state_ex2 <- AnalyzeLook_PC(
  state = state_ex2,
  look = 2,
  p_raw = c(H1 = 0.02, H3 = 0.04),
  selection = c("H1", "H3"),
  plotGraphs = FALSE
)

print("EXAMPLE 2 - Look-2 analysis output")
print(state_ex2$mcpObj)
print(state_ex2$completion_reason)

#===================================================================================================
# EXAMPLE 3 - Select hypotheses and change the graph
#===================================================================================================
# H4 is dropped at look 2.  Enrollment can then be redirected to the remaining
# treatment arms.  The inferential strategy is also changed at that look:
# H1 receives the largest initial weight, and the transition matrix gives
# rejected H1 weight to H2/H3 rather than retaining the original graph.
# Setting up all input parameters
wi_ex3 <- c(H1 = 0.5, H2 = 0.5, H3 = 0, H4 = 0)
g_ex3 <- matrix(
  c(
    0, 0.5, 0.5, 0,
    0.5, 0, 0, 0.5,
    0, 1, 0, 0,
    1, 0, 0, 0
  ),
  byrow = TRUE,
  nrow = 4,
  dimnames = list(names(wi_ex3), names(wi_ex3))
)

corr_ex3 <- corr_ex2

# Setting up the design
state_ex3 <- SetupAnalysis_PC(
  WI = wi_ex3,
  G = g_ex3,
  test.type = "Partly-Parametric",
  alpha = 0.025,
  planned_info_frac = c(0.5, 1.0),
  typeOfDesign = "asOF",
  plotGraphs = FALSE
)

print("Example 3: Design")
print(state_ex3)

state_ex3 <- AnalyzeLook_PC(
  state = state_ex3,
  look = 1,
  p_raw = c(H1 = 0.04, H2 = 0.18, H3 = 0.12, H4 = 0.22),
  Correlation = corr_ex3,
  plotGraphs = FALSE
)

print("EXAMPLE 3 - Look-1 analysis output")
print(state_ex3$mcpObj)

# Setting up new weights and transition matrix for look 2
new_w_ex3 <- c(H1 = 0.5, H2 = 0.25, H3 = 0.25)
new_g_ex3 <- matrix(
  c(
    0, 0.5, 0.5,
    0.5, 0, 0.5,
    0.5, 0.5, 0
  ),
  byrow = TRUE,
  nrow = 3,
  dimnames = list(names(new_w_ex3), names(new_w_ex3))
)

corr_l2_ex3 <- corr_ex3[1:3, 1:3]
corr_l2_ex3[1, 2] <- corr_l2_ex3[2, 1] <- 0.3
corr_l2_ex3[1, 3] <- corr_l2_ex3[3, 1] <- 0.4

print("EXAMPLE 3 - New graph weights and transition matrix before look 2:")
print(new_w_ex3)
print(new_g_ex3)

state_ex3 <- AnalyzeLook_PC(
  state = state_ex3,
  look = 2,
  p_raw = c(H1 = 0.03, H2 = 0.10, H3 = 0.08),
  selection = c("H1", "H2", "H3"),
  new_weights = new_w_ex3,
  new_G = new_g_ex3,
  Correlation = corr_l2_ex3,
  plotGraphs = FALSE
)

print("EXAMPLE 3 - analysis output for look 2")
print(state_ex3$mcpObj)
print(state_ex3$completion_reason)
