# --------------------------------------------------------------------------------------------------
#
# ©2025 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

#######################################################################
#####   Script to Run Combining P-value Analysis examples #############
#######################################################################

if (interactive()) {

library(AdaptGMCP)

#-----------------------Alpha------------------------
alpha <- 0.025

#----------------Information Fraction----------------
#The number of lookes is same as the length of info_frac
info_frac <- c(0.5,0.7,1)

#-------------------Boundary Type-------------------
# O'Brien & Fleming = 'OF', Pocock = 'P', Wang & Tsiatis Delta class = 'WT'
# Pampallona & Tsiatis = 'PT', Haybittle & Peto = 'HP',
# Optimum design within Wang & Tsiatis class = 'WToptimum'
# O'Brien & Fleming type alpha spending = 'asOF', Pocock type alpha spending = 'asP'
# Kim & DeMets alpha spending = 'asKD', Hwang, Shi & DeCani alpha spending = 'asHSD'
# no early efficacy stop = 'noEarlyEfficacy'
# default is "asOF"

typeOfDesign <- 'asOF'


#----------------Initial Weights---------------------
#The number of hypothesis(k) is same as the length of WI
WI <- c(1/2,1/2,0,0)

#---------------Transition Matrix--------------------
#The dimension of the matrix is (Number of Hypothesis x Number of Hypothesis)
G <- matrix(c(0,0.5,0.5,0,
              0.5,0,0,0.5,
              0,1,0,0,
              1,0,0,0),
            nrow = 4, byrow = TRUE)

#-------------------Test Type------------------------
# Testing Procedure
# Bonferroni = 'Bonf' , Sidak = 'Sidak', Simes ='Simes'
# Dunnett = 'Dunnett', Partly Parametric = 'Partly-Parametric'
test.type <- 'Partly-Parametric'


#---------------Correlation--------------------------
#Need to modify only for Dunnett and Partly Parametric test else will be ignored inside the code.
#The dimension of the matrix is (Number of Hypothesis x Number of Hypothesis)
Correlation <- matrix(c(1,0.5,NA,NA,
                        0.5,1,NA,NA,
                        NA,NA,1,0.5,
                        NA,NA,0.5,1),
                        byrow=TRUE, nrow= 4)

#-------------------Plot Intermediate Graphs------------------------
plotGraphs <- TRUE

#--------------Run Analysis--------------------------
###For Interim-look inputs follow the R console. To quit, press the Esc key#####
adaptGMCP_PC(
  WI = WI,
  G = G,
  test.type = test.type,
  alpha = alpha,
  info_frac = info_frac,
  typeOfDesign = typeOfDesign,
  Correlation = Correlation,
  Selection = T,
  UpdateStrategy = T,
  plotGraphs = plotGraphs
)

}
