# --------------------------------------------------------------------------------------------------
#
# ©2025 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

#######################################################################
###############   Script to Run CER Analysis examples #################
#######################################################################

#######################################################################
## EXAMPLE 1
#######################################################################

if (interactive()) {
   print("Running CER Analysis Example 1-----------------------------------------")

   library(AdaptGMCP)

   alpha <- 0.025
   info_frac <- c(0.5,1)
   typeOfDesign <- 'asOF'
   WI <- rep(1/2, 2)
   G <- matrix(c(0, 1,
               1, 0),
               nrow = 2, byrow = TRUE)
   test.type <- 'Parametric'
   Correlation <- matrix(c(1,0.5,
                           0.5,1),
                           byrow=TRUE, nrow= 2)
   plotGraphs <- TRUE

   #Input for look 1: raw p-values: H1=0.00001, H2=0.02.
   adaptGMCP_CER(nArms=3, nEps=1, SampleSize=300, EpType = list(EP1 = "Continuous"),
               sigma = list(EP1 = c(1, 1, 1)), allocRatio = c(1, 1, 1),
               WI=WI, G=G, test.type = test.type, alpha=0.025,
               info_frac=info_frac, typeOfDesign = typeOfDesign,
               AdaptStage2 = TRUE, plotGraphs = TRUE)
   print("Finished CER Analysis Example 1-----------------------------------------")
}

#######################################################################
## EXAMPLE 2
#######################################################################

if (interactive()) {

   print("Running CER Analysis Example 2-----------------------------------------")
   library(AdaptGMCP)

   ########################## Inputs ##############################
   #-----------------------Total Sample Size------------------------
   SampleSize <- 400

   #-----------------------Design Alpha------------------------
   alpha <- 0.025

   #-----------------------Number of Arms------------------------
   nArms <- 3

   #-------------------Number of Endpoints------------------------
   nEps  <- 2

   #-------------Arm-Wise planned Std. Dev. for each endpoints-----
   # Vector of length = nArms
   # Not required for test.type = 'Non-Parametric'
   sigma <- list('Ep1' = c(1,1,1),
               'Ep2' = c(1,1,1))

   #-----------------------Arm-Wise Allocation Ratio------------------------
   # Vector of length = nArms
   # Not required for test.type = 'Non-Parametric'
   allocRatio <- c(1,1,1)

   #----------------Initial Weights---------------------
   #The number of hypothesis=(nEps*(nArms-1)) is same as the length of WI
   WI <-  c(0.5,0.5,0,0)

   #---------------Transition Matrix--------------------
   #The dimension of the matrix is (Number of Hypothesis x Number of Hypothesis)
   G <- matrix(c(0,1/2,1/2,0,
               1/2,0,0,1/2,
               0,1,0,0,
               1,0,0,0),
            nrow = nEps*(nArms-1), byrow = TRUE)

   #----------------Information Fraction----------------
   #The number of lookes is same as the length of info_frac(for FSD info_frac = 1)
   info_frac <- c(0.5,1)

   #-------------------Boundary Type-------------------
   # O'Brien & Fleming = 'OF', Pocock = 'P', Wang & Tsiatis Delta class = 'WT'
   # Pampallona & Tsiatis = 'PT', Haybittle & Peto = 'HP',
   # Optimum design within Wang & Tsiatis class = 'WToptimum'
   # O'Brien & Fleming type alpha spending = 'asOF', Pocock type alpha spending = 'asP'
   # Kim & DeMets alpha spending = 'asKD', Hwang, Shi & DeCani alpha spending = 'asHSD'
   # no early efficacy stop = 'noEarlyEfficacy'
   # default is "asOF"

   typeOfDesign <- 'asOF'

   #-------------------Test Type------------------------
   # Testing Procedure
   # options = 'Parametric', 'Partly-Parametric', 'Non-Parametric'
   # Note: For parametric tests the the number of endpoints must be 1, whereas for partly parametric case it has to be greater than 1
   test.type <- 'Parametric'


   #-------------------Adaptation Flag------------------------
   # AdaptStage2 : TRUE = Adaptation options will be given after stage-1
   AdaptStage2 <- TRUE

   #-------------------Plot Intermediate Graphs------------------------
   plotGraphs <- TRUE

   #--------------Run Analysis--------------------------
   ###For Interim-look inputs follow the R console. To quit, press the Esc key#####
   adaptGMCP_CER(nArms = nArms, nEps = nEps, sigma = sigma,
               allocRatio = allocRatio, SampleSize = SampleSize,
               alpha = alpha, WI = WI, G = G, info_frac = info_frac,
               typeOfDesign = typeOfDesign, test.type = test.type,
               AdaptStage2 = AdaptStage2,plotGraphs = plotGraphs)

   print("Finished CER Analysis Example 2-----------------------------------------")
}
