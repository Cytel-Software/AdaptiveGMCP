# Batch_FS_GMCP_Sim_Bin.R
# This file contains code for executing a batch of simulations for fixed sample
# GMCP designs with binary endpoints.

library(tidyverse)

# We will use the function SimulateMAMSMEPWrapper() for executing a batch.

# # First constructing a model as required by SimulateMAMSMEPWrapper() and testing it
# # manually
# num_arms <- 5 # four treatment arms versus a common control
# ep_type <- list(EP1 = "Binary") # One binary endpoint
# arm_props <- list(EP1 = c(0.1, 0.1, 0.1, 0.1, 0.1))
# alloc_ratio <- c(1, 1, 1, 1, 1) # balanced design
# nTotSS <- 100 * num_arms
#
# weights <- rep(1/4, 4)
# # To compare with East Bonferroni, all edge weights must be specified as 0.
# trans_mat <- matrix(rep(0, 16), byrow = T, nrow = 4)
#
# sBinVar <- "UnPooled"
# dTypeIErr <- 0.025
# sTest <- "Bonf"
#
# nSims <- 100
#
# # Calling the simulation function
# out1 <- simMAMSMEP(Method = "CombPValue", alpha = dTypeIErr, SampleSize = nTotSS,
#                    TestStatBin = sBinVar,
#                    FWERControl = "CombinationTest",
#                    nArms = num_arms, nEps = 1, lEpType = ep_type,
#                    Arms.Prop = arm_props, Arms.alloc.ratio = alloc_ratio,
#                    WI = weights, G = trans_mat,
#                    test.type = sTest, info_frac = 1,
#                    nSimulation = nSims, Seed="Random", EastSumStat = NULL,
#                    Parallel = T)
#
# out1

# Now trying the same example using SimulateMAMSMEPWrapper()
# dfInput <- read_csv("internalData/BatchInput_FS_GMCP_Sim_Bin.csv")
# dfInput <- read_csv("internalData/BatchInput_AGMCP_Sim_Bin_2.csv")
dfInput <- read_csv("internalData/BatchInput_AGMCP_Sim_Bin_3.csv")
sOutFilePrefix <- "Output"
sOutPath <- "internalData/"

nModelsToRun <- dfInput$ModelID # c(1, 10, 26, 33, 66, 77, 102)

# # TRIAL RUN - START >>>>>>>>>>>>
# # To do a trial run, uncomment this block so that the tests are run with a
# # small number of simulations rather than the number specified in the input
# # file.
dfInput$nSimulation <- 100 # 10000 #
# dfInput$Parallel <- FALSE
# # TRIAL RUN - OVER >>>>>>>>>>>>

tStartTime <- Sys.time()

dfOutput <- SimulateMAMSMEPWrapper(dfInput = dfInput %>%
                                 filter(ModelID %in% nModelsToRun))

tElapTime <- Sys.time() - tStartTime

dfOutput

#Save Output
sTimeNow <- format(Sys.time(), "%d%h%y-%H_%M")
sOutPath1 <- paste0(sOutPath, sOutFilePrefix, "_", sTimeNow, ".csv")
# sOutPath1 <- paste0(sOutPath, "Output_FS_GMCP_Sim_Bin", ".csv")
write.csv(dfOutput, sOutPath1, row.names = F)

#Execution details
SysInfo <- Sys.info()
cat("Execution performed on ", SysInfo['nodename'],"\n",
    "Execution time", tElapTime)
