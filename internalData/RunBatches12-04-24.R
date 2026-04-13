# --------------------------------------------------------------------------------------------------
#
# ©2025 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------


#Uncomment the following code to run simulation;
#short cut: 1) select all the following lines 2) ctrl+shift+c]

if (interactive()) {


#Load the package
# library(AdaptGMCP)

###########-----------Inputs Need to be Modified for each run------#########
# Input Csv containing all scenarios(Change the path before reading the data)
InputDF <- read.csv('InputScenarios.csv',
                    fileEncoding = "UTF-8-BOM")

# Output csv file name(without the .csv extension)
# Format <OutFileName>_<execution date and time>.csv
OutFileName <- "Output"

#Output file directory
OutPath <- ""

##################################################################################
###########-----------Inputs Need to be Modified for customized run------#########

# Set Parallel as TRUE(always set as TRUE)
InputDF$Parallel <- T

# Specify the modelIDs to run
# If all models need to be executed then set it as  modelsToRun <- InputDF$ModelID
modelsToRun <- InputDF$ModelID
# modelsToRun <- c(1,2,3,4)

##################################################################################
##########-----------Execution, No Modification is required-----------############

#Start Time
start.time <- Sys.time()

#Run Simulation Batches

outDF <- simMAMSMEP_Wrapper(InputDF = InputDF %>% filter(ModelID %in% modelsToRun))

#Elapsed Time
elapsed.time <- Sys.time()-start.time

#Print Output
outDF

#Save Output
timeNow <- format(Sys.time(), "%d%h%y-%H_%M")
OutPath1 <- paste0(OutPath,OutFileName,"_",timeNow,".csv")
write.csv(outDF,OutPath1, row.names = F)

#Execution details
SysInfo <- Sys.info()
cat("Execution performed on ", SysInfo['nodename'],"\n",
    "Execution time", elapsed.time)
###################################################################################

}

