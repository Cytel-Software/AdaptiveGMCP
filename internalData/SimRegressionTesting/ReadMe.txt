AdaptGMCP simulation regression testing
=======================================

Run these simulations on the Azure HPC machine. Details of this machine are in the following document:
OneDrive: https://cytelinc-my.sharepoint.com/:w:/g/personal/aniruddha_deshmukh_cytel_com/IQCcIGyGo8XrQZaLPLaEWn4IAeYLFENfpQN8CyUNnWW9pWY?e=YNvPw1
Local path: "C:\Users\Aniruddha.Deshmukh\OneDrive - Cytel\Projects\CIT\MAMS\adaptgmcp-resources\HPC Machines\Access to High Performance Machine 1 1.docx".

In order to run the regression test:
1. Copy and install the latest AdaptGMCP package on the HPC machine.
2. Copy the following files to the HPC machine:
   - Input file for simulations: "AdaptiveGMCP\internalData\Mixed-2OrMoreEPs.csv"
   - R script file for running the simulation batch: "AdaptiveGMCP\internalData\Batch_2Lk_AdaptGMCP_Sim_Bin.R"
     In this script, use Mixed-2OrMoreEPs.csv as the batch input file, hardcode the following parameters, and simulate all scenarios:
          dfInput$nSimulation <- 50
          dfInput$nSimulation_Stage2 <- 25

>> Baseline output:
"Out_Baseline_Mixed-2OrMoreEPs_14Sep26-Sim1=50, Sim2=25, all models.csv" is the output computed by simulating the same batch using the package version from 18 Mar 2026. This is the version that we tested thoroughly for simulations with multiple continuous, binary, and mixed (continuous + binary) endpoints.
Use this as baseline simulation output for regression testing of PC and CER simulations.

>> Regression run on 15 Sep 2026
"Out_Mixed-2OrMoreEPs_15Sep26-Sim1=50, Sim2=25, all models.csv"
Result: All output matched perfectly with the baseline. The only differences between the two CSV files are in the "HoursTaken" column, which is expected.

