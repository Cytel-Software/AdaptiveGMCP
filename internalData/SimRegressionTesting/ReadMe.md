# AdaptGMCP simulation regression testing

Run these simulations on the Azure HPC machine. Details of this machine are in the following document:
> OneDrive: https://cytelinc-my.sharepoint.com/:w:/g/personal/aniruddha_deshmukh_cytel_com/IQCcIGyGo8XrQZaLPLaEWn4IAeYLFENfpQN8CyUNnWW9pWY?e=YNvPw1

> Local path: "C:\Users\Aniruddha.Deshmukh\OneDrive - Cytel\Projects\CIT\MAMS\adaptgmcp-resources\HPC Machines\Access to High Performance Machine 1 1.docx".

In order to run the regression test:

1. Copy and install the latest AdaptGMCP package on the HPC machine.
2. Copy the following files to the HPC machine:
   - Input file for simulations: "AdaptiveGMCP\internalData\Mixed-2OrMoreEPs.csv"
   - R script file for running the simulation batch: "AdaptiveGMCP\internalData\Batch_2Lk_AdaptGMCP_Sim_Bin.R"
     In this script, use Mixed-2OrMoreEPs.csv as the batch input file, hardcode the following parameters, and simulate all scenarios:
          dfInput$nSimulation <- 50
          dfInput$nSimulation_Stage2 <- 25

IMPORTANT: this test purposely makes use of a limited number of simulations. In the past, we have executed these
test cases with much larger number of simulations like 20000 on the HPC machine. However, that takes a very long time
to complete. So, as a part of regression testing, we have fixed much smaller number of simulations above.

## Baseline output:

"Out_Baseline_Mixed-2OrMoreEPs_22Oct26-Sim1=50, Sim2=25, all models.csv" is the output computed by simulating the same batch using the package version 2.3.3 available on 22 Oct 2026. In this version, simulation results changed a bit because I fixed [bug 204](https://github.com/Cytel-Software/AdaptiveGMCP/issues/204). These are correct results. So, we will use them as the baseline henceforth.

We have discarded the old baseline "Out_Baseline_Mixed-2OrMoreEPs_14Sep26-Sim1=50, Sim2=25, all models.csv", which was computed by simulating the same batch using the package version from 18 Mar 2026. This is the version that we tested thoroughly for simulations with multiple continuous, binary, and mixed (continuous + binary) endpoints. However, it had bug 204.

## Template for documenting regression runs

### Regression run on `<date>`

- Output file: `<output filename>`
- Overall result: `<describe overall result here: E.g. All output matched perfectly with the baseline. The only differences between the two CSV files are in the "HoursTaken" column, which is expected.>`

DO NOT EDIT THIS TEMPLATE. Copy it when you want to document results of a new regression run.

## Regression runs

In this section, we document results of simulation regression testing done from time to time. Document them in the following format:

### Regression run on 22 Oct 2026

- Output file: "Out_Mixed-2OrMoreEPs_22Oct26-Sim1=50, Sim2=25, all models.csv"
- Overall result: These results match with the baseline perfectly. However, that is expected as we used these same results as the new baseline. Future regression runs will be a real test.

