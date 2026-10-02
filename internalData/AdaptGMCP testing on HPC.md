# AdaptGMCP testing on HPC

## Input-Output file mapping

### Test batch 1: CER method, single binary endpoint, simulations

* Output file: "Analysis of SimOut\_CER\_Bin\_1ep3arms.xlsx", sheet "Comp.With.N"
* Input file: "Inp\_CER\_Bin\_1ep3arms-Comp.With.N.csv"


### Test batch 2: CER method, two binary or mixed endpoints, simulations

* Output file: "Analysis\_CER\_2Bin\_OR\_Mixed.xlsx", sheet "OutCER2BinORMixed"
* Input file: "Inp\_CER\_2Bin\_OR\_Mixed.csv"

## Test runs

### Sep 28, 2026 - Oct 02, 2026

### Test batch 1: CER method, single binary endpoint, simulations

* Output file: "Analysis of SimOut\_CER\_Bin\_1ep3arms.xlsx", sheet "Comp.With.N"
* Input file: "Inp\_CER\_Bin\_1ep3arms-Comp.With.N.csv"
* Models completed: 68 models from model 9 to 113 with some models excluded
* Baseline output: Baseline\_Out\_CER\_Bin\_1ep3arms-Comp.With.N.csv
* Actual output: Actual\_Out\_CER\_Bin\_1ep3arms-Comp.With.N.csv

>> Baseline output was created using the January 2026 version of the package. That was the version when we had completed thorough testing of the binary, continuous, and mixed endpoints CER simulations successfully.

>> Actual output file was created by appending outputs of smaller batches. I tried to run the full batch in the beginning, but the remote HPC restarted in between since IT installed some Windows update. So, only models 9-25 were completed. After that, I started submitting 10 models at a time.

There are some differences in the baseline and actual outputs. But they are small enough to be disregarded.

Differences appear in only three columns:
- Global.Power: 30 cells differ slightly; maximum difference is 0.0000041.
- FWER: 30 cells differ identically to Global.Power.
- StagewiseRejection_Count: 42 cells differ only in formatting—e.g., actual 18,219 versus baseline 18219; the underlying count appears equivalent.
The largest numerical change is on row 10: 0.0268716 in Actual versus 0.0268757 in Baseline, which is a difference of 0.0000041. No substantive differences were found in the remaining columns.

#### SUMMARY
We have verified that the latest version of the package as of Sep 28, 2026 produces materially the same output as the baseline output created using the January 2026 version of the package. That was the version when we had completed thorough testing of the binary, continuous, and mixed endpoints CER simulations successfully.

### Test batch 2: CER method, two binary or mixed endpoints, simulations
