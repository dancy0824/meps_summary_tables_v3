This folder contains code to calculate tabular estimates for each app, plus instructions to update tables when new data years are released.

## Updating tables

After the yearly release of the Full-Year-Consolidated File (FYC), the app tables can be updated by running (double-clicking) [UPDATE.bat](UPDATE.bat) (on Windows). This is a batch file that runs:
1. [puf_update.R](puf_update.R) to read in most recent updates to Public Use Files (PUF) releases
2. [puf_transfer.R](puf_transfer.R) transfers new PUFs from MEPS website to local folder C:/MEPS (may need to create this folder if it doesn't exist)
3. XX/run_USE.R (XX = app folder) to run estimates on new data
4. XX/run_merge.R (XX = app folder) to append new estimates onto existing data
