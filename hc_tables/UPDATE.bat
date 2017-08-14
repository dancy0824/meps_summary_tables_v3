PATH=C:\Users\emily.mitchell\Documents\R\R-3.3.3\bin

Rscript shared\puf_update.R
Rscript shared\puf_transfer.R

cd "hc1_use"
START update_USE.bat

cd "..\hc2_care"
START update_CARE.bat