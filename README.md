# MA-Economics-Project
Assessing the Impacts of Uncertainty on Firm Performance
This README describes how to replicate results included in the manuscript.

The firm-level dataset, uncertainty measures, and macroeconomic variables are included as DTA files.

The DO files should be run consecutively. 
DO files 1-3 are used to merge, clean, and describe the data, respectively.
DO file 4 is used to perform panel regressions.
DO file 5 is used to aggregate the firm-level panel data into time series data, which is subsequently used in VAR modelling.

VAR modelling is performed using the "Canada VAR Modelling" and "US VAR Modelling" R files.
