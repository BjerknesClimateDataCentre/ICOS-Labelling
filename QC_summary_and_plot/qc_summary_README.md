The 'qc_summary.R' script
==========================================================================

### About the script ###
The script takes the quality controlled (QCed) data from QuinCe and creates a
summary of the QC.

### How to run the script ###
Before run the script, add the dataset (as exported from QuinCe with the 'ICOS
Labelling' format) in the input folder.

This is needed from the settings file:
- station_type: 'SOOP' or 'FOS'
- raw_co2_colname: the name of the raw co2 data column (this header is the
same as in the raw data, and therefore different for each station)