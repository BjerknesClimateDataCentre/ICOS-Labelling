The "1_FOS_contros_pCO2" script is used to calculate pco2 from the contros system
This script is the first to be ran when labelling a FOS contros station, and
the input is the raw data from the station.

The "2_FOS_contros_diagnostic" script compares the measured vs calculated 
co2s. This script must be ran in a later step in the labelling process because
the inputfile needs to only contain final measurements (need to have ran the
equilibrium script to extract the final measurements). Use e.g. the 
output file from the extract final value script.