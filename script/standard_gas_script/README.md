About the Standard Gas Anomaly Script
==========================================================================

### Description ###
Script that plots and checks the standard gas anomalies.

### Requirements ###
1. Raw data in the input folder. (This will change in near future. Plan to get
data from rds files, created in a different script).
2. There must be a 'settings.json' file in the same directory as this script.
The script needs the following from this settings file:
- required_columns: the name of the run type, std value and co2 columns in the
raw data file
- std_names: The names of the standard gases in the data file
- std_legends: What the standard gases should be called in the figures (should
be "STD1" etc.)
- std_approx_values: The approximate value of the standard gases (usually round
to nearest 50). To be used in the plot labels
- remove_flush: True or False on wheather flush rows has to be filtered out
- n_rows_flush: If remove_flush is set to True, how many rows should be removed
per std measurement cycle?
- co2_missing_value: Are there missing values that has to be filtered out
- y_lims: Set the y plot range limits (plot first with -5,5 - then adjust if
needed)
- qc_ranges: The ranges defining a good or questionable anomaly. Should not be
changed.

### Output ###
The scipt creates two plots: a figure with all the standard gas anomalies vs
time; and a box plot. It also creates a text file with information about
number of values outside the questionable and bad range for each standard
gas, and how much the drift in each anomaly was in the measurement period.