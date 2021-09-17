About the Atmospheric CO2 check Script
==========================================================================

### Description ###
Script that creates plots and checks related to the atmospheric co2 measuremnts
from stations undergoing labelling.

### Requirements ###
1. Raw data in the input folder. (This will change in near future. Plan to get
data from rds files, created in a different script).
2. If relevant for labelling, there must be an external dataset in the input/
external directory. The data can be found here: Station map:
https://www.esrl.noaa.gov/gmd/dv/iadv/. See example of data format in the file
external_data_format_example.txt (in same directory as this file).
3. There must be a 'settings.json' file in the same directory as this script.
The script needs the following from this settings file:
- required_columns: the name of various columns needed
- datatime_format: The format of the date and time columns
- atm_type_name: How the atmospheric measurements are shown in the run type
column
- plot_settings: Various informations needed for creating the plot
- qc_range: The lower and uppper limit for good quality data (do not change
these)

### Output ###
The scipt creates a plot with the atmospheric co2 vs time; prints the number of
 measurements outside of accepted range; creates histogram of measurement
 sequences, plots the standard deviations per sequence on a 'map', and if
requested, compares the measurements with an atmospheric station.