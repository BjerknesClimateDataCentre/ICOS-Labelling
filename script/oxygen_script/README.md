About the Oxygen Script
==========================================================================

### Description ###
Sript that creates the plots and stats related to the oxygen measuremtns needed
for the labelling step 2 report.

### Requirements ###
1. Raw data in the input folder. (This will change in near future. Plan to get
data from rds files, created in a different script).
2. There must be a 'settings.json' file in the same directory as this script.
The script needs the following from this settings file:
- required_columns: the name of various columns needed
- datatime_format: The format of the date and time columns
- plot_settings: Various informations needed for creating the plot. Note the
'sat_scale_coeff', which is used to edit the second y axis range. Increase the
coefficient to get a lower y max range.
- remove missing: a true or false if missing values should be removed from the
data before plotting
- missing value: the missing value that should be removed
- qc_range: The lower and uppper limit for good quality data (do not change
these)

### Output ###
Script creates a plot of oxygen vs time (oxygen saturation is included
in the same plot if this is provided in the raw data). The script also creates
a file with the total number of oxygen measurements; the number of measurements
outside the plot range; and number of measurements outside the accepted range.