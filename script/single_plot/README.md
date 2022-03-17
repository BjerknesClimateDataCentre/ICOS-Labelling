About the Single Plot and Filter Script
==========================================================================

### Description ###
Script that makes plot(s) of a single parameter vs time. This is used when
adding a plot from the raw data to the labelling report, and/or when
filtering out data (due to low water flow, sensor on land etc.) before
processing in QuinCe.

### Requirements ###
1. Raw data in the input folder. (This will change in near future. Plan to get
data from rds files, created in a different script).
2. There must be a 'settings.json' file in the same directory as this script.
The script needs the following from this settings file:
- y_colname: Name of the variable in the raw input file
- date_colname: Name of the date column
- time_colname: Name of the time column
- datetime_format The format of the date and time, e.g. '%Y-%m-%d %H:%M:%S'.
Separate date and time with space if they are on differen columns.
- y_lims: The y limits of the plot (in case of multiple input files, this y
limit will be the same for all plots)
- y_label: The label to put on the plot(s)
- add_letter: Boolean about wether a plot letter should be added or not
- letter_corner: Alternatives are 'topleft', 'topright', 'bottomleft', and
'bottomright'
- plot_letter: The letter string, e.g. 'a)'
- add_filter: Boolean about wether the script should output filtered data
- filter_lim_low and filter_lim_high: The thresholds for the filtering on the
variable in the 'y_colname'

### Output ###
Output one plot per input file of the chosen variable, and if stated in the
settings, one filtered datafile per input datafile.