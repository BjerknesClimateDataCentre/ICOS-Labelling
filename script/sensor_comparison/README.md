About the Sensor Comparison Script
==========================================================================

### Description ###
Sript that compares the output from two sensors measuring the same variable.

### Requirements ###
1. Raw or processed data in the input folder. When using raw data, the data
from the two sensors needs to be in the same file first (use merge script to
achieve this)
2. There must be a 'settings.json' file in the same directory as this script.
The script needs the following from this settings file:
- 'input_file_format' and 'input_file_delim': The file type and deliminator
for the input file(s)
- required_columns: Specify the column names of date/time and the sensor data
- plot_settings/sensors_vs_time:
	- 'make_plot': true or false
	- 'x_lims' and 'y_lims': The limits for the axis (set initially to "NA,NA"
	and adjust if needed)
	- 'x_lab' and 'y_lab': The axis labels
	- legends: These names will be written in the plot itself
	- 'legend_height': Adjust the hight of the legend if the text colides with
	the datapoints in the plot
- plot_settings/sensor1_vs_sensor2:
	- 'make_plot': true or false
	- 'rows_are_aligned': true or false about wether the data from the two
	sensors appear on the same row (this is often not the case when use quince-
	processed input data). If this is set to false the script will align the
	rows which are within the 'time_diff_cutoff'
	- 'time_diff_cutoff': See 'rows_are_aligned' above...
	- 'x_lims' and 'y_lims': The limits for the axis (set initially to "NA,NA"
	and adjust if needed)
	- 'x_lab' and 'ylab': The axis labels

### Output ###
Depending on the settings, the script produce one or two Figures:
- a Figure showing the sensors vs time, with one plot per sensor
- a Figure showing a scatterplot where the two sensor data are plotted against
eachother