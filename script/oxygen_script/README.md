About the Oxygen Script
==========================================================================

### Description ###
Sript that can convert oxygen (mL/L to umol/kg), plot it, and perform oxygen
range checks.

### Requirements ###
1. Raw data in the input folder. (This will change in future. Plan to get
data from rds files, created in a different script).
2. There must be a 'settings.json' file in the same directory as this script.
The script needs the following from this settings file:
- required_columns: the name of various columns needed and the datetime format
(not needed if the date and times are in 6 individual columns)
- convert_settings: These are boolean variables explaining if/how a unit
conversion should be performed. The script does not yet include calculation
using sigm_theta! If sigma_theta is provide in the data set this
setting to 'true' and add this calculation to the script.
- filter_settings: For now the only filter feature added is removal of missing
value before making an output file and plot
- plot_settings: Various informations needed for creating the plot. Note the
'sat_scale_coeff', which is used to edit the second y axis range. Increase the
coefficient to get a lower y max range.
- range_settings: Boolean for wether the range check should be perfomed and
its limits (do not change these). Range check unit is for umol/kg only!

### Output ###
Depending on the settings, the script produce an output file with converted,
filtered data; a plot of oxygen vs time (oxygen saturation is included
in the same plot if this is provided in the raw data); a file with the total
 number of oxygen measurements; the number of measurements outside the plot
  range; and number of measurements outside the accepted range.