About the QC Summary and Plots Script
==========================================================================

## Script 'qc_summary.R' ##

### Description ###
The 'qc_summary' sript makes a summary of how many and what kind of QC messages
each parameter got from the QC procedures in QuinCe.

### Requirements ###
1. The file 'processed_data.rds' must be in the 'data' folder (one level back).
This data object is based on the processed and QCed data as exported from
QuinCe (with format 'ICOS OTC Labelling').
2. There must be a 'settings.json' file in the same directory as this script.
The script needs the following from this settings file:
- station_type: 'SOOP' or 'FOS'
- raw_co2_colname: the name of the raw co2 data column (this header is the same
 as in the raw data, and therefore different for each station)

### Output ###
A text file containing the QC summary in the output folder.


## Script 'default_plot_settings.R' ##

### Description ###
All plot settings can be edited, however the setting when starting out with
plotting new data is always the same. There is one default setting for SOOP
and another default setting for FOS. This script resets the settings to this
default.

### Requirements ###
1. There must be a settings json file in the same directory as this script
('settings.json'). Specify in this file if should reset to default for SOOP
or FOS ("station_type": "SOOP"/"FOS").
2. There must be a default settings folder with a json file containing the
defaults plot settings ('..default_settings/default_settings.json').

### Output ###
The all_plot_settings section in the settings.json file will contain the
default settings for the chosen station type.

## Script 'plots.R' ##

### Description ###
The 'plots.R' script is used to create most of the plots for the labelling
report. These are: all plots of measurements over time; calculated fco2 and
deltaT over time; intake temperature vs equilibrator temperature; and fco2 vs
xco2.

### Requirements ###
1. The file 'processed_data.rds' must be in the 'data' folder (one level back).
This data object is based on the processed and QCed data as exported from
QuinCe (with format 'ICOS OTC Labelling').
2. There must be a 'settings.json' file in the same directory as this script.
The script needs the following from this settings file:
- 'raw_co2_colname': the name of the raw co2 data column (this header is
the same as in the raw data, and therefore different for each station).
- 'all_plots': Which plots to create and how (see below).

#### About the all_plot settings ####
The settings.json file contains a list ("all_plots") of all plots that can
be created and information about how the plot should look. For each plot there
are these settings:
- make_plot: true or false. Determines if plot should be created or not.
- y_name/x_name: the name of the parameters on the x and y axis. The name must
 match the spelling in the 'header_config.json' file.
- y_filter_bad/x_filter_bad: true or false. If true, only good values (qc flag
= 2) are plotted. Cannot be true for data columns witout acompanying flag column, e.g. datetime and deltaT.
- y_lims/x_lims: The axis ranges in the format "min,max". It is not possible
to only change the min or only change the max. They must either both be NA, or
both be a number.
- y_lab/x_lab: The axis labels (use unicode for special character)
- letter_string: The letter text to add to the plot
- letter_position_name: The name of the corner where the letter string should
be added. Options are "topleft", "topright", "bottomleft" and "bottomright".

Any of these setting can be edited, which makes the script very flexible.
However, some combinations will make the script crach, e.g. filtering bad
values for deltaT (deltaT does not have an acompanying flag column).

For most situations it will be sufficient to start out with the default
settings and only edit the axis ranges and/or letter locations as needed. The
default settings can be restored by running the script
'default_plot_settings.R'.

### Output ###
The plots are stored in the output folder, together with a text file
containing the number of measurements ouf of plot range (if any). These are
needed in the labelling report text.