The 'plots.R' script
==========================================================================

### About the script ###
The plot script is used to create most of the plots for the labelling report.
These are: all plots of measurements over time; calculated fco2 and deltaT over
time; intake temperature vs equilibrator temperature; and fco2 vs xco2.

### How to run the script ###
Before run the script, add the dataset (as exported from QuinCe with the 'ICOS
Labelling' format) in the input folder, and edit the settings.json file if
needed. The settings file is used to specify:
- the name of the raw co2 data column (this header is the same as in the raw
data, and therefore different for each station)
- which plots to create and how (see below)

### The plot settings ###
The settings file contains a list ("all_plot_settings") of all plots that can
be created. For each plot there are these settings:
- make_plot: true or false. Determines if plot should be created or not.
- y_name/x_name: the name of the parameters on the x and y axis. The name must
 match the spelling in the header_config.
- y_filter_bad/x_filter_bad: true or false. If true, only good values (qc flag
= 2) are plotted. Cannot be true for datetime and deltaT.
- y_lab/x_lab: The axis labels (use unicode for special character)
- y_lims/x_lims: The axis ranges in the format "min,max". It is not possible
to only change the min or only change the max. They must either both be NA, or
both be a number.
- letter_string: The letter text to add to the plot
- letter_position_name: The name of the corner where the letter string should
be added. Options are "topleft", "topright", "bottomleft" and "bottomright".

Any of these setting can be edited, which makes the script very flexible.
However, some combinations will make the script crach, e.g. filtering bad
values for deltaT (deltaT does not have an acompanying flag column).

For most situations it will be sufficient to start out with the default
settings and only edit the axis ranges and/or letter locations as needed.