About the Check Equilibrium Script
==========================================================================

### Description ###
Sript that visualise the measurement sequences in different ways and produce a
new datafile where only one row of measurements is kept for each sequence.

### Requirements ###
1. Raw data in the input folder. (This will change in future. Plan to get
data from rds files, created in a different script).
2. There must be a 'settings.json' file in the same directory as this script.
The script needs the following from this settings file:
- 'date_colname' and 'time_colname': The name of date and time columns. Set to
false if date and time is instead given in 6 individual columns
- 'datetime_format': Relevant if date and time are stored in two columns.
Separate them by space, e.g. "%d/%m/%Y %H:%M:%S"
- 'year_colname', 'month...', etc.: Name of the datetime related columns.

- 'check_whole_sequence': true or false. This determines if whole sequences
as given in the data should be included in the visualisations. Alternatively...
- 'check_last_n': Number of final measuremnts per sequence to be included in
the visualisations.
- 'max_timediff': How many seconds of timedifference to allow for within a
measurement sequence

- 'fig1_n_random_plots': Number of random plots to show (current layout work
best with 4!)
- 'fig1_ylab': Y label to use in figure 1. This is optional since the co2 can
be either xco2 or pco2
- 'fig2_letter_position': Where to place the plot letter in figure 2. Options
are 'topright', 'topleft', 'bottomright', and 'bottomleft'.

- 'extract_whole_sequence': true or false. This determines if whole sequences
as given in the data should be included when calulating the average of the co2
per sequence. Alternatively...
- 'extract_last_n': Number of final measuremnts per sequence to be included in
the averaging calulcations (this would then be clear from the visualisations:
how many measurements are done after the measurements seem to have reached an
equilibrium). If this number is set to 1, this means only the final measurement
per sequence is included in the output file. Note that this section of the script
is not written yet, the script will only work with extracting of the whole
sequence.
- 'average_other_cols': List of other column names, besides co2, where sequence
 vise averages must be calculated.

### Output ###
Depending on the settings, the script produce a figure showing some random co2
measuremnt sequences; a plot of all co2 measurement sequences standard deviation
over time; and a histogram of the standard deviations.

The script also outputs a data file - in the exact same format as the input
file - containin only one row of measurements from each measurement sequence.
This extracted row is based on the final row per measurement sequence, but for
co2 (and other columns specified in the settings file) the value is either that
found in the final row of each sequence; or an average of all measurements per
sequence; or an average of the tail of each sequence.