Data Analysis for Station Labelling for ICOS
==========================================================================

### Project Description
All stations in the Integrated Carbon Observation System (ICOS) network must go
through a station labelling process to ensure a high quality standardised
station network (see more information about labelling on the
[ICOS OTC website](https://otc.icos-cp.eu/labelling)). This process includes a
testing period where data from the stations are analysed using the R scripts
found in this github repository.
[QuinCe](https://github.com/BjerknesClimateDataCentre/QuinCe) (a system for
automatic processing and QC of data) is also used for this data analysis
procedure.


### Data Analysis Procedure ###
There are two station types - Ship of Opportunity (SOOP) station and Fixed
Ocean Station (FOS). Below follows a description of the data analysis procedure
for each station type, with refences to the R scripts used. However, note that
 since all stations are different (differ in instrument- and sensor-setup, raw
 data format, parameters measured, sensors requiring different calculations and
processing etc.), the procedure is not fixed and can vary a lot from station to
station, especially for FOS stations. As new stations go through the labelling
 process, the existing scripts may need editing and new scripts needs to be
 created.

##### SOOP #####
- Reformat and cleanup the original files if needed. This includes e.g.
concatenate daily/weekly files into one file, edit strange date/time format,
edit strange position format, filter out data with low water flow, etc.
- Check gas standard measurements. Use R scripts ‘1_STD_graphs.R’ and
‘2_STD_box_plot.R’ to produce figures visualising how the measurements of the
standard gases compare to their supposed value.
- Merge CO2 and hydrography files if needed. For SOOP stations the CO2 data are
 often separated from the hydrography data. In that case these files need to be
 merged. Use R script ‘0.convert_to_doy’ and ‘1_merge.R’.
- Create a new instrument in QuinCe with the merged file as the format example.
 Upload the merged file, let quince run the automatic quality control, and
 export the resulting file with the quality check results. Use this exported
 file as input in the R scripts ‘1.no_external_data.R’, ‘2.summary’,
 ‘3.plot_measurements’, and ‘4.plot_indicators_only_good_values.R’ to create
 plots of the measured data, calculated indices (deltaT and fCO2) and the table
  showing the quality control results. Before running these scripts, remember
  to remove quality control messages for parameters which are not core
  parameters for SOOP (change flag from 4 to 2 if a row then has no quality
  messages).
- Check the atmospheric CO2 data. Run R scrips ‘1_Atm_CO2_plot.R’,
‘2_qualityCheck_plots.R’ and possibly ‘3_external_plot.R’ if there are nearby
atmospheric stations to compare with.
- Check the oxygen data if needed with script '1_oxygen_plot_rangeCheck.R'.

##### FOS #####
- Reformat and cleanup original files if needed (csv/tsv data format, edit date
 and formats position, add position).
- Check if measurements equilibrate. For FOS stations, the raw files often
include all measurements taken in a measurements sequence, and not just the
final equilibrated actual data value. We then check if these measurement
sequences equilibrate nicely (a sequence should show a logarithmic growth
indicating that the measurements equilibrate). Run R scrips ‘1_FOS_check_equ.R’
 and ‘2_FOS_check_equ2.R’ to produce figures showing measurements cycles. Run
 R script ‘3_FOS_extract_final.R’ to create a new datafile where only the final
  measurements in a cycles are included. Use this file in the following steps.
- Create a new instrument in QuinCe. (If use QuinCe version ‘QuinCamilla’ the
file needs to be adjusted to account for the fact that this version cannot
actually deal with data from FOS stations: add fake column with run type, and
some fake rows for standard measurements; make a copy of the SST column and use
as fake equilibrator temperature; if internal pressure is not measured add
fake internal pressure column with value 1000). Upload the file to QuinCe,
run the automatic quality control, and export the resulting file with the
quality check results. Use this exported file as input in the R scrips
‘1.no_external_data.R’, ‘2.summary’ and ‘3.plot_measurements’ to create plots of
the measured data and the table showing the quality control results. Only run
the R script ‘4.plot_indicators_only_good_values.R’ if use a QuinCe version
which correctly calculates fCO2 for FOS. Before running these R scripts,
remember to remove quality control messages for parameters which are not core
parameters for FOS (change flag from 4 to 2 if a row then has no message).
- If use a QuinCe version which does not calculate fCO2 correctly, run the R
script ‘1_FOS_calulations’ to produce the fCO2 plots.
- Additional steps might be to calculate oxygen and ph. Processing of these
parameters are sensor dependent and so there is not an R script for this yet.


### Script description ###
See more detailed information inside the individual script files.