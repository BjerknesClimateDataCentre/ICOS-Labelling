########################################################################
#### This script runs all Scripts in folder "VOS_Atmospheric_CO2_plot"
##############


#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
# Input parameters

# Input for the raw atmospheric plot
date_col <-c(1)
time_col <- c(1)
dt_format <- "%d/%m/%Y %H:%M:%S"            # e.g. "%d/%m/%y %H:%M:%S"
CO2_col <- 12
CO2_col_name <- c("CO2_PHYS")
run_type_col <- 3
atm_type_name <- c("ATM")
lat_col <- 5
lon_col <- 6
lat_name <- c("LATX")
lon_name <- c("LONX")
letter <- "d)"                              # Letter to use in plot (depends on how many other measurement plots)
position <- "bottomleft"                    # Depends on letter position of the other measurement plots


#Input for comparison with external data
CO2_ext_name <- "analysis_value"
change_year_from <- NA   # The station data year will be changed in case this period is not available for the external station. If not nessesary assign "NA".
change_year_to <- NA     # If not nessesary assign "NA"
add_to_external <- NA       # In case we change years in data. This accounts for yearly increase. If not nessesary assign "NA" 
max_lon <- NA              # In case we want to compare a bounding box instead of all station data
min_lon <- NA
max_lat <- NA
min_lat <- NA


#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
input_from_main <- TRUE

source('1_Atm_CO2_plot.R')
source('2_qualityCheck_plots.R')
source('3_external_plot.R')

#Remove these parameters in case individual scripts will be ran imediately after this one.
rm("date_col")
rm("time_col")
rm("dt_format")
rm("CO2_col")
rm("CO2_col_name")
rm("run_type_col")
rm("atm_type_col")
rm("lat_col")
rm("lon_col")
rm("lat_name")
rm("lon_name")
rm("letter")
rm("position")
rm("CO2_ext_name")
rm("change_year_from")
rm("change_year_to")
rm("add_to_external")
rm("max_lon")
rm("min_lon")
rm("max_lat")
rm("min_lat")

input_from_main <- FALSE