########################################################################
#### This script runs all Scripts in folder "Summary_and_plot_script"
##############


#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
# Input parameters

date_col <-1
time_col <- 1
dt_format <- "%d/%m/%Y %H:%M:%S"            # e.g. "%d/%m/%y %H:%M:%S"
run_type_col <- 3
CO2_col <- 12
CO2_name <- "CO2_PHYS"
std_val_col <- 10
std_val_name <- "STD"
std_names <- c("STD1","STD2","STD3","STD4")


#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
input_from_main <- TRUE

source('1_STD_graphs.R')
source('2_STD_box_plot.R')

#Remove these parameters in case individual scripts will be ran imediately after this one.
rm("date_col")
rm("time_col")
rm("dt_format")
rm("run_type_col")
rm("CO2_col")
rm("CO2_name")
rm("std_val_col")
rm("std_val_name")
rm("std_names")

input_from_main <- FALSE