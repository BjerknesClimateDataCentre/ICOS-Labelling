########################################################################
#### This script runs all Scripts in folder "STD"

##############


#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
# Input parameters

date_col <-4
time_col <- 5
dt_format <- "%d/%m/%y%H:%M:%S"            # e.g. "%d/%m/%y %H:%M:%S"
run_type_col <- 2
CO2_col <- 9
CO2_name <- "CO2"
std_val_col <- 7
std_val_name <- "updated_std_val"

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