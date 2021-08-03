#####################################################################################
### JOINING TWO DATASETS
####################################################################################

### Description:
# This script joins two data tables together based on the closest match in time,
# within a maximum allowed time difference. 

##-----------------------------------------------------------------------------
### INPUT PARAMETERS


# Name of files to merge (the 'pri' file is the "most important one"- all rows are kept in
# the output merged file e.g. the co2 file, while the sst rows are added if there is a match
filename_pri <- "34FM20200815-ICOS OTC Labelling_losGatos_wDOY_2020.txt"  
filename_sec <- "mea_all_stdval_wDOY_2020.txt"

# The files must contain the date and time in DOY format (if does not: use the 'convert_to_doy' script) 
# Specify the column storing the DOY
doy_col_pri <- c(1)           
doy_col_sec <- c(1)             

# What should be added to the columns in order to separate their source files ('_' needed)
suffix_pri <- "_Processed"          
suffix_sec <- "_Raw"       

# Maximum allowed time difference in hours (usually use 5)
max_time_diff <-(1/(60))/24     

# The output filename
output_filename <- "merged.txt"


##-----------
# Initial settings
library(readr)
library(dplyr)
library(fuzzyjoin)

Sys.setlocale("LC_ALL", "English"); 

##-----------
# Import data and store in separate tibles

filepath_pri <- paste("input/", filename_pri, sep="")
filepath_sec <- paste("input/", filename_sec, sep="")

df_pri <- read_tsv(filepath_pri)
df_sec <- read_tsv(filepath_sec)

#----------
# Chronology check. 

# Find number of non-chronological rows and write result to console. Non chornology
# needs to be fixed manually before can continue sctipt.

# Primary file:
n_non_chron_pri <- sum(diff(df_pri$DOY) < 0)
if (n_non_chron_pri != 0) {
  non_chron_row_pri <- which(diff(df_pri$DOY) < 0)
  cat("Primary file: Row(s) not in chronologic order:","\n",non_chron_row_pri,"\n")
  stop()
}

# Secondary file:
n_non_chron_sec <- sum(diff(df_sec$DOY) < 0)
if (n_non_chron_sec != 0) {
  non_chron_row_sec <- which(diff(df_sec$DOY) < 0)
  cat("Secondary file: Row(s) not in chronologic order:","\n",non_chron_row_sec,"\n")
  stop()
}

cat("Both files are in chronological order!","\n")

#----------
# Join data together
df_merged <- difference_join(df_pri,
                             df_sec,
                             by=c('DOY'='DOY'),
                             max_dist=max_time_diff,
                             mode='left',
                             distance_col="time_diff")

# The above join function finds all matches within the allowed difference.
# The following piping only leaves the best match in the merged data
df_merged <- df_merged %>%
  group_by(DOY.x) %>%
  slice_min(time_diff)

# Write the new merged data to file
out_file <- paste("output/", output_filename, sep="")
write_tsv(df_merged2, file=out_file)