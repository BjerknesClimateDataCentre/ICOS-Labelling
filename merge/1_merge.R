################################################################################
### JOINING TWO DATASETS
################################################################################

### Description:
# This script joins two datasets together by close enough matched time stamps.
# The allowed time difference is set in the input parameters.
# 
# The joining is a left-join, meaning the merged dataset contains all rows from 
# the primary dataset (typically the CO2 dataset), and matches from the 
# 'secondary' dataset (typically the hydrography data) are only included if rows
# are withing the allowed time difference.

### Requirements:
# - The two input datasets must be stored in the input folder located at the 
# same directory as this script.

### Output:
# - The merged data will be located in the output folder 


#-------------------------------------------------------------------------------
# INPUT PARAMETERS
#-------------------------------------------------------------------------------

# Filenames
filename_pri <- "34FM20200815-ICOS OTC Labelling_losGatos_wDOY_2020.txt"
filename_sec <- "mea_all_stdval_wDOY_2020.txt"

# The files must contain date and time in Day-of-year (DOY) format (if they do
# not, use the 'convert_to_doy' script). Specify which column contains the DOY.
doy_col_pri <- c(1)
doy_col_sec <- c(1)

# Maximum allowed time difference in minutes
max_timediff_minutes <- 1

# The output filename
output_filename <- "merged.txt"


#-------------------------------------------------------------------------------
# INITIAL SETTINGS
#-------------------------------------------------------------------------------

library(readr)
library(dplyr)
library(fuzzyjoin)

Sys.setlocale("LC_ALL", "English");


#-------------------------------------------------------------------------------
# IMPORT DATA 
#-------------------------------------------------------------------------------

filepath_pri <- paste("input/", filename_pri, sep="")
filepath_sec <- paste("input/", filename_sec, sep="")

df_pri <- read_tsv(filepath_pri)
df_sec <- read_tsv(filepath_sec)


#-------------------------------------------------------------------------------
# CHRONOLOGY CHECK
#-------------------------------------------------------------------------------

# Create function which stops script if the date time are not chronological
chron_check <- function(DOY_col, filetype) {
  row_not_chron <- which(diff(DOY_col) < 0)
  if (length(row_not_chron) != 0) {
    stop("Error: These row(s) from ", as.character(filetype),
                " are not in chronologic order:","\n",
                list(row_not_chron),"\n")
  }
}

# Run check function on both datasets
chron_check(df_pri$DOY, 'Primary')
chron_check(df_sec$DOY, 'Secondary')


#-------------------------------------------------------------------------------
# CREATE JOINED DATA
#-------------------------------------------------------------------------------

# Convert the unit of the maximum allowed time difference from minutes to days
# to match the unit of the DOY columns
max_timediff <- (max_timediff_minutes/60)/24

# Join data together into new tibble
df_merged <- difference_join(df_pri,
                             df_sec,
                             by=c('DOY'='DOY'),
                             max_dist=max_timediff,
                             mode='left',
                             distance_col="timediff")

# The above joining keeps all matches within the allowed difference. The 
# following piping only keps the best match if there are duplicates.
df_merged <- df_merged %>%
  group_by(DOY.x) %>%
  slice_min(timediff)


#-------------------------------------------------------------------------------
# WRITE OUTPUT
#-------------------------------------------------------------------------------

# Write the new data to file
out_file <- paste("output/", output_filename, sep="")
write_tsv(df_merged, file=out_file)