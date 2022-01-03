################################################################################
### JOINING TWO DATASETS
################################################################################

#-------------------------------------------------------------------------------
# INITIAL SETTINGS
#-------------------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Load packages
library(readr)
library(jsonlite)
library(dplyr)
library(fuzzyjoin)

Sys.setlocale("LC_ALL", "English");


#-------------------------------------------------------------------------------
# IMPORT SETTINGS AND DATA
#-------------------------------------------------------------------------------

# Import settings
settings <- read_json(path = "settings.json", format = "json")

# Import primary data (see Readme file for description on what this means in 
# terms of the merging)
if (settings$read_from_data_folder$processed_data){
  df_pri <- readRDS(file = "../data/processed_data.rds")
} else {
  filename_pri <- settings$read_from_input_folder$filename_pri
  df_pri <- read_tsv(paste0("input/",filename_pri))
  # First assume the file is tab separated, but re-import if it is comma.
  if(ncol(df_pri)==1){
    df_pri <- read_csv(paste0("input/",filename_pri))
  }
}

# Import secondary data
if (settings$read_from_data_folder$raw_data){
  print("Cannot read raw data from the data folder, use input folder")
} else {
  filename_sec <- settings$read_from_input_folder$filename_sec
  df_sec <- read_tsv(paste0("input/",filename_sec))
  # First assume the file is tab separated, but re-import if it is comma.
  if(ncol(df_sec)==1){
    df_sec <- read_csv(paste0("input/",filename_sec))
  }
}


#-------------------------------------------------------------------------------
# FUNCTIONS
#-------------------------------------------------------------------------------

# Function that adds a date time column to the data. Required inputs are the
# name of the data, its date and time colnames, and their formats.
assign_datetime <- function(df, date_colname, time_colname, datetime_format){
  df_datetime <- df %>%
    mutate(datetime = case_when(
      # ... the date and time is given in one column
      (date_colname == time_colname) ~
        as.POSIXct(df[[date_colname]], format = datetime_format),
      # ... the date and time is in two columns
      # (have not tested if this code works)
      (length(strsplit(date_colname, ",")) == 1 & 
         length(strsplit(time_colname, ",")) == 1) ~
        as.POSIXct(paste(df[[date_colname]], df[[time_colname]]),
                   format = datetime_format),
      # ... the year, month, day, hour, minute and seconds are in separate cols
      # (have not tested if this code works)
      (length(strsplit(date_colname, ",")) != 1) ~
        as.POSIXct(paste(
          strsplit(date_colname,',')[1], strsplit(date_colname,',')[2],
          strsplit(date_colname,',')[3], strsplit(time_colname,',')[1],
          strsplit(time_colname,',')[2], strsplit(time_colname,',')[3]),
          format = datetime_format))
    )
  return(df_datetime)
}


#-------------------------------------------------------------------------------
# IDENTIFY DATE AND TIMES
#-------------------------------------------------------------------------------

# Extract column names and formats related to date and time from the settings
for (column_key in names(settings$datetime_settings)) {
  assign(column_key, settings$datetime_settings[[column_key]])
}

# Assign a datetime column to the datasets
df_pri_datetime <- assign_datetime(df_pri,date_colname_pri, time_colname_pri,
                                   datetime_format_pri)
df_sec_datetime <- assign_datetime(df_sec,date_colname_sec,time_colname_sec,
                                   datetime_format_sec)


#-------------------------------------------------------------------------------
# CHRONOLOGY CHECK
#-------------------------------------------------------------------------------

# THIS DOES NOT WORK!!!

chronology_check_pri <- df_pri_datetime %>%
  rowwise() %>%
  mutate(check = all(diff(c_across(contains('datetime'))) > 0)) %>%
  select(check)

# Get values of non-chronology rows
which(!chronology_check_pri$check)

# Create function which stops script if the date time are not chronological
chron_check <- function(DOY_col, filetype) {
  row_not_chron <- which(diff(df_sec$datetime) < 0)
  if (length(row_not_chron) != 0) {
    stop("Error: These row(s) from ", as.character(filetype),
                " are not in chronologic order:","\n",
                list(row_not_chron),"\n")
  }
}

# Run check function on both datasets
chron_check(df_pri_datetime$datetime, 'Primary')
chron_check(df_sec_datetime$datetime, 'Secondary')


#-------------------------------------------------------------------------------
# CREATE JOINED DATA
#-------------------------------------------------------------------------------

# Convert the unit of the maximum allowed time difference from minutes to days
# to match the unit of the DOY columns
max_timediff <- (max_timediff_minutes/60)/24

# Join data together into new tibble
df_merged_full <- difference_join(df_pri,
                             df_sec,
                             by=c('DOY'='DOY'),
                             max_dist=max_timediff,
                             mode='left',
                             distance_col="timediff")

# The above joining keeps all matches within the allowed difference. The 
# following piping only keps the best match if there are duplicates.
df_merged <- df_merged_full %>%
  group_by(DOY.x) %>%
  slice_min(timediff)


#-------------------------------------------------------------------------------
# WRITE OUTPUT
#-------------------------------------------------------------------------------

# Write the new data to file
out_file <- paste("output/", output_filename, sep="")
write_tsv(df_merged, file=out_file)