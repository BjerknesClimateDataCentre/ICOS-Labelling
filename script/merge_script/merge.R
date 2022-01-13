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

# Remove existing files in the output directory
if (!is.null(list.files("output"))) {
  file.remove(dir(paste0(getwd(), "/output"), pattern = "", full.names = TRUE))
}


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
  if(ncol(df_pri) == 1){
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
  if(ncol(df_sec) == 1){
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
    mutate(date__time = case_when(
      # ... the date and time is given in one column:
      (date_colname == time_colname) ~
        as.POSIXct(df[[date_colname]], format = datetime_format, tz = "UTC"),
      # ... the date and time is in two columns:
      (length(strsplit(date_colname, ",")) == 1 & 
         length(strsplit(time_colname, ",")) == 1) ~
        as.POSIXct(paste(df[[date_colname]], df[[time_colname]]),
                   format = datetime_format, tz = "UTC"),
      # ... the year, month, day, hour, minute and seconds are in separate cols:
      # (have not tested if this bit works)
      (length(strsplit(date_colname, ",")) != 1) ~
        as.POSIXct(paste(
          strsplit(date_colname,',')[1], strsplit(date_colname,',')[2],
          strsplit(date_colname,',')[3], strsplit(time_colname,',')[1],
          strsplit(time_colname,',')[2], strsplit(time_colname,',')[3]),
          format = datetime_format, tz = "UTC"))
    )
  return(df_datetime)
}

# Function checks if a data tibble has chronological datetime. If not, it stops 
# the script and prints the non-chronological row numbers.
check_chron <- function(df) {
  not_chron <- df %>%
    mutate(diff = c(1,diff(date__time))) %>%
    mutate(row_nr = row_number()) %>%
    select(row_nr, diff) %>%
    filter(diff < 0)
  
  if (nrow(not_chron) > 0) {
    stop("'", deparse(substitute(df)), "'",
         " not chornological at row(s): ", "\n", 
         list(not_chron$row_nr))
  } else {
    paste0("'", deparse(substitute(df)), "' is chornological.")
  }
}


#-------------------------------------------------------------------------------
# IDENTIFY DATETIME AND RUN CHRONOLOGY CHECK
#-------------------------------------------------------------------------------

# Extract column names and formats related to date and time from the settings
for (column_key in names(settings$datetime_settings)) {
  assign(column_key, settings$datetime_settings[[column_key]])
}

# Assign a datetime column to the datasets
df_pri_datetime <- assign_datetime(df_pri, date_colname_pri, time_colname_pri,
                                   datetime_format_pri)
df_sec_datetime <- assign_datetime(df_sec, date_colname_sec, time_colname_sec,
                                   datetime_format_sec)

# Check if chronological
check_chron(df_pri_datetime)
check_chron(df_sec_datetime)


#-------------------------------------------------------------------------------
# CREATE DAILY MERGED DATA AND WRITE OUTPUT FILES 
#-------------------------------------------------------------------------------

# The merge function used can get errors if the data tibbles are too big. 
# Therefore: split the data into daily data tibbles, run the merging, and store
# the daily merged files to the output folder. These can be concatenated later
# in a different software (like PanTool).

# Store the unique days in the pri data tibble
distinct_days <- unique(format(df_pri_datetime$date__time, format='%Y-%m-%d'))

# Loop through the unique days and create merged output files
for (day in distinct_days){
  
  # Filter the pri and sec data tibble on the day
  df_pri_day <- df_pri_datetime %>%
    filter(format(date__time, format='%Y-%m-%d') == day)
  df_sec_day <- df_sec_datetime %>%
    filter(format(date__time, format='%Y-%m-%d') == day)
  
  # Join data together into new tibble
  df_merged_full <- difference_join(
                      df_pri_day,
                      df_sec_day,
                      by = c('date__time' = 'date__time'),
                      max_dist = (as.numeric(settings$max_timediff_minutes)*60),
                      mode = 'left',
                      distance_col = "timediff")
  
  # The above joining keeps all matches within the allowed difference. The 
  # following piping only keeps the best match.
  df_merged <- df_merged_full %>%
    group_by(date__time.x) %>%
    slice_min(timediff)
  
  # Write the merged data to file
  out_file <- paste("output/merged_", day ,".txt")
  write_tsv(df_merged, file = out_file)
}