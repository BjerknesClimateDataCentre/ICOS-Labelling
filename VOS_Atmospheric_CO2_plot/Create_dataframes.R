##############################################################################
# Script to create the dataframes/lists that the plotting routines
# AtmCO2, QualityCheck, External, AtmCO2 map, will use
# Only VOS lines
# RCP Oct 2018
##############################################################################
### Run the script in a directory which contains an input folder and an output
### folder. The Input folder needs to contain raw data file(s) (they need to
### have the same format and standards). Output folder will conatin one plot
### (per file) showing the atmospheric CO2 values over time.

# Clear plots
if(!is.null(dev.list())) dev.off()
# Clean workspace
rm(list=ls())

#-----------------------------------------------------------------------------
# Info to input manually
#-----------------------------------------------------------------------------
if (!exists("input_from_main")) {
  input_from_main <- FALSE
}

if (!input_from_main) {
  # Labeling station info
  date_col <- c(3)
  time_col <- c(4)
  dt_format <- "%d/%m/%y %H:%M:%S"            # e.g. "%d/%m/%y %H:%M:%S"
  CO2_col <- 8
  CO2_col_name <- c("CO2.um.m")
  run_type_col <- 1
  atm_type_name <- c("ATM")
  lat_col <- 24
  lon_col <- 25
  lat_name <- c("SYS.STR.PosLat")
  lon_name <- c("SYS.STR.PosLon")
  # atmospheric CO2 from external station info
  externalstation <- c("psa", "cpt", "hba")
  CO2_ext_name <- "analysis_value"
  # Output Rdata file(s) additional text
  appendtext <- "PS"
  # Range values
  # DO NOT CHANGE these (established by labelling document)
  atm_co2_max <- 500
  atm_co2_min <- 300
  # Define flags for CO2
  #fNAvalues <- c(-65535, 65535)
  fNAvalues <- c()
  fbadvalues <- c(0, 1000) 
  fquestvalues <- c(atm_co2_min, atm_co2_max)
  fmanualvalues <- c()
}

#-----------------------------------------------------------------------------
# Custom functions
#-----------------------------------------------------------------------------
timestamp <- function() {
  as.character(as.POSIXct(date(), tz = "UTC", format = "%a %b %d %H:%M:%S %Y"),
               format = "%Y%m%dT%H%M%S")
}

#-----------------------------------------------------------------------------
# Import ALL labeling station data (df)
#-----------------------------------------------------------------------------

input_dir <- "input"
output_dir <- "output"

# List all files in input directory. SHOULD BE ONLY ONE STATION AT A TIME
input_files <- list.files(input_dir)

# Get the path to file and read the data
in_file <- paste(input_dir, "/", input_files[1], sep = "")
df <-
  read.table(
    in_file,
    header = T,
    sep = "\t",
    strip.white = TRUE,
    fileEncoding = "UTF8",
    na.strings = c("", "NaN"),
    stringsAsFactors = FALSE
  )

# If a number has "too many" decimals, it's read as character (with
# stringsAsFactors FALSE). Loop through the variables to check and convert if
# necessary
required_numeric_vars = c(CO2_col_name, lat_name, lon_name)
for (nv in 1:length(required_numeric_vars)) {
  if (is.character(df[[required_numeric_vars[nv]]])) {
    df[[required_numeric_vars[nv]]] <-
      as.numeric(df[[required_numeric_vars[nv]]])
  }
}

# Identify date and time (The if statement is related to different ways the
# date/time can be reported in the raw file)
if (length(date_col) > 1) {
  date.time <- as.POSIXct(paste(df[, date_col[1]], df[, date_col[2]],
                                df[, date_col[3]], df[, time_col[1]],
                                df[, time_col[2]], df[, time_col[3]]),
                          tz = "UTC",
                          format = dt_format)
} else if (date_col == time_col) {
  date.time <- as.POSIXct(df[, date_col], tz = "UTC", format = dt_format)
} else {
  date.time <- as.POSIXct(paste(df[, date_col], df[, time_col]),
                          tz = "UTC", format = dt_format)
}
# Add the date time column to the data frame
df$date.time <- date.time

#------------------------------------------------------------------------------
# Extract the ATM data (df_sub)
#------------------------------------------------------------------------------
# Create a column which identifies the ATM sequences.
new_col_header <- c("ATM_seq")
df[[new_col_header]] <- NA

count <- 0
for (j in 1:nrow(df)) {
  if (df[j, run_type_col] != atm_type_name) {
    df$ATM_seq[j] <- NA
  } else if (df[j, run_type_col] == df[j - 1, run_type_col]) {
    df$ATM_seq[j] <- count
  } else {
    df$ATM_seq[j] <- count + 1
    count <- count + 1
  }
}

ATM_seq_col <-  ncol(df)

# Extract only the rows with ATM data, and the columns needed for plotting the rest.
cols <-
  c(ncol(df) - 1,
    run_type_col,
    CO2_col,
    lat_col,
    lon_col,
    ATM_seq_col)
df_sub <- df[df[, run_type_col] == atm_type_name, cols]

# Change the col names here (not later) to be able to run only chunks of the code
# below (aka, plots) of latitude, longitude and CO2 before write output (do this
# to make the next codes work)
colnames(df_sub) <-
  c("date.time", "TYPE", "CO2", "latitude", "longitude", "ATM_seq")

# Create the flags for NA, bad, questionable and customized (manual)
# questionable values
df_sub$fNA <- df_sub$CO2 %in% fNAvalues
df_sub$fbad <-
  (df_sub$CO2 < fbadvalues[1] | df_sub$CO2 > fbadvalues[2])
df_sub$fquest <-
  ((df_sub$CO2 > fbadvalues[1]  & df_sub$CO2 < fquestvalues[1]) |
     (df_sub$CO2 > fquestvalues[2] & df_sub$CO2 < fbadvalues[2]))

if (is.numeric(fmanualvalues)) {
  message("This feature hasn't been implemented yet")
}

#------------------------------------------------------------------------------
# Calculate mean and standard deviation for each ATM section (df_mean_std)
# df_mean_std_noNA is df_mean_std calculated ignoring the NA flag values
#------------------------------------------------------------------------------
# Create df_mean_std dataframe 
df_sub2 <- df_sub
df_mean_std <- data.frame(matrix(, max(df_sub2$ATM_seq), 6))
colnames(df_mean_std) <-
  c("date.time",
    "latitude",
    "longitude",
    "ATM_seq",
    "mean",
    "std.dev")

# Using ALL df_sub data (including the numbers that could be considered NA (e.g.
# 999, 65535...))
for (i in 1:max(df_sub2$ATM_seq)) {
  df_dummy <- subset(df_sub2, ATM_seq == i)
  
  df_mean_std$mean[i] <-  mean(df_dummy$CO2, na.rm = TRUE)
  if (is.na(df_mean_std$mean[i])) {
    df_mean_std$std.dev[i] <- NA  } 
  else {
  df_mean_std$std.dev[i] <- sqrt(sum((df_dummy$CO2 - mean(df_dummy$CO2, na.rm = TRUE))^2, na.rm=T)/
        (nrow(df_dummy[!is.nan(df_dummy$CO2)])))
  }
  df_mean_std$date.time[i] <- toString(df_dummy$date.time[1])
  df_mean_std$latitude[i] <- df_dummy$latitude[1]
  df_mean_std$longitude[i] <- df_dummy$longitude[1]
  df_mean_std$ATM_seq[i] <- df_dummy$ATM_seq[1]
  
  rm(df_dummy)
}

df_mean_std$date.time <- as.POSIXct(df_mean_std$date.time, tz = "UTC",
                                    format = "%Y-%m-%d %H:%M:%S")

# Removing the numbers that could be considered NA (e.g.
# 999, 65535...))
df_sub3 <- df_sub
df_sub3$CO2[df_sub3$fNA] <- NA
df_mean_std_noNA <- data.frame(matrix(, max(df_sub3$ATM_seq), 6))

for (i in 1:max(df_sub3$ATM_seq)) {
  df_dummy <- subset(df_sub3, ATM_seq == i)
  
  df_mean_std_noNA$mean[i] <- mean(df_dummy$CO2, na.rm = TRUE)
  if (is.na(df_mean_std_noNA$mean[i])) {
    df_mean_std_noNA$std.dev[i] <- NA  } 
  else {
    df_mean_std_noNA$std.dev[i] <- 
      sqrt(sum((df_dummy$CO2 - mean(df_dummy$CO2, na.rm = TRUE))^2, na.rm=T)/
                                     (nrow(df_dummy[!is.nan(df_dummy$CO2)])))
  } 
  # THIS IS THE SAMPLE STD (divided by n-1): sd(df_dummy$CO2, na.rm = TRUE)
  df_mean_std_noNA$date.time[i] <- toString(df_dummy$date.time[1])
  df_mean_std_noNA$latitude[i] <- df_dummy$latitude[1]
  df_mean_std_noNA$longitude[i] <- df_dummy$longitude[1]
  df_mean_std_noNA$ATM_seq[i] <- df_dummy$ATM_seq[1]
  
  rm(df_dummy)
}

df_mean_std_noNA$date.time <- as.POSIXct(df_mean_std_noNA$date.time, tz = "UTC",
                                         format = "%Y-%m-%d %H:%M:%S")

#------------------------------------------------------------------------------
# Create dataframe of external atm CO2 data (df_external) 
#------------------------------------------------------------------------------
external_dir <- "external_data"

# Import the external data
input_file2 <- list.files(external_dir,
                          pattern = paste(".*txt$", sep = ""))
in_file2 <- paste(external_dir, "/", input_file2, sep = "")

# Use list, that can be nested
df_external <- list()
for (i in 1:length(in_file2)) {

  cin_file2 <- in_file2[i]
  cin_station <- (strsplit(cin_file2,"_"))[[1]][3]
  
  df_external[[cin_station]] <- read.table(
    cin_file2,
    header = T,
    sep = "\t",
    strip.white = TRUE,
    fileEncoding = "UTF8"
  )
  df_external[[cin_station]]$date_external <- as.POSIXct(
    paste(
      df_external[[cin_station]][, 2],
      df_external[[cin_station]][, 3],
      df_external[[cin_station]][, 4],
      df_external[[cin_station]][, 5],
      df_external[[cin_station]][, 6],
      df_external[[cin_station]][, 7]
    ),
    tz = "UTC",
    format = "%Y %m %d %H %M %S"
  )
  
  # If external station does not have data from year when labeling station was
  # measured, "move" the external station years and values (+3/year) to the
  # labeling one
  enddatedif <- max(df_external[[cin_station]]$date_external)-max(df_mean_std$date.time)
  
  if ( enddatedif < 0){
    addyear <- ceiling(abs(as.numeric(enddatedif))/365)
    addCO2 <- addyear * 3
    
    df_external[[cin_station]]$date_external_orig <- df_external[[cin_station]]$date_external
    df_external[[cin_station]]$analysis_value_orig <- df_external[[cin_station]]$analysis_value
    
    df_external[[cin_station]]$date_external <-
      df_external[[cin_station]]$date_external + addyear*365*24*60*60
    df_external[[cin_station]]$analysis_value <-
      df_external[[cin_station]]$analysis_value + addCO2
  }
  
}

#------------------------------------------------------------------------------
# Create dataframe of "cleaned-up" station data + co-located
# external data (df_station)
#------------------------------------------------------------------------------

# Little trick to not copy the whole code twice (w/wo NA) 
# Use a short loop. Implement function in the future??

for (j in c(1,2)) {
  if (j==1) {df_station <- df_mean_std
  } else if (j==2) {df_station <- df_mean_std_noNA}
  
  # Remove lines (atm sequences) with standard value higher than 1
  df_station <- df_station[!(df_station$std.dev > 1), ]
  # Remove lines with standard value = NA
  df_station <- df_station[!(is.na(df_station$std.dev)), ]
  df_station2 <- df_station
  
  #Remove lines with lat and lon =NA
  df_station <- df_station[!(is.na(df_station$longitude)), ]
  df_station <- df_station[!(is.na(df_station$latitude)), ]
  df_station3 <- df_station
  
  # Add external* headers to station data frame
  allextfiles <- strsplit(in_file2,"_")
  allext <- sapply(allextfiles, `[`,3)
  external_col_header <- c(paste(allext,"_date",sep=""), 
                           paste(allext,"_CO2",sep=""))
  for (i in 1:length(external_col_header)) {
    df_station[, external_col_header[i]] <- NA
  }
  
  # Co-locate and add external data to the station data, row by row.
  for (i in 1:length(in_file2)) {
    
    # Set station name  
    cin_file2 <- in_file2[i]
    cin_station <- (strsplit(cin_file2,"_"))[[1]][3]
    
    station_finished <- 0
    external_finished <- 0
    
    current_station_row <- 1
    current_external_row <- 1
    
    
    while (station_finished == 0 && external_finished == 0) {
      found_match <- 0
      current_diff <-
        abs(difftime(df_station$date.time[current_station_row], 
                     df_external[[cin_station]]$date_external[current_external_row], 
                     unit = "secs"))
      
      while (found_match == 0 && external_finished == 0) {
        current_diff <-
          abs(difftime(df_station$date.time[current_station_row], 
                       df_external[[cin_station]]$date_external[current_external_row], 
                       unit = "secs"))
        
        if (current_external_row + 1 <= nrow(df_external[[cin_station]])) {
          next_diff <-
            abs(difftime(
              df_station$date.time[current_station_row],
              df_external[[cin_station]]$date_external[current_external_row + 1],
              unit = "secs"
            ))
          
          if (next_diff <= current_diff) {
            current_diff <- next_diff
            current_external_row <- current_external_row + 1
          } else {
            found_match <- 1
          }
        } else {
          external_finished <- 1
        }
      }
      
      cin_station_date <- paste(cin_station,"_date", sep="")
      df_station[[cin_station_date]][current_station_row] <-
        # Why save dates as strings??
#        toString(df_external[[cin_station]]$date_external_adj[current_external_row])
      df_external[[cin_station]]$date_external[current_external_row]
      # rename external_CO2 field with the name of the station it's compared with
      cin_station_CO2 <- paste(cin_station,"_CO2", sep="")
      df_station[[cin_station_CO2]][current_station_row] <-
        df_external[[cin_station]]$analysis_value[current_external_row]
      
      current_station_row <- current_station_row + 1
      if (current_station_row > nrow(df_station)) {
        station_finished <- 1
      }
    }
    
  }
  
  if (j==1) {df_station_NA <- df_station}
  if (j==2) {df_station_noNA <- df_station}

}

# Rename to keep naming consistency
df_station <- df_station_NA
rm(df_station_NA)

#------------------------------------------------------------------------------
# Cleanup workspace and save df_ variables in Rdata object 
#------------------------------------------------------------------------------

rm(list = c(ls(pattern = "df_sub[0-9]"),  "df_dummy", list = ls(pattern = "df_station[0-9]")))

save(
  list = ls(pattern = "df.*"),
  file = paste("ATM_", appendtext, "_", timestamp(), ".RData", sep =
                 "")
)

# Clear plots
if(!is.null(dev.list())) dev.off()
# Clean workspace
rm(list=ls())