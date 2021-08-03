#####################################################################################################
### Converts raw FOS data from E2M3A (from sensor pCO2-3) to human readable format                ### 
#####################################################################################################

# This script imports the raw data (by reading one line at the time in for/while loop) and store
# the data in the imported data frame template. Once this is complete we got all the raw data in 
# a more sensible format. The next step is then to choose (as input parameters) which parameters
# we want as output, and afterwards, reformat the data so that it fits into QuinCe.

#-----
# About E2M3A IDs (mail Stefano 2018-05-15):
#$SBE37PO: Sea-Bird SBE 37-SMP-ODO CTD @ 2m
#$PCO2-2: Pro Oceanus CO2-Pro @2m (S/N 29-103-45 )
#$PCO2-3: Pro Oceanus CO2-Pro @2m (S/N 34-215-45 )

#During the maintenance of the E2M3A buoy we switch the Pro Oceanus, and sometimes also the 
# SBE 37-SMP-ODO CTD with some other one.

##-----------------------------------------------------------------------------
##-----------------------------------------------------------------------------

# Set working directory to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Input params to be assigned:

# Which ID_name (look in json file) is used for the parameters we wish to extract
lat_col <- "gprmc_latitude"
lon_col <- "gprmc_longitude"
sst_col <- "sbe37po_temp"
press_col <- "sbe37po_press"
cond_col <- "sbe37po_cond"
co2_col <- "pco2_2_CO2"


other <- c(oxy="sbe37po_oxy")         # Make this "other-varaible" to allow for easy addition of other IDs
                                      # in the future 

              
zeroing_col <- "pco2_2_instrumentZero"


# What is the limiting parameter (lowest frequency)
trigger <- co2_col

# Make variable with all params (including position) in the prefered order
all_params <- c(lat_col, lon_col, sst_col, press_col, cond_col, co2_col, zeroing_col, other[[1]])   # Replaced "order"

# What is the maximum allowed time offset between measurements?
max_time_ofset <- 2700 # in seconds

# Remove rows where certain parameters are NA
remove_if_na <- c(sst_col,co2_col, "latitude", "longitude")           




##-----------------------------------------------------------------------------
##-----------------------------------------------------------------------------
## CREATE THE FUNCTIONS WE NEED 

# This function extracts data and dates from the "from_df" column to the "to_array" column. 
extract_data <- function(from_df, to_array){
  
  # Loop through the data columns in current row
  for (i in 2:ncol(from_df)){
    if (!is.na(from_df[[i]])) {
      
      # Copy data from the "current_row" to the "to_print" array 
      to_array[i] <- as.character(from_df[[i]])
      
      # Copy the datetime from the "current_row" to the "to_print" array
      datetime_col <- length(from_df) + i - 1
      to_array[datetime_col] <- as.character(from_df[[1]])
    
    }
  }

  return(to_array)
}

##---------
# Function to convert datetime format

#iso_dateTime_func <- function(old_date, date_format, old_time, time_format){

# Reformat date


# Reformat time
#  if(time_format == "hhmmss"){

#    iso_time <- 
#  }

#  iso_dateTime <- paste(iso_date, "T", iso_time, sep="")
#  return(iso_dateTime)
#}


##---------
# Function to convert position format


##-----------------------------------------------------------------------------
# SET SOME SETTINGS AND IMPORT THE DATA AND TEMPLATE

Sys.setlocale("LC_ALL", "English"); 

# Install packages
library(rjson)

input_dir<- "input"
output_dir<-"output"
tag_dir <- "tag_details"
function_dir <- "functions"

# List all raw data files in input directory
input_files <- list.files(input_dir)
file_loop <- 1
#for (file_loop in 1:length(input_files)) {
in_file <- paste(input_dir, "/", input_files[file_loop], sep="")     # When turn on loop, change 1 to "file_loop"!


# Import tag details for MIRAMARE station
tags_file <- fromJSON(paste(readLines("tag_details/Tag_details_E2M3A_pCO2-2.json"),collapse=""))

# Import functions file... (not created yet)

##-----------------------------------------------------------------------------
# REFORMAT THE RAW DATA TO "NORMAL" FORMAT 
# (without removing any information)

# Open the raw file and save all lines in variable "linn"
conn <- file(in_file,open="r")
linn <-readLines(conn)

# Create empty array to be filled with output data (as long as the columns possible from this station)
# to_output <- rep(NA,length(tags_file[[1]]))

# Loop through the raw data lines and store values in the data frame template
data_count <- 1
data_finished <- 0

# Create output file
output_df_all <- paste("output/1.all_data_",input_files[file_loop], sep="")
sink(output_df_all)

# Print header to output file
cat(paste(as.character(tags_file[[1]]), collapse="\t"))
cat("\n")

while (data_finished==0) {
  # Save current data line
  string <- linn[data_count]
  string_split <- strsplit(string,",")
  
  # Create output line
  to_output <- rep(NA,length(tags_file[[1]]))
  
  # Find the position in the json file where the current tag is described
  line_tag <- string_split[[1]][1]
  if (is.na(line_tag)){
    tag_number <- NA
  } else {
    for (i in 1:length(tags_file[[2]])) {
      if(line_tag == tags_file[[2]][[i]][1]) {
        tag_number <- i
        break
      } else {
        tag_number <- NA
      }
    }
  }  
   
  # If tag_number is found, and if number of filds match... 
  if (!is.na(tag_number)) {
    if (length(string_split[[1]]) == tags_file[[2]][[tag_number]]$number_of_fields) {
    # ... then save the copy-to-and-from as vectors,... 
    copy_col_from <- tags_file[[2]][[tag_number]]$copy_col_from
    copy_col_to <- tags_file[[2]][[tag_number]]$copy_col_to
    
    # ... add data to the output array,...
    for (i in 1:length(copy_col_from)) {
      to_output[copy_col_to[i]] <- string_split[[1]][copy_col_from[i]]
    }   
    
    #... and write output to file.
    line_to_print <- paste(to_output, collapse="\t")
    cat(line_to_print, "\n")
    }
  }
    
  # Increase data counter
  data_count <- data_count + 1
  
  # Check if reached end of raw data file
  if (data_count >= length(linn)) {
    data_finished <- 1
  }

}

close(conn)
sink()


#------------------------------------------------------------------------------------
## REFORMAT DATETIME AND POSITION

# Import the new txt file in the output folder
df <- read.table(output_df_all, header=TRUE, sep="\t")





# Create proper date/time cols (combine info from different cols and reformat ISO)
df$datetime <- rep(0,nrow(df))
for (i in 1:nrow(df)){
  
  # When date is in the "date" column...
  if (!is.na(df$date[i])) {
    
    # Extract date including leading zeros
    date <- sprintf("%06d", df$date[i])     # format is mmddyy
    # Rearange date format
    iso_date <- gsub('([0-9][0-9])([0-9][0-9])([0-9][0-9])','20\\3-\\1-\\2', date)
    time <- sprintf("%06d", df$time[i])
    iso_time <- gsub('([0-9][0-9])([0-9][0-9])([0-9][0-9])','\\1:\\2:\\3',time)
    
    # Combine date and time
    df$datetime[i] <- paste(iso_date, "T", iso_time, sep="")                 
  
  # When date is in the "gprmc" column...
  } else {
    
    date <- sprintf("%06d", df$gprmc_date[i])      # format is ddmmyy
    iso_date <- gsub('([0-9][0-9])([0-9][0-9])([0-9][0-9])','20\\3-\\2-\\1', date)
    time_to_int <- as.integer(df$gprmc_time[i])
    time <- sprintf("%06d", time_to_int)
    iso_time <- gsub('([0-9][0-9])([0-9][0-9])([0-9][0-9])','\\1:\\2:\\3',time)
    
    df$datetime[i] <- paste(iso_date, "T", iso_time, sep="")         

  }
}


# Create proper position (convert from degrees and decimal minutes)

df$latitude <- rep(NA,nrow(df))

for (j in 1:nrow(df)){
  # Select only rows with latitude
  if(!is.na(df$gprmc_lat[j])){
    old_lat <- df$gprmc_lat[j]       # Foramt degreeMinutes.desimalMinutes
    # Extract the degree part of string
    degrees <- as.numeric(substr(old_lat, start = 1, stop = 2))
    # Extract the minutes part of string
    minutes <- as.numeric(substr(old_lat, start = 3, stop = nchar(old_lat)))
    # Convert minutes to desimal degrees
    degree_desimal <- minutes/60
    # Add degree and desimal degree
    df$latitude[j] <- as.numeric(sprintf("%.6f", degrees + degree_desimal))
    
    # Change sign if the latitude is in the south
    if(df$gprmc_latDir[j]=="S") {
      df$latitude[j] <- df$latitude[j]*-1
    }
  }
}

# Same procedure for longitude
df$longitude <- rep(NA,nrow(df))

for (k in 1:nrow(df)){
  if(!is.na(df$gprmc_lon[k])){
    old_lon <- df$gprmc_lon[k]
    degrees <- as.numeric(substr(old_lon, start = 1, stop = 2))
    minutes <- as.numeric(substr(old_lon, start = 3, stop = nchar(old_lon)))
    degree_desimal <- minutes/60
    df$longitude[k] <- as.numeric(sprintf("%.6f", degrees + degree_desimal))
    if(df$gprmc_lonDir[k]=="W") {
      df$longitude[k] <- df$longitude[k]*-1
    }
  }
}


#------------------------------------------------------------------------------------
## EXTRACT THE COLUMNS WE WANT:

# Create new data frame to be filled with the wanted columns
new_df <- data.frame(df$datetime, df$latitude, df$longitude)
colnames(new_df)<-c("datetime", "latitude", "longitude")

# add the wanted columns to the empty data frame (start from 3 to avoid the latitude and longitude)
for (l in all_params[3:length(all_params)]) {
  new_df[[l]] <- df[[l]]
}

# Since many columns are removed there is likely rows containing just NAs- Remove these
na_vector <- rep(0,nrow(new_df))
for (m in 1:nrow(new_df)) {
    # This gives 1 for rows without any data
    if (!all(is.na(new_df[m,(2:ncol(new_df))]))) {
      na_vector[m] <- 1
    }  
}
# Gives False for rows without data
na_logical <- na_vector == 1

new_df<- new_df[na_logical,]



output_new_df <- paste("output/2.chosen_cols_with_zeroing_",input_files[file_loop], sep="")
write.table(new_df, file = output_new_df, sep ="\t", row.names=FALSE)

#----
# Remove CO2 rows which are to low because of zeroing (the rows where the CO2 instrumentZero changes)
new_df$delete_col <- rep(0,nrow(new_df))

zero_timestamp <- c(0)
count <- 2

for (h in 1:nrow(new_df)){
  
  if (!is.na(new_df$pco2_2_instrumentZero[h])){
    zero_timestamp[count] <- new_df$pco2_2_instrumentZero[h]
    zero_timstamp_diff <- zero_timestamp[count] - zero_timestamp[count-1]
    if (zero_timstamp_diff != 0) {
      new_df$delete_col[h] <- 1
    }
    count <- count + 1
  }
  
}

# REmove the rows with zeroing co2
new_df <- subset(new_df, new_df$delete_col==0)
# Remove the dummy columns
new_df <- new_df[,1:(ncol(new_df)-1)]


output_new_df <- paste("output/2.5.chosen_cols_",input_files[file_loop], sep="")
write.table(new_df, file = output_new_df, sep ="\t", row.names=FALSE)



#------------------------------------------------------------------------------------
# CONVERT TO FORMAT ACCEPTED BY QUINCE
# Need to move measurements from different sensors (with different IDs) but close enough
# time stamp to the same row.

# Will go through new_df row by row and look the trigger parameter (the paramter starting a measurement sequence),
# and combine this with the following parameters which are close enough in time.

# Create a new data frame which will be the QuinCe ready version of the data
# The quince_df will have the same cols as the new_df + date cols for each parameter + 2 date cols for lat and long

# Create the colnames for the new data frame
colnames_quince_df <- c(colnames(new_df), paste("datetime_", colnames(new_df)[2:ncol(new_df)], sep=""))

# Tell R that datetime column in new_df is a date
date.time <- as.POSIXct(new_df[,1], tz="UTC", format="%Y-%m-%dT%H:%M:%S")
new_df[,1] <- date.time

# Create output file with the quince friendly data frame
output_quince_prelim <- paste("output/3.quince_prelim_", input_files[file_loop], sep="")
sink(output_quince_prelim)


# Print header to output file
cat(paste(as.character(colnames_quince_df), collapse="\t"))
cat("\n")

# Assign some start values before loop through the lines in new_df
trigger_col <- which(colnames(new_df)==trigger)
data_finished <- 0
row_count <- 1
to_print <- rep(NA,length(colnames_quince_df))
state <- "look_for_trigger"


while (data_finished == 0) {
  current_row <- new_df[row_count,]
  
  ##-----------------------------
  ## STATE IS "LOOK FOR TRIGGER"
  if (state == "look_for_trigger") {
    
    if (!is.na(current_row[trigger_col])) { 
      # Function to extract trigger and other data on current row to array
      to_print <- extract_data(current_row,to_print)
      # Save the date.time when this measurement cycle starts
      measure_cycle_start <- as.POSIXct(current_row[[1]], tz="UTC", format="%Y-%m-%d %H:%M:%S")
      state <- "find_other_fields"
    }
    
    
    ##-----------------------------
    ## STATE IS "FIND OTHER FIELDS"
  } else if (state == "find_other_fields") {
    
    measure_cycle_datetime <- as.POSIXct(current_row[[1]], tz="UTC", format="%Y-%m-%d %H:%M:%S")
    time_diff <- abs(difftime(measure_cycle_datetime, measure_cycle_start, units="s"))
    
    
    # If a new trigger is reached: write data, clear array and extract data from trigger row
    if (!is.na(current_row[trigger_col])) {
      cat(to_print, sep="\t")
      cat("\n")
      to_print <- rep(NA,length(colnames_quince_df))
      
      to_print <- extract_data(current_row, to_print)
      measure_cycle_start <- as.POSIXct(current_row[[1]], tz="UTC", format="%Y-%m-%d %H:%M:%S")
      
      # Else if timegap is to large: write data, clear array, and set back state
    } else if (time_diff > max_time_ofset) {
      cat(to_print, sep="\t")
      cat("\n")
      to_print <- rep(NA,length(colnames_quince_df))
      state <- "look_for_trigger"
      
      # Or if non of the above: extract data from current row into array
    } else {
      # Function to extract all data from row into array
      to_print <- extract_data(current_row, to_print) #cat("Extract all data on current row. ")
    }
    
  }
  
  
  row_count <- row_count + 1
  if (row_count > nrow(new_df)) {
    data_finished <- 1
  }
  
}
# Print last row
cat(to_print, sep="\t")
cat("\n")
sink()




#-----------------------------------------------------------------------------------
# Import the quince_df and do minor adjustments

# Import the new txt file in the output folder
quince_df <- read.table(output_quince_prelim, header=TRUE, sep="\t")

# Make a copy
quince_df_nans <- quince_df

# Remove rows where certain parameters are NA
for (q in remove_if_na) {
  if(q!=trigger) {
    keep_row <- which(!is.na(quince_df[[q]]))
    quince_df <- quince_df[keep_row,]
  }
}

# Copy the co2 date time to first row and assign datetime class
quince_df$datetime <- as.POSIXct(quince_df[[paste("datetime_", co2_col, sep="")]],tz="UTC", format="%Y-%m-%d %H:%M:%S")

# Make a copy of quince_df to be used for datetime diagnostics further down
quince_df_diag <- quince_df

# Remove the other datetimes
quince_df <- quince_df[,1:(ncol(quince_df)-length(all_params))]

output_quince <- paste("output/QuinCeReady_",input_files[file_loop], sep="")
write.table(quince_df, file = output_quince, sep ="\t", row.names=FALSE)

write.table(quince_df_diag, file = "output/diag_data.txt", sep ="\t", row.names=FALSE)

#-----------------------------------------------------------------------------------
# DATETIME DIAGNOSTIC
# Plot the dates for the different parameters and position

time_diag_image_name <- paste("output/4.timediff_plot",input_files[file_loop], "png", sep=".")

png(filename=time_diag_image_name)

# Want to have all diff plots in one image. 
par(mfrow=c(length(all_params)/2,2))
    
# Add columns with date time difference between parameters
# Loop through all date time columns
for (o in all_params[3:length(all_params)]) {
  column_name <- paste("datetime_",o,sep="")
  new_column_name <- paste(column_name, "_timediff",sep="")
  
  # Loop through all rows
  for (p in 1:nrow(quince_df_diag)) {
    # Make the datetime column a datetime class
    quince_df_diag[[column_name]] <- as.POSIXct(quince_df_diag[[column_name]],tz="UTC", format="%Y-%m-%d %H:%M:%S")
    # Calculate the time difference in minutes
    time_diff <- difftime(quince_df_diag$datetime[p], quince_df_diag[[column_name]][p], units="mins")
    # Create new column with th
    quince_df_diag[[new_column_name]][p] <- time_diff 
  }
  
  # Make subset without Nan before can plotting
  p_is_not_na <- which(!is.na(quince_df_diag[[new_column_name]]))
  quince_df_diag_sub <- quince_df_diag[p_is_not_na,]
  
  ylabel <- paste(o, " time differenece [minutes]", sep="")
  # Only plot if there is data in the new data frame
  if(length(quince_df_diag_sub[,1]) > 0) {
    plot (quince_df_diag_sub$datetime, quince_df_diag_sub[[new_column_name]], xlab="datetime (for co2)", ylab = ylabel)
  }
}


dev.off()



#-----------------------------------------------------------------------------------
# COMPARE WITH PROCESSED DATA RECEIVED
# Plot the CO2 I "found" vs final CO2 from the PI

##----------
#PI_date_format <- "%d/%m/%Y %H:%M"
#PI_date_col_name <- "datetime"
#PI_co2_col_name <- "CO2_uATM"
#
##---------
#
# Import PI processed data
#PI_data_dir <- "PI_processed"
#PI_file <- list.files(PI_data_dir)
#PI_file_path <- paste(PI_data_dir, "/", PI_file, sep="")
#PI_df <- read.table(PI_file_path,header=TRUE, sep="\t")
#
#date_time <- as.POSIXct(PI_df[[PI_date_col_name]],tz="UTC", format=PI_date_format)
#PI_df[[PI_date_col_name]] <- date_time#
#
#co2_compare_image_name <- paste("output/5.co2_compare_plot", input_files[1], "png", sep=".")
#png(co2_compare_image_name, width=1000)
#  
#plot(PI_df[[PI_date_col_name]], PI_df[[PI_co2_col_name]], ylab="CO2", xlab="Time", ylim = c(200,600))
#points(quince_df$datetime, quince_df[[co2_col]], col="red")
#legend(x="bottomleft", legend=c("PI", "us"), pch=c("o","o"), col=c("black", "red"))
#
#dev.off()



#------------------------------------------------------------------------------------
#}   # End for loop
