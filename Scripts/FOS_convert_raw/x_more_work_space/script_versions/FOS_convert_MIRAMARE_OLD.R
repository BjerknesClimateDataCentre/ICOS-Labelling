#####################################################################
### Converts raw FOS data to human readable format                ### 
#####################################################################

# This script imports the raw data (by reading one line at the time in for/while loop) and store
# the data in the imported data frame template. Once this is complete we got all the raw data in 
# a more sensible format. The next step is then to choose (as input parameters) which parameters
# we want as output, and afterwards, reformat the data so that it fits into QuinCe.

#-----
# About Miramare (MAMBO) IDs (mail Stefano 2018-05-15):

#$SBE16PLS:  Sea-Bird SBE 16Plus @ 10m
#$SBE37O:  Sea-Bird SBE 37-SMP-ODO CT @ 2m  (no pressure sensor)
#$PCO2CV: Pro-Oceanus CO2-Pro CV @ 2m
#$MRDMT: don't care, sensors not installed


##-----------------------------------------------------------------------------
##-----------------------------------------------------------------------------
# Input params to be assigned:

# Which ID_name (look in template) is used for the parameters we wish to extract
lat <- "gprmc_latitude"
lon <- "gprmc_longitude"
sst <- "sbe37o_temp"
sal <- "sbe16pls_salinity"
co2 <- "pco2cv_CO2"

other <- c(oxy="sbe37o_oxy")

# What is the limiting parameter (lowest frequency)
limiting_param <- sst

# What is the order of measurements. Remember to include position (has to be string). It is sufficient
# to meantion from each ID one).
order <- c(sst, other[[1]], lat, lon, co2, sal)

# What is the maximum allowed time offset between measurements?
max_time_ofset <- 2700 # in seconds

# Remove rows where certain parameters are NA
remove_if_na <- c(sst,co2)           # not nessesary to include lat and lon


##-----------------------------------------------------------------------------
##-----------------------------------------------------------------------------
# SET SOME SETTINGS AND IMPORT THE DATA AND TEMPLATE

Sys.setlocale("LC_ALL", "English"); 

input_dir<- "input"
output_dir<-"output"
template_dir <- "template"

# List all raw data files in input directory
input_files <- list.files(input_dir)
#file_loop <- 1
for (file_loop in 1:length(input_files)) {
in_file <- paste(input_dir, "/", input_files[file_loop], sep="")     # When turn on loop, change 1 to "file_loop"!

# Import data frame template for MIRAMARE station
df <- read.table("templates/Miramare_template.txt",header=T, fileEncoding="UTF8")


##-----------------------------------------------------------------------------
# REFORMAT THE RAW DATA TO "NORMAL" FORMAT 
# (without removing any information)

# Open the raw file and save all lines in variable "linn"
conn <- file(in_file,open="r")
linn <-readLines(conn)

# Loop through the raw data lines and store values in the data frame template
df_count <- 1
data_count <- 1
data_finished <- 0

# Create output file
output_df_all <- paste("output/1.all_data_",input_files[file_loop], sep="")
sink(output_df_all)

# Print header to output file
cat(paste(as.character(colnames(df)), collapse="\t"))
cat("\n")

while (data_finished==0) {
  string <- linn[data_count]
  string_split <- strsplit(string,",")
  df[1,]<-NA
  
  if (grepl("GPRMC", string)) {
     if(length(string_split[[1]])==13) {
     df$gprmc_date[df_count] <- sprintf("%06d",as.numeric(string_split[[1]][10]))
     df$gprmc_time[df_count] <- sprintf("%06d",floor(as.numeric(string_split[[1]][2])))
     df$gprmc_positionStatus[df_count] <- string_split[[1]][3]
     df$gprmc_lat[df_count] <- string_split[[1]][4]
     df$gprmc_latDir[df_count] <- string_split[[1]][5]
     df$gprmc_lon[df_count] <- string_split[[1]][6]
     df$gprmc_lonDir[df_count] <- string_split[[1]][7]
     df$gprmc_speed[df_count] <- string_split[[1]][8]
     df$gprmc_heading[df_count] <- string_split[[1]][9]   
     }
  }

  if (grepl("METEO", string)) {
    if(length(string_split[[1]])==15) {
    df$timestamp[df_count] <- string_split[[1]][2]
    df$date[df_count] <- sprintf("%06d",as.numeric(string_split[[1]][3]))
    df$time[df_count] <- string_split[[1]][4]
    df$meteo_windDir[df_count] <- string_split[[1]][5]
    df$meteo_windSpeed[df_count] <- string_split[[1]][6]
    df$meteo_temp[df_count] <- string_split[[1]][7]
    df$meteo_press[df_count] <- string_split[[1]][8]
    df$meteo_hum[df_count] <- string_split[[1]][9]
    df$meteo_compass[df_count] <- string_split[[1]][10]
    df$meteo_windSpeed2[df_count] <- string_split[[1]][11]
    df$meteo_windSpeedGust[df_count] <- string_split[[1]][12]
    df$meteo_windDirGust[df_count] <- string_split[[1]][13]
    df$meteo_numOfSamp[df_count] <- string_split[[1]][14]   
    }
  }
  
  if (grepl("SBE16PLS", string)) {
    if(length(string_split[[1]])==14) {
    df$timestamp[df_count] <- string_split[[1]][2]
    df$date[df_count] <- sprintf("%06d",as.numeric(string_split[[1]][3]))
    df$time[df_count] <- string_split[[1]][4]
    df$sbe16pls_temp[df_count] <- string_split[[1]][5]
    df$sbe16pls_cond[df_count] <- string_split[[1]][6]
    df$sbe16pls_press[df_count] <- string_split[[1]][7]
    df$sbe16pls_oxy[df_count] <- string_split[[1]][8]
    df$sbe16pls_pH[df_count] <- string_split[[1]][9]
    df$sbe16pls_chlorophyll[df_count] <- string_split[[1]][10]
    df$sbe16pls_turbidity[df_count] <- string_split[[1]][11]
    df$sbe16pls_irradiance[df_count] <- string_split[[1]][12]
    df$sbe16pls_salinity[df_count] <- string_split[[1]][13]
    df$sbe16pls_instrumentDateTime[df_count] <- string_split[[1]][14]   
  }
  }
  
  if (grepl("SBE37O", string)) {
    if(length(string_split[[1]])==9) {
    df$timestamp[df_count] <- string_split[[1]][2]
    df$date[df_count] <- sprintf("%06d",as.numeric(string_split[[1]][3]))
    df$time[df_count] <- string_split[[1]][4]
    df$sbe37o_temp[df_count] <- string_split[[1]][5]
    df$sbe37o_cond[df_count] <- string_split[[1]][6]
    df$sbe37o_oxy[df_count] <- string_split[[1]][7]
    df$sbe37o_instrumentDate[df_count] <- string_split[[1]][8]
    df$sbe37o_instrumentTime[df_count] <- string_split[[1]][9]   
  }
  }

  if (grepl("SBE37PO", string)) {
    df$timestamp[df_count] <- string_split[[1]][2]
    df$date[df_count] <- sprintf("%06d",as.numeric(string_split[[1]][3]))
    df$time[df_count] <- string_split[[1]][4]
    df$sbe37po_temp[df_count] <- string_split[[1]][5]
    df$sbe37po_cond[df_count] <- string_split[[1]][6]
    df$sbe37po_press[df_count] <- string_split[[1]][7]
    df$sbe37po_oxy[df_count] <- string_split[[1]][8]
    df$sbe37po_instrumentDate[df_count] <- string_split[[1]][9]
    df$sbe37po_instrumentTime[df_count] <- string_split[[1]][10]
  }
  
  if (grepl("PCO2CV", string)) {
    if(length(string_split[[1]])==15) {
    df$timestamp[df_count] <- string_split[[1]][2]
    df$date[df_count] <- sprintf("%06d",as.numeric(string_split[[1]][3]))
    df$time[df_count] <- string_split[[1]][4]
    df$pco2cv_instrumentDateTime[df_count] <- string_split[[1]][5]
    df$pco2cv_instrumentZero[df_count] <- string_split[[1]][7]
    df$pco2cv_instrumentMeasure[df_count] <- string_split[[1]][8]
    df$pco2cv_CO2[df_count] <- string_split[[1]][9]
    df$pco2cv_opticalCellTemp[df_count] <- string_split[[1]][10]
    df$pco2cv_hum[df_count] <- string_split[[1]][11]
    df$pco2cv_humTemp[df_count] <- string_split[[1]][12]
    df$pco2cv_gasStreamPress[df_count] <- string_split[[1]][13]
    df$pco2cv_irgaEdtectorTemp[df_count] <- string_split[[1]][14]
    df$pco2cv_irgaSourceTemp[df_count] <- string_split[[1]][15]
  }
  }
   
  if (grepl("MRDMT", string)) {
    if(length(string_split[[1]])==9) {
    df$timestamp[df_count] <- string_split[[1]][2]
    df$date[df_count] <- sprintf("%06d",as.numeric(string_split[[1]][3]))
    df$time[df_count] <- string_split[[1]][4]
    df$mrdmt_solarRad[df_count] <- string_split[[1]][5]
    df$mrdmt_infraredRad[df_count] <- string_split[[1]][6]
    df$mrdmt_infraredSensorTemp[df_count] <- string_split[[1]][7]
    df$mrdmt_infraredRadTempCorr[df_count] <- string_split[[1]][8]
    df$mrdmt_numOfSamp[df_count] <- string_split[[1]][9]
  }
  }
  
  # Write output to file
  if(!all(is.na(df[1,]))) {
  df_line <- as.character(df[df_count,])
  line_to_print <- paste(df_line, collapse="\t")
  cat(line_to_print, "\n")
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
    time <- sprintf("%06d", df$gprmc_time[i])
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

parameters <- c(sst,sal,co2,other)

# Create new data frame to be filled with the wanted columns
new_df <- data.frame(df$datetime, df$latitude, df$longitude)
colnames(new_df)<-c("datetime", "gprmc_latitude", "gprmc_longitude")

# add the wanted columns to the empty data frame
for (l in parameters) {
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

output_new_df <- paste("output/2.chosen_cols_",input_files[file_loop], sep="")
write.table(new_df, file = output_new_df, sep ="\t", row.names=FALSE)


#------------------------------------------------------------------------------------
# CONVERT TO FORMAT ACCEPTED BY QUINCE
# Need to move measurements from different sensors (with different IDs) but close enough
# time stamp to the same row.

# Will go through new_df and look for rows where the limiting_parameter is present (the mandartory 
# paramter with the lowest frequency), and merge with the other parameters which are closest (this is 
# done by measurement cycle order so closest is always afterwards in time)

# Create a new data frame which will be the QuinCe ready version of the data
# The quince_df will have the same cols as the new_df + date cols for each parameter + 2 date cols for lat and long
ncol_quince_df <- ncol(new_df) + length(parameters) + 2
quince_df <- data.frame(matrix(ncol = ncol_quince_df, nrow = 0))
colnames_quince_df <- c(colnames(new_df), paste("datetime_", order, sep=""))
colnames(quince_df) <- colnames_quince_df

# Tell R that datetime column in new_df is a date
date.time <- as.POSIXct(new_df[,1], tz="UTC", format="%Y-%m-%dT%H:%M:%S")
new_df[,1] <- date.time


# Create output file with the quince friendly data frame
output_quince_prelim <- paste("output/3.quince_prelim_", input_files[file_loop], sep="")
sink(output_quince_prelim)

# Print header to output file
cat(paste(as.character(colnames(quince_df)), collapse="\t"))
cat("\n")


# Loop through new_df and pick out values for the quince_df
new_df_finished <- 0
new_df_count <- 1

while (new_df_finished == 0) {
  # Search for rows where the limiting parameter is present and start a new df column (NAs)
  
  if (!is.na(new_df[[limiting_param]][new_df_count])) {
    quince_df[1,] <- NA     
    
    # Adds the limiting param measurement and date to the quince_df,
    quince_df[[limiting_param]] <- new_df[[limiting_param]][new_df_count]
    quince_df[[paste("datetime_",limiting_param,sep="")]] <- as.character(new_df$datetime[new_df_count])
    
    # loop through the other parameters in the order and add them to the new quince_df row
    for (n in order) {
      if (n!=limiting_param){
        
        # Find the closest row after (or equal to) the new_df_count which contains a measurement of"n"       
        row_found <- 0 
        row_count <- 0
        
        
        while ((row_found == 0) && ((new_df_count+row_count) <= nrow(new_df))) {
          
          # Check if the current row_count position for parameter n is numeric or not. If it is, and if time diff is smaller than
          # the chosen max ofset, use this value in the quince_df. If the parameters i na, increase counter, but shut down while
          # loop if the counter increase hits a new limiting value 
          
          if (!is.na(new_df[[n]][new_df_count + row_count])) {            
            
            # The data point is found, but only add if the time offset to the limiting value on this row is less than a threshold
            time_diff <- abs(difftime(new_df$datetime[new_df_count], new_df$datetime[new_df_count + row_count], units="s")) 
            if (time_diff < max_time_ofset) {
              quince_df[[n]] <- new_df[[n]][new_df_count + row_count]
              quince_df[[paste("datetime_", n, sep="")]] <- as.character(new_df$datetime[new_df_count + row_count])
            }
            row_found <- 1
            
          } else { 
            row_count <- row_count + 1
            if (!is.na(new_df[[limiting_param]][new_df_count + row_count])) {
              row_found <- 1
            }         
          }            
        }
        
      }
    }
    
    # Write new row to output file (only if the row contains CO2)   
    quince_df_line <- as.character(quince_df[1,])
    line_to_print2 <- paste(quince_df_line, collapse="\t")
    cat(line_to_print2, "\n")  
  }
  
  # Move on to the next new_df row
  new_df_count <- new_df_count + 1
  
  # Check if while condition should be changed (if we run off file)
  if (new_df_count > nrow(new_df)) {
    new_df_finished <- 1
  } 
}

sink()



#-----------------------------------------------------------------------------------
# Import the quince_df and do minor adjustments

# Import the new txt file in the output folder
quince_df <- read.table(output_quince_prelim, header=TRUE, sep="\t")

# Make a copy
quince_df_nans <- quince_df

# Remove rows where certain parameters are NA
for (q in remove_if_na) {
  if(n!=limiting_param) {
    keep_row <- which(!is.na(quince_df[[q]]))
    quince_df <- quince_df[keep_row,]
  }
}

# Copy the co2 date time to first row and assign datetime class
quince_df$datetime <- as.POSIXct(quince_df[[paste("datetime_", co2, sep="")]],tz="UTC", format="%Y-%m-%d %H:%M:%S")

# Make a copy of quince_df to be used for datetime diagnostics further down
quince_df_diag <- quince_df

# Remove the other datetimes
quince_df <- quince_df[,1:(ncol(quince_df)-length(order))]

output_quince <- paste("output/QuinCeReady_",input_files[file_loop], sep="")
write.table(quince_df, file = output_quince, sep ="\t", row.names=FALSE)

write.table(quince_df_diag, file = "output/diag_data.txt", sep ="\t", row.names=FALSE)

#-----------------------------------------------------------------------------------
# DATETIME DIAGNOSTIC
# Plot the dates for the different parameters and position

time_diag_image_name <- paste("output/4.timediff_plot",input_files[file_loop], "png", sep=".")

png(filename=time_diag_image_name)

# Want to have all diff plots in one image. 
par(mfrow=c(length(order)/2,2))
    
# Add columns with date time difference between parameters
# Loop through all date time columns
for (o in order) {
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
#points(quince_df$datetime, quince_df[[co2]], col="red")
#legend(x="bottomleft", legend=c("PI", "us"), pch=c("o","o"), col=c("black", "red"))
#
#dev.off()



#------------------------------------------------------------------------------------
}   # End for loop
