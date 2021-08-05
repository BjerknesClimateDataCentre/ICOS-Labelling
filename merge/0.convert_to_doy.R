#####################################################################################
### Function for converting the time format to day of year, with fractions (doy)
###########################

Sys.setlocale("LC_ALL", "English"); 

##-----------------------------------------------------------------------------

### Input parameters:
date_col <- c(1)
time_col <- c(1)
date_format <- "%d/%m/%Y %H:%M:%S"

file_sepp <- "\t"


##-----------------------------------------------------------------------------

# Import data to merge and store in data frames
input_dir<-"input"
output_dir<-"output"

input_files <- list.files(input_dir)

in_file <- paste(input_dir, "/", input_files[1], sep="")

df <- read.table(in_file,header=T, sep=file_sepp, fileEncoding="UTF8")

ncol_df <- ncol(df)

#----------
# Fix date and time 
if (length(date_col) > 1) {
  date.time <- as.POSIXct(paste(df[,date_col[1]], df[,date_col[2]], df[,date_col[3]], df[,time_col[1]], df[,time_col[2]], df[,time_col[3]]), tz="UTC", format=date_format) 
} else if (date_col == time_col) {
  date.time <- as.POSIXct(df[,date_col], tz="UTC", format=date_format)
} else {
  date.time <- as.POSIXct(paste(df[,date_col], df[,time_col]), tz="UTC", format=date_format)          
}
df$date.time <- date.time



#----------
# Fix date and time 

# Get the day of year (witout fraction)
df$day <- strftime(df$date.time, format="%j", tz="GMT")

# Calculate the day fraction using the hours, minutes and seconds
df$hour <- as.numeric(format(df$date.time, "%H"))
df$minute <- as.numeric(format(df$date.time, "%M"))
df$second <- as.numeric(format(df$date.time, "%S"))  
df$fraction <- round(df$hour/24 + df$minute/1440 + df$second/86400, 7)

# The doy will be the day number + the fraction
df$doy <- as.numeric(df$day) + df$fraction

# Create a new data frame where the dummy columns are not included
df_out <- df[,1:ncol_df]
df_out$DOY <- df$doy

# Reorder to get doy as first column
df_out <- df_out[c(ncol(df_out),2:ncol(df_out)-1)]

#----------
# Write new file with the doy column 
out_file <- paste(output_dir, "/",input_files[1],"_wDOY.txt", sep="")
write.table(df_out, file=out_file, row.names=FALSE, quote = FALSE, fileEncoding="UTF8", sep="\t")















