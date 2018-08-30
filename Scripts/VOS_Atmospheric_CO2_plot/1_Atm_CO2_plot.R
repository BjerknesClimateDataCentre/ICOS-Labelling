########################################################################################
### Function for creating atmospheric CO2 plot
###########################
### For VOS only

### Run the script in a directory which contains an input folder and an output folder.
### The Input folder needs to contain raw data file(s) (they need to have the same format and standards).
### Output folder will conatin one plot (per file) showing the atmospheric CO2 values over time.


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Input params to be assigned:
if (!input_from_main) {

date_col <-c(1)
time_col <- c(1)
dt_format <- "%d/%m/%Y %H:%M:%S"            # e.g. "%d/%m/%y %H:%M:%S"
CO2_col <- 12
CO2_col_name <- c("CO2_PHYS")
run_type_col <- 3
atm_type_name <- c("ATM")
lat_col <- 5
lon_col <- 6
lat_name <- c("LATX")
lon_name <- c("LONX")

letter <- "d)"                              # Letter to use in plot (depends on how many other measurement plots)
position <- "bottomleft"                    # Depends on letter position of the other measurement plots

}


#-----------------------
# Consider to change these axis ranges after viewing plots.
CO2atm_ylim_min <- NA
CO2atm_ylim_max <- NA

#-----------------------
# Do not change these:
atm_co2_max <- 500
atm_co2_min <- 300

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Remove old figures produced by this script from the output directory
images <- list.files("output", pattern="^[0].*png$",)
for (image_loop in 1:length(images)) {
  image <- paste("output", "/", images[image_loop], sep="")
  file.remove(image)
}

Sys.setlocale("LC_ALL", "English"); 

# Write output to txt file
output_file <- paste("output", "/", "1_Atm_CO2_plot_info_out-of-range.txt", sep="")
sink(output_file)

#------------------------------------------------------------------------------
# Import data:

input_dir<- "input"
output_dir<-"output"
  
# List all files in input directory
input_files <- list.files(input_dir)
 
# Loop through the input files
#for (file_loop in 1:length(input_files)) {
     
# Get the path to file and read the data 
in_file <- paste(input_dir, "/", input_files[1], sep="")
df <- read.table(in_file,header=T, sep = "\t", strip.white=TRUE, fileEncoding="UTF8")
	    
# Identify date and time
# (The if statement is related to different ways the date/time can be reported in the raw file)
if (length(date_col) > 1) {
  date.time <- as.POSIXct(paste(df[,date_col[1]], df[,date_col[2]], df[,date_col[3]], df[,time_col[1]], df[,time_col[2]], df[,time_col[3]]), tz="UTC", format=dt_format) 
} else if (date_col == time_col) {
  date.time <- as.POSIXct(df[,date_col], tz="UTC", format=dt_format)
} else {	
  date.time <- as.POSIXct(paste(df[,date_col], df[,time_col]), tz="UTC", format=dt_format)          
}
# Add the date time column to the data frame
df$date.time <- date.time

#------------------------------------------------------------------------------
# Extract the ATM data        

# Create a column which identifies the ATM sequences. 
new_col_header <- c("ATM_seq")
df[[new_col_header]] <- NA

count <- 0
for (j in 1:nrow(df)) {
    if (df[j,run_type_col] != atm_type_name) {
        df$ATM_seq[j] <- NA
    } else if (df[j,run_type_col] == df[j-1,run_type_col]) {
        df$ATM_seq[j] <- count
    } else {
        df$ATM_seq[j] <- count + 1
        count <- count + 1
    }
}
                     
ATM_seq_col <-  ncol(df)
#write.table(df, file = "output/test.txt", sep ="\t")
      
# Extract only the rows with ATM data, and the columns needed for plotting the rest. 
cols <- c(ncol(df)-1, run_type_col, CO2_col, lat_col, lon_col, ATM_seq_col)
df_sub <- df[df[,run_type_col]==atm_type_name,cols]   
	 

#------------------------------------------------------------------------------

## PLOT ATMOSPHERIC CO2 VS TIME
if(is.numeric(CO2atm_ylim_min)) {
  output_file_name <- paste(output_dir, "/", "0.AtmCO2_", "plot_own-range.png", sep="")
  CO2atm_ylims <- c(CO2atm_ylim_min, CO2atm_ylim_max)
} else if ((max(df_sub[[CO2_col_name]]) > atm_co2_max) || (min(df_sub[[CO2_col_name]]) < atm_co2_min)) {
    output_file_name <- paste(output_dir, "/", "0.AtmCO2_", "plot_bad-range.png", sep="")
    CO2atm_ylims <- c(atm_co2_min,atm_co2_max)	 
} else {
  output_file_name <- paste(output_dir, "/", "0.AtmCO2_", "plot.png", sep="")
  CO2atm_ylims <- c(min(na.omit(df_sub[[CO2_col_name]])),max(na.omit(df_sub[[CO2_col_name]])))	 
}
png(output_file_name)
par(mar=c(5,5,2,2))
plot (df_sub$date.time, df_sub[[CO2_col_name]], xlab="Time", ylab=expression("Atmospheric xCO"[2]*" [ppm]"), ylim = CO2atm_ylims , cex.lab=1.5,cex.axis=1.3)
legend(position, letter, bty="n", cex=2.5)
dev.off()
    	

#------------------------------------------------------------------------------

# Change the col name of latitude, longitude and CO2 before write output (do this to make the next codes work)
colnames(df_sub) <- c("date.time", "TYPE", "CO2", "latitude", "longitude", "ATM_seq")


# Write range checking to screen. How many values are out of range?
total_meas <- length(df_sub$CO2)
oor <- sum(df_sub$CO2 > atm_co2_max, df_sub$CO2 < atm_co2_min)
cat("Number of CO2atm measurements out of range: ", oor, " (", round((oor/total_meas)*100,2), "%).", sep="")

sink()

