########################################################################################
### Function for creating atmospheric CO2 plot
###########################
### For VOS only


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Input params to be assigned:
#if (!input_from_main) {

date_col <-c(20)
time_col <- c(21)
dt_format <- "%d.%m.%Y %H:%M:%S"            # e.g. "%d/%m/%y %H:%M:%S"
o2_col <- 52
o2_col_name <- 'O2_sss_uM'

letter <- "g)"                            # Letter to use in plot (depends on how many other measurement plots)
position <- "bottomleft"                    # Depends on letter position of the other measurement plots

specify_axis_label <- FALSE               # If true it only shows the last and first month in the plot
                                           # This was nessecary for some Polarstern data, but usualy set to FALSE!

QC_rows_old <- 318              # How many rows got QC message from QuinCamilla (see output from summary script)
QuinCe_timelag <- 0             # This sctipt compares dates in QuinCamilla exported files and raw files. QuinCamilla changes the time zone. 
                                # We therefore need to know the time difference (in hours).

add_sat <- TRUE                  # Add saturation to the plot (with second y-axis)
sat_col_name <- 'Saturation_go'

#Run type to keep:
#run_type <- 'EQU'

#}


#-----------------------
# Set to NA but can be changed after viewing plots.
o2_ylim_min <- NA
o2_ylim_max <- NA

#-----------------------
# Do not change these:
oxygen_min <- 50
oxygen_max <- 400

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
output_file <- paste("output", "/", "out_of_range.txt", sep="")
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
## FILETER DATA (remove runtype not EQU and rows witout TSG data)

# ON HOLD!


#------------------------------------------------------------------------------
## PLOT OXYGEN VS TIME

if(is.numeric(o2_ylim_min)) {
  output_file_name <- paste(output_dir, "/", "1.oxygen_", "plot_own-range.png", sep="")
  o2_ylims <- c(o2_ylim_min, o2_ylim_max)
} else {
  output_file_name <- paste(output_dir, "/", "1.oxygen_", "plot.png", sep="")
  o2_ylims <- c(min(na.omit(df[[o2_col_name]])),max(na.omit(df[[o2_col_name]])))	 
}

png(output_file_name)
par(mar=c(5,5,2,5))
if (specify_axis_label == TRUE) {
  plot (df$date.time, df[[o2_col_name]], xlab="Time", ylab = expression("O"[2]*" ["*mu*"mol/l]"), ylim = o2_ylims , cex.lab=1.5,cex.axis=1.3, xaxt='n')
  ticks.at <- seq(min(df$date.time), max(df$date.time), by = "months")
  ticks.lab <- format(ticks.at, format = "%b")
  axis(1, at = ticks.at, labels = ticks.lab, cex.lab=1.5, cex.axis=1.3)
} else {
  plot (df$date.time, df[[o2_col_name]], xlab="Time", ylab = expression("O"[2]*" ["*mu*"mol/l]"), ylim = o2_ylims , cex.lab=1.5,cex.axis=1.3)
}
# Add O2 sat to plot
if (add_sat == TRUE){
  par(new = TRUE)
  plot(df$date.time, df[[sat_col_name]] , xaxt = "n", yaxt = "n", ylab = "", xlab = "", col = "red")
  axis(side = 4, cex.axis=1.3)
  mtext("Saturation", side = 4, line=2.5, cex=1.5)
  legend("topright", c("Concentration", "Saturation"), pch=c(1,1), col = c("black", "red"), cex=1.2)
}

legend(position, letter, bty="n", cex=2.5)


dev.off()
    	

#------------------------------------------------------------------------------
# OUT OF RANGE

# Find number of oxygen measurements:
n_nans <- sum(is.na(df[[o2_col_name]]))
total_meas <- nrow(df) - n_nans
cat("Number of o2 measurements: ", total_meas, '\n', sep="")

# Out of plot range:
oopr <- sum(na.omit(df[[o2_col_name]] > o2_ylim_max), na.omit(df[[o2_col_name]] < o2_ylim_min))
cat("Plot 1: Number of o2 measurements out of plotting range: ", oopr, " (", round((oopr/total_meas)*100,2), "%).",'\n', sep="")

# Out of accepted range:
oor <- sum(na.omit(df[[o2_col_name]] > oxygen_max), na.omit(df[[o2_col_name]] < oxygen_min))
cat("Number of o2 measurements out of accepted range (50-400 umol/l): ", oor, " (", round((oor/total_meas)*100,2), "%).", sep="")




sink()

#------------------------------------------------------------------------------
# FIND HOW OXYGEN RANGE CHECK CHANGES THE NUMBER OF ROWS WITH ERRORS FOR THIS STATION
# PS: THE FOLLOWING CODE WORKS, BUT IS NOT CORRECT WAY TO DO THIS. THE WXPORTED FILES
# ONLY CONTAIN RUN TYPE EQU, WHILE THE OXYGEN CHECK AND PLOT IS FOR ALL RUN TYPES. tHESE
# ANALYSIS ARE THEREFORE NOT COMPARABLE AND SHOULD NOT BE COMBINED IN THIS WAY.!

# Find data.time where oxygen is out of range
#o2_row_above <- which(df[[o2_col_name]]>oxygen_max)
#o2_row_below <- which(df[[o2_col_name]]<oxygen_min)

#o2_row_oor <- sort(c(o2_row_above,o2_row_below))
#o2_dates <- df$date.time[o2_row_oor]
#o2_dates <- as.character(o2_dates)


# Import the exported file from QuinCe (this file contains the other error messages)
#input_file2 <- list.files("exported_file")
#in_file2 <-  paste("exported_file/", input_file2, sep="")
#df_exp <- read.csv(in_file2,header=T, sep=",", fileEncoding="UTF8")
#df_exp$Date <- as.POSIXct(df_exp$Date, tz="UTC", format="%Y-%m-%d %H:%M:%S")

# Find date.time where exported file contains message
#QC_message_rows <- which(df_exp$Automatic.QC.Message!="")
#QC_dates <- df_exp$Date[QC_message_rows] 

# Adjust the exported dates for the timelag given in QuinCe 
#QC_dates_adj <- QC_dates + (QuinCe_timelag*60*60)
#QC_dates_adj <- as.character(QC_dates_adj)


# Find out if the o2 dates are in the exported file
#compare <- o2_dates %in% QC_dates_adj
#additional_rows <- sum(compare==FALSE)

#QC_rows_new <- QC_rows_old + additional_rows
#QC_perc_new <- round((QC_rows_new/nrow(df_exp))*100,2)

# Pring new QC row results to screen
#cat("\n","Rows with some kind of QC message when including all parameters: ", QC_rows_new, " (", QC_perc_new, "%).", sep="")
