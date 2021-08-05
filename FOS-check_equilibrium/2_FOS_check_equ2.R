#####################################################################################
### Function for checking the standard deviation of the last measurements in a sequence.
### If the standard deviations are low, this means the measurement sequence is equilibrating
###########################


##-----------------------------------------------------------------------------
### This script has some input parameters.

time_diff_col <- c(16)              # If there is no timediff column, need to use something else to determine the sequences, e.g. sequence number (not writen in the script yet)



numb_of_meas <- 50                   # How many measurements per sequence are used to calculate the standard deviation
max_seq_diff <- 3600                # Minimum time passed to separate one sequence from another (3600 = 1h)


remove_outliers<- TRUE
outlier_def <- 2                    # How much bigger than median of the final x measurements to get excluded from std dev calculation (2 = twice as big)



#------------------------------------------------------------------------------
# Will first create new data frame with columns sequence number, start_dateTime, and the final x measurements in each sequence 

# Create the new data frame
co2_seq_header <- rep(NA,numb_of_meas)
seq_df <- data.frame(matrix(ncol=2+numb_of_meas, nrow=0)) 
for (i in 1:numb_of_meas) {
  co2_seq_header[i] <- sprintf("m%s",i) 
}
colnames(seq_df) <- c("seq_nr", "dateTime_start", co2_seq_header)


# Add data to the data frame in a while loop 
row_count <- 1
sequence <- 1
data_finished <- 0 
state <- "find_end_of_seq"

while (data_finished == 0) {
  
  if (state == "find_end_of_seq"){
    # If the next time difference is huge, change state.
    if(df[row_count+1,time_diff_col] > 3600){
      state <- "write_to_data_frame"
    }
  }
  
  if (state == "write_to_data_frame" & row_count > 5){
    
    #Write the sequence and dateTime
    seq_df[sequence,1] <- sequence
    seq_df[sequence,2] <- toString(df[row_count-5,ncol(df)])
    
    
    # Write the measurements
    for (j in 1:numb_of_meas) {
      seq_df[sequence,j+2] <- df[row_count-j+1,CO2_col]
    }
    
    sequence <- sequence + 1
    state <- "find_end_of_seq"
  }
  
  row_count <- row_count + 1
  if (row_count+1 > nrow(df)) {
    data_finished <- 1
  }
  
}

# Change the dateTime column from string to proper date and time
date.time2 <- as.POSIXct(seq_df[,2], tz="UTC", format="%Y-%m-%d %H:%M:%S" )
seq_df$dateTime_start <- date.time2

#------------------------------------------------------------------------------
# Add a column with the standard deviation (and how many values to calculate it) for each sequence

seq_df$std_dev <- NA
seq_df$n <- rep(numb_of_meas,nrow(seq_df))

for (k in 1:nrow(seq_df)){
  
  measurements <- rep(NA,numb_of_meas)
  
  for (l in 1:numb_of_meas) {
    measurements[l] <- seq_df[k,l+2]
  }
  
  # If remove outliers, change them to NA
  if (remove_outliers==TRUE){
    medi <- median(measurements)
    for (m in 1:length(measurements)) {
      if (measurements[m]> (outlier_def*medi)) {
        measurements[m] <- NA
      }
    }
    seq_df$n[k] <- length(na.omit(measurements))
  }
  
  
  seq_df$std_dev[k] <- sd(na.omit(measurements))
}



#------------------------------------------------------------------------------
# Plot the results 


# Plot the standard deviation vs date
png(paste(output_dir, "/", "3_std_", "plot.png", sep=""))
par(mar=c(5,5,2,2))
plot(seq_df$dateTime_start, seq_df$std_dev, 
#    ylab=expression("standard deviation ["*mu*"atm]"), xlab="Time", 
     ylab="standard deviation [ppm]", xlab="Time",
     cex.lab=1.5,cex.axis=1.3)
legend("topright", "c)", bty="n", cex=2.5) 

dev.off()

# Plot histogram showing the standard deviations
png(paste(output_dir, "/", "4_std_", "hist.png", sep=""))
par(mar=c(5,5,2,2))
hist(seq_df$std_dev, 
 #    xlab=expression("standard deviation ["*mu*"atm]"),
     xlab="standard deviation [ppm]",
     ylab="Frequency", 
     cex.lab=1.5,cex.axis=1.3, main="",breaks=20, xlim=c(0,5))
legend("topright", "d)", bty="n", cex=2.5) 

dev.off()


#------------------------------------------------------------------------------
# Calculate and print to text file how many % of std's are below 2 uatm. 
sink("output/percent_stdDev.txt")

std_higher_2 <- length(which(seq_df$std_dev>2))
total_std <- length(seq_df$std_dev)
percent_lower_2 <- round(((total_std-std_higher_2)/total_std)*100,0)

cat(percent_lower_2, " % of the standard deviations of the final ",numb_of_meas, " measurements were lower than 2 ppm.","\n\n", sep="") 

# How many sequences had outliers:
if(remove_outliers== TRUE) {
  outliers<- c(5-(seq_df$n))
  out_0 <- length(which(outliers==0))
  out_1 <- length(which(outliers==1))
  out_2 <- length(which(outliers==2))
  percent_out_0 <- round((out_0/length(outliers))*100,0)
  percent_out_1 <- round((out_1/length(outliers))*100,0)
  percent_out_2 <- round((out_2/length(outliers))*100,0)
  
  cat("Note that:","\n" ,sep="")
  cat(percent_out_0," % of sequences had 0 outliers.", "\n" ,sep="")
  cat(percent_out_1," % of sequences had 1 outlier.", "\n" ,sep="")
  cat(percent_out_2," % of sequences had 2 outliers.", "\n" ,sep="")


}


sink()




#------------------------------------------------------------------------------
# In order to deside which measument to go forward with, we plot some zoomed in on some random sequences.
# If the final measurements go upwards towards equilibium we use the final measurement.
# If the final measurements fluctuates we use an average of them as our "final" co2 measurement.


png(paste(output_dir, "/", "5_cycle_zoomed_", "plot.png", sep=""))
par(mfrow=c(2,2))
par(oma = c(4, 5, 1, 0))
par(mar = c(2, 2, 1, 1))

dummy_xaxis <- c(1:numb_of_meas)


for (o in 1:4){

random_seq <- round(runif(1,1,length(seq_df[,1])),0)

# Create the y axis with the final co2 measurements
yaxis <- rep(NA,numb_of_meas)
for (n in 1:numb_of_meas) {
  yaxis[n] <- seq_df[random_seq,n+2]
}

# Reverse order to get the measurements cronological (the first meas in seq_df is the newest)
yaxis <- rev(yaxis)

# If remove outliers, change them to NA
if(remove_outliers== TRUE) {
  medi <- median(yaxis)
  yaxis[which(yaxis>2*medi)] <- NA
}

# Make plot
plot(dummy_xaxis, yaxis, cex.lab=1.5,cex.axis=1.3,  xaxt = "n")

}

mtext("Time", side = 1, outer = TRUE, line = 2, cex=1.5)
#mtext(expression("pCO"[2]*" ["*mu*"atm]"), side = 2, outer = TRUE, line = 2, cex=1.5)
mtext(expression("xCO"[2]*" [ppm]"), side = 2, outer = TRUE, line = 2, cex=1.5)

dev.off()








