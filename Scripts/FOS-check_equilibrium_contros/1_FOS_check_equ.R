#####################################################################################
### Function for checking if equilibrium is reached for the raw FOS data
###########################

Sys.setlocale("LC_ALL", "English"); 

##-----------------------------------------------------------------------------

### The script has some input parameters:


sepp <- "\t"
CO2_col <- c(19)                        # This is for raw xCO2 or pCO2
additional_CO2_col <- 20                # Use if are more than one co2 in raw file (xCO2 AND pCO2). Assign as NA if not relevant
date_col <- c(24)
time_col <- c(24)
dt_format <- "%Y-%m-%d %H:%M:%S"  

cycle_length <- 5                       # Approximate time between 2 measurements cycles (in hours)


#-----------------
# Do not change these
step <- 200                             # How many measuments to plot in each window
nn <- 5                                 # How many plots

#------------------------------------------------------------------------------

  input_dir<-"input"
  output_dir<-"output"
  
  # List all files in input directory
  input_files <- list.files(input_dir)
  
  
  # Loop through the input files
#  for (file_loop in 1:length(input_files)) {
     
    
    # Get the path to file and read the data 
    in_file <- paste(input_dir, "/", input_files[1], sep="")
    df <- read.table(in_file,header=T, sep=sepp, fileEncoding="UTF8")
    
    
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
    
    

 #------------------------------------------------
  
# Plot all the CO2 measurements
#png(paste(output_dir, "/", "0_all_CO2_time_", "plot.png", sep=""))
#par(mar=c(5,5,2,2))
#plot(df$date.time, df[[CO2_col]], ylab=expression("xCO"[2]*" [ppm]"), xlab="Time", cex.lab=1.5,cex.axis=1.3)
#legend("topright", "a)", bty="n", cex=2.5) 
#dev.off()

#------
# Plot a section of the CO2_measurements
location <- sort(runif(nn,0,0.9))          
png(paste(output_dir, "/", "1_chunks_CO2_time_", "plot.png", sep=""))
par(mfrow=c(length(location),1))
par(oma = c(4, 5, 1, 0))
par(mar = c(2, 2, 1, 1))

#get plot limits
rows <- nrow(df)
beg <- rep(0,length(location))
ending <- rep(0,length(location))

for (i in 1:length(location)) {
  beg[i] <- round(rows*location[i])
  ending[i] <- round(rows*location[i])+step
  plot(df$date.time[beg[i]:ending[i]], df[[CO2_col]][beg[i]:ending[i]], cex.lab=1.5, cex.axis=1.3, xaxt = "n")
  axis(1, tick=FALSE, df$date.time[beg[i]:ending[i]], format(df$date.time[beg[i]:ending[i]],"%Y-%m-%d"), cex.axis=1.3)
}

mtext('Time', side = 1, outer = TRUE, line = 2, cex=1.5)
mtext(expression("pCO"[2]*" ["*mu*"atm]"), side = 2, outer = TRUE, line = 2, cex=1.5)
mtext('a)', side = 2, outer=TRUE, line = 2, cex=2.5, at=1, adj=1, las=2)
dev.off()


#------
# Plot one cycle

random <- runif(1,0,0.9)
dummy_beg <- round(random*nrow(df),0)

time_limit <- (cycle_length*60*60)/2

#find begining
done <- 0
loop <- 0
while (done==0) {
  
  this_diff <- abs(difftime(df$date.time[dummy_beg+loop], df$date.time[dummy_beg+loop+1], unit ="sec"))
  if(this_diff > time_limit){
    beg2 <- dummy_beg + loop + 1
    done <- 1
  }
  loop <- loop +1  
}

#find_end
done <- 0
loop <- 1
while (done==0) {
  this_diff <- abs(difftime(df$date.time[beg2+loop], df$date.time[beg2+loop+1], unit ="sec"))
  if(this_diff > time_limit) {
    ending2 <- beg2 + loop
    done <- 1
  }
  loop <- loop+1
}

# make plot
png(paste(output_dir, "/", "2_one_cycle_", "plot.png", sep=""))
par(mar=c(5,5,2,2))
plot(df$date.time[beg2:ending2], df[[CO2_col]][beg2:ending2], ylab=expression("pCO"[2]*" ["*mu*"atm]"), xlab="Time", cex.lab=1.5,cex.axis=1.3, xaxt = "n")
axis(1, tick=FALSE, df$date.time[beg2:ending2], format(df$date.time[beg2:ending2],"%Y-%m-%d %H:%M:%S"), cex.axis=1.3)
legend("bottomright", "b)", bty="n", cex=2.5) 
dev.off()






