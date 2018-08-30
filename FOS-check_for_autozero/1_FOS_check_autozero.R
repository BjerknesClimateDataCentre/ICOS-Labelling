#####################################################################################
### Function for checking if the raw measurements show signs of the auto-zeroing 
### performed on certain FOS stations 
###########################

Sys.setlocale("LC_ALL", "English"); 

##-----------------------------------------------------------------------------

### The script has some input parameters:


sepp <- "\t"
CO2_col <- c(6)                        # This is for xCO2 or pCO2

date_col <- c(1)
time_col <- c(1)

dt_format <- "%Y-%m-%dT%H:%M:%S"  


step <- 100                             # How many measuments to plot in each window
nn <- 5                                 # HOw many plots


#------------------------------------------------------------------------------

  input_dir<-"input"
  output_dir<-"output"
  
  # List all files in input directory
  input_files <- list.files(input_dir)
  
  
  # Loop through the input files
  #for (file_loop in 1:length(input_files)) {
     
    
    # Get the path to file and read the data 
    in_file <- paste(input_dir, "/", input_files[1], sep="")
    df <- read.table(in_file,header=T, sep=sepp, fileEncoding="UTF8")
    
    if (date_col != time_col){
    dateTime <- paste(df[,date_col],"T", df[,time_col], sep="")
    } else {
    dateTime <- df[,date_col] 
    }
    
    
    date.time <- as.POSIXct(dateTime, tz="UTC", format=dt_format)
    df$date.time <- date.time

  
    # Identify date and time
    # (The if statement is related to different ways the date/time can be reported in the raw file)
    #if (length(date_col) > 1) {
    #  date.time <- as.POSIXct(paste(df[,date_col[1]], df[,date_col[2]], df[,date_col[3]], df[,time_col[1]], df[,time_col[2]], df[,time_col[3]]), tz="UTC", format=dt_format) 
    #} else if (date_col == time_col) {
    #    date.time <- as.POSIXct(df[,date_col], tz="UTC", format=dt_format)
    #} else {
    #    date.time <- as.POSIXct(paste(df[,date_col], df[,time_col]), tz="UTC", format=dt_format)          
    #}
    ## Add the date time column to the data frame
    #df$date.time <- date.time
    
    

 #------------------------------------------------
# Create subset data frame containing only rows with CO2 measurements
df_subset <- subset(df, !is.na(df[[CO2_col]]))

  #df_subset$row_count <- c(1:nrow(df_subset))

# Add row counter to be used in plotting
#row_count <- (rep(1:9, nrow(df_subset)))   # 5285
#df_subset$row_count <- row_count[1:nrow(df_subset)]

#diff <- c(0)
#for (i in 1:nrow(df_subset)) {
#  diff[i] <- df_subset[[CO2_col]][i+1]-df_subset[[CO2_col]][i]
#}
#df_subset$diff <- round(diff,3)

# Plot all the CO2 measurements
#png(paste(output_dir, "/", "all_CO2_", "plot.png", sep=""))
#par(mar=c(5,5,2,2))
#plot(df_subset$row_count, df_subset$diff, ylab=expression("CO"[2]*" [ppm]"), xlab="Time", cex.lab=1.5,cex.axis=1.3)
#legend("topright", "a)", bty="n", cex=2.5) 
#dev.off()

# Write the new data frame out in new file.
#out_file <- paste(output_dir, "/", input_files[1], sep="")
#write.table(df_subset, file=out_file, row.names=FALSE, fileEncoding="UTF8", sep="\t")

location <- sort(runif(nn,0,0.9))          

png(paste(output_dir, "/", "1_chunks_CO2_time_", "plot", 1, ".png", sep=""))
par(mfrow=c(length(location),1))
par(oma = c(4, 5, 1, 0))
par(mar = c(2, 2, 1, 1))

#get plot limits
rows <- nrow(df_subset)
beg <- rep(0,length(location))
ending <- rep(0,length(location))

for (i in 1:length(location)) {
  beg[i] <- round(rows*location[i])
  ending[i] <- round(rows*location[i])+step
  plot(df_subset$date.time[beg[i]:ending[i]], df_subset[[CO2_col]][beg[i]:ending[i]], cex.lab=1.5, cex.axis=1.3, xaxt = "n")
  axis(1, tick=FALSE, df_subset$date.time[beg[i]:ending[i]], format(df_subset$date.time[beg[i]:ending[i]],"%Y-%m-%d"), cex.axis=1.3)
}

mtext('Time', side = 1, outer = TRUE, line = 2, cex=1.5)
mtext(expression("pCO"[2]*" ["*mu*"atm]"), side = 2, outer = TRUE, line = 2, cex=1.5)
mtext('a)', side = 2, outer=TRUE, line = 2, cex=2.5, at=1, adj=1, las=2)
dev.off()



#}


