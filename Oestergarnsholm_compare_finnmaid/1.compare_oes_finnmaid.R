#####################################################################################
### Function for checking if equilibrium is reached for the raw FOS data
###########################

Sys.setlocale("LC_ALL", "English"); 

##-----------------------------------------------------------------------------

### Input parameters:

date_col_oes <- c(2,3,4)              
time_col_oes <- c(5,6,7)
dt_format_oes <- "%Y %m %d %H %M %S"                   # e.g. "%d/%m/%y %H:%M:%S" or "%Y %m %d %H %M %S" 
fco2_col_oes <- c(17)

date_col_finn <- c(9,10,11)              
time_col_finn <- c(12,13,14)
dt_format_finn <- "%Y %m %d %H %M %S"                   # e.g. "%d/%m/%y %H:%M:%S" or "%Y %m %d %H %M %S" 
fco2_col_finn <- c(25)
lat_col_finn <- c(3)
lon_col_finn <- c(4)


lat_box <- c(57.2, 57.6)                              # Siv used 57.2 - 57.6 N and 18.9 - 19.1 E 
lon_box <- c(18.9, 19.1)  

section1 <- c(150,239)                                # Daynumber start and end of sections to plot separately
section2 <- c(315,366)


##-----------------------------------------------------------------------------

# Import data to merge and store in data frames
input_dir<-"input"
output_dir<-"output"

input_files <- list.files(input_dir)

in_file1 <- paste(input_dir, "/", input_files[1], sep="")
in_file2 <- paste(input_dir, "/", input_files[2], sep="")

df_finn <- read.table(in_file1,header=T, sep="\t", fileEncoding="UTF8")
df_oes <- read.table(in_file2,header=T, sep="\t", fileEncoding="UTF8")

##-----------------------------------------------------------------------------
# Identify date and time

# Oestergarnsholm
# (The if statement is related to different ways the date/time can be reported in the raw file)
if (length(date_col_oes) > 1) {
  date.time_oes <- as.POSIXct(paste(df_oes[,date_col_oes[1]], df_oes[,date_col_oes[2]], df_oes[,date_col_oes[3]], df_oes[,time_col_oes[1]], df_oes[,time_col_oes[2]], df_oes[,time_col_oes[3]]), tz="UTC", format=dt_format_oes) 
} else if (date_col_oes == time_col_oes) {
  date.time_oes <- as.POSIXct(df_oes[,date_col_oes], tz="UTC", format=dt_format_oes)
} else {
  date.time_oes <- as.POSIXct(paste(df_oes[,date_col_oes], df_oes[,time_col_oes]), tz="UTC", format=dt_format_oes)          
}
# Add the date time column to the data frame
df_oes$date.time_oes <- date.time_oes


# Finnmaid
# (The if statement is related to different ways the date/time can be reported in the raw file)
if (length(date_col_finn) > 1) {
  date.time_finn <- as.POSIXct(paste(df_finn[,date_col_finn[1]], df_finn[,date_col_finn[2]], df_finn[,date_col_finn[3]], df_finn[,time_col_finn[1]], df_finn[,time_col_finn[2]], df_finn[,time_col_finn[3]]), tz="UTC", format=dt_format_finn) 
} else if (date_col_finn == time_col_finn) {
  date.time_finn <- as.POSIXct(df_finn[,date_col_finn], tz="UTC", format=dt_format_finn)
} else {
  date.time_finn <- as.POSIXct(paste(df_finn[,date_col_finn], df_finn[,time_col_finn]), tz="UTC", format=dt_format_finn)          
}
# Add the date time column to the data frame
df_finn$date.time_finn <- date.time_finn



##-----------------------------------------------------------------------------
# Create subset with the finnmaid bounding box


df_finn_boxed_prelim <- subset(df_finn, df_finn[,lat_col_finn] > lat_box[1] & df_finn[,lat_col_finn] < lat_box[2]) 
df_finn_boxed <- subset(df_finn_boxed_prelim, df_finn_boxed_prelim[,lon_col_finn] > lon_box[1] & df_finn_boxed_prelim[,lon_col_finn] < lon_box[2]) 


##-----------------------------------------------------------------------------
# Plot the fco2 from the two stations vs time

# Plot oestergarnsholm
png(paste(output_dir, "/", "1_fco2_vs_time_", "plot.png", sep=""), width=2000, height=750)
par(mar = c(5, 5, 2, 2))
plot(df_oes$date.time_oes, df_oes[,fco2_col_oes],
     ylab=expression("fco2 ["*mu*"atm]"), xlab="Time",
     cex.lab=1.5,cex.axis=1.3, type="p", col="black"
)


# Add finnmaid to plot
points(df_finn_boxed$date.time_finn, df_finn_boxed[,fco2_col_finn], col="red")

legend("topleft", legend=c("Oestergarnsholm","Finnmaid"), col=c("black","red"), bty="n", cex=1.3, pch=1)

dev.off()



##-----------------------------------------------------------------------------
# Since there is a big gap in the oestergarnsholm data we split the plot in two and exclude the gap.

section1 <- c(150,239)                                # Daynumber start and end of sections to plot separately
section2 <- c(315,366)


# Find start and end row numbers for the two section
start1 <- min(which(df_oes$DayTime_SAMI > section1[1]))
end1 <- max(which(df_oes$DayTime_SAMI < section1[2]))

start2 <- min(which(df_oes$DayTime_SAMI > section2[1]))
end2 <- max(which(df_oes$DayTime_SAMI < section2[2]))

  
  
# Plot section1  
png(paste(output_dir, "/", "2_fco2_vs_time_", "plot_section1.png", sep=""), width=2000, height=750)
par(mar = c(5, 5, 2, 2))
plot(df_oes$date.time_oes[start1:end1], df_oes[start1:end1,fco2_col_oes],
     ylab=expression("fco2 ["*mu*"atm]"), xlab="Time",
     cex.lab=1.5,cex.axis=1.3, type="p", col="black"
)
points(df_finn_boxed$date.time_finn, df_finn_boxed[,fco2_col_finn], col="red")
legend("topleft", legend=c("Oestergarnsholm","Finnmaid"), col=c("black","red"), bty="n", cex=1.3, pch=1)
dev.off()


# Plot section2  
png(paste(output_dir, "/", "3_fco2_vs_time_", "plot_section2.png", sep=""), width=2000, height=750)
par(mar = c(5, 5, 2, 2))
plot(df_oes$date.time_oes[start2:end2], df_oes[start2:end2,fco2_col_oes],
     ylab=expression("fco2 ["*mu*"atm]"), xlab="Time",
     cex.lab=1.5,cex.axis=1.3, type="p", col="black"
)
points(df_finn_boxed$date.time_finn, df_finn_boxed[,fco2_col_finn], col="red")
legend("topleft", legend=c("Oestergarnsholm","Finnmaid"), col=c("black","red"), bty="n", cex=1.3, pch=1)
dev.off()






