#####################################################################################
### Function for checking if equilibrium is reached for the raw FOS data
###########################

Sys.setlocale("LC_ALL", "English"); 

##-----------------------------------------------------------------------------

### Input parameters:

date_col_oes <- c(33,34,35)              
time_col_oes <- c(36,37,38)
dt_format_oes <- "%Y %m %d %H %M %S"                   # e.g. "%d/%m/%y %H:%M:%S" or "%Y %m %d %H %M %S" 
fco2_col_oes <- c(48)
fco2_col_finn <- c(25)
fco2_avg_col_finn <- c(31)

#section1 <- c(150,239)                                # Daynumber start and end of sections to plot separately
#section2 <- c(315,366)


##-----------------------------------------------------------------------------

# Import data to merge and store in data frames
input_dir<-"input"
output_dir<-"output"

input_files <- list.files(input_dir)

in_file1 <- paste(input_dir, "/", input_files[1], sep="")

df <- read.table(in_file1,header=T, sep="\t", fileEncoding="UTF8")

##-----------------------------------------------------------------------------
# Identify date and time

# Oestergarnsholm
# (The if statement is related to different ways the date/time can be reported in the raw file)
if (length(date_col_oes) > 1) {
  date.time <- as.POSIXct(paste(df[,date_col_oes[1]], df[,date_col_oes[2]], df[,date_col_oes[3]], df[,time_col_oes[1]], df[,time_col_oes[2]], df[,time_col_oes[3]]), tz="UTC", format=dt_format_oes) 
} else if (date_col_oes == time_col_oes) {
  date.time <- as.POSIXct(df[,date_col_oes], tz="UTC", format=dt_format_oes)
} else {
  date.time <- as.POSIXct(paste(df[,date_col_oes], df[,time_col_oes]), tz="UTC", format=dt_format_oes)          
}
# Add the date time column to the data frame
df$date.time <- date.time


##-----------------------------------------------------------------------------


##-----------------------------------------------------------------------------
# Calculate delta fco2

delta_fco2 <- rep(NA,nrow(df))
for (i in 1:nrow(df)){
  delta_fco2[i] <- df[i,fco2_col_oes] - df[i,fco2_col_finn]
}
df$delta_fco2 <- delta_fco2


# Delta using aerage
delta_fco2_avg <- rep(NA,nrow(df))
for (i in 1:nrow(df)){
  delta_fco2_avg[i] <- df[i,fco2_col_oes] - df[i,fco2_avg_col_finn]
}
df$delta_fco2_avg <- delta_fco2_avg




##-----------------------------------------------------------------------------
# Plot the delta fco2 from the two stations vs time

# Plot oestergarnsholm
png(paste(output_dir, "/", "1_delta_fco2_vs_time_", "plot.png", sep=""), width=2000, height=750)
par(mar = c(5, 5, 2, 2))
plot(df$date.time, df$delta_fco2,
     ylab=expression("fco2 ["*mu*"atm]"), xlab="Time",
     cex.lab=1.5,cex.axis=1.3, type="p", col="black",
     main ="Delta fco2"
)

points(df$date.time, df$delta_fco2_avg, col="red")
legend("topleft", legend=c("measured_fco2_at_finnmaid","average_fco2_at_finnmaid"), col=c("black","red"), bty="n", cex=1.3, pch=1)

dev.off()



out_file <- paste(output_dir, "/", "merged_finn_oes_2hour_57.2-57.6N_18.9-19.2E.txt", sep="")
write.table(df, file=out_file, row.names=FALSE, fileEncoding="UTF8", sep="\t")






