#####################################################################################
### Function for calculating fCO2 for Contros sensors
###########################

### The current QuinCamilla cannot calculate correct fCO2 for FOS data.
### Therefore these calculations will happen
### in this script instead. Script will also plot fCO2.



##-----------------------------------------------------------------------------
# Input params to be assigned:


sepp <- "\t"                                      # File separator used

date_col <-c(24)
time_col <- c(24)
dt_format <- "%Y-%m-%d %H:%M:%S"                  # e.g. "%d/%m/%y %H:%M:%S"

 


#------------------------------------------------------------------------------
# IMPORT DATA

Sys.setlocale("LC_ALL", "English"); 

input_dir<- "input"
output_dir<-"output"
  
# List all files in input directory
input_files <- list.files(input_dir)
  
  
  # Loop through the input files
file_loop <- 1
#for (file_loop in 1:length(input_files)) {
     
    
# Get the path to file and read the data 
in_file <- paste(input_dir, "/", input_files[file_loop], sep="")
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
# Remove the fake standards rows
df_sub <- df#subset(df, df$run_time=="EQU")

#------------------------------------------------
# PLOT THE DIFFERENCE IN PCO2  (to see effect of adjusting for drift AND span)

df_sub$pCO2_diff <- rep(NA, nrow(df_sub))
for (i in 1:nrow(df_sub)){
  df_sub$pCO2_diff[i] <- df_sub$pCO2_corr[i] - df_sub$pCO2[i] 
}


# Plot pCO2 difference 
png(paste(output_dir, "/", "1_pco2diff_", "plot.png", sep=""))
par(mar=c(5,5,2,2))
tryCatch(plot(df_sub$date.time, df_sub$pCO2_diff, ylim=c(-13,4), ylab = expression("pCO"[2]*" ["*mu*"atm]"), xlab = "Time", cex.lab=1.5,cex.axis=1.3), error=function(e) {})   
#legend("bottomright", "a)", bty="n", cex=2.5)    
dev.off()








# #------------------------------------------------
# Write outputfile

#output_df <- paste("output/CO2_calculated_",input_files[file_loop], sep="")
#write.table(df, file = output_df, sep ="\t", row.names=FALSE)
