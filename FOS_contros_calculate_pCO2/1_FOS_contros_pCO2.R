#####################################################################################
### Function for calculating fCO2 for Contros sensors
###########################

### The current QuinCamilla cannot calculate correct fCO2 for FOS data.
### Therefore these calculations will happen
### in this script instead. Script will also plot fCO2.



##-----------------------------------------------------------------------------
# Input params to be assigned:


sepp <- "\t"                                      # File separator used


date_col <-c(1)
time_col <- c(2)
dt_format <- "%Y-%m-%d %H:%M:%S"                  # e.g. "%d/%m/%y %H:%M:%S"

runtime_col_name <- "Runtime_sec"
start_runtime <- 0                                # Is this always 0?
end_runtime <- ????                            # Need to get the start and end runtime from PI.


skip <- 3                                      # How many zero rows to skip before averaging (PI used 3)



# The following is found in calibration sheets (the 'pre' from the calibration before the measurements, and the
# 'post' from the calibration after the measurements):
k1_pre <- 5.789870e-02
k2_pre <- 1.031659e-06
k3_pre <- 2.750357e-10

k1_post <- 5.042788e-02
k2_post <- 2.628951e-06
k3_post <- 1.397107e-10

fTsensor <- 9849.41                 # From pre sheet
FF <- 62256


p0 <- 1013.25
T0 <- 273.15





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
# CALCULATE THE RUNNING k's

# Create funtion to get the k's, with input k_pre and k_post
get_k <- function(k_pre,k_post) {
  k <- rep(NA, nrow(df))  
  for (i in 1:nrow(df)) {
    current_runtime <- df$Runtime[i]
    k[i] <- abs(((k_pre - k_post)*(current_runtime/(end_runtime-start_runtime))) - k_pre)
  }     
  return(k)
}

# Use the funtion to calculate all k's
df$k1 <- get_k(k1_pre,k1_post)
df$k2 <- get_k(k2_pre,k2_post)
df$k3 <- get_k(k3_pre,k3_post)


# #------------------------------------------------
# CALCULATE THE S_PROC

# #----
# Get: Smark2beam = Sraw / Sref * f(Tsensor)
Smark2beam <- rep(NA, nrow(df))
for (i in 1:nrow(df)) {
Smark2beam[i] <- df$Signal_raw[i]/df$Signal_ref[i] * fTsensor
}
df$Smark2beam <- Smark2beam


# #----
# Get: Smark2beamZ (related to the Zero col)

df$Smark2beamZ <- rep(NA, nrow(df))
new_sequene_row <- 1
row_count <- 1
df_finished <- 0
state <- "find_sequence"
sum <- 0

while (df_finished == 0) {
  
  ## STATE: FIND NEW SEQUENCE
  if (state == "find_sequence") {   
    if (df$Zero[row_count] == 1) {
      sequence_start <- row_count
      state <- "skip_rows" 
    }
  } 
  
  ## STATE: SKIP ROWS
  if (state == "skip_rows") {
    row_count <- row_count + skip
    state <- "calc_avg"
    avg_start <- row_count
  }
    
  ## STATE: CALCULATE AVERAGE
  if (state=="calc_avg") {    
    if (df$Zero[row_count]==1) {
      sum <- sum + df$Smark2beam[row_count]
    } else {
      avg <- sum /(row_count - avg_start)
      df$Smark2beamZ[sequence_start] <- avg
      sum <- 0
      state <- "find_sequence"
    }    
  }

  # Move to next row
  row_count <- row_count + 1
  if (row_count > nrow(df)) {
    df_finished <- 1
  }

}

# Copy the Smark2beamZ numbers to the following cols using for loop
# But first, assign the first value outside the loop
df$Smark2beamZCopy <- rep(NA, nrow(df))

non_NAs <- which(!is.na(df$Smark2beamZ))
first_non_NAs_pos <- non_NAs[1]
current_beam <- df$Smark2beamZ[first_non_NAs_pos]

for (i in 1:nrow(df)) {  
  if (!is.na(df$Smark2beamZ[i])) {
    current_beam <- df$Smark2beamZ[i]
  }
    df$Smark2beamZCopy[i] <- current_beam
}


# #----
# Get: S'DC(t) = Smark2beam/Smark2beamZCopy

df$SmarkDCt <- rep(NA, nrow(df))
for (i in 1:nrow(df)) {
  df$SmarkDCt[i] <- df$Smark2beam[i]/df$Smark2beamZCopy[i]
}


# #----
# Get: Sproc(t) = F(1-S'DC(t))

df$Sproct <- rep(NA, nrow(df))
for (i in 1:nrow(df)) {
  df$Sproct[i] <- FF*(1-df$SmarkDCt[i])
}



# #------------------------------------------------
# CALCULATE THE xCO2,wet and pCO2

df$xCO2wet <- rep(NA, nrow(df))
for (i in 1:nrow(df)){
  term1 <- (df$k3[i]*(df$Sproct[i]^3)) + (df$k2[i]*(df$Sproct[i]^2)) + (df$k1[i]*(df$Sproct[i]^1))
  term2 <- (p0*(df$T_gas[i]+273.15))/(T0*df$p_NDIR[i])   
  df$xCO2wet[i] <- term1*term2 
}

# #----
df$pCO2 <- rep(NA, nrow(df))
for (i in 1:nrow(df)){
   df$pCO2[i] <- df$xCO2wet[i]*(df$p_in[i]/1013.25)
}





# Plot calculated xCO2 vs measured xCO2
png(paste(output_dir, "/", "1_xco2_time_", "plot.png", sep=""))
par(mar=c(5,5,2,2))
tryCatch(plot(df$xCO2_corr, df$xCO2wet, ylim = c(0,350), xlim = c(0,350), ylab = expression("Calculated xCO"[2]*" [ppm]"), xlab = expression("Measured xCO"[2]*" [ppm]"), cex.lab=1.5,cex.axis=1.3), error=function(e) {})   
abline(0,1, col="red")
legend("bottomright", "a)", bty="n", cex=2.5)    
dev.off()

# Plot calculated pCO2 vs measured pCO2
png(paste(output_dir, "/", "2_pco2_time_", "plot.png", sep=""))
par(mar=c(5,5,2,2))
tryCatch(plot(df$pCO2_corr, df$pCO2, ylim = c(0,350), xlim = c(0,350), ylab = expression("Calculated pCO"[2]*" ["*mu*"atm]"), xlab = expression("Measured pCO"[2]*" ["*mu*"atm]"), cex.lab=1.5,cex.axis=1.3), error=function(e) {})   
abline(0,1, col="red")
legend("bottomright", "b)", bty="n", cex=2.5)    
dev.off()


# #------------------------------------------------
# Write outputfile

output_df <- paste("output/CO2_calculated_",input_files[file_loop], sep="")
write.table(df, file = output_df, sep ="\t", row.names=FALSE)






# #------------------------------------------------
# #------------------------------------------------
# SOME DIAGNOSTICS PLOTS 

# THESE PLOTS ARE BETTER TO RUN IN THE SCRIPT FOS-check_equilibrium after measuremetns before equilibrium is removed.

# #----
# Investigate difference in measured and calculated pco2 (to see effect of adjusting for drift AND span)

#df$pCO2_diff <- rep(NA, nrow(df))
#for (i in 1:nrow(df)){
#  df$pCO2_diff[i] <- df$pCO2_corr[i] - df$pCO2[i] 
#}


# Plot pCO2 difference 
#png(paste(output_dir, "/", "3_pco2diff_time_drift_and_span_adjustment", "plot.png", sep=""))
#par(mar=c(5,5,2,2))
#tryCatch(plot(df$date.time, df$pCO2_diff, ylim =c(-3,7), ylab = expression("Drift and span adjustment on pCO"[2]*" ["*mu*"atm]"), xlab = "Time", cex.lab=1.5,cex.axis=1.3), error=function(e) {})   
#legend("bottomright", "a)", bty="n", cex=2.5)    
#dev.off()




# #------------------------------------------------
# # CALCULATE AND PLOT fCO2 (IN ANOTHER SCRIPT BECAUSE NEED TO MERGE WITH CTD DATA FIRST!)
    
# Calculate fCO2 (based on equation is Steve's document "Fixing wet stations")
#fCO2 <-  rep(NA, nrow(df))
#for (i in 1:nrow(df))  {
#  kelvin <- (df[[SST_name]][i]) + 273.15
#  B <- -1636.75 + (12.0408*kelvin) - (0.0327957*kelvin^2) + ((3.16528*1e-5)*kelvin^3)
#  delta <- 57.7 - (0.118*kelvin)
#  fCO2[i] <- (df$pCO2[i])*exp(((B + 2*(delta*1e-6))*(1013.25*1e-6))/(8.314472*kelvin))
#} 
#df$fCO2 <- fCO2
   
    
 
      
#    # Plot fCO2  vs. time 
#    png(paste(output_dir, "/", "1_fCO2_time_", "plot.png", sep=""))
#    par(mar=c(5,5,2,2))
#    tryCatch(plot(df_sub$date.time, df_sub$fCO2, ylab = expression("fCO"[2]*" ["*mu*"atm]"), ylim =c(500,560), xlab = "Time", cex.lab=1.5,cex.axis=1.3), error=function(e) {})
#    if(which_CO2=="xCO2"){
#     legend("bottomright", "a)", bty="n", cex=2.5) 
#    }
#    dev.off()
    
#     # Plot fCO2 vs xCO2
#     png(paste(output_dir, "/", "2_fCO2_xCO2_", "plot.png", sep=""))
#     par(mar=c(5,5,2,2))
#     tryCatch(plot(df_sub[[CO2_name]], df_sub$fCO2, ylab = expression("fCO"[2]*" ["*mu*"atm]"), xlab = expression("pCO"[2]*" ["*mu*"atm]"), ylim=c(200,500),xlim=c(200,500), cex.lab=1.5,cex.axis=1.3), error=function(e) {})
#     legend("bottomright", "b)", bty="n", cex=2.5) 
#     dev.off()



