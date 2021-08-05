#####################################################################################
### Function for calculating fCO2, ph and oxygen for FOS raw data.
###########################

### The current QuinCamilla cannot calculate correct fCO2 for FOS data, and does
### not deal with oxygen or pH at all. Therefore these calculations will happen
### in this script instead. Script will also plot these.
getwd()

getSrcDirectory(function(x) {x})

getwd()

##-----------------------------------------------------------------------------
# Input params to be assigned:


sepp <- "\t"                                        # File separator used
date_col <-c(2)
time_col <- c(3)
dt_format <- "%m/%d/%y %H:%M:%S"                   # e.g. "%d/%m/%y %H:%M:%S" or "%Y %m %d %H %M %S" 

which_CO2 <- "pCO2"                               # Choose between xCO2 and pCO2 

CO2_name <- "CO2_sami"                             # Use dots instead of space
SST_name <- "SST_degC_sbe"
Peq_name <- 1000                           #    Use 1000 if this is not measured.



salinity <- TRUE                                    # If FALSE we use 35 fixed.
sal_name <- "Sal_sbe"


is_O2 <- TRUE
is_pH <- FALSE

O2_name <- "O2_sbe43_umol_pr_l_sbe"
O2_calculations <- "no"                           # Fill inn "no" if calculations are not needed, "yes" if it is needed

pH_name <- "pH"
pH_temp_name <- "pH.Temp"

is_pressure <- TRUE                                 # Pressure related to depth
pressure_name <- "depth_m_sbe"    
is_depth <- TRUE
depth_name <- "depth_m_sbe"

is_cond <- FALSE
cond_name <- "sbe37po_cond"

O2_letter <- "e)"                                    # Lettering for measurement plot in report
pH_letter <- "f)"
press_letter <- "d)"
cond_letter <- NA
letter_position <- "bottomleft"                    # Depends on where letters are in other measurement plots

run_type_col <- FALSE                                # Set to false if there is no run type and all are ocean measurements
ocean_type_name <- "EQU"

#-------------------------
fco2_ylim_min <-  350                             
fco2_ylim_max <-  1200

depth_ylim_min <- 4
depth_ylim_max <- 7

#------------------------------------------------------------------------------

   Sys.setlocale("LC_ALL", "English"); 


  input_dir<- "input"
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
    
    
    # Select only water measurements:
    if(run_type_col == FALSE){
      df_sub <- df
    } else {
     df_sub <- df[df[,run_type_col]==ocean_type_name,] 
    }

 #------------------------------------------------
 # CALCULATE AND PLOT fCO2

if (which_CO2 == "xCO2"){


     # Calulate pCO2 (based on CO-Pro manual, equation 4.1 and 4.2)
    pCO2 <- rep(0, length(df_sub[,1]))
    if (is.numeric(Peq_name)) {
      for (i in 1:length(pCO2)) {
        pCO2[i] <- (Peq_name/1013.25)*df_sub[[CO2_name]][i]
        }
     } else {  
      for (i in 1:length(pCO2)) {
         pCO2[i] <- (df_sub[[Peq_name]][i]/1013.25)*df_sub[[CO2_name]][i]
      }
    }
    
    # Add to dataset
    df_sub$pCO2<-pCO2
} else if (which_CO2 == "pCO2") {
  df_sub$pCO2 <- df_sub[[CO2_name]]
  
}
    
    # Calculate fCO2 (based on equation is Steve's document "Fixing wet stations")
    fCO2 <-  rep(0, length(df_sub[,1]))
    for (l in 1:length(fCO2))  {
      
      kelvin <- (df_sub[[SST_name]][l]) + 273.15
      B <- -1636.75 + (12.0408*kelvin) - (0.0327957*kelvin^2) + ((3.16528*1e-5)*kelvin^3)
      delta <- 57.7 - (0.118*kelvin)
      if (is.numeric(Peq_name)) {
        fCO2[l] <- (df_sub$pCO2[l])*exp(((B + 2*(delta*1e-6))*(Peq_name*1e-6))/(8.314472*kelvin))
      } else {
        fCO2[l] <- (df_sub$pCO2[l])*exp(((B + 2*(delta*1e-6))*(df_sub[[Peq_name]][l]*1e-6))/(8.314472*kelvin))
      }
    } 
    # Add to dataset # 
    df_sub$fCO2 <- fCO2
   
    
 
    # Plot fCO2  vs. time 
    png(paste(output_dir, "/", "1_fCO2_time_", "plot.png", sep=""))
    par(mar=c(5,5,2,2))
#    tryCatch(plot(df_sub$date.time, df_sub$fCO2, ylim=c(100,1200), ylab = expression("fCO"[2]*" ["*mu*"atm]"), xlab = "Time", cex.lab=1.5, cex.axis=1.3), error=function(e) {})
    if (is.numeric(fco2_ylim_min)) {
      tryCatch(plot(df_sub$date.time, df_sub$fCO2, ylab = expression("fCO"[2]*" ["*mu*"atm]"), xlab = "Time", cex.lab=1.5, cex.axis=1.3, ylim=c(fco2_ylim_min,fco2_ylim_max)), error=function(e) {})
    } else {
      tryCatch(plot(df_sub$date.time, df_sub$fCO2, ylab = expression("fCO"[2]*" ["*mu*"atm]"), xlab = "Time", cex.lab=1.5, cex.axis=1.3), error=function(e) {})
    }

#    tryCatch(plot(df_sub$date.time, df_sub$fCO2, ylab = expression("fCO"[2]*" ["*mu*"atm]"), xlab = "Time", xaxt='n'), error=function(e) {})
#    ticks.at <- seq(min(df_sub$date.time), max(df_sub$date.time), by = "months")
#    ticks.lab <- format(ticks.at, format = "%b")
#    axis(1, at = ticks.at, labels = ticks.lab, cex.lab=1.5, cex.axis=1.3)
    
    if(which_CO2=="xCO2") {
     legend("bottomright", "a)", bty="n", cex=2.5) 
    }
    dev.off()
    
    if(which_CO2=="xCO2") {
     # Plot fCO2 vs xCO2
     png(paste(output_dir, "/", "2_fCO2_xCO2_", "plot.png", sep=""))
     par(mar=c(5,5,2,2))
     #tryCatch(plot(df_sub[[CO2_name]], df_sub$fCO2, ylab = expression("fCO"[2]*" ["*mu*"atm]"), xlab = expression("pCO"[2]*" ["*mu*"atm]"), ylim=c(200,500),xlim=c(200,500), cex.lab=1.5,cex.axis=1.3), error=function(e) {})
     tryCatch(plot(df_sub[[CO2_name]], df_sub$fCO2, ylab = expression("fCO"[2]*" ["*mu*"atm]"), xlab = expression("xCO"[2]*" ["*mu*"atm]"), cex.lab=1.5,cex.axis=1.3), error=function(e) {})
      legend("bottomright", "b)", bty="n", cex=2.5) 
     dev.off()
    }
 
 
    
 #------------------------------------------------
# CALCULATE AND PLOT OXYGEN
 if (is_O2==TRUE) {
 
    # Plot measured O2 vs time   
    png(paste(output_dir, "/", "3_measured_O2_time_", "plot.png", sep=""))
    par(mar=c(5,5,2,2))
      tryCatch(plot(df_sub$date.time, df_sub[[O2_name]], ylab = expression("O"[2]*" ["*mu*"mol/l]"), xlab = "Time", cex.lab=1.5,cex.axis=1.3), error=function(e) {})

    #tryCatch(plot(df_sub$date.time, df_sub[[O2_name]], ylab = expression("O"[2]*" ["*mu*"mol/kg]"), xlab = "Time", cex.lab=1.5,cex.axis=1.3), error=function(e) {})
    # tryCatch(plot(df_sub$date.time, df_sub[[O2_name]], ylab = expression("O"[2]*" [ml/l]"), ylim=c(4,6), xlab = "Time", cex.lab=1.5,cex.axis=1.3), error=function(e) {})
     #tryCatch(plot(df_sub$date.time, df_sub[[O2_name]], ylab = expression("O"[2]*" ["*mu*"M]"), xlab = "Time", cex.lab=1.5,cex.axis=1.3), error=function(e) {})
    #legend(letter_position, O2_letter, bty="n", cex=2.5) 
    legend(letter_position, O2_letter, bty="n", cex=2.5) 
    dev.off()
  
    
    
    if (O2_calculations == "yes" ) {
      # Calculate oxygen:
      
      # First, set the constants:
      B0 <- c(-6.24097e-3)
      B1 <- c(-6.93498e-3)
      B2 <- c(-6.90358e-3)
      B3 <- c(-4.29155e-3)
      C0 <- c(-3.11680e-7)
      
      oxy <- rep(0, length(df_sub[,1]))
      for (j in 1:length(oxy)) {
        Ts <- log((298.15-df_sub[[SST_name]][j])/(273.15+df_sub[[SST_name]][j]))
        exponent <- ( df_sub[[sal_name]][j]*((B0)+(B1*Ts)+(B2*(Ts^2))+(B3*(Ts^3))))+(C0*((df_sub[[sal_name]][j])^2))
        oxy[j] <- df_sub[[O2_name]][j]*exp(exponent)                   
      }
      # Add to dataset
      df_sub$calc_O2 <- oxy
    
      # Plot calculated O2 vs time   
      png(paste(output_dir, "/", "4_O2_time_", "plot.png", sep=""))
      par(mar=c(5,5,2,2))
      tryCatch(plot(df_sub$date.time, df_sub$calc_O2, ylab = expression("O"[2]*" ["*mu*"M]"), xlab = "Time", cex.lab=1.5,cex.axis=1.3), error=function(e) {})
      legend("topright", "e)", bty="n", cex=2.5) 
      dev.off()
    
    
      # Plot calculate O2 vs measured O2 
      png(paste(output_dir, "/", "5_O2_meas_vs_calc_", "plot.png", sep=""))
      par(mar=c(5,5,2,2))
      tryCatch(plot(df_sub[[O2_name]], df_sub$calc_O2, ylab = expression("Calculated O"[2]*" ["*mu*"M]"), xlab = expression("Measured O"[2]*" ["*mu*"M]"), cex.lab=1.5,cex.axis=1.3), error=function(e) {})
      legend("bottomright", "b)", bty="n", cex=2.5) 
      dev.off()
    }    
    
 
 
 }
 





 #------------------------------------------------
# CALCULATE AND PLOT pH
 
 if (is_pH==TRUE) {
 
    # Calculate pH:
    pH_final <- rep(0, length(df_sub[,1]))
    for (k in 1:length(pH_final)) {
      bracket <- ((1/(273.4+(df_sub[[SST_name]][k])))-(1/(273.15+(df_sub[[pH_temp_name]][k]))))
      pH_final[k] <- (df_sub[[pH_name]][k])+(1245.69*bracket)+(0.0021*(35-(df_sub[[sal_name]][k])))     
    }
    # Add to dataset
    df_sub$calc_pH <- pH_final

    
    # Plot measured pH vs time
    png(paste(output_dir, "/", "6_measured_pH_time_", "plot.png", sep=""))
    par(mar=c(5,5,2,2))
#    tryCatch(plot(df_sub$date.time, df_sub[[pH_name]], ylab = "pH", xlab = "Time", cex.lab=1.5,cex.axis=1.3), error=function(e) {})
    tryCatch(plot(df_sub$date.time, df_sub[[pH_name]], ylab = "pH", xlab = "Time", 
                  ylim=c(7.5,8.5), cex.lab=1.5,cex.axis=1.3), error=function(e) {})
    legend(letter_position, pH_letter, bty="n", cex=2.5) 
    dev.off()
    
    # Plot calculated pH vs time
    png(paste(output_dir, "/", "7_pH_time_", "plot.png", sep=""))
    par(mar=c(5,5,2,2))
 #   tryCatch(plot(df_sub$date.time, df_sub$calc_pH, ylab = "pH", xlab = "Time", cex.lab=1.5,cex.axis=1.3), error=function(e) {})
    tryCatch(plot(df_sub$date.time, df_sub$calc_pH, ylab = "pH", xlab = "Time", 
                  ylim=c(7.5,8.5),cex.lab=1.5,cex.axis=1.3), error=function(e) {})
    legend("bottomright", "a)", bty="n", cex=2.5) 
    dev.off()
 
    # Plot calculated pH vs measured pH 
    png(paste(output_dir, "/", "8_pH_meas_vs_calc_", "plot.png", sep=""))
    par(mar=c(5,5,2,2))
 #   tryCatch(plot(df_sub[[pH_name]], df_sub$calc_pH, ylab = "Calculated pH", xlab = "Measured pH", cex.lab=1.5,cex.axis=1.3), error=function(e) {})
    tryCatch(plot(df_sub[[pH_name]], df_sub$calc_pH, 
                  ylab = "Calculated pH", xlab = "Measured pH", 
                  ylim=c(7.5,8.5), xlim=c(7.5,8.5),
                  cex.lab=1.5,cex.axis=1.3), error=function(e) {})
    legend("bottomright", "b)", bty="n", cex=2.5) 
    dev.off()
  
    
  
    
  }
  
  
 # }
  


# Write the new data frame out in new file.
out_file <- paste(output_dir, "/", input_files[1],"_fco2.txt", sep="")
write.csv(df_sub, file=out_file, row.names=FALSE, fileEncoding="UTF8")


#-------------------------
# Plot pressure for figure with measurements
if (is_pressure==TRUE) {
  png(paste(output_dir, "/", "9_pressure_", "plot.png", sep=""))
  par(mar=c(5,5,2,2))
  tryCatch(plot(df_sub$date.time, df_sub[[pressure_name]], ylab = "Pressure [dbar]", xlab = "Time", cex.lab=1.5,cex.axis=1.3), error=function(e) {})
  legend(letter_position, press_letter, bty="n", cex=2.5) 
  dev.off()
}

# Plot depth if provided
if (is_depth==TRUE) {
  png(paste(output_dir, "/", "9_depth_", "plot.png", sep=""))
  par(mar=c(5,5,2,2))
  if (is.numeric(fco2_ylim_min)) {
    tryCatch(plot(df_sub$date.time, df_sub[[depth_name]], ylab = "Depth [m]", xlab = "Time", cex.lab=1.5,cex.axis=1.3, ylim=c(depth_ylim_min, depth_ylim_max)), error=function(e) {})
  } else {
    tryCatch(plot(df_sub$date.time, df_sub[[depth_name]], ylab = "Depth [m]", xlab = "Time", cex.lab=1.5,cex.axis=1.3), error=function(e) {})
  }
  
  legend(letter_position, press_letter, bty="n", cex=2.5) 
  dev.off()
}

#-------------------------
# Plot conductivity for figure with measurements
if (is_cond == TRUE) {
  png(paste(output_dir, "/", "10_conductivity_", "plot.png", sep=""))
  par(mar=c(5,5,2,2))
  tryCatch(plot(df_sub$date.time, df_sub[[cond_name]], ylab = "Conductivity [S/m]", xlab = "Time", ylim = c(4.5,6.5), cex.lab=1.5,cex.axis=1.3), error=function(e) {})
  legend(letter_position, cond_letter, bty="n", cex=2.5) 
  dev.off()
}



#-------------------------
# Print numbers of values outside plot area to console

# fCO2
if(!is.na(fco2_ylim_min)) {
  outlier_fco2_min <- sum(na.omit(df_sub$fCO2) < fco2_ylim_min)
  percent_outlier_fco2_min <- round((outlier_fco2_min/nrow(df_sub))*100,2)
  cat("\n", "Number of fco2 lower than ", fco2_ylim_min, ": ", outlier_fco2_min, " (", percent_outlier_fco2_min, "%)", sep="")
}
if(!is.na(fco2_ylim_max)) {
  outlier_fco2_max <- sum(na.omit(df_sub$fCO2) > fco2_ylim_max)
  percent_outlier_fco2_max <- round((outlier_fco2_max/nrow(df_sub))*100,2)
  cat("\n", "Number of fco2 higher than ", fco2_ylim_max, ": ", outlier_fco2_max, " (", percent_outlier_fco2_max, "%)", sep="")
}

# Depth
if(!is.na(depth_ylim_min)) {
  outlier_depth_min <- sum(na.omit(df_sub[[depth_name]]) < depth_ylim_min)
  percent_outlier_depth_min <- round((outlier_depth_min/nrow(df_sub))*100,2)
  cat("\n", "Number of depth lower than ", depth_ylim_min, ": ", outlier_depth_min, " (", percent_outlier_depth_min, "%)", sep="")
}
if(!is.na(depth_ylim_max)) {
  outlier_depth_max <- sum(na.omit(df_sub[[depth_name]]) > depth_ylim_max)
  percent_outlier_depth_max <- round((outlier_depth_max/nrow(df_sub))*100,2)
  cat("\n", "Number of depth higher than ", depth_ylim_max, ": ", outlier_depth_max, " (", percent_outlier_depth_max, "%)", sep="")
}


#-------------------------
# Some additional plots to check what is going on with pH

#png(paste(output_dir, "/", "7a_pH_SST_", "plot.png", sep=""))
#par(mar=c(5,5,2,2))
#plot(df_sub$SST,df_sub$pH)
#dev.off()


#png(paste(output_dir, "/", "7b_pH_vs_pH_temp2_", "plot.png", sep=""))
#par(mar=c(5,5,2,2))
#df_subset <- subset(df_sub, SST>15.4 & SST<16.0) 
#plot(df_subset$pH, df_subset$calc_pH, ylim =c(7.82,7.92), xlim=c(7.7,8.1), main="For temperature between 15.4 and 16.0 ")
#dev.off()


#png(paste(output_dir, "/", "7c_pH_vs_pH_temp1_", "plot.png", sep=""))
#par(mar=c(5,5,2,2))
#df_subset <- subset(df_sub, SST>16.8 & SST<17.5) 
#plot(df_subset$pH, df_subset$calc_pH, ylim =c(7.82,7.92), xlim=c(7.7,8.1), main="For temperature between 16.8 and 17.5 ")
#dev.off()


#png(paste(output_dir, "/", "7d_pH_vs_pH_temp3_", "plot.png", sep=""))
#par(mar=c(5,5,2,2))
#df_subset <- subset(df_sub, SST>19.0 & SST<20) 
#plot(df_subset$pH, df_subset$calc_pH, ylim =c(7.82,7.92), xlim=c(7.7,8.1), main="For temperature between 19.0 and 20.0 ")
#dev.off()


#-------------------------
## Plotting ph.temp showed that this is constant for a long time. Here I plot pH calc vs time, and pH meas vs. PH calc withouth the 
## bad pH temp rows. 
#png(paste(output_dir, "/", "8a_pH_temp_vs_time_", "plot.png", sep=""))
#par(mar=c(5,5,2,2))
#plot(df_sub$date.time, df_sub[[pH_temp_name]])
#dev.off()

#png(paste(output_dir, "/", "8b_pH_calc_vs_time_", "plot.png", sep=""))
#par(mar=c(5,5,2,2))
#df_subset <- subset(df_sub, df_sub[[pH_temp_name]] < 8.0 ) 
#plot(df_subset$date.time, df_subset$calc_pH)
#legend("topright", "c)", bty="n", cex=2.5) 
#dev.off()

#png(paste(output_dir, "/", "8c_pH_calc_vs_pH_meas_", "plot.png", sep=""))
#par(mar=c(5,5,2,2))
#plot(df_subset[[pH_name]], df_subset$calc_pH)
#legend("topright", "d)", bty="n", cex=2.5) 
#dev.off()


#-------------------------
# On request from Ingunn, remove the data when pH temp is not 20, and plot calc ph vs meas pH (fig 6)
# and meas ph vs SST (fig 7a)


#df_subset <- subset(df_sub, pH.Temp > 15 ) 

# Plot calculated pH vs measured pH 
#png(paste(output_dir, "/", "new_6_pH_meas_vs_calc_", "plot.png", sep=""))
#par(mar=c(5,5,2,2))
#tryCatch(plot(df_subset[[pH_name]], df_subset$calc_pH, ylab = "Calculated pH", xlab = "Measured pH", cex.lab=1.5,cex.axis=1.3), error=function(e) {})
#legend("topright", "b)", bty="n", cex=2.5) 
#dev.off()

#png(paste(output_dir, "/", "new_7a_pH_SST_", "plot.png", sep=""))
#par(mar=c(5,5,2,2))
#plot(df_subset$SST,df_subset$pH, ylab = "measured pH", xlab = "SST")
#dev.off()



#png(paste(output_dir, "/", "new_5_pH_time_", "plot.png", sep=""))
#par(mar=c(5,5,2,2))
#tryCatch(plot(df_subset$date.time, df_subset$calc_pH, ylab = "pH", xlab = "Time", cex.lab=1.5,cex.axis=1.3), error=function(e) {})
#legend("topright", "a)", bty="n", cex=2.5) 
#dev.off()

#png(paste(output_dir, "/", "new_measured_pH_time_", "plot.png", sep=""))
#par(mar=c(5,5,2,2))
#tryCatch(plot(df_subset$date.time, df_subset[[pH_name]], ylab = "pH", xlab = "Time", cex.lab=1.5,cex.axis=1.3), error=function(e) {})
#legend("topright", "g)", bty="n", cex=2.5) 
#dev.off()


