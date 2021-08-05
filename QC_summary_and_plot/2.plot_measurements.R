##################
## PLOTS ALL MEASUREMENTS VS TIME
## For VOS and FOS


#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
# Input parameters:

# Chose which parameters to plot. All core parameters must be plotted if they exist and make sense (do not plot constants).
#check.var("plot_SST", TRUE)                      
#if (!input_from_main) {

plot_SST <- TRUE                      # Core for VOS and FOS
plot_eqTemp <- TRUE                   # Core for VOS
plot_sal <- TRUE                     # Core for FOS
plot_eqPress <- TRUE                  # Core for VOS
plot_xCO2sw <- TRUE                   # xCO2 or pCO2 is core for VOS and FOS
plot_pCO2sw <- FALSE
plot_DepthPressure <- FALSE           # Pressure (depth) is plotted for FOS, but in another script. The reason we need to know this here is for the plot lettering

letter_location <- "bottomleft"      # Alternatives are "bottomright", "bottomleft", "topleft", "topright"

fix_xaxis <- FALSE                   # Set to true if the xaxis only shows year and you want ticks for months instead
                                      # Do not use this one! Only use true if x-label distances does not make sense.

xco2_colname <- 'CO2.um.m..umol.mol.1.'   # The column name of the raw xco2 is what we want to plot

# REMEMEBER TO CHAGNE THE TEMP PLOT LABEL MANUALLY ACCORDING TO STATION TYPE:
# FOR FOS - Sea Surface Temperature
# FOR SOOP - Intake Temperature

#}




#-----------------s
# Consider to change axis ranges after viewing plots.
# If these are NA, axis max and min are data max and min, as long as these are higher/lower than questionable range.
# Values below overwrites the lim's in the rest of the script.
# Need to assign for both min and max!
 
                                               #  As a reference, here are questionable ranges:
SST_ylim_min <- 13                             # SST -10:50
SST_ylim_max <-  26
  
eqTemp_ylim_min <- 13                       # eqTemp -10:50
eqTemp_ylim_max <- 26

sal_ylim_min <- 35.5                            # sal 0:50
sal_ylim_max <- 38.5

eqPress_ylim_min <- 990                         # eqPress 750:1250
eqPress_ylim_max <- 1035

xCO2_ylim_min <- 330                        # xCO2 80:1200
xCO2_ylim_max <- 465


pCO2_ylim_min <- NA                          # pCO2 80:1200
pCO2_ylim_max <- NA

  
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

# Remove old figures produced by this script from the output directory
images <- list.files(INPUT_DIR, pattern="^[1-5].*png$",)
for (image_loop in 1:length(images)) {
  image <- paste(INPUT_DIR, "/", images[image_loop], sep="")
  file.remove(image)
}

Sys.setlocale("LC_ALL", "English"); 
library(ncdf4)


# File locations
INPUT_DIR <- "output"
OUTPUT_DIR <- "output"

# Get the files in the input directory
input_files <- list.files(INPUT_DIR, pattern="csv$")

#for (file_loop in 1:length(input_files)) {
  # Load the data
  cat("\r", input_files[1], "               ")
  in_file <- paste(INPUT_DIR, "/", input_files[1], sep="")
  data <- read.csv(in_file,header=T, fileEncoding="UTF8")
  
  dates <- as.POSIXlt(data[["Date.Time"]], "%Y-%m-%dT%H:%M:%S.000Z", tz="UTC")

  


  # Create lettering counter
  letters <- c("a)", "b)", "c)", "d)", "e)", "f)")
  count <- 0

  
  ## PLOT INTAKE TEMPERATURE:
  if(plot_SST == TRUE) {
    count <- count + 1   
    if (is.numeric(SST_ylim_min)) {
      output_file_name <- paste(OUTPUT_DIR, "/", "1.Intake_temp_", "plot_own-range.png", sep="")
      SST_ylims <- c(SST_ylim_min,SST_ylim_max)
    } else if ((max(na.omit(data[["Temp..degC....C."]])) > 50) || (min(na.omit(data[["Temp..degC....C."]])) < -10)) {

      output_file_name <- paste(OUTPUT_DIR, "/", "1.Intake_temp_", "plot_bad-range.png", sep="")
      SST_ylims <- c(-10,50)
    } else {
      output_file_name <- paste(OUTPUT_DIR, "/", "1.Intake_temp_", "plot.png", sep="")
      SST_ylims <- c(min(na.omit(data[["Temp..degC....C."]])), max(na.omit(data[["Temp..degC....C."]])))
    }    
    png(output_file_name)
    par(mar=c(5,5,2,2))
    
    # Adjust x-axis if requested in the unput
    if (fix_xaxis==FALSE) {
      tryCatch(plot(dates, data[["Temp..degC....C."]], ylab = expression(paste("Intake Temperature [",degree,"C]")), xlab = "Time", ylim = SST_ylims, cex.lab=1.5,cex.axis=1.3), error=function(e) {}) 
    } else {
      tryCatch(plot(dates, data[["Temp..degC....C."]], ylab = expression(paste("Intake Temperature [",degree,"C]")), xlab = "Time", ylim = SST_ylims, cex.lab=1.5, cex.axis=1.3, xaxt='n'), error=function(e) {}) 
      ticks.at <- seq(min(dates), max(dates), by = "months")
      ticks.lab <- format(ticks.at, format = "%b")
      axis(1, at = ticks.at, labels = ticks.lab, cex.lab=1.5, cex.axis=1.3)
    }
    

    legend(letter_location, letters[count], bty="n", cex=2.5)
    dev.off()
  }
  # If manually edit the axis ranges, give number of outliers not plotted in console
  if(!is.na(SST_ylim_min)) {
    outlier_SST_min <- sum(na.omit(data$Temp..degC....C.) < SST_ylim_min)

    percent_outlier_SST_min <- round((outlier_SST_min/nrow(data))*100,2)
    cat("\n", "Number of SST lower than ", SST_ylim_min, ": ", outlier_SST_min, " (", percent_outlier_SST_min, "%)", sep="")
  }
  if(!is.na(SST_ylim_max)) {
    outlier_SST_max <- sum(na.omit(data$Temp..degC....C.) > SST_ylim_max)

    percent_outlier_SST_max <- round((outlier_SST_max/nrow(data))*100,2)
    cat("\n", "Number of SST higher than ", SST_ylim_max, ": ", outlier_SST_max, " (", percent_outlier_SST_max, "%)", sep="")
  }
  
 

  ## PLOT OF EQUILIBRATOR TEMPERATURE:
  if (plot_eqTemp == TRUE) {
    count <- count + 1   
    if (is.numeric(eqTemp_ylim_min)) {
      output_file_name <- paste(OUTPUT_DIR, "/", "2.Equil_temp_", "plot_own-range.png", sep="")
      eqTemp_ylims <- c(eqTemp_ylim_min, eqTemp_ylim_max)
    } else if ((max(na.omit(data[["Temperature.of.Equilibration..degC....C."]])) > 50) || (min(na.omit(data[["Temperature.of.Equilibration..degC....C."]])) < -10)) {
      output_file_name <- paste(OUTPUT_DIR, "/", "2.Equil_temp_", "plot_bad-range.png", sep="")
      eqTemp_ylims <- c(-10,50)
    } else {
      output_file_name <- paste(OUTPUT_DIR, "/", "2.Equil_temp_", "plot.png", sep="")
      eqTemp_ylims <- c(min(na.omit(data[["Temperature.of.Equilibration..degC....C."]])),max(na.omit(data[["Temperature.of.Equilibration..degC....C."]])))
    }  
    png(output_file_name)
    par(mar=c(5,5,2,2))
     
    
    # Adjust x-axis if requested in the unput
    if (fix_xaxis==FALSE) {
      tryCatch(plot(dates, data[["Temperature.of.Equilibration..degC....C."]], ylab = expression(paste("Equilibrator Temperature [",degree,"C]")), xlab = "Time", ylim = eqTemp_ylims, cex.lab=1.5,cex.axis=1.3), error=function(e) {}) 
    } else {
      tryCatch(plot(dates, data[["Temperature.of.Equilibration..degC....C."]], ylab = expression(paste("Equilibrator Temperature [",degree,"C]")), xlab = "Time", ylim = eqTemp_ylims, cex.lab=1.5,cex.axis=1.3,  xaxt='n' ), error=function(e) {}) 
      ticks.at <- seq(min(dates), max(dates), by = "months")
      ticks.lab <- format(ticks.at, format = "%b")
      axis(1, at = ticks.at, labels = ticks.lab, cex.lab=1.5, cex.axis=1.3)
    }
    

    legend(letter_location, letters[count], bty="n", cex=2.5)
    dev.off()
  }
  # If manually edit the axis ranges, give number of outliers not plotted in console
  if(!is.na(eqTemp_ylim_min)) {
    outlier_eqTemp_min <- sum(na.omit(data$Temperature.of.Equilibration..degC....C.) < eqTemp_ylim_min)

    percent_outlier_eqTemp_min <- round((outlier_eqTemp_min/nrow(data))*100,2)
    cat("\n", "Number of eqTemp lower than ", eqTemp_ylim_min, ": ", outlier_eqTemp_min, " (", percent_outlier_eqTemp_min, "%)", sep="")
  }
  if(!is.na(eqTemp_ylim_max)) {
    outlier_eqTemp_max <- sum(na.omit(data$Temperature.of.Equilibration..degC....C.) > eqTemp_ylim_max)

    percent_outlier_eqTemp_max <- round((outlier_eqTemp_max/nrow(data))*100,2)
    cat("\n", "Number of eqTemp higher than ", eqTemp_ylim_max, ": ", outlier_eqTemp_max, " (", percent_outlier_eqTemp_max, "%)", sep="")
  }
  

  ## PLOT OF SALINITY: 
  if (plot_sal == TRUE) {
    count <- count + 1 
    if (is.numeric(sal_ylim_min)) {
      output_file_name <- paste(OUTPUT_DIR, "/", "3.Salinity_", "plot_own-range.png", sep="")
      sal_ylims <- c(sal_ylim_min, sal_ylim_max)
   } else if ((max(na.omit(data[["P_sal..psu...psu."]])) > 50) || (min(na.omit(data[["P_sal..psu...psu."]])) < 0)) {
     output_file_name <- paste(OUTPUT_DIR, "/", "3.Salinity_", "plot_bad-range.png", sep="")
     sal_ylims <- c(0,50)
   } else { 
     output_file_name <- paste(OUTPUT_DIR, "/", "3.Salinity_", "plot.png", sep="")
     sal_ylims <- c(min(na.omit(data[["P_sal..psu...psu."]])), max(na.omit(data[["P_sal..psu...psu."]])))
   }
  png(output_file_name)
  par(mar=c(5,5,2,2))
  
  # Adjust x-axis if requested in the unput
  if (fix_xaxis==FALSE) {
    tryCatch(plot(dates, data[["P_sal..psu...psu."]], ylab = "Salinity [PSU]", xlab = "Time", ylim = sal_ylims, cex.lab=1.5,cex.axis=1.3), error=function(e) {})
  } else {
    tryCatch(plot(dates, data[["P_sal..psu...psu."]], ylab = "Salinity [PSU]", xlab = "Time", ylim = sal_ylims, cex.lab=1.5,cex.axis=1.3,  xaxt='n' ), error=function(e) {})
    ticks.at <- seq(min(dates), max(dates), by = "months")
    ticks.lab <- format(ticks.at, format = "%b")
    axis(1, at = ticks.at, labels = ticks.lab, cex.lab=1.5, cex.axis=1.3)
  }
  

  legend(letter_location, letters[count], bty="n", cex=2.5)
  dev.off()
  }
  # If manually edit the axis ranges, give number of outliers not plotted in console
  if(!is.na(sal_ylim_min)) {
    outlier_sal_min <- sum(na.omit(data$P_sal..psu...psu.) < sal_ylim_min)

    percent_outlier_sal_min <- round((outlier_sal_min/nrow(data))*100,2)
    cat("\n", "Number of salinity lower than ", sal_ylim_min, ": ", outlier_sal_min, " (", percent_outlier_sal_min, "%)", sep="")
  }
  if(!is.na(sal_ylim_max)) {
    outlier_sal_max <- sum(na.omit(data$P_sal..psu...psu.) > sal_ylim_max)

    percent_outlier_sal_max <- round((outlier_sal_max/nrow(data))*100,2)
    cat("\n", "Number of salinity higher than ", sal_ylim_max, ": ", outlier_sal_max, " (", percent_outlier_sal_max, "%)", sep="")
  }
    
  
  
  ## PLOT OF EQUILIBRATOR PRESSURE:
  if (plot_eqPress == TRUE) {
    count <- count + 1
    if (is.numeric(eqPress_ylim_min)) {
      output_file_name <- paste(OUTPUT_DIR, "/", "4.Equil_press", "plot_own-range.png", sep="")
      eqPress_ylims <- c(eqPress_ylim_min,eqPress_ylim_max)
  } else if ((max(na.omit(data[["Pressure.in.Equilibrator..hPa."]])) > 1250) || (min(na.omit(data[["Pressure.in.Equilibrator..hPa."]])) < 750)) {

      output_file_name <- paste(OUTPUT_DIR, "/", "4.Equil_press", "plot_bad-range.png", sep="")
      eqPress_ylims <- c(750,1250)
   } else {
      output_file_name <- paste(OUTPUT_DIR, "/", "4.Equil_press", "plot.png", sep="")
      eqPress_ylims <- c(min(na.omit(data[["Pressure.in.Equilibrator..hPa."]])),max(na.omit(data[["Pressure.in.Equilibrator..hPa."]])))
    }
  png(output_file_name)
  par(mar=c(5,5,2,2))
  
  # Adjust x-axis if requested in the unput
  if (fix_xaxis==FALSE) {
    tryCatch(plot(dates, data[["Pressure.in.Equilibrator..hPa."]], ylab = "Equilibrator Pressure [mbar]", xlab = "Time", ylim = eqPress_ylims, cex.lab=1.5,cex.axis=1.3), error=function(e) {}) 
  } else {
    tryCatch(plot(dates, data[["Pressure.in.Equilibrator..hPa."]], ylab = "Equilibrator Pressure [mbar]", xlab = "Time", ylim = eqPress_ylims, cex.lab=1.5,cex.axis=1.3, xaxt ='n'), error=function(e) {}) 
    ticks.at <- seq(min(dates), max(dates), by = "months")
    ticks.lab <- format(ticks.at, format = "%b")
    axis(1, at = ticks.at, labels = ticks.lab, cex.lab=1.5, cex.axis=1.3)
  }
  

  legend(letter_location, letters[count], bty="n", cex=2.5)
  dev.off()
  }
  # If manually edit the axis ranges, give number of outliers not plotted in console
  if(!is.na(eqPress_ylim_min)) {
    outlier_eqPress_min <- sum(na.omit(data$Pressure.in.Equilibrator..hPa.) < eqPress_ylim_min)

    percent_outlier_eqPress_min <- round((outlier_eqPress_min/nrow(data))*100,2)
    cat("\n", "Number of eqPress lower than ", eqPress_ylim_min, ": ", outlier_eqPress_min, " (", percent_outlier_eqPress_min, "%)", sep="")
  }
  if(!is.na(eqPress_ylim_max)) {
    outlier_eqPress_max <- sum(na.omit(data$Pressure.in.Equilibrator..hPa.) > eqPress_ylim_max)

    percent_outlier_eqPress_max <- round((outlier_eqPress_max/nrow(data))*100,2)
    cat("\n", "Number of eqPress higher than ", eqPress_ylim_max, ": ", outlier_eqPress_max, " (", percent_outlier_eqPress_max, "%)", sep="")
  }
  

  # Save one letter for the Depth plot (get this from another script using raw data)
  if(plot_DepthPressure == TRUE){
    count <-  count+1
  } 
 

  ## PLOT OF MEASURED xCO2:
  if (plot_xCO2sw == TRUE) {
    count <- count + 1
    if (is.numeric(xCO2_ylim_min)) {
      output_file_name <- paste(OUTPUT_DIR, "/", "5.CO2_measured_", "plot_own-range.png", sep="")
      xCO2_ylims <- c(xCO2_ylim_min, xCO2_ylim_max)
   } else if ((max(na.omit(data[[xco2_colname]])) > 1200) || (min(na.omit(data[[xco2_colname]])) < 80)) {

      output_file_name <- paste(OUTPUT_DIR, "/", "5.CO2_measured_", "plot_questionable-range.png", sep="")
      xCO2_ylims <- c(80,1200)
   } else {
      output_file_name <- paste(OUTPUT_DIR, "/", "5.CO2_measured_", "plot.png", sep="")
      xCO2_ylims <- c(min(na.omit(data[[xco2_colname]])),max(na.omit(data[[xco2_colname]])))
    }
   png(output_file_name)
   par(mar=c(5,5,2,2))
   
   # Adjust x-axis if requested in the input
   if (fix_xaxis==FALSE) {
     tryCatch(plot(dates, data[[xco2_colname]], ylab = expression("Sea Surface xCO"[2]*" [ppm]"), xlab = "Time", ylim = xCO2_ylims, cex.lab=1.5,cex.axis=1.3), error=function(e) {})
   } else {
     tryCatch(plot(dates, data[[xco2_colname]], ylab = expression("Sea Surface xCO"[2]*" [ppm]"), xlab = "Time", ylim = xCO2_ylims, cex.lab=1.5,cex.axis=1.3, xaxt='n'), error=function(e) {})
     ticks.at <- seq(min(dates), max(dates), by = "months")
     ticks.lab <- format(ticks.at, format = "%b")
     axis(1, at = ticks.at, labels = ticks.lab, cex.lab=1.5, cex.axis=1.3)
   }
   

   legend(letter_location, letters[count], bty="n", cex=2.5)
   dev.off()
  }
  # If manually edit the axis ranges, give number of outliers not plotted in console
  if(!is.na(xCO2_ylim_min)) {
    outlier_xCO2_min <- sum(na.omit(data$xCO2.in.Water...Calibrated..umol.mol.1.) < xCO2_ylim_min)

    percent_outlier_xCO2_min <- round((outlier_xCO2_min/nrow(data))*100,2)
    cat("\n", "Number of xCO2 lower than ", xCO2_ylim_min, ": ", outlier_xCO2_min, " (", percent_outlier_xCO2_min, "%)", sep="")
  }
  if(!is.na(xCO2_ylim_max)) {
    outlier_xCO2_max <- sum(na.omit(data$xCO2.in.Water...Calibrated..umol.mol.1.) > xCO2_ylim_max)

    percent_outlier_xCO2_max <- round((outlier_xCO2_max/nrow(data))*100,2)
    cat("\n", "Number of xCO2 higher than ", xCO2_ylim_max, ": ", outlier_xCO2_max, " (", percent_outlier_xCO2_max, "%)", sep="")
  }
  
  
  
  ## PLOT OF MEASURED pCO2:
  if (plot_pCO2sw == TRUE) {
    count <- count + 1
    if (is.numeric(pCO2_ylim_min)) {
      output_file_name <- paste(OUTPUT_DIR, "/", "5.CO2_measured_", "plot_own-range.png", sep="")
      pCO2_ylims <- c(pCO2_ylim_min,pCO2_ylim_max) 
   } else if ((max(na.omit(data[["CO2..measured."]])) > 1200) || (min(na.omit(data[["CO2..measured."]])) < 80)) {
      output_file_name <- paste(OUTPUT_DIR, "/", "5.CO2_measured_", "plot_questionable-range.png", sep="")
      pCO2_ylims <- c(80,1200)
   } else {
      output_file_name <- paste(OUTPUT_DIR, "/", "5.CO2_measured_", "plot.png", sep="")
      pCO2_ylims <- c(min(na.omit(data[["CO2..measured."]])), max(na.omit(data[["CO2..measured."]])))
   }
   png(output_file_name)
   par(mar=c(5,5,2,2))
   #tryCatch(plot(dates, data[["CO2..measured."]], ylab = expression(paste("Sea Surface pCO"[2]*" [",mu,"atm]")), xlab = "Time", ylim =  pCO2_ylims, cex.lab=1.5,cex.axis=1.3), error=function(e) {})
   
   # Adjust x-axis if requested in the input
   if (fix_xaxis==FALSE) {
     tryCatch(plot(dates, data[["CO2..measured."]], ylab = expression(paste("Sea Surface pCO"[2]*" [ppm]")), xlab = "Time", ylim =  pCO2_ylims, cex.lab=1.5,cex.axis=1.3), error=function(e) {})
   } else {
     tryCatch(plot(dates, data[["CO2..measured."]], ylab = expression(paste("Sea Surface pCO"[2]*" [ppm]")), xlab = "Time", ylim =  pCO2_ylims, cex.lab=1.5,cex.axis=1.3, xaxt='n'), error=function(e) {})
     ticks.at <- seq(min(dates), max(dates), by = "months")
     ticks.lab <- format(ticks.at, format = "%b")
     axis(1, at = ticks.at, labels = ticks.lab, cex.lab=1.5, cex.axis=1.3)
   }
   
   legend(letter_location, letters[count], bty="n", cex=2.5)

   dev.off()
  }
  # If manually edit the axis ranges, give number of outliers not plotted in console
  if(!is.na(pCO2_ylim_min)) {
    outlier_pCO2_min <- sum(na.omit(data[["CO2..measured."]]) < pCO2_ylim_min)

    percent_outlier_pCO2_min <- round((outlier_pCO2_min/nrow(data))*100,2)
    cat("\n", "Number of pCO2 lower than ", pCO2_ylim_min, ": ", outlier_pCO2_min, " (", percent_outlier_pCO2_min, "%)", sep="")
  }
  if(!is.na(pCO2_ylim_max)) {
    outlier_pCO2_max <- sum(na.omit(data[["CO2..measured."]]) > pCO2_ylim_max)

    percent_outlier_pCO2_max <- round((outlier_pCO2_max/nrow(data))*100,2)
    cat("\n", "Number of pCO2 higher than ", pCO2_ylim_max, ": ", outlier_pCO2_max, " (", percent_outlier_pCO2_max, "%)", sep="")
  }

  
#cat("\n")

#}

