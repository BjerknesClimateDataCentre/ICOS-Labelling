##################
## PLOTS different indicators (e.g. delta T)
## For VOS only

#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
# Input parameters:
#if (!input_from_main) {

letter_location2 <- "bottomleft"
deltaT_letter <- "a)"               # Set to "a)" for report and "b)" for executive summary 

specify_xlabel <- FALSE 

# REMEMEBER TO CHAGNE THE TEMP PLOT LABEL MANUALLY ACCORDING TO STATION TYPE:
# FOR FOS - Sea Surface Temperature
# FOR SOOP - Intake Temperature

#}
#-----------------
# Consider to change these axis ranges after viewing plots.

deltaT_ylim_min <- NA
deltaT_ylim_max <- NA

TvsT_xlim_min <- NA
TvsT_xlim_max <- NA
TvsT_ylim_min <- NA
TvsT_ylim_max <- NA


fCO2_ylim_min <- NA
fCO2_ylim_max <- NA

CO2vsCO2_xlim_min <- NA
CO2vsCO2_xlim_max <- NA
CO2vsCO2_ylim_min <- NA
CO2vsCO2_ylim_max <- NA


#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

# Remove old figures produced by this script from the output directory
images <- list.files(INPUT_DIR, pattern="^[6-9].*png$",)
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
cat("\r", input_files[1], "               ")
in_file <- paste(INPUT_DIR, "/", input_files[1], sep="")
data <- read.csv(in_file,header=T, fileEncoding="UTF8")
  
  
dates <- as.POSIXlt(data[["Date.Time"]], "%Y-%m-%dT%H:%M:%S.000Z", tz="UTC")
data$dates <- dates 
  
# Make subset of data where only flag 2 are included.
data_sub <- subset(data, fCO2..uatm..QC.Flag==2)#
#data_sub <- data

#data_sub$fCO2[data_sub$fCO2>600] <- NA 
#data_sub$CO2..measured.[data_sub$CO2..measured.>600] <- NA 

#-----------------------------------------------------------------
## TEMPERATURE PLOTS (if eqTemp is measured)

if(plot_eqTemp == TRUE) {
  
  ## PLOT OF DELTA TEMPERATURE:
  if (is.numeric(deltaT_ylim_min)) {
    output_file_name <-  paste(OUTPUT_DIR, "/", "6.SecInd_Delta_temp_", "plot_good-values_own-range.png", sep="")
    deltaT_ylims <- c(deltaT_ylim_min,deltaT_ylim_max)
  } else if ((max(na.omit(data_sub[["Water.Equilibrator.Temperature.Difference...C....C."]])) > 20) || (min(na.omit(data_sub[["Water.Equilibrator.Temperature.Difference...C....C."]])) < -20)) {
    output_file_name <-  paste(OUTPUT_DIR, "/", "6.SecInd_Delta_temp_", "plot_good-values_questionable-range.png", sep="")
    deltaT_ylims <- c(-20,20) 
  } else {
    output_file_name <- paste(OUTPUT_DIR, "/", "6.SecInd_Delta_temp_", "plot_good-values.png", sep="")
    deltaT_ylims <- c(min(na.omit(data_sub[["Water.Equilibrator.Temperature.Difference...C....C."]])),max(na.omit(data_sub[["Water.Equilibrator.Temperature.Difference...C....C."]])))  
  }
  png(output_file_name)
  par(mar=c(5,5,2,2))
  
  if (specify_xlabel == TRUE) {
    tryCatch(plot(data_sub[["dates"]], data_sub[["Water.Equilibrator.Temperature.Difference...C....C."]], ylab = expression(paste(Delta, "Temperature [",degree,"C]")), xlab = "Time", ylim = deltaT_ylims, cex.lab=1.5, cex.axis=1.3, xaxt='n'), error=function(e) {}) 
    ticks.at <- seq(min(data_sub[["dates"]]), max(data_sub[["dates"]]), by = "months")
    ticks.lab <- format(ticks.at, format = "%b")
    axis(1, at = ticks.at, labels = ticks.lab, cex.lab=1.5, cex.axis=1.3)
  } else {
    tryCatch(plot(data_sub[["dates"]], data_sub[["Water.Equilibrator.Temperature.Difference...C....C."]], ylab = expression(paste(Delta, "Temperature [",degree,"C]")), xlab = "Time", ylim = deltaT_ylims, cex.lab=1.5, cex.axis=1.3), error=function(e) {}) 
  }
  
  legend(letter_location2, deltaT_letter, bty="n", cex=2.5)
  dev.off()
  
  
  
  ## PLOT INTAKE TEMP VS EQU TEMP
  if(is.numeric(TvsT_xlim_min)) {
    output_file_name <- paste(OUTPUT_DIR, "/", "7.SecInd_EqT_vs_SST_", "plot_good-values_own-range.png", sep="")
    TvsT_xlims <- c(TvsT_xlim_min,TvsT_xlim_max) 
    TvsT_ylims <- c(TvsT_ylim_min,TvsT_ylim_max)
  } else {
    output_file_name <- paste(OUTPUT_DIR, "/", "7.SecInd_EqT_vs_SST_", "plot_good-values.png", sep="")
    TvsT_xlims <- c(max(min(na.omit(data_sub[["Temp..degC....C."]])),-10),min(max(na.omit(data_sub[["Temp..degC....C."]])),50))
    TvsT_ylims <- c(max(min(na.omit(data_sub[["Temperature.of.Equilibration..degC....C."]])),-10),min(max(na.omit(data_sub[["Temperature.of.Equilibration..degC....C."]])),50))
  }
  png(output_file_name)
  par(mar=c(5,5,2,2))
  tryCatch(plot(data_sub[["Temp..degC....C."]], data_sub[["Temperature.of.Equilibration..degC....C."]], ylab = expression(paste("Equilibrator Temperature [",degree,"C]")), xlab = expression(paste("Intake Temperature [",degree,"C]")), ylim=TvsT_ylims, xlim=TvsT_xlims, cex.lab=1.5, cex.axis=1.3), error=function(e) {})
  legend(letter_location2, "b)", bty="n", cex=2.5)
  dev.off()
}  


#-----------------------------------------------------------------
## CO2 PLOTS


## PLOT OF FCO2:
if (is.numeric(CO2vsCO2_xlim_min)) {
   output_file_name <- paste(OUTPUT_DIR, "/", "8.SecInd_fCO2_calculated_", "plot_good-values_own-range.png", sep="")
   fCO2_ylims <- c(fCO2_ylim_min, fCO2_ylim_max)
} else if ((max(na.omit(data_sub[["fCO2..uatm...uatm."]])) > 1200) || (min(na.omit(data_sub[["fCO2..uatm...uatm."]])) < 80)) {
   output_file_name <- paste(OUTPUT_DIR, "/", "8.SecInd_fCO2_calculated_", "plot_good-values_questionable-range.png", sep="")
   fCO2_ylims <- c(80,1200)
} else {
   output_file_name <- paste(OUTPUT_DIR, "/", "8.SecInd_fCO2_calculated_", "plot_good_values.png", sep="")
   fCO2_ylims <- c(min(na.omit(data_sub[["fCO2..uatm...uatm."]])),max(na.omit(data_sub[["fCO2..uatm...uatm."]])))
} 
png(output_file_name)
par(mar=c(5,5,2,2))
if (specify_xlabel == TRUE) {
  tryCatch(plot(data_sub[["dates"]], data_sub[["fCO2..uatm...uatm."]], ylab = expression("fCO"[2]*" ["*mu*"atm]"), xlab = "Time", ylim = fCO2_ylims, cex.lab=1.5, cex.axis=1.3, xaxt='n'), error=function(e) {}) 
  ticks.at <- seq(min(data_sub[["dates"]]), max(data_sub[["dates"]]), by = "months")
  ticks.lab <- format(ticks.at, format = "%b")
  axis(1, at = ticks.at, labels = ticks.lab, cex.lab=1.5, cex.axis=1.3)
} else {
  tryCatch(plot(data_sub[["dates"]], data_sub[["fCO2..uatm...uatm."]], ylab = expression("fCO"[2]*" ["*mu*"atm]"), xlab = "Time", ylim = fCO2_ylims, cex.lab=1.5, cex.axis=1.3), error=function(e) {}) 
}
legend(letter_location2, "a)", bty="n", cex=2.5)
dev.off()


  
## PLOT MEASURED VS CALCULATED CO2
if(is.numeric(CO2vsCO2_xlim_min)) {
  output_file_name <- paste(OUTPUT_DIR, "/", "9.SecInd_co2_meas_vs_calc_", "plot_good-values_own-range.png", sep="")
  CO2vsCO2_xlims <- c(CO2vsCO2_xlim_min, CO2vsCO2_xlim_max)
  CO2vsCO2_ylims <- c(CO2vsCO2_ylim_min, CO2vsCO2_ylim_max)
} else {
  output_file_name <- paste(OUTPUT_DIR, "/", "9.SecInd_co2_meas_vs_calc_", "plot_good-values.png", sep="")
  CO2vsCO2_xlims <- c(max(min(na.omit(data_sub[[xco2_colname]])),80),min(max(na.omit(data_sub[[xco2_colname]])),1200))
  CO2vsCO2_ylims <- c(max(min(na.omit(data_sub[["fCO2..uatm...uatm."]])),80),min(max(na.omit(data_sub[["fCO2..uatm...uatm."]])),1200))
}
png(output_file_name)
par(mar=c(5,5,2,2))
tryCatch(plot(data_sub[[xco2_colname]], data_sub[["fCO2..uatm...uatm."]], ylab = expression("fCO"[2]*" ["*mu*"atm]"), xlab = expression("xCO"[2]*" [ppm]"), ylim = CO2vsCO2_ylims, xlim = CO2vsCO2_xlims, cex.lab=1.5, cex.axis=1.3), error=function(e) {})
legend(letter_location2, "b)", bty="n", cex=2.5)
dev.off()


#}

#cat("\n")
