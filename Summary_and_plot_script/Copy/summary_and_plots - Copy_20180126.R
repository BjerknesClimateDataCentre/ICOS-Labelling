library(ncdf4)

# File locations
INPUT_DIR <- "output"
OUTPUT_DIR <- "output"

# Get the files in the input directory
input_files <- list.files(INPUT_DIR, pattern="csv$")

for (file_loop in 1:length(input_files)) {
	# Load the data
	cat("\r", input_files[file_loop], "               ")
	in_file <- paste(INPUT_DIR, "/", input_files[file_loop], sep="")
	data <- read.csv(in_file,header=T, fileEncoding="UTF8")

	# Get the message counts
	message_names <- vector(mode="character", length=0)
	message_counts <- vector(mode="numeric", length=0)
	message_rows <- 0

	for (row in 1:nrow(data)) {
		messages <- as.character(data[["Automatic.QC.Message"]][row])
		if (nchar(messages) > 0) {
			message_rows <- message_rows + 1
			message_list <- unlist(strsplit(messages, ";"))

			for (m in 1:length(message_list)) {
				message <- as.character(message_list[m])

				if (nchar(message) > 0) {
					message_index <- which(message_names == message)
					if (length(message_index) == 0) {
						message_names[length(message_names) + 1] <- message
						message_counts[length(message_counts) + 1] <- 1
					} else {
						message_counts <- replace(message_counts, message_index, message_counts[message_index] + 1)
					}
				}
			}
		}
	}

	summary_file <- paste(OUTPUT_DIR, "/", input_files[file_loop], ".summary.txt", sep="")
	sink(summary_file)
	cat("SUMMARY FOR FILE ", input_files[file_loop], "\n\n\n", sep="")
	cat("QC Messages\n")
	cat("===========\n")
	cat("Total rows with messages:", message_rows,"\n\n")

	for (i in 1:length(message_names)) {
		cat(message_names[i], ": ", message_counts[i], " (", format(round(message_counts[i] / nrow(data) * 100, 2), nsmall=2), "%)\n", sep="")
	}
	sink()

  
##################
## PLOTS:
  
dates <- as.POSIXlt(data[["Date"]], "%Y-%m-%d %H:%M:%S", tz="UTC")

  
## PLOT INTAKE TEMPERATURE:
  if ((max(data[["Intake.Temperature"]]) > 50) || (min(data[["Intake.Temperature"]]) < -10)) {
    png(paste(OUTPUT_DIR, "/", "1.Intake_temp_", "plot_modified.png", sep=""))
    tryCatch(plot(dates, data[["Intake.Temperature"]], ylab = "Intake Temperature [degC]", xlab = "Time", ylim = c(-10,50)), error=function(e) {})
    dev.off()   
  } else {
    png(paste(OUTPUT_DIR, "/", "1.Intake_temp_", "plot.png", sep=""))
    tryCatch(plot(dates, data[["Intake.Temperature"]], ylab = "Intake Temperature [degC]", xlab = "Time"), error=function(e) {})
    dev.off()   
  }
    
    
## PLOT OF SALINITY: 
  if ((max(data[["Salinity"]]) > 50) || (min(data[["Salinity"]]) < 0)) {
    png(paste(OUTPUT_DIR, "/", "2.Salinity_", "plot_modified.png", sep=""))
    tryCatch(plot(dates, data[["Salinity"]], ylab = "Salinity", xlab = "Time", ylim = c(0,50)), error=function(e) {})
    dev.off()
  } else { 
    png(paste(OUTPUT_DIR, "/", "2.Salinity_", "plot.png", sep=""))
	  tryCatch(plot(dates, data[["Salinity"]], ylab = "Salinity", xlab = "Time"), error=function(e) {})
	  dev.off()
  }
  

## PLOT OF EQUILIBRATOR TEMPERATURE:
  if ((max(data[["Equilibrator.Temperature"]]) > 50) || (min(data[["Equilibrator.Temperature"]]) < -10)) {
    png(paste(OUTPUT_DIR, "/", "3.Equil_temp_", "plot_modified.png", sep=""))
	  tryCatch(plot(dates, data[["Equilibrator.Temperature"]], ylab = "Equilibrator Temperature [degC]", xlab = "Time", ylim = c(-10,50)), error=function(e) {})
	  dev.off()
  } else {
    png(paste(OUTPUT_DIR, "/", "3.Equil_temp_", "plot.png", sep=""))
    tryCatch(plot(dates, data[["Equilibrator.Temperature"]], ylab = "Equilibrator Temperature [degC]", xlab = "Time"), error=function(e) {})
    dev.off()
      }


## PLOT OF EQUILIBRATOR PRESSURE:
 if ((max(data[["Equilibrator.Pressure"]]) > 1250) || (min(data[["Equilibrator.Pressure"]]) < 750)) {
	 png(paste(OUTPUT_DIR, "/", "4.Equil_press", "plot_modified.png", sep=""))
	 tryCatch(plot(dates, data[["Equilibrator.Pressure"]], ylab = "Equilibrator Pressure [hPa]", xlab = "Time", ylim = c(750,1250)), error=function(e) {})
	 dev.off()
 } else {
   png(paste(OUTPUT_DIR, "/", "4.Equil_press", "plot.png", sep=""))
   tryCatch(plot(dates, data[["Equilibrator.Pressure"]], ylab = "Equilibrator Pressure [hPa]", xlab = "Time"), error=function(e) {})
   dev.off()
 }


## PLOT OF ATMOSPHERIC PRESSURE (if any):
if (is.numeric(data[["Atmospheric.Pressure"]])) {
if ((max(data[["Atmospheric.Pressure"]]) > 1250) || (min(data[["Atmospheric.Pressure"]]) < 750)) { 
  png(paste(OUTPUT_DIR, "/", "Atm_press", "plot_modified.png", sep=""))
  tryCatch(plot(dates, data[["Atmospheric.Pressure"]], ylab = "Atmospheric Pressure [hPa]", xlab = "Time", ylim = c(750,1250)), error=function(e) {})
  dev.off()
} else {
  png(paste(OUTPUT_DIR, "/", "Atm_press", "plot.png", sep=""))
  tryCatch(plot(dates, data[["Atmospheric.Pressure"]], ylab = "Atmospheric Pressure [hPa]", xlab = "Time"), error=function(e) {})
  dev.off()
}
}


## PLOT OF MEASURED CO2:
if ((max(data[["CO2..measured."]]) > 1200) || (min(data[["CO2..measured."]]) < 80)) {
  png(paste(OUTPUT_DIR, "/", "5.CO2_measured_", "plot_modified.png", sep=""))
  tryCatch(plot(dates, data[["CO2..measured."]], ylab = "CO2 measured [uatm]", xlab = "Time", ylim = c(80,1200)), error=function(e) {})
  dev.off()
} else {
  png(paste(OUTPUT_DIR, "/", "CO2_measured_", "plot.png", sep=""))
  tryCatch(plot(dates, data[["CO2..measured."]], ylab = "CO2 measured [uatm]", xlab = "Time"), error=function(e) {})
  dev.off()
}


## PLOT OF FCO2:
if ((max(na.omit(data[["fCO2"]])) > 1200) || (min(na.omit(data[["fCO2"]])) < 80)) {
    png(paste(OUTPUT_DIR, "/", "6.fCO2_calculated_", "plot_modified.png", sep=""))
  	tryCatch(plot(dates, data[["fCO2"]], ylab = "fCO2 [uatm]", xlab = "Time", ylim = c(80,1200)), error=function(e) {})
  	dev.off()
} else {
    png(paste(OUTPUT_DIR, "/", "6.fCO2_calculated_", "plot.png", sep=""))
  	tryCatch(plot(dates, data[["fCO2"]], ylab = "fCO2 [uatm]", xlab = "Time"), error=function(e) {})
  	dev.off()
}
	

## PLOT OF DELTA TEMPERATURE:
if ((max(data[["Delta.Temperature"]]) > 20) || (min(data[["Delta.Temperature"]]) < -20)) {
  png(paste(OUTPUT_DIR, "/", "SecInd_Delta_temp_", "plot_modified.png", sep=""))
	tryCatch(plot(dates, data[["Delta.Temperature"]], ylab = "Delta Temperature [degC]", xlab = "Time", ylim = c(-20,20)), error=function(e) {})
	dev.off()
} else {
  png(paste(OUTPUT_DIR, "/", "SecInd_Delta_temp_", "plot_modified.png", sep=""))
  tryCatch(plot(dates, data[["Delta.Temperature"]], ylab = "Delta Temperature [degC]", xlab = "Time"), error=function(e) {})
  dev.off()
}
  

## PLOT MEASURED VS CALCULATED CO2
xrange <- c(max(min(na.omit(data[["CO2..measured."]])),80),min(max(na.omit(data[["CO2..measured."]])),1200))
yrange <- c(max(min(na.omit(data[["fCO2"]])),80),min(max(na.omit(data[["fCO2"]])),1200))

png(paste(OUTPUT_DIR, "/", "SecInd_co2_meas_vs_calc_", "plot.png", sep=""))
tryCatch(plot(data[["CO2..measured."]], data[["fCO2"]], ylab = "fCO2 calculated", xlab = "Measured CO2", ylim=yrange, xlim=xrange), error=function(e) {})
dev.off()



## PLOT INTAKE TEMP VS EQU TEMP
xrange <- c(max(min(na.omit(data[["Intake.Temperature"]])),-10),min(max(na.omit(data[["Intake.Temperature"]])),50))
yrange <- c(max(min(na.omit(data[["Equilibrator.Temperature"]])),-10),min(max(na.omit(data[["Equilibrator.Temperature"]])),50))

png(paste(OUTPUT_DIR, "/", "SecInd_EqT_vs_SST_", "plot.png", sep=""))
tryCatch(plot(data[["Intake.Temperature"]], data[["Equilibrator.Temperature"]], ylab = "Equilibrator temperature", xlab = "Intake Temperature",ylim=yrange, xlim=xrange), error=function(e) {})
dev.off()



}

cat("\n")