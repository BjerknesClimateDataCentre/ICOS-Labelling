# ########################################################################################
# ### Function for creating atmospheric CO2 plot
# ###########################
# ### For VOS only
# 
# ### Run the script in a directory which contains an input folder and an output folder.
# ### The Input folder needs to contain raw data file(s) (they need to have the same format and standards).
# ### Output folder will conatin one plot (per file) showing the atmospheric CO2 values over time.
# 
# # Append timestamp to output produced, also free text (?)
# timestamp <- function() {
#   as.character(
#   as.POSIXct(date(),tz="UTC", format="%a %b %d %H:%M:%S %Y"), 
#   format="%Y%m%dT%H%M%S")
# }
# 
# appendtext <- ""
# 
# 
# 
# #------------------------------------------------------------------------------
# #------------------------------------------------------------------------------
# # Input params to be assigned:
# #if (!input_from_main) { # This throws error "object not found" because input_from_main does not exist yet


# Clear plots
if (!is.null(dev.list()))
  dev.off()
# Clean workspace
rm(list = ls())

# Append timestamp to output produced, also free text (?)
timestamp <- function() {
  as.character(as.POSIXct(date(), tz = "UTC", format = "%a %b %d %H:%M:%S %Y"),
               format = "%Y%m%dT%H%M%S")
}


load(file = "ATM_PS_20190207T092421.RData")
appendtext <- paste("PS", sep = "")
output_dir <- "output"

if (!exists("input_from_main")) {input_from_main <- FALSE}
if (!input_from_main) {

  # Letter to use in plot (depends on how many other measurement plots)
    letter <- "d)"                             
  # Depends on letter position of the other measurement plots
    position <- "bottomleft"                   
  
}

# Consider to change these axis ranges after viewing plots.
CO2atm_ylim_min <- NA # -66000
CO2atm_ylim_max <- NA # 66000

# # Do not change these:
 atm_co2_max <- 500
 atm_co2_min <- 300
# 

 
 # Write output to txt file
 output_file <- paste("output", "/", 
                      "1_Atm_CO2_plot_info_out-of-range",
                      timestamp(),appendtext,".txt", sep="")
 sink(output_file)
 
 # Write range checking to screen. How many values are out of range?
 total_meas <- length(df_sub$CO2)
 oor <- sum(df_sub$CO2 > atm_co2_max, df_sub$CO2 < atm_co2_min, na.rm = TRUE)
 cat("Number of CO2atm measurements out of range: ", oor, 
     " (", round((oor/total_meas)*100,2), "%).", sep="")
 
 sink()
 
# 
 #fNA <- df_sub$CO2 %in% fNAvalues
 #fbad <- (df_sub$CO2 < fbadvalues[1] | df_sub$CO2 > fbadvalues[2])
 #fquest <- ((df_sub$CO2 > 0  & df_sub$CO2 < 300)| (df_sub$CO2 >500 & df_sub$CO2 < 1000))
# 

#------------------------------------------------------------------------------

## PLOT ATMOSPHERIC CO2 VS TIME
# Set limits
if(is.numeric(CO2atm_ylim_min)) {
  output_file_name <- paste(output_dir, "/", "0.AtmCO2_", "plot_own-range",
                            timestamp(),appendtext,".png", sep="")
  output_file_name_color <- paste(output_dir, "/", "0.AtmCO2_", "plot_own-range",
                            timestamp(),appendtext,"_color.png", sep="")
  CO2atm_ylims <- c(CO2atm_ylim_min, CO2atm_ylim_max)
} else if ((max(df_sub$CO2, na.rm=TRUE) > atm_co2_max)
           || (min(df_sub$CO2, na.rm=TRUE) < atm_co2_min)) {
    output_file_name <- paste(output_dir, "/", "0.AtmCO2_", "plot_bad-range",
                              timestamp(),appendtext,".png", sep="")
    output_file_name_color <- paste(output_dir, "/", "0.AtmCO2_", "plot_bad-range",
                                    timestamp(),appendtext,"_color.png", sep="")
    CO2atm_ylims <- c(atm_co2_min,atm_co2_max)	 
} else {
  output_file_name <- paste(output_dir, "/", "0.AtmCO2_", "plot",
                            timestamp(),appendtext,".png", sep="")
  output_file_name_color <- paste(output_dir, "/", "0.AtmCO2_", "plot",
                                  timestamp(),appendtext,"_color.png", sep="")
  CO2atm_ylims <- c(min(na.omit(df_sub$CO2)),
                    max(na.omit(df_sub$CO2)))	 
}

png(output_file_name, width=1600,height=1600, res=300)
par(mar=c(5,5,2,2))
plot (df_sub$date.time, df_sub$CO2, 
      xlab="Time", ylab=expression("Atmospheric xCO"[2]*" [ppm]"), 
      ylim = CO2atm_ylims , cex.lab=1.5,cex.axis=1.3)
legend(position, letter, bty="n", cex=2.5)
dev.off()
    	

# Plot bad data in different color


png(output_file_name_color, 
    width=1600,height=1600, res=300)

par(mar=c(5,5,2,2))
plot (df_sub$date.time[!df_sub$fbad & !df_sub$fquest], df_sub$CO2[!df_sub$fbad & !df_sub$fquest], 
      xlab="Time", ylab=expression("Atmospheric xCO"[2]*" [ppm]"), 
      ylim = CO2atm_ylims , cex.lab=1.5,cex.axis=1.3, col="black")
points (df_sub$date.time[df_sub$fbad], df_sub$CO2[df_sub$fbad], 
      xlab="Time", ylab=expression("Atmospheric xCO"[2]*" [ppm]"), 
      ylim = CO2atm_ylims , cex.lab=1.5,cex.axis=1.3, col="red", pch=8)
points (df_sub$date.time[df_sub$fquest], df_sub$CO2[df_sub$fquest], 
        xlab="Time", ylab=expression("Atmospheric xCO"[2]*" [ppm]"), 
        ylim = CO2atm_ylims , cex.lab=1.5,cex.axis=1.3, col="orange", pch=2)
points (df_sub$date.time[df_sub$fNA], df_sub$CO2[df_sub$fNA], 
        xlab="Time", ylab=expression("Atmospheric xCO"[2]*" [ppm]"), 
        ylim = CO2atm_ylims , cex.lab=1.5,cex.axis=1.3, col="blue", pch=3)

legend("topright", legend=c("good [300-500]","bad (<0 & >1000)","questionable","NA"),bty = "n", cex = 1,
       col = c("black","red","orange","blue"), pch =c(1,8,2,3))
#legend(position, letter, bty="n", cex=2.5)
dev.off()


#------------------------------------------------------------------------------

# Change the col name of latitude, longitude and CO2 before write output (do this to make the next codes work)
#colnames(df_sub) <- c("date.time", "TYPE", "CO2", "latitude", "longitude", "ATM_seq")



