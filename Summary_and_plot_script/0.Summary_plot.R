########################################################################
#### This script runs all Scripts in folder "Summary_and_plot_script"
##############


#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
# Input parameters

# Chose which parameters to plot. All core parameters must be plotted if they exist and make sense (do not plot constants).
plot_SST <- TRUE                     # Core for VOS and FOS
plot_eqTemp <- FALSE                   # Core for VOS
plot_sal <- FALSE                     # Core for FOS
plot_eqPress <- TRUE                  # Core for VOS
plot_xCO2sw <- TRUE                   # xCO2 or pCO2 is core for VOS and FOS
plot_pCO2sw <- FALSE
plot_DepthPressure <- FALSE           # Pressure (depth) is plotted for FOS, but in another script. The reason we need to know this here is for the lettering


letter_location <- "topleft"      # Plot lettering positions for measurement plots. Alternatives are "bottomright", "bottomleft", "topleft", "topright"
letter_location2 <- "topleft"      # Lettering position in secondary indices plots.

deltaT_letter <- "a)"   

#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
input_from_main <- TRUE

source('1.no_external_data.R')
source('2.summary.R')
source('3.plot_measurements.R')

#Remove these parameters in case individual scripts will be ran imediately after this one.
rm("plot_SST")
rm("plot_eqTemp")
rm("plot_sal")
rm("plot_eqPress")
rm("plot_xCO2sw")
rm("plot_pCO2sw")
rm("plot_DepthPressure")
rm("letter_location")
rm("letter_location2")
rm("deltaT_letter")

input_from_main <- FALSE