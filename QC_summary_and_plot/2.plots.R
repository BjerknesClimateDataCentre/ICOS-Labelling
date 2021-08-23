################################################################################
### PLOT DATA IN THE EXPORT FILE
################################################################################

### DESCRIPTION:
# This script creates multiple plots needed for the report:
# - all measurements vs time
# - deltaT vs time
# - fco2 vs time

### REQUIREMENTS:
# - The dataset as exported from Quince (with format 'ICOS OTC Labelling') must 
# be in the input folder in the same directory as this script.
# - Informatiton about which plots to create must be specified in the 
# "settings.json" file. There are several plot option you need to set for each
# parameter (plot letter, y label, etc.).

### OUTPUT:
# The plots are stored in the output folder, together with a text file 
# containing the number of measurements ouf of plot range (if any). These are
# needed in the labelling report text.


#-------------------------------------------------------------------------------
# INITIAL SETTINGS
#-------------------------------------------------------------------------------

# Load packages
library(readr)
library(dplyr)
library(jsonlite)
library(ggplot2)

# Set the locale (needed for correct spelling of months in plots)
Sys.setlocale("LC_ALL", "English");

# Change the plot font (subscript 2 does not work in the png with default font)
windowsFonts(Times=windowsFont("Times New Roman"))


#-------------------------------------------------------------------------------
# IMPORT DATA AND CONFIG FILE
#-------------------------------------------------------------------------------

# Import data as tibble
datafile_name <- list.files("input",pattern="csv$")
datafile_path <- paste("input/",datafile_name, sep="")
df <- read_csv(datafile_path)

# Import header config and change the headers of the tibble
header_config <- read_json(path="header_config.json",format="json")
for (header in names(header_config$header_converter)){
  if (header %in% names(df)) {
    colnames(df)[which(names(df) == header)] <- 
      header_config$header_converter[[header]]
  }
}

# Import the settings
settings <- read_json(path="settings.json", format="json")

# Update column names related to the raw CO2
colnames(df)[which(names(df) == settings$raw_CO2_colname)] <- "raw_CO2"


#-------------------------------------------------------------------------------
# FUNCTION(S)
#-------------------------------------------------------------------------------

# This function returns information about where in the plot to put the letter
# string. Required inputs are the name of the parameter, and in which corner
# of the plot the letter should be added.
create_letter_position <- function(letter_position_name, param_name) {
  
  # Get the row number in the positions template data frame where information 
  # about the requested corner is given
  position_index <- which(positions_template$location==letter_position_name)
  
  # The y-position is either the max or min of the parameter. Which one is
  # determined by the sign in the ypos column in the position template.
  if (positions_template$ypos[position_index] > 0) {
    ypos <- max(na.omit(as.numeric(df_to_plot[[param_name]])))
  } else {
    ypos <- min(na.omit(as.numeric(df_to_plot[[param_name]])))
  }
  
  # The rest of the letter position information is extracted straight from 
  # the position template data frame
  xpos <- positions_template$xpos[position_index] 
  hjustvar <- positions_template$hjustvar[position_index]
  vjustvar <- positions_template$vjustvar[position_index]
  
  # Store all position details in a vector and return it
  letter_position <- list(xpos,ypos,hjustvar,vjustvar)
  return(letter_position)
}

# This function creates a plot and store it in the output folder. Required 
# inputs are which parameter to plot (param), the number of the plot (used in 
# the png filename), the parameters label and plotting range, a letter string 
# and where to place it.
create_plot <- function(param_name, plot_count, y_lab, y_lims, letter_string,
                        letter_position) {

  # Set up the image file
  filename <- paste("output/",plot_count,"_",param_name,".png", sep="")
  png(filename)
  
  # Create a ggplot and add multiple features
  ret <- ggplot(df_to_plot, 
                aes(x = datetime, y = as.numeric(df_to_plot[[param_name]]))) +
    geom_point() +
    xlab("Time") + ylab(y_lab) + 
    # Specify monthly ticks with short month names as label
    scale_x_datetime(date_breaks="1 month", date_labels = '%b') +
    # Change plot layout to another theme and so some adjustments to the theme
    theme_bw() +
    theme(text=element_text(family="Times"),
          axis.text=element_text(size=rel(1.5)),
          axis.title=element_text(size=rel(1.7)))  +
    # Add the plot letter string
    annotate("text",
             x = letter_position[[1]],
             y = letter_position[[2]], 
             label = letter_string,
             hjust = letter_position[[3]],
             vjust = letter_position[[4]],
             size=9)
  
  # Change the y plot range if this was specified in the settings
  if (!is.na(y_lims[1])){
    ret <- ret + ylim(y_lims[1], y_lims[2])
  }
  
  # Create the plot and image file
  print(ret)
  dev.off()
}


#-------------------------------------------------------------------------------
# CREATE THE PLOTS
#-------------------------------------------------------------------------------

# Create a data frame with location information about each corner in a
# plot. This data frame is later used to determine where to place the letter 
# string in plots.
positions_template <- data.frame(
  xpos = c(min(df$datetime),min(df$datetime),max(df$datetime),max(df$datetime)),
  ypos =  c(-Inf,Inf,-Inf,Inf),
  hjustvar = c(-1,-1,1,1),
  vjustvar = c(-1,1,-1,1),
  location = c("bottomleft","topleft","bottomright","topright"))

# Set start values before the plotting for loop (plot numbers are used in 
# plot file names)
plot_count <- 1

# Set up the text file which will be filled with information about number of 
# measurements out of the plot range
sink(file = "output/out_of_range.txt")
sink_file_empty <- TRUE

# Create the plots in a for loop
for (plot_config in settings$all_plot_settings){
  if (plot_config$make_plot) {
    
    # Extract all plot settings in a loop (keep the same variable name as in
    # the settings file)
    for (plot_setting_key in names(plot_config)) {
      assign(plot_setting_key, plot_config[[plot_setting_key]])
    }
    
    # Create a vector of the y limits. (The ylim given in the settings file is a 
    # string containing the min and max. This is to ensure that either none or 
    # both are given. Only giving one limit does not work.)
    y_lims <- as.numeric(unlist(strsplit(y_lims, ",")))
    
    # Filter out all bad data if this is specified in the settings
    if (good_only){
      df_to_plot <- df %>%
        filter(get(paste(param_name, "_flag", sep="")) == 2)
      print(nrow(df_to_plot))
    } else {
      df_to_plot <- df
    }
    
    # Get the letter position details
    letter_position <- create_letter_position(letter_position_name, param_name)
    
    # Create the plot
    create_plot(param_name, plot_count, y_lab, y_lims, letter_string,
                letter_position)
    
    # Print outliers to file if there are any
    if (!is.na(y_lims[1])){
      
      # Get the total number of measurements for the parameter in question 
      n_meas <- length(na.omit(as.numeric(df$peq)))

      # Find number (and percent) of lower values and print
      outlier_low <- sum(na.omit(as.numeric(df[[param_name]])) < y_lims[1])
      percent_low <- round((outlier_low/n_meas)*100,1)
      cat("\nNumber of ", param_name, " measurements lower than ", y_lims[1], ": ",
          outlier_low, " (", percent_low, "%)", sep="")
      
      # Find number (and percent) of higher values and print
      outlier_high <- sum(na.omit(as.numeric(df[[param_name]])) > y_lims[2])
      percent_high <- round((outlier_high/n_meas)*100,1)
      cat("\nNumber of ", param_name, " measurements higher than ", y_lims[2], ": ",
          outlier_high, " (", percent_high, "%)", sep="")
      
      # Since outliers are printed, the sink file is not empty anymore
      sink_file_empty <- FALSE
    }
  }
  plot_count <- plot_count + 1
}

# If the sink file is empty, add a messsage that no values were out of plot 
# range
if (sink_file_empty) {
  cat("No measurements are out of the plot range:)")
}

sink()