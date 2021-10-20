################################################################################
### PLOT DATA IN THE EXPORT FILE
################################################################################

#-------------------------------------------------------------------------------
# INITIAL SETTINGS
#-------------------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Load packages
library(readr)
library(dplyr)
library(jsonlite)
library(ggplot2)

# Set the locale (needed for correct spelling of months in plots)
Sys.setlocale("LC_ALL", "English");

# Change the plot font (subscript 2 does not work in the png with default font)
windowsFonts(Times = windowsFont("Times New Roman"))

# Remove existing files in the output directory
if (!is.null(list.files("output"))) {
  file.remove(dir(paste0(getwd(), "/output"),
                  pattern = "", full.names = TRUE))
}


#-------------------------------------------------------------------------------
# IMPORT DATA AND SETTINGS
#-------------------------------------------------------------------------------

# Import processed data
df <- readRDS(file = "../data/processed_data.rds")

# Import the settings
settings <- read_json(path = "settings.json", format = "json")


#-------------------------------------------------------------------------------
# FUNCTION(S)
#-------------------------------------------------------------------------------

# This function returns information about where in the plot to put the letter
# string. Required inputs are the name of the parameter, and in which corner
# of the plot the letter should be added.
create_letter_position <- function(letter_position_name, y_name, x_name,
                                   y_lims, x_lims) {
  
  # Get the row number in the positions template data frame where information 
  # about the requested corner is given
  position_index <- which(positions_template$location == letter_position_name)
  
  # The x and y-positions are either the max or min of the parameter. Which one
  # is determined by the sign in the xpos and ypos column in the position 
  # template. (!! This is confusing code - improve how this is done !!)
  if (positions_template$ypos[position_index] > 0) {
    
    if (is.na(y_lims[2])){
      ypos <- max(na.omit(as.numeric(df_to_plot[[y_name]])))
    } else {
      ypos <- na.omit(y_lims[2])
    }
   
  } else {
  
    if (is.na(y_lims[1])) {
      ypos <- min(na.omit(as.numeric(df_to_plot[[y_name]])))
    } else {
      ypos <- na.omit(y_lims[1])
    }
  
  }
  
  if (positions_template$xpos[position_index] > 0){
    
    if (is.na(x_lims[2])) {
      xpos <- max(na.omit(as.numeric(df_to_plot[[x_name]])))
    } else {
      xpos <- na.omit(x_lims[2])
    }
  
  } else {
  
    if (is.na(x_lims[1])) {
      xpos <- min(na.omit(as.numeric(df_to_plot[[x_name]])))
    } else {
      xpos <- na.omit(x_lims[1])
    }

  }
  
  # Change xpos class back to posixct if x axis data is date time
  if (x_name == "datetime") {
    xpos <- as.POSIXct(xpos, origin = '1970-01-01')
  }
  
  # The remaining letter position information is extracted straight from 
  # the position template data frame
  hjustvar <- positions_template$hjustvar[position_index]
  vjustvar <- positions_template$vjustvar[position_index]
  
  # Store all position details in a vector and return it
  letter_position <- list(xpos, ypos, hjustvar, vjustvar)
  return(letter_position)
}

# This function creates a plot and store it in the output folder. Required 
# inputs are which parameter to plot (param), the number of the plot (used in 
# the png filename), the parameters label and plotting range, a letter string 
# and where to place it.
create_plot <- function(plot_count, y_name, x_name, y_lab, x_lab, y_lims,
                        x_lims, letter_string, letter_position) {

  # Set up the image file
  filename <- paste0("output/", plot_count, "_", y_name, "_vs_", x_name, ".png")
  png(filename)
  
  # Create a ggplot and add multiple features
  ret <- ggplot(df_to_plot, 
                aes(x = df_to_plot[[x_name]], y = df_to_plot[[y_name]])) +
    geom_point() +
    xlab(x_lab) + ylab(y_lab) + 
    # Change plot layout to another theme and so some adjustments to the theme
    theme_bw() +
    theme(text = element_text(family = "Times"),
          axis.text = element_text(size = rel(1.5)),
          axis.title = element_text(size = rel(1.7)))
  
  # Add the plot letter string if provided
  if (letter_position != ""){
    ret <- ret + annotate("text",
                          x = letter_position[[1]],
                          y = letter_position[[2]], 
                          label = letter_string,
                          hjust = letter_position[[3]],
                          vjust = letter_position[[4]],
                          size = 9)
  }
  
  # Change the y and x plot range if this was specified in the settings
  if (!is.na(y_lims[1])){
    ret <- ret + ylim(y_lims[1], y_lims[2])
  }
  if (!is.na(x_lims[1])){
    ret <- ret + xlim(x_lims[1], x_lims[2])
  }
  
  # If x-axis is time: specify monthly ticks with short month names as label
  if (x_name == "datetime") {
    
    # The time between ticks depends on the length of the dataset
    timespan <- as.numeric(df$datetime[nrow(df)]-df$datetime[1])
    if (timespan < 240) {
      breaks = "2 months"
    } else if (timespan > 240 & timespan < 650) {
      breaks = "4 months"
    } else {
      breaks = "6 monhts"
    }
    
    ret <- ret + scale_x_datetime(date_breaks = breaks, date_labels = '%b-%y')
  } 
  
  # Create the plot and image file
  print(ret)
  dev.off()
}

# Function which prints the number of values out of plot range. Inputs are the
# name of the parameter, and the chosen axis limits.
out_of_range <- function(axis_name, lims) {
  n_meas <- length(na.omit(df_to_plot[[axis_name]]))
  
  outlier_low <- sum(na.omit(df_to_plot[[axis_name]]) < lims[1])
  percent_low <- round((outlier_low/n_meas)*100, 1)
  cat("\nPlot ", plot_count, ": Number of ", axis_name, 
      " measurements lower than ", lims[1], ": ", outlier_low, " (", 
      percent_low, "%)", sep = "")
  
  outlier_high <- sum(na.omit(df_to_plot[[axis_name]]) > lims[2])
  percent_high <- round((outlier_high/n_meas)*100, 1)
  cat("\nPlot ", plot_count, ": Number of ", axis_name, 
      " measurements higher than ", lims[2], ": ", outlier_high, " (",
      percent_high, "%)\n", sep = "")
}

# This function prints a warning if the plot range exceeds the pre-defined
# questionable/bad range for the given parameter. Required inputs are the name
# of the parameter, the given limits and the warning limits.
limit_warning <- function(param_name, given_lims, warning_lims) {
  
  plot_min <- max(na.omit(given_lims[1]),min(na.omit(df_to_plot[[param_name]])))
  plot_max <- min(na.omit(given_lims[2]),max(na.omit(df_to_plot[[param_name]])))
  
  if (plot_min < warning_lims[1]) {
    cat(paste0("\nWarning: The lower limit exceeds questionable/bad range ",
              warning_lims[1], " for ", param_name))
  }
  
  if (plot_max > warning_lims[2]) {
    cat(paste0("\nWarning: The higher limit exceeds questionable/bad range ",
              warning_lims[2], " for ", param_name))
  }
  
}


#-------------------------------------------------------------------------------
# CREATE THE PLOTS
#-------------------------------------------------------------------------------

# Create a data frame with location information about each corner in a
# plot. This data frame is later used to determine where to place the letter 
# string in plots.
positions_template <- data.frame(
  xpos = c(-Inf, -Inf, Inf, Inf),
  ypos = c(-Inf, Inf, -Inf, Inf),
  hjustvar = c(-1, -1, 1, 1),
  vjustvar = c(-1, 1, -1, 1),
  location = c("bottomleft", "topleft", "bottomright", "topright"))

# Set up the text file which will be filled with information about number of 
# measurements out of the plot range
sink(file = "output/out_of_range.txt")
sink_file_empty <- TRUE

# Create a loop counter and create plots in a loop (one plot per iteration)
plot_count <- 1
for (plot_config in settings$all_plots){
  if (plot_config$make_plot) {
    
    # Extract all plot settings (use same variable name as in the settings file)
    for (plot_setting_key in names(plot_config)) {
      assign(plot_setting_key, plot_config[[plot_setting_key]])
    }
    
    # Filter out rows with bad data if this is specified in the settings
    df_to_plot <- df %>%
      {if (y_filter_bad) filter(., get(paste0(y_name, "_flag")) == 2) 
        else .} %>%
      {if (x_filter_bad) filter(., get(paste0(x_name, "_flag")) == 2)
        else .}
    
    # If data is stored as character, change to numeric (datetime is always 
    # read correctly as date class)
    if (is.character(df_to_plot[[y_name]])) {
      df_to_plot[[y_name]] <- as.numeric(df_to_plot[[y_name]])
    } 
    if (is.character(df_to_plot[[x_name]])) {
      df_to_plot[[x_name]] <- as.numeric(df_to_plot[[x_name]])
    } 
    
    # Create a vector of the axis limits. (The limits are given in the settings
    # file as string containing min and max. This is to ensure that either none
    # or both are given - only giving one limit does not work.)
    y_lims <- as.numeric(unlist(strsplit(y_lims, ",")))
    x_lims <- as.numeric(unlist(strsplit(x_lims, ",")))
    
    # Get the letter position details
    if (letter_position_name != "") {
      letter_position <- create_letter_position(letter_position_name, 
                                              y_name, x_name, y_lims, x_lims)
    } else {
      letter_position <- ""
    }
    
    # Create the plot
    create_plot(plot_count, y_name, x_name, y_lab, x_lab, y_lims, x_lims,
                letter_string, letter_position)
    
    # Write the numbers of values out of range if limits were specified
    if (!is.na(y_lims[1])) {
      out_of_range(y_name, y_lims)
      sink_file_empty <- FALSE
    }
    if (!is.na(x_lims[1])) {
      out_of_range(x_name, x_lims)
      sink_file_empty <- FALSE
    }
    
    # Create a vector of the pre-defined axis warning limits
    y_warning_lims <- as.numeric(unlist(strsplit(y_warning_lims, ",")))
    x_warning_lims <- as.numeric(unlist(strsplit(x_warning_lims, ",")))
    
    # If the parameter is not datetime, check if the plot limits exceeds the 
    # pre-defined questionable/bad limits. If so, the function writes a warning
    # in the sink file.
    if (is.numeric(df_to_plot[[y_name]]) & !is.na(y_lims[1])) {
      limit_warning(y_name, y_lims, y_warning_lims)
    }
    if (is.numeric(df_to_plot[[x_name]]) & !is.na(x_lims[1])) {
      limit_warning(x_name, x_lims, x_warning_lims)
    }
    cat("\n")
  }
  plot_count <- plot_count + 1
}

# If the sink file is empty, add a message that no values were out of plot range
if (sink_file_empty) {
  cat("No measurements are out of the plot range:)")
}

sink()