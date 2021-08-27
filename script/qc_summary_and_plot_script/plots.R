################################################################################
### PLOT DATA IN THE EXPORT FILE
################################################################################

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

# Remove existing files in the output directory
if (!is.null(list.files("output"))) {
  file.remove(dir(paste(getwd(),"/output",sep=""),
                  pattern = "", full.names = TRUE))
}


#-------------------------------------------------------------------------------
# IMPORT DATA AND SETTINGS
#-------------------------------------------------------------------------------

# Import processed data
df <- readRDS(file = "../data/processed_data.rds")

# Import the settings
settings <- read_json(path="settings.json", format="json")

# Update column names related to the raw CO2
colnames(df)[which(names(df) == settings$raw_co2_colname)] <- "raw_co2"
colnames(df)[which(names(df) == paste(settings$raw_co2_colname," QC Flag",sep=""))] <-
  "raw_co2_flag"


#-------------------------------------------------------------------------------
# FUNCTION(S)
#-------------------------------------------------------------------------------

# This function returns information about where in the plot to put the letter
# string. Required inputs are the name of the parameter, and in which corner
# of the plot the letter should be added.
create_letter_position <- function(letter_position_name, y_name, x_name) {
  
  # Get the row number in the positions template data frame where information 
  # about the requested corner is given
  position_index <- which(positions_template$location==letter_position_name)
  
  # The x and y-positions are either the max or min of the parameter. Which one
  # is determined by the sign in the xpos and ypos column in the position 
  # template.
  if (positions_template$ypos[position_index] > 0) {
    ypos <- max(na.omit(as.numeric(df_to_plot[[y_name]])))
  } else {
    ypos <- min(na.omit(as.numeric(df_to_plot[[y_name]])))
  }
  
  if (positions_template$xpos[position_index] > 0){
    xpos <- max(na.omit(as.numeric(df_to_plot[[x_name]])))
  } else {
    xpos <- min(na.omit(as.numeric(df_to_plot[[x_name]])))
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
  letter_position <- list(xpos,ypos,hjustvar,vjustvar)
  return(letter_position)
}

# This function creates a plot and store it in the output folder. Required 
# inputs are which parameter to plot (param), the number of the plot (used in 
# the png filename), the parameters label and plotting range, a letter string 
# and where to place it.
create_plot <- function(plot_count, y_name, x_name, y_lab, x_lab, y_lims,
                        x_lims, letter_string, letter_position) {

  # Set up the image file
  filename <- paste("output/",plot_count,"_",y_name,"_vs_",x_name,".png",sep="")
  png(filename)
  
  # Create a ggplot and add multiple features
  ret <- ggplot(df_to_plot, 
                aes(x = df_to_plot[[x_name]], y = df_to_plot[[y_name]])) +
    geom_point() +
    xlab(x_lab) + ylab(y_lab) + 
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
  
  # Change the y and x plot range if this was specified in the settings
  if (!is.na(y_lims[1])){
    ret <- ret + ylim(y_lims[1], y_lims[2])
  }
  if (!is.na(x_lims[1])){
    ret <- ret + xlim(x_lims[1], x_lims[2])
  }
  
  # If x-axis is time: specify monthly ticks with short month names as label
  if (x_name == "datetime") {
    ret <- ret + scale_x_datetime(date_breaks="1 month", date_labels = '%b')
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
  percent_low <- round((outlier_low/n_meas)*100,1)
  cat("\n", plot_count, ": Number of ", axis_name, 
      " measurements lower than ", lims[1], ": ", outlier_low, " (", 
      percent_low, "%)", sep="")
  
  outlier_high <- sum(na.omit(df_to_plot[[axis_name]]) > lims[2])
  percent_high <- round((outlier_high/n_meas)*100,1)
  cat("\n", plot_count, ": Number of ", axis_name, 
      " measurements higher than ", lims[2], ": ", outlier_high, " (",
      percent_high, "%)\n", sep="")
}


#-------------------------------------------------------------------------------
# CREATE THE PLOTS
#-------------------------------------------------------------------------------

# Create a data frame with location information about each corner in a
# plot. This data frame is later used to determine where to place the letter 
# string in plots.
positions_template <- data.frame(
  xpos = c(-Inf,-Inf,Inf,Inf),
  ypos =  c(-Inf,Inf,-Inf,Inf),
  hjustvar = c(-1,-1,1,1),
  vjustvar = c(-1,1,-1,1),
  location = c("bottomleft","topleft","bottomright","topright"))

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
    
    # Filter out all bad data if this is specified in the settings
    if (y_filter_bad & x_filter_bad){
      df_to_plot <- df %>%
        filter(get(paste(y_name, "_flag", sep="")) == 2,
               get(paste(x_name, "_flag", sep="")) == 2)
    } else if (y_filter_bad & !x_filter_bad) {
      df_to_plot <- df %>%
        filter(get(paste(y_name, "_flag", sep="")) == 2)
    } else if (!y_filter_bad & x_filter_bad) {
      df_to_plot <- df %>%
        filter(get(paste(x_name, "_flag", sep="")) == 2)
    } else {
      df_to_plot <- df
    }
    
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
    letter_position <- create_letter_position(letter_position_name, 
                                              y_name, x_name)
    
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
    
  }
  plot_count <- plot_count + 1
}

# If the sink file is empty, add a messsage that no values were out of plot 
# range
if (sink_file_empty) {
  cat("No measurements are out of the plot range:)")
}

sink()