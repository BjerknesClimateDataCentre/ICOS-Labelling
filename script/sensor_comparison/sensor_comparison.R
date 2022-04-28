################################################################################
### SENSOR COMPARISON SCRIPT
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
library(ggpubr)
library(gridExtra)

# Set the locale (needed for correct spelling of months in plots)
Sys.setlocale("LC_ALL", "English");

# Change the plot font (subscript 2 does not work in the png with default font)
windowsFonts(Times = windowsFont("Times New Roman"))


#-------------------------------------------------------------------------------
# FUNCTIONS
#-------------------------------------------------------------------------------

# Extract settings and store as variables
extract_settings <- function(settings){
  for (key in names(settings)) {
    assign(key, settings[[key]], envir = .GlobalEnv)
  }
}

# Get the limits to use for plotting. If the limits are not assigned in the
# settings file (="NA,NA"), use the max and min of the data
get_lims <- function(lims, data_col){
  lims <- as.numeric(unlist(strsplit(lims, ",")))
  if (is.na(lims[1]) & is.na(lims[2])) {
    lims[1] <- min(na.omit(data_col))
    lims[2] <- max(na.omit(data_col))
  }
  return(lims)
}


#-------------------------------------------------------------------------------
# IMPORT DATA AND SETTINGS
#-------------------------------------------------------------------------------

# Extract all settings and store the required colnames in variables
settings <- read_json(path = "settings.json", format = "json")
extract_settings(settings$required_columns)

# Import data
file_list <- list.files("input",
                        recursive = TRUE,
                        pattern = paste0("\\.",settings$input_file_format,"$"),
                        full.names = TRUE)
df <- readr::read_delim(file_list, delim = settings$input_file_delim)

# Create a datetime column
if (date_colname == time_colname & is.character(date_colname)) {
  df <- df %>%
    mutate(datetime = as.POSIXct(df[[date_colname]], format = datetime_format))
} else if (date_colname != time_colname & is.character(date_colname)) {
  df <- df %>%
    mutate(datetime = as.POSIXct(paste(df[[date_colname]], df[[time_colname]]),
                                 format = datetime_format))
} else {  # Case of 6 separate date time columns
  df <- df %>%
    mutate(datetime = as.POSIXct(paste0(
      df[[year_colname]], "-", df[[month_colname]], "-", df[[day_colname]], " ",
      df[[hour_colname]], ":", df[[minute_colname]], ":", df[[second_colname]]),
      format = "%Y-%m-%d %H:%M:%S"))
}

# Simplify data before plotting: only keep needed data and rename
# !!! Change this to allow more than two sensor columns !!!
df_simple <- df %>%
  select(datetime,
         all_of(sensor_colnames[[1]]),
         all_of(sensor_colnames[[2]])) %>%
  rename(sensor1 = all_of(sensor_colnames[[1]]),
         sensor2 = all_of(sensor_colnames[[2]])) %>%
  filter(!is.na(sensor1) | !is.na(sensor2))


#-------------------------------------------------------------------------------
# PLOT 1: BOTH SENSORS VS TIME
#-------------------------------------------------------------------------------

# Extract plot settings
extract_settings(settings$plot_settings$sensors_vs_time)

if (make_plot) {
  
  # Specify monthly ticks with short month names as label
  # The time between ticks depends on the length of the dataset
  timespan <- as.numeric(
    df_simple$datetime[nrow(df_simple)] - df_simple$datetime[1])
  if (timespan < 75) {
    breaks = "2 weeks"
    label_format = '%d-%b-%y'
  } else if (timespan < 240 & timespan >= 75) {
    breaks = "2 months"
    label_format = '%b-%y'
  } else if (timespan > 240 & timespan < 750) {
    breaks = "4 months"
    label_format = '%b-%y'
  } else {
    breaks = "6 months"
    label_format = '%b-%y'
  }
  
  # Make the plot y-axis limits numeric
  y_lims <- as.numeric(unlist(strsplit(y_lims, ",")))
  if (is.na(y_lims[1]) & is.na(y_lims[2])) {
    for (sensor in names(df_simple)){
      if (sensor != "datetime"){
        y_lims[1] <- min(na.omit(min(na.omit(df_simple[[sensor]])),y_lims[1]))
        y_lims[2] <- max(na.omit(max(na.omit(df_simple[[sensor]])),y_lims[2]))
       
      }
    }
  }
  
  # Create the plot objects in a loop
  png("output/1.sensors_vs_time.png")
  plot_list <- list()
  count <- 1
  for (sensor in names(df_simple)) {
    if (sensor != "datetime"){
      
      plot_list[[count]] <- ggplot(df_simple,
                                   aes(x = datetime, y = .data[[sensor]])) +
        geom_point(color = count + 1, size = 1) +
        # Hide axis labels - will be added later
        xlab("") + ylab("") + 
        ylim(y_lims[1], y_lims[2]) +
        scale_x_datetime(date_breaks = breaks, date_labels = label_format) +
        # Change to another layout theme
        theme_bw() +
        # Add axis label text and edit size of axis texts
        theme(text = element_text(family = "Times"),
              axis.text = element_text(size = rel(1.4)),
              axis.title = element_text(size = rel(1.7))) +
        # Add plot label (allow annotation outside plot area with clip 'off')
        annotate("text", 
                 x = min(df_simple$datetime), 
                 y = y_lims[2]*as.numeric(legend_height),
                 hjust = -0.25,
                 vjust = -1.3,
                 label = legends[[count]],
                 size = 4.5,
                 family = "Times",
                 colour = count + 1,
                 fontface = 2) +
        coord_cartesian(clip = "off")
      
      # Increase top margin to upper plot to make space for the plot tittle
      if (count == 1) {
        plot_list[[count]] <- plot_list[[count]] + 
          theme(plot.margin = unit(c(14, 5.5, 5.5, 5.5), "points"))
      }
      
      count <- count + 1
    }
  }
  
  # Create the axis labels to be shared by all plots in the figure
  text_left <- text_grob(y_lab,
                         rot = 90, vjust = 1, size = 19, family = "Times")
  text_bottom <- text_grob(x_lab, size = 19, family = "Times")
  
  # Arrange the plots in the figure and add the common axis labels
  grid.arrange(grobs = plot_list, ncol = 1, left = text_left,bottom = text_bottom,
               heights = rep(2, count-1))
  
  dev.off()
}


#-------------------------------------------------------------------------------
# PLOT 2: SENSOR VS SENSOR
#-------------------------------------------------------------------------------

extract_settings(settings$plot_settings$sensor1_vs_sensor2)

if (make_plot) {
  if (!rows_are_aligned) {
    # In order to make scatter plot, must have the sensor data on the same row
    # Create a data frame with a column for time difference. When the time 
    # difference is below the selected cut off, use the sensor data from those 
    # rows.
    df_simple <- df_simple %>%
      mutate(time_diff = 
               difftime(df_simple$datetime, lag(df_simple$datetime, unit="s"))) %>%
      mutate(sensor2_shifted = 
               case_when((time_diff < as.numeric(time_diff_cutoff)) & (!is.na(sensor2)) ~ sensor2,
                         (time_diff < as.numeric(time_diff_cutoff)) & (is.na(sensor2)) ~ lag(sensor2)
               )) %>%
      mutate(sensor1_shifted = 
               case_when((time_diff < as.numeric(time_diff_cutoff)) & (!is.na(sensor1)) ~ sensor1,
                         (time_diff < as.numeric(time_diff_cutoff)) & (is.na(sensor1)) ~ lag(sensor1)
               ))
    
    # Cleanup the scatter data frame before plotting
    df_simple <- df_simple %>%
      filter(time_diff < as.numeric(time_diff_cutoff)) %>%
      select(datetime, sensor1_shifted, sensor2_shifted) %>%
      rename(sensor1 = sensor1_shifted, sensor2 = sensor2_shifted)
  }
  
  x_lims <- get_lims(x_lims, df_simple$sensor1)
  y_lims <- get_lims(y_lims, df_simple$sensor2)
  
  png("output/2.sensor1_vs_sensor2.png")
  plot2 <- ggplot(df_simple, aes(x = sensor1, y = sensor2)) +
    geom_point() +
    xlab(x_lab) + ylab(y_lab) + 
    ylim(y_lims[1], y_lims[2]) +
    xlim(x_lims[1], x_lims[2]) +
    theme_bw() +
    theme(text = element_text(family = "Times"),
          axis.text = element_text(size = rel(1.4)),
          axis.title = element_text(size = rel(1.7)))
  print(plot2)
  dev.off()
}