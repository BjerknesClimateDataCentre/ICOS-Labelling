################################################################################
### MAKE PLOT OF SINGLE VARIABLE VS TIME, AND FILTER DATA IF NEEDED          ###
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
windowsFonts(Times = windowsFont("Times New Roman"))

# Remove existing files in the output directory
if (!is.null(list.files("output"))) {
  file.remove(dir(paste0(getwd(), "/output"), pattern = "", full.names = TRUE))
}


#-------------------------------------------------------------------------------
# FUNCTIONS
#-------------------------------------------------------------------------------

# Function that adds a date time column to the data. Required inputs are the
# name of the data, its date and time colnames, and their formats.
assign_datetime <- function(df, date_colname, time_colname, datetime_format){
  df_datetime <- df %>%
    mutate(datetime = case_when(
      # ... the date and time is given in one column:
      (date_colname == time_colname) ~
        as.POSIXct(df[[date_colname]], format = datetime_format, tz = "UTC"),
      # ... the date and time is in two columns:
      (length(strsplit(date_colname, ",")) == 1 & 
         length(strsplit(time_colname, ",")) == 1) ~
        as.POSIXct(paste(df[[date_colname]], df[[time_colname]]),
                   format = datetime_format, tz = "UTC"),
      # ... the year, month, day, hour, minute and seconds are in separate cols:
      # (have not tested if this bit works)
      (length(strsplit(date_colname, ",")) != 1) ~
        as.POSIXct(paste(
          strsplit(date_colname,',')[1], strsplit(date_colname,',')[2],
          strsplit(date_colname,',')[3], strsplit(time_colname,',')[1],
          strsplit(time_colname,',')[2], strsplit(time_colname,',')[3]),
          format = datetime_format, tz = "UTC"))
    )
  return(df_datetime)
}

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
      ypos <- max(as.numeric(na.omit(df_to_plot[[y_name]])))
    } else {
      ypos <- na.omit(y_lims[2])
    }
    
  } else {
    
    if (is.na(y_lims[1])) {
      ypos <- min(as.numeric(na.omit(df_to_plot[[y_name]])))
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


#-------------------------------------------------------------------------------
# CREATE PLOT(S) AND RUN FILTER(S)
#-------------------------------------------------------------------------------

# Import and extract settings
settings <- read_json(path = "settings.json", format = "json")
for (key in names(settings)) {
  assign(key, settings[[key]])
}

# Make the plot ranges numeric (if na: replace with max and min value)
y_lims <- as.numeric(unlist(strsplit(y_lims, ",")))
if (is.na(y_lims[1])) {
  y_lims[1] <- min(na.omit(df_to_plot[[y_colname]]))
  y_lims[2] <- max(na.omit(df_to_plot[[y_colname]]))
}
x_lims <- c(NA,NA) # Always datetime on x axis in this script!

# Create a data frame with location information about each corner in a
# plot. This data frame is later used to determine where to place the letter 
# string in plots.
positions_template <- data.frame(
  xpos = c(-Inf, -Inf, Inf, Inf),
  ypos = c(-Inf, Inf, -Inf, Inf),
  hjustvar = c(-1, -1, 1, 1),
  vjustvar = c(-1, 1, -1, 1),
  location = c("bottomleft", "topleft", "bottomright", "topright"))

# Loop through input files and make plots (and filter if)
data_files <- list.files("input", pattern = ".txt")
for (file in data_files){
  
  # Import data and assign datetime
  file_path <- paste0("input/", file)
  df <- read_tsv(file_path)
  df_to_plot <- assign_datetime(df, date_colname, time_colname, datetime_format)
  
  # Create letter position
  if (add_letter){
    letter_position <- create_letter_position(letter_corner, y_colname,
                                              "datetime", y_lims, x_lims)
  }
  
  # Set up the image file
  output_filename <- paste0("output/", y_colname, "_vs_time_", file, ".png") 
  png(output_filename)
  
  # Create a ggplot and add multiple features
  ret <- ggplot(df_to_plot, 
                aes(x = df_to_plot$datetime, y = df_to_plot[[y_colname]])) +
    geom_point() +
    xlab("Time") + ylab(y_label) + 
    # Change plot layout to another theme and so some adjustments $o the theme
    theme_bw() +
    theme(text = element_text(family = "Times"),
          axis.text = element_text(size = rel(1.5)),
          axis.title = element_text(size = rel(1.7))) +
    ylim(y_lims[1], y_lims[2])
  
  if (add_letter){
    ret <- ret + annotate("text", 
                          x = letter_position[[1]],
                          y = letter_position[[2]], 
                          label = plot_letter,
                          hjust = letter_position[[3]],
                          vjust = letter_position[[4]],
                          size = 9)
  }
  
  # Specify monthly ticks with short month names as label
  # The time between ticks depends on the length of the dataset
  timespan <- as.numeric(df_to_plot$datetime[nrow(df)]-df_to_plot$datetime[1])
  if (timespan < 240) {
    breaks = "2 months"
  } else if (timespan > 240 & timespan < 750) {
    breaks = "4 months"
  } else {
    breaks = "6 months"
  }
  ret <- ret + scale_x_datetime(date_breaks = breaks, date_labels = '%b-%y')
  
  # Create the plot and image file
  print(ret)
  dev.off()
}