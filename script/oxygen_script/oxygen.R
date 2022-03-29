################################################################################
### OXYGEN PLOTS AND STATS
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


#-------------------------------------------------------------------------------
# FUNCTIONS
#-------------------------------------------------------------------------------

extract_settings <- function(settings){
  for (key in names(settings)) {
    assign(key, settings[[key]], envir = .GlobalEnv)
  }
}

# This function returns information about where in the plot to put the letter
# string. Required inputs are the name of the parameter, and in which corner
# of the plot the letter should be added.
create_letter_position <- function(letter_position_name, y_name, x_name,
                                   y_lims, x_lims) {
  
  # Create a data frame with location information about each corner in a
  # plot. This data frame is later used to determine where to place the letter 
  # string in plots.
  positions_template <- data.frame(
    xpos = c(-Inf, -Inf, Inf, Inf),
    ypos = c(-Inf, Inf, -Inf, Inf),
    hjustvar = c(-1, -1, 1, 1),
    vjustvar = c(-1, 1, -1, 1),
    location = c("bottomleft", "topleft", "bottomright", "topright"))
  
  # Get the row number in the positions template data frame where information 
  # about the requested corner is given
  position_index <- which(positions_template$location == letter_position_name)
  
  # The x and y-positions are either the max or min of the parameter. Which one
  # is determined by the sign in the xpos and ypos column in the position 
  # template. (!! This is confusing code - improve how this is done !!)
  if (positions_template$ypos[position_index] > 0) {
    
    if (is.na(y_lims[2])){
      ypos <- max(na.omit(as.numeric(df_simple[[y_name]])))
    } else {
      ypos <- na.omit(y_lims[2])
    }
    
  } else {
    
    if (is.na(y_lims[1])) {
      ypos <- min(na.omit(as.numeric(df_simple[[y_name]])))
    } else {
      ypos <- na.omit(y_lims[1])
    }
    
  }
  
  if (positions_template$xpos[position_index] > 0){
    
    if (is.na(x_lims[2])) {
      xpos <- max(na.omit(as.numeric(df_simple[[x_name]])))
    } else {
      xpos <- na.omit(x_lims[2])
    }
    
  } else {
    
    if (is.na(x_lims[1])) {
      xpos <- min(na.omit(as.numeric(df_simple[[x_name]])))
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
# IMPORT DATA AND SETTINGS
#-------------------------------------------------------------------------------

dataname <- list.files("input", pattern = ".txt")
datapath <- paste0("input/", dataname)
df <- read_tsv(datapath)

# Store settings in variables
settings <- read_json(path = "settings.json", format = "json")
for (setting_name in names(settings)){
  extract_settings(settings[[setting_name]])
}

# Create a datetime column
if (date_colname & time_colname){
  df_datetime <- df %>%
    mutate(datetime = as.POSIXct(paste(df[[date_colname]], df[[time_colname]]),
                                 format = settings$datetime_format))
} else {
  df_datetime <- df %>%
    mutate(datetime = as.POSIXct(paste0(
      df[[year_colname]], "-", df[[month_colname]], "-", df[[day_colname]], " ",
      df[[hour_colname]], ":", df[[minute_colname]], ":", df[[second_colname]]),
      format = "%Y-%m-%d %H:%M:%S"))
}


#-------------------------------------------------------------------------------
# CONVERT, FILTER AND WRITE DATA
#-------------------------------------------------------------------------------

if (do_convertion) {
  if (!using_sigma_theta) {
    df_datetime <- df_datetime %>%
      mutate(df_datetime, {{o2_colname}} := round(df_datetime[[o2_colname]]*44.6,3))
  }
} #else {
#}

if (remove_missing) {
  df_datetime <- df_datetime %>%
    filter(df_datetime[[o2_colname]] != missing_value)
}

# Remove the final datatime column before output the data
df_to_output <- df_datetime %>%
  select(-datetime)

# Write the filtered data to file
out_file <- paste0("output/", strsplit(dataname, '.txt'), "_O2-converted.txt")
write_tsv(df_to_output, file = out_file)


#-------------------------------------------------------------------------------
# PLOT 
#-------------------------------------------------------------------------------

# Simplify data before plot and range check: only keep needed data and rename
if (saturation_colname & add_sat) {
  df_simple <- df_datetime %>%
    select(datetime, all_of(o2_colname), all_of(saturation_colname)) %>%
    rename(o2=all_of(o2_colname), saturation=all_of(saturation_colname))
} else {
  df_simple <- df_datetime %>%
    select(datetime, all_of(o2_colname)) %>%
    rename(o2=all_of(o2_colname))
}

if (make_plot) {

  # Make the plot ranges numeric (if na: replace with max and min oxygen value)
  o2_ylims <- as.numeric(unlist(strsplit(o2_ylims, ",")))
  if (is.na(o2_ylims[1])) {
    o2_ylims[1] <- min(na.omit(df_simple$o2))
    o2_ylims[2] <- max(na.omit(df_simple$o2))
  }

  # Get the letter position details
  letter_position <- create_letter_position(letter_position_name, y_name = "o2", 
                                            x_name = "datetime", 
                                            y_lims = c(o2_ylims[1], o2_ylims[2]),
                                            x_lims = c(NA, NA))

  # Set up the image file
  png("output/1.o2_vs_time.png")

  # Create the plot
  plot <-  ggplot(df_simple, aes(x = datetime)) +
    geom_point(aes(y = o2)) +
    xlab("Time") +
    # Specify monthly ticks with short month names as label
    #scale_x_datetime(date_breaks = "1 month", date_labels = '%b') +
    # Change plot layout to another theme and so some adjustments to the theme
    theme_bw() +
    theme(text = element_text(family = "Times"),
          axis.text = element_text(size = rel(1.5)),
          axis.title = element_text(size = rel(1.7))) +
    # Add the plot letter string
    annotate("text",
             x = letter_position[[1]],
             y = letter_position[[2]], 
             label = letter_string,
             hjust = letter_position[[3]],
             vjust = letter_position[[4]],
             size = 9)

  # Specify y tick labels format and their spacing
  timespan <- as.numeric(df_simple$datetime[nrow(df_simple)]-df_simple$datetime[1])
  if (timespan < 240) {
    breaks = "2 months"
  } else if (timespan > 240 & timespan < 650) {
    breaks = "4 months"
  } else {
    breaks = "6 monhts"
  }
  plot <- plot + scale_x_datetime(date_breaks = breaks, date_labels = '%b-%y')

  # Create the y axis(es) and its ranges
  if (add_sat) {
    sat_scale_coeff <- as.numeric(settings$plot_settings$sat_scale_coeff)
    plot <- plot + 
      geom_point(aes(y = saturation*sat_scale_coeff), color = 'red') +
      scale_y_continuous(name = y_lab, limits = c(o2_ylims[1], o2_ylims[2]),
                         sec.axis = sec_axis(~./sat_scale_coeff,
                                             name = "Saturation"))
  } else {
    plot <- plot + 
      ylab(y_lab) +
      ylim(o2_ylims[1], o2_ylims[2])
  }

  # Create the image file
  print(plot)
  dev.off()
}


#-------------------------------------------------------------------------------
# WRITE STATS
#-------------------------------------------------------------------------------

if (check_range) {
  # Set up the file for stats
  sink(file = "output/oxygen_stats.txt")
  
  # Write total number of oxygen measurements
  n_oxygen <- length(na.omit(df_simple$o2))
  cat("The total number of oxygen measurements: ", n_oxygen, "\n", sep="")

  # Write number of oxygen out of plotting range
  if (make_plot){
    n_outside_plot <- sum(na.omit(df_simple$o2 < o2_ylims[1])) +
      sum(na.omit(df_simple$o2 > o2_ylims[2]))
    percent_outside <-round((n_outside_plot/n_oxygen)*100, 1)
    cat("Measurements outside plot area (", o2_ylims[1], ":", o2_ylims[2],
        ") is: ", n_outside_plot, " (", percent_outside, "%)\n", sep = "")
  }

  # Write number of oxygen out of acceptable range
  qc_ranges_num <- as.numeric(unlist(strsplit(qc_ranges, ",")))
  good_min <- qc_ranges_num[1]
  good_max <- qc_ranges_num[2]

  n_bad <- sum(na.omit(df_simple$o2 < good_min)) + sum(na.omit(df_simple$o2 > good_max))
  percent_bad <- round((n_bad/n_oxygen)*100, 1)
  cat("Number of bad measurements (outside the good range ", good_min,
      ":", good_max, ") is: ", n_bad, " (", percent_bad, "%)\n", sep = "")

  sink()
}