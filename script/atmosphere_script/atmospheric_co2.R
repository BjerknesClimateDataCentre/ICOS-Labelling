################################################################################
### ATMOSPHERIC CO2 PLOTS AND STATS
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
  file.remove(dir(paste0(getwd(), "/output"), pattern = "", full.names = TRUE))
}


#-------------------------------------------------------------------------------
# IMPORT DATA AND SETTINGS
#-------------------------------------------------------------------------------

# Import data
datafile_name <- list.files("input", pattern = ".txt")
datafile_path <- paste0("input/", datafile_name)
df <- read_tsv(datafile_path)

# Import settings
settings <- read_json(path = "settings.json", format = "json")

# Import external data if specified in settings
if (settings$external_comparison) {
  external_datafile_name <- list.files("input/external")
  external_datafile_path <- paste0("input/external/", external_datafile_name)
  df_external <- read_csv(external_datafile_path, 
                          col_types = cols("sample_site_code" = col_character(),
                                           .default = col_number()))
}


#-------------------------------------------------------------------------------
# FUNCTIONS
#-------------------------------------------------------------------------------

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
      ypos <- max(as.numeric(na.omit(df_mod_full[[y_name]])))
    } else {
      ypos <- na.omit(y_lims[2])
    }
    
  } else {
    
    if (is.na(y_lims[1])) {
      ypos <- min(as.numeric(na.omit(df_mod_full[[y_name]])))
    } else {
      ypos <- na.omit(y_lims[1])
    }
    
  }
  
  if (positions_template$xpos[position_index] > 0){
    
    if (is.na(x_lims[2])) {
      xpos <- max(as.numeric(na.omit(df_mod_full[[x_name]])))
    } else {
      xpos <- na.omit(x_lims[2])
    }
    
  } else {
    
    if (is.na(x_lims[1])) {
      xpos <- min(as.numeric(na.omit(df_mod_full[[x_name]])))
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
# MODIFY THE DATA 
#-------------------------------------------------------------------------------
# Specify date and time; remove all unnecessary columns and rows; get the 
# sequence number and calculate mean and standard deviation per sequence

# Extract the column names from the settings
for (column_key in names(settings$required_columns)) {
  assign(column_key, settings$required_columns[[column_key]])
}

# Specify date/time column(s) !!M ve to read raw data script !!
df <- df %>%
  mutate(datetime = as.POSIXct(paste(df[[date_colname]], df[[time_colname]]),
                               format = settings$datetime_format))

# Modify the dataset: Only select the needed columns and rename them; only keep
# rows that are atmospheric measurements
df_mod <- df %>%
  select(datetime, all_of(lat_colname), all_of(lon_colname), 
         all_of(run_type_colname), all_of(co2_colname)) %>%
  rename(latitude = all_of(lat_colname),
         longitude = all_of(lon_colname),
         run_type = all_of(run_type_colname),
         co2 = all_of(co2_colname)) %>%
  filter(run_type == settings$atm_type_name) %>%
  select(-run_type)

# Add sequence number and calculate mean and standard deviation per sequence
df_mod_full <- df_mod %>%  
  mutate(new_sequence = ifelse(
    is.na(abs(difftime(df_mod$datetime, lag(df_mod$datetime), units = "secs"))),
    TRUE, ifelse(abs(difftime(df_mod$datetime, lag(df_mod$datetime), 
                              units = "secs")) > 3600, TRUE, FALSE))) %>%
  mutate(sequence = cumsum(new_sequence)) %>%
  select(-new_sequence) %>%
  group_by(sequence) %>%
  mutate(mean = mean(co2), 
         std = sd(co2),
         std_plot_size = case_when((std < 0.5) ~ 0.1,
                                   (std > 0.5 & std < 1) ~ 0.25,
                                   (std > 1 & std < 2) ~ 0.75,
                                   TRUE ~ 6),
         std_hist = ifelse((std > 2), 2, std))


#-------------------------------------------------------------------------------
# PLOT ATMOSPHERIC CO2 VS TIME 
#-------------------------------------------------------------------------------

# Extract the plot settings
for (plot_key in names(settings$plot_settings$co2_vs_time)) {
  assign(plot_key, settings$plot_settings$co2_vs_time[[plot_key]])
}

# Make the plot ranges numeric
y_lim_min <- as.numeric(y_lim_min)
y_lim_max <- as.numeric(y_lim_max)

# Get the letter position details
letter_position <- create_letter_position(letter_position_name, y_name = "co2", 
                        x_name = "datetime", y_lims = c(y_lim_min, y_lim_max),
                        x_lims = c(NA, NA))

# Set up the image file
png("output/1.atmpspheric_co2_vs_time.png")

# Create the plot
plot_1 <-  ggplot(df_mod_full, aes(x = datetime, y = co2)) +
  geom_point() +
  xlab("Time") + ylab(y_lab) + 
  # Specify monthly ticks with short month names as label
  scale_x_datetime(date_breaks = "1 month", date_labels = '%b') +
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

# Change the range in plot if specified in settings
if (!is.na(y_lim_min)){
  plot_1 <- plot_1 + ylim(y_lim_min, y_lim_max)
}

# Create the image
print(plot_1)
dev.off()


#-------------------------------------------------------------------------------
# WRITE OUT OF RANGE AND QC STATS
#-------------------------------------------------------------------------------

# Set up the file for stats
sink(file = "output/atmos_stats.txt")

# Write total number of measurements
n_atmos <- length(na.omit(df_mod_full$co2))
cat("The total number of atmospheric measurements: ", n_atmos, "\n", sep="")

# Write number of measureents outside the plotting area
n_outside_plot <- sum(df_mod_full$co2 > y_lim_max) + sum(df_mod_full$co2 < y_lim_min)
percent_outside <- round((n_outside_plot/nrow(df_mod_full))*100, 1)
cat("Plot 1. Measurements outside plot area (", y_lim_min, ":", y_lim_max,
    ") is: ", n_outside_plot, " (", percent_outside, "%)\n", sep = "")

# Write number of bad measurements 
good_min <- as.numeric(settings$qc_range$good_min)
good_max <- as.numeric(settings$qc_range$good_max)
n_bad <- sum(df_mod_full$co2 < good_min) + sum(df_mod_full$co2 > good_max)
percent_bad <- round((n_bad/nrow(df_mod_full))*100, 1)
cat("Plot 1. Number of bad measurements (outside the good range ", good_min,
    ":", good_max, ") is: ", n_bad, " (", percent_bad, "%)\n", sep = "")


#-------------------------------------------------------------------------------
# CREATE STANDARD DEVIATION 'MAP'
#-------------------------------------------------------------------------------

# Extract the plot settings
for (plot_key in names(settings$plot_settings$std_dev_map)) {
  assign(plot_key, settings$plot_settings$std_dev_map[[plot_key]])
}

# Make the plot ranges numeric
y_lim_min <- as.numeric(y_lim_min)
y_lim_max <- as.numeric(y_lim_max)
x_lim_min <- as.numeric(x_lim_min)
x_lim_max <- as.numeric(x_lim_max)

# Get the letter position details
letter_position <- create_letter_position(letter_position_name, 
                        y_name = "latitude", x_name = "longitude", 
                        y_lims = c(y_lim_min, y_lim_max),
                        x_lims = c(x_lim_min, y_lim_max))

# Set up the image file
png("output/2.std_dev_map.png")

plot_2 <-  ggplot(df_mod_full, aes(x = longitude, y = latitude)) +
  # Use the std_plot_size column to scale the points
  geom_point(aes(size = std_plot_size), shape = 1, show.legend = F) +
  # Icrease the size of the points in plot
  scale_size(range = c(0, 20)) +
  xlab("Longitude") + ylab("Latitude") + 
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

# Change the range in plot if specified in settings
if (!is.na(y_lim_min)){
  plot_2 <- plot_2 + ylim(y_lim_min, y_lim_max)
}
if (!is.na(x_lim_min)){
  plot_2 <- plot_2 + xlim(x_lim_min, x_lim_max)
}

# Create the image
print(plot_2)
dev.off()


#-------------------------------------------------------------------------------
# CREATE STANDARD DEVIATION HISTOGRAM
#-------------------------------------------------------------------------------

# Extract the plot settings
for (plot_key in names(settings$plot_settings$histogram)) {
  assign(plot_key, settings$plot_settings$histogram[[plot_key]])
}

# Set up the image file
png("output/3.histogram.png")

plot_3 <- ggplot(df_mod_full, aes(x = std_hist)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "grey") +
  scale_x_continuous(breaks = seq(0, 2, 0.5),
                     labels = c("> 0.5", "> 1", "> 1.5", "> 2", "< 2")) +
  stat_bin(binwidth = 0.5, geom = "text", aes(label = ..count..), vjust = -0.5,
          size = 5, family = "Times") + 
  labs(x = "Standard deviation [ppm]", y = "Atmospheric Sequences") +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        axis.text = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.7)))

plot_3 <- plot_3 +
  annotate("text",
         x = 1.75,
         y = max(ggplot_build(plot_3)$data[[1]]$count), 
         label = letter_string,
         hjust = -1,
         vjust = 1,
         size = 9)

# Create the image
print(plot_3)
dev.off()


#-------------------------------------------------------------------------------
# COMPARE WITH EXTERNAL DATA
#-------------------------------------------------------------------------------

if (settings$external_comparison) {
  
  # Extract the plot settings
  for (plot_key in names(settings$plot_settings$external_plot)) {
    assign(plot_key, settings$plot_settings$external_plot[[plot_key]])
  }
  
  # Modify the external dataset if the data to compare does not overlap in time:
  # add a value to the co2 measurements (typically 3 ppm per year mismatch); and
  # add to the year column so that the years in the two datasets are the same. 
  # The numbers to be added are extracted from the settings file.
  df_external <- df_external %>%
    {if (add_to_external_value != "NA")
      mutate(., analysis_value = analysis_value + as.numeric(add_to_external_value)) %>%
      mutate(., sample_year = sample_year + as.numeric(add_to_external_year))
    else .} 
  
  # Specify a datetime column
  df_external <- df_external %>%
    mutate(datetime = as.POSIXct(paste(df_external$sample_year, 
                      df_external$sample_month, df_external$sample_day,
                      df_external$sample_hour, df_external$sample_minute,
                      df_external$sample_seconds), tz = "UTC", 
                      format = "%Y %m %d %H %M %S"))

  # Modify the main dataset: Remove rows with datetimes not overlapping with
  # the external dataset; Filter out positions to far from the external station
  # (these filters needs to be specified in the settings)
  df_mod_full <- df_mod_full %>%
    {if (x_lim_min != "NA") 
      filter(., datetime > as.POSIXct(x_lim_min)) else .} %>% 
    {if (x_lim_max != "NA") 
      filter(., datetime < as.POSIXct(x_lim_max)) else .} %>%
    {if (lat_min != "NA")
      filter(., latitude > as.numeric(lat_min)) else .} %>%
    {if (lat_max != "NA")
      filter(., latitude < as.numeric(lat_max)) else .} %>%
    {if (lon_min != "NA")
      filter(., longitude > as.numeric(lon_min)) else .} %>%
    {if (lon_max != "NA")
      filter(., longitude < as.numeric(lon_max)) else .}
  
  # Create the y limits for the plot
  if (is.na(as.numeric(y_lim_min))){
    y_lims <- c(
      min(na.omit(df_mod_full$mean), na.omit(df_external$analysis_value)),
      max(na.omit(df_mod_full$mean), na.omit(df_external$analysis_value)))
  } else {
    y_lims <- c(as.numeric(y_lim_min), as.numeric(y_lim_max))
  }
  
  # Get the letter position details
  letter_position <- create_letter_position(letter_position_name, 
                      y_name = "mean", x_name = "datetime", 
                      y_lims = c(as.numeric(y_lim_min), as.numeric(y_lim_max)),
                      x_lims = c(NA, NA))
  
  # Set up the image file
  png("output/4.external_comparison.png")
  
  plot_4 <- ggplot() + 
    geom_point(data = df_mod_full, aes(x = datetime, y = mean)) + 
    geom_point(data = df_external, aes(x = datetime, y = analysis_value),
               color ='red', size = 4) +
    xlab("Time") + ylab(expression("Atmospheric xCO "[2]*" [ppm]")) + 
    ylim(y_lims) +
    # Specify monthly ticks with short month names as label
    scale_x_datetime(date_breaks = "1 month", date_labels = '%b') +
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
  
  # Create the image
  print(plot_4)
  dev.off()
}

# Write number of measurements (not external) outside of the plot range
n_outside_plot_ext <- sum(df_mod_full$co2 > y_lim_max) + sum(df_mod_full$co2 < y_lim_min)
percent_outside_ext <- round((n_outside_plot_ext/nrow(df_mod_full))*100, 1)
cat("Plot 4. Measurements (not external) outside plot area (", y_lim_min, ":",
    y_lim_max, ") is: ", n_outside_plot_ext, " (", percent_outside_ext, "%)\n",
    sep = "")

# Write to output file if dummy values were added to the external data
if (settings$external_comparison){
  cat("4. Note: Since the datasets did not overlap in time, dummy value ", 
      add_to_external_value, " were added to the external co2 data (to account 
      for the increase in co2 over time), and value ", add_to_external_year,
      " were added to the external year (in order to make a plot where data can
      be easily compared).")
}

sink()