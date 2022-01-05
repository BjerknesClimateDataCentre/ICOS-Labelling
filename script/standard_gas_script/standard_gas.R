################################################################################
### GAS STANDARD GRAPHS, BOX PLOT AND STATS
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

# Remove existing files in the output directory
if (!is.null(list.files("output"))) {
  file.remove(dir(paste(getwd(), "/output", sep = ""),
                  pattern = "", full.names = TRUE))
}

#-------------------------------------------------------------------------------
# IMPORT DATA AND SETTINGS
#-------------------------------------------------------------------------------

# Import data
datafile_name <- list.files("input") #, pattern = "csv$")
datafile_path <- paste("input/", datafile_name, sep = "")
df <- read_tsv(datafile_path)

# Import settings
settings <- read_json(path = "settings.json", format = "json")


#-------------------------------------------------------------------------------
# MODIFY THE DATA 
#-------------------------------------------------------------------------------
# In short: remove all unnecessary columns; only keep rows with standard gas
# measurements; remove the flush values (if any) and calculate the std anomaly.

# Extract the required column names from the settings
for (column_key in names(settings$required_columns)) {
  assign(column_key, settings$required_columns[[column_key]])
}

# Extract the names of standard value used in data file
std_names <- c()
for (name in settings$std_names) {
  std_names <- append(std_names, name) 
}

# Specify date/time column(s) !!Move to read raw data script !!
df <- df %>%
  mutate(datetime = as.POSIXct(paste(df[[date_colname]], df[[time_colname]]),
                               format = settings$datetime_format))


# Modify the dataset: Only select the needed columns and rename them; only keep
# rows that are std measurements; and  add the calculated std anomaly
df_mod <- df %>%
  select(datetime, all_of(run_type_colname), all_of(std_value_colname),
         all_of(co2_colname)) %>%
  rename(run_type = all_of(run_type_colname),
         std_val = all_of(std_value_colname),
         co2 = all_of(co2_colname)) %>%
  filter(run_type %in% std_names) %>%
  mutate(anomaly = co2 - std_val) 

# Remove flush values if required. (Add a dummy column 'flush' containing 
# true/false about whether the value is a flush or not. Remove rows where flush 
# is true and finally remove the dummy column.)
if (settings$remove_flush) {
  df_mod <- df_mod %>% 
    mutate(flush = ifelse(run_type != 
                   lag(run_type, n = as.numeric(settings$n_rows_flush)), 
                   TRUE, FALSE)) %>%
    filter(flush == FALSE) %>%
    select(-flush)
}

# Remove missing value (!! change to allow multiple missing values !!)
if (settings$remove_missing_value) {
  if (is.numeric(settings$missing_value)) {
    df_mod <- df_mod %>%
      filter(df_mod$co2 != as.numeric(settings$missing_value))
  } else {
    df_mod <- df_mod %>%
      filter(df_mod$co2 != settings$missing_value)
  }
}

#-------------------------------------------------------------------------------
# CREATE THE STD PLOTS FIGURE AND STATS
#-------------------------------------------------------------------------------

# Extract the approximate value for each standard gas and create labels
std_approx_values <- c()
for (value in settings$std_approx_values) {
  std_approx_values <- append(std_approx_values, value) 
}

std_labels <- c()
for (j in 1:length(std_approx_values)) {
  std_labels <- append(std_labels, paste(std_names[j], " (~", std_approx_values[j],
                                         ")", sep = ""))
}

# Extract the qc ranges from the settings file
for (qc_key in names(settings$qc_ranges)){
  assign(qc_key, as.numeric(settings$qc_ranges[[qc_key]]))
}

# Set up the file for stats
sink(file = "output/std_stats.txt")

# Set up the image file
filename <- paste("output/1.std_scatter.png", sep = "")
png(filename)

# Create the plot objects in a loop, one std plot/stat per iteration
plot_list <- list()
for (i in 1:length(std_names)) {
  
  # Filter away all data that are not from the standard gas in question
  df_std <- df_mod %>%
    filter(run_type == std_names[i])
  
  #--------------
  # CREATE PLOT
  #--------------
  # Filter away values outside plot area (needed for linear_model)
  df_std_good <- df_std %>%
    filter(anomaly < as.numeric(settings$y_lims$y_lim_max),
           anomaly > as.numeric(settings$y_lims$y_lim_min)) %>%
    mutate(datetime_sec = as.numeric(datetime))
  
  # Create a linear model to be added to the plot and used for stats later
  reg <- lm(anomaly ~ datetime, data = df_std_good)
  drift_intercept <- coef(reg)[[1]]
  drift_slope <- coef(reg)[[2]]
  drift_p_value <- round(summary(reg)$coefficient[2, 4], 3)
  
  # Create the plot
  plot_list[[i]] <- ggplot(df_std, aes(x = datetime, y = anomaly)) +
      geom_point(color = i+1) +
      # Hide axis labels - will be added later
      xlab("") + ylab("") + 
      # Set the plot range limits as given in settings file
      ylim(as.numeric(settings$y_lims$y_lim_min),
           as.numeric(settings$y_lims$y_lim_max)) + 
      # Set one tick per month on x axis
      scale_x_datetime(date_breaks = "1 month", date_labels = '%b') + 
      # Change to another layout theme
      theme_bw() +
      # Add plot label (allow annotation outside plot area with clip 'off')
      annotate("text", 
               x = min(df_std$datetime), 
               y = as.numeric(settings$y_lims$y_lim_max) * 0.80,
               hjust = -0.25,
               vjust = -1.3,
               label = std_labels[i],
               size = 4.5,
               family = "Times") +
      coord_cartesian(clip = "off") +
      # Add axis label text and edit size of axis texts
      theme(text = element_text(family = "Times"),
            axis.text = element_text(size = rel(1.4)),
            axis.title = element_text(size = rel(1.7))) +
      # Make the 0 line more visible
      geom_hline(yintercept = 0) +
      # Add the linear regression line
      geom_abline(intercept = drift_intercept, slope = drift_slope,
                  colour = "red", size = 0.5)
  
  # Increase top margin to upper plot to make space for the plot tittle
  if (i == 1) {
    plot_list[[i]] <- plot_list[[i]] + 
      theme(plot.margin = unit(c(14, 5.5, 5.5, 5.5), "points"))
  }
  
  # Hide x axis text for all plots except the last one
  #if (i<length(std_names)) {
  #  plot_list[[i]] <- plot_list[[i]] + theme(axis.text.x = element_blank())
  #}
  
  #--------------
  # CREATE STATS
  #--------------
  
  cat("Stats for: ", std_labels[i], "\n")
  
  # Get number of values outside the plotting area
  n_outside_plot <- 
    sum(df_std$anomaly > as.numeric(settings$y_lims$y_lim_max)) + 
    sum(df_std$anomaly < as.numeric(settings$y_lims$y_lim_min))
  percent_outside <- round((n_outside_plot/nrow(df_std))*100, 1)
  cat("  Values outside plot area (", settings$y_lims$y_lim_min, ":", 
      settings$y_lims$y_lim_max, ") is: ", n_outside_plot, " (", 
      percent_outside, "%)\n", sep = "")
  
  # Calculate and print number and percent of good values
  n_good <- sum(df_std$anomaly > good_min & df_std$anomaly < good_max)
  percent_good <- round((n_good/nrow(df_std))*100, 1)
  cat("  Good values (", good_min, ",", good_max, "): ", n_good, " (", 
      percent_good, "%)\n", sep = "")
  
  # Calculate and print number and percent of questionable values
  n_questionable <-
    sum(df_std$anomaly > questionable_min & df_std$anomaly < good_min) +
    sum(df_std$anomaly > good_max & df_std$anomaly < questionable_max)
  percent_questionable <- round((n_questionable/nrow(df_std))*100, 1)
  cat("  Questionable values (", questionable_min, ",", questionable_max, "): ", 
      n_questionable, " (", percent_questionable, "%)\n", sep = "")
  
  # Calculate and print number and percent of bad values
  n_bad <- 
    sum(df_std$anomaly > questionable_max) + 
    sum(df_std$anomaly < questionable_min)
  percent_bad <- round((n_bad/nrow(df_std))*100, 1)
  cat("  Bad values: ", n_bad, " (", percent_bad, "%)\n", sep = "")
  
  # Calculate and print drift
  time_total <- df_std_good$datetime_sec[nrow(df_std_good)] - 
    df_std_good$datetime_sec[1]
  drift <- round(drift_slope * time_total, 2)
  cat("  Drift over shown time frame: ", drift, " [ppm] (p-value: ", 
      drift_p_value, ")", "\n\n", sep = "")
}

# Close the writing to stats file
sink()

# Create the axis labels to be shared by all plots in the figure
text_left <- text_grob("Calibration anomaly [ppm]",
                   rot = 90, vjust = 1, size = 19, family = "Times")
text_bottom <- text_grob("Time", size = 19, family = "Times")

# Arrange the plots in the figure and add the common axis labels
grid.arrange(grobs = plot_list, ncol = 1, left = text_left,bottom = text_bottom,
             heights = c(2, 2, 2, 2))

dev.off()


#-------------------------------------------------------------------------------
# CREATE THE STD BOX PLOT
#-------------------------------------------------------------------------------

# Set up the image file
filename <- paste("output/2.std_boxplot.png", sep = "")
png(filename)

# Create the plot object
boxplot <- ggplot(df_mod, aes(x = run_type, y = anomaly, fill = run_type)) + 
  geom_boxplot() + 
  # Hide x axis labels - add new y axis label
  xlab("") + ylab("Calibration anomaly [ppm]") + 
  # Rename the x axis box labels
  scale_x_discrete(labels = std_labels) +
  # Make the 0 line more visible
  geom_hline(yintercept = 0) +
  # Set new layout theme
  theme_bw() +
  # Edit the size of text and remove legend
  theme(text = element_text(family = "Times"),
        axis.text = element_text(size = rel(1.4)),
        axis.title = element_text(size = rel(1.7)),
        legend.position = "none")

# Calculate the y limits (lowest 25 quantile and the highest 75 quantile)
low_quantiles <- c()
high_quantiles <- c()
for (std in std_names) {
  quantiles <- quantile(filter(df_mod, run_type==std)$anomaly)[c(2,4)]
  low_quantiles <- append(low_quantiles, quantiles[1])
  high_quantiles <- append(high_quantiles, quantiles[2])
}
ylims_box <- c(min(low_quantiles), max(high_quantiles))

# Set the y limts to the boxplot
boxplot <- boxplot + coord_cartesian(ylim = ylims_box)

# Create the box plot image
print(boxplot)
dev.off()