################################################################################
### GAS STANDARD GRAPHS, BOX PLOT AND STATS
################################################################################

#-------------------------------------------------------------------------------
# INITIAL SETTINGS
#-------------------------------------------------------------------------------

# Clear plots
#if (!is.null(dev.list())) {
#  dev.off()
#}

# Reset sink file
#for (i in seq_len(sink.number())) {
#  sink(NULL)
#}

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
windowsFonts(Times=windowsFont("Times New Roman"))


#-------------------------------------------------------------------------------
# IMPORT DATA AND SETTINGS
#-------------------------------------------------------------------------------

# Import data
datafile_name <- list.files("input") #,pattern="csv$")
datafile_path <- paste("input/",datafile_name, sep="")
df <- read_tsv(datafile_path)

# Import header config file and store the header converter as data frame
settings <- read_json(path="settings.json",format="json")


#-------------------------------------------------------------------------------
# FUNCTION(S)
#-------------------------------------------------------------------------------


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
stds <- c()
for (name in settings$std_names) {
  stds <- append(stds, name) 
}

# Modify the dataset: Only select the needed columns and rename them; only keep
# rows that are std measurements; and  add the calculated std anomaly
df_mod <- df %>%
  select(date_time, all_of(run_type), all_of(std_value), all_of(co2)) %>%
  rename(datetime = date_time, 
         run_type = all_of(run_type),
         std_val = all_of(std_value),
         co2 = all_of(co2)) %>%
  filter(run_type %in% stds) %>%
  mutate(anomaly = co2 - std_val) 

# Remove flush values if required. (Add a dummy column 'flush' containing 
# true/false about whether the value is a flush or not. Remove rows where flush 
# is true and finally remove the dummy column.)
if (settings$remove_flush) {
  df_mod <- df_mod %>% 
    mutate(flush = ifelse(run_type != 
                   lag(run_type,n=as.numeric(settings$n_rows_flush)),
                   TRUE,FALSE)) %>%
    filter(flush == FALSE) %>%
    select(-flush)
}

# Remove missing values (!!! Allow for more than one missing value)
df_mod <- df_mod %>%
  filter(df_mod$co2 != as.numeric(settings$co2_missing_value))


#-------------------------------------------------------------------------------
# CREATE THE STD PLOTS FIGURE
#-------------------------------------------------------------------------------

# Set up the image file
filename <- paste("output/std_plot.png",sep="")
png(filename)

# Create the plot objects in a loop, one std plot per iteration
plot_list <- list()
for (i in 1:length(stds)) {
  
  # Filter away all data that are not from the standard gas in question
  df_std <- df_mod %>%
    filter(run_type == stds[i])
  
  # Filter away values outside plot area and create a linear model to be added
  # to the plot and used for stats later
  df_std_good <- df_std %>%
    filter(anomaly < as.numeric(settings$y_lims$y_lim_max),
           anomaly > as.numeric(settings$y_lims$y_lim_min)) %>%
    mutate(datetime_sec = as.numeric(datetime))
  reg <- lm(anomaly ~ datetime, data = df_std_good)
  
  # Create the plot
  plot_list[[i]] <- ggplot(df_std, aes(x = datetime, y = anomaly)) +
      geom_point(color=i+1) +
      # Hide axis labels - will be added later
      xlab("") + ylab("") + 
      # Set the plot range limits as given in settings file
      ylim(as.numeric(settings$y_lims$y_lim_min),
           as.numeric(settings$y_lims$y_lim_max)) + 
      # Set one tick per month on x axis
      scale_x_datetime(date_breaks="1 month", date_labels = '%b') + 
      # Change to another layout theme
      theme_bw() +
      # Create label with std name to be added on top of plot
      labs(title = paste("STD ",i," (",stds[i],")",sep="")) +
      # Add label text and exit size of axis texts
      theme(text = element_text(family="Times"),
            axis.text = element_text(size=rel(1.5)),
            axis.title = element_text(size=rel(1.7)),
            plot.title = element_text(hjust = 1, size=rel(1.4))) + 
      # Make the 0 line more visible
      geom_hline(yintercept=0) +
      # Add the linear regression line
      geom_abline(intercept = coef(reg)[[1]],slope = coef(reg)[[2]],
                  colour = "red", size = 0.5)
  
  # Hide x axis text for all plots except the last one (Comment this out for now
  # since it is risky to hide axis tick text when the x axis is not shared)
  #if (i<length(stds)) {
  #  plot_list[[i]] <- plot_list[[i]] + theme(axis.text.x = element_blank())
  #}
  
  # Write stats
}

# Create the axis labels to be shared by all plots in the figure
text_left <- text_grob("Calibration anomaly [ppm]",
                   rot=90, vjust=1, size=19, family = "Times")
text_bottom <- text_grob("Time", size=19, family = "Times")

# Arrange the plots in the figure and add the common axis labels
grid.arrange(grobs = plot_list, ncol = 1, left = text_left,bottom = text_bottom)

dev.off()