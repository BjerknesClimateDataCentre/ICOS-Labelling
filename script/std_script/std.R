################################################################################
### GAS STANDARD VALUES PLOTS
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
# rows that are std measurements; and add the std anomaly
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
# CREATE THE STD PLOTS AND STATS
#-------------------------------------------------------------------------------

filename <- paste("output/std_plot.png",sep="")
png(filename)

plot_list <- list()
for (i in 1:length(stds)) {
  
  # Create datasubset
  df_std <- df_mod %>%
    filter(run_type == stds[i])
  
  # Create the plots
  plot_list[[i]] <- ggplot(df_std, aes(x = datetime, y = anomaly)) +
      geom_point() +
      xlab("Time") + ylab("Calibration anomaly [ppm]") + 
      ylim(as.numeric(settings$ylims$y_lim_min),
           as.numeric(settings$ylims$y_lim_max)) + 
      scale_x_datetime(date_breaks="1 month", date_labels = '%b') + 
      theme_bw() +
      theme(text=element_text(family="Times"),
            axis.text=element_text(size=rel(1.5)),
            axis.title=element_text(size=rel(1.7)))
  
  # Write stats
}

do.call("grid.arrange", c(plot_list, ncol = 1, nrow = 3))
dev.off()