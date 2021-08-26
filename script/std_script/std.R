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

# Set the locale (needed for correct spelling of months in plots)
Sys.setlocale("LC_ALL", "English");


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
  mutate(anomaly = std_val - co2) 

# Remove flush values if required. (Add a dummy column 'flush' containing 
# true/false about whether the valueis a flush or not. Remove rows where flush 
# is true and finally remove the dummy column.)
if (settings$remove_flush) {
  df_mod <- df_mod %>% 
    mutate(flush = ifelse(run_type != 
                   lag(run_type,n=as.numeric(settings$n_rows_flush)),
                   TRUE,FALSE)) %>%
    filter(flush == FALSE) %>%
    select(-flush)
}


#-------------------------------------------------------------------------------
# CREATE THE STD PLOTS
#-------------------------------------------------------------------------------
