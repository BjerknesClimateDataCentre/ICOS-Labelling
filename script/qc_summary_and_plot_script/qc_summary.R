################################################################################
### CREATE A SUMMARY OF QC MESSAGES FROM QUINCE
################################################################################

#-------------------------------------------------------------------------------
# INITIAL SETTINGS
#-------------------------------------------------------------------------------

# Load packages
library(readr)
library(jsonlite)
library(dplyr)
library(tidyr)

# Remove existing files in the output directory
if (!is.null(list.files("output"))) {
  file.remove(dir(paste(getwd(),"/output",sep=""), pattern = "",
                  full.names = TRUE))
}


#-------------------------------------------------------------------------------
# IMPORT DATA, HEADER CONFIG AND SETTINGS FILE
#-------------------------------------------------------------------------------

# Import processed data
df <- readRDS(file = "../data/processed_data.rds")

# Import the settings
settings <- read_json(path="settings.json", format="json")

# Update column names related to the raw CO2
colnames(df)[which(names(df) == settings$raw_co2_colname)] <- "raw_co2"
colnames(df)[which(names(df) == paste(settings$raw_co2_colname," QC Flag",sep=""))] <-
  "raw_co2_flag"
colnames(df)[which(names(df) == paste(settings$raw_co2_colname," QC Comment",sep=""))] <-
  "raw_co2_comm"


#-------------------------------------------------------------------------------
# FUNCTION(S)
#-------------------------------------------------------------------------------

# This function takes a column/parameter name and prints its QC summary
QC_summary <- function(param_name){
  
  # Print the parameter name as subheader
  cat("\n------------------------------------------------------------\n")
  cat(param_name,":\n", sep="")
  cat("----------\n")
  
  # Filter out rows with nan for the current paramter
  df_filter <- df %>% 
    filter(!is.na(as.numeric(!!as.symbol(param_name))))
  
  cat("  Total rows: ", nrow(df_filter),"\n\n", sep="")
  
  # Get and print flag frequency
  flag_counts <- df_filter %>%
    group_by(flags = !!as.symbol(paste(param_name, "_flag", sep=""))) %>%
    summarize(n=n())
  
  for (flag in flag_counts$flags) {
    count <- flag_counts$n[which(flag_counts$flags==flag)]
    percent <- round((count/nrow(df_filter))*100,2)
    cat("  Rows with flag ", flag,": ", count, " (", percent, "%)\n", sep="")
  } 
  
  # Get and print the QC message frequency
  untidy_message_counts <- df_filter %>%
    group_by(messages = !!as.symbol(paste(param_name, "_comm", sep=""))) %>%
    summarize(n=n()) %>%
    filter(!is.na(messages))
  
  if (nrow(untidy_message_counts) != 0) {
    message_counts <- untidy_message_counts %>%
      mutate(messages = strsplit(messages,";")) %>%
      unnest(messages) %>%
      filter(messages != "") %>%
      group_by(messages) %>%
      summarize(n=sum(n)) %>%
      arrange(desc(n))
    
    cat("\n  QC Messages\n")
    for (message in message_counts$messages) {
      count <- message_counts$n[which(message_counts$messages==message)]
      percent <- round((count/nrow(df_filter))*100,2)
      cat("    '", message,"': ", count, " (", percent, "%)\n", sep="")
    }
  }
}


#-------------------------------------------------------------------------------
# CREATE THE SUMMARY FILE
#-------------------------------------------------------------------------------

summaryfile_path <- paste("output/QC_summary.txt", sep="")
sink(summaryfile_path)
cat("=================================================================\n")
cat("QC SUMMARY\n", sep="")
cat("===========\n\n")

# Create list of parameters to do summaries for
if (settings$station_type == "SOOP"){
  param_list <- c("lat","lon","temp","teq","peq")
} else {
  param_list <- c("lat","lon","temp","sal")
}
param_list <- append(param_list, c("raw_co2","fco2"))

# Create and print the QC summaries
for (param in param_list){
  QC_summary(param)
} 

sink()