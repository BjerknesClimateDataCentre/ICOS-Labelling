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
  file.remove(dir(paste0(getwd(), "/output"), pattern = "", full.names = TRUE))
}


#-------------------------------------------------------------------------------
# IMPORT DAT AND SETTINGS FILE
#-------------------------------------------------------------------------------

# Import processed data
df <- readRDS(file = "../data/processed_data.rds")

# Import the settings
settings <- read_json(path = "settings.json", format = "json")


#-------------------------------------------------------------------------------
# FUNCTION(S)
#-------------------------------------------------------------------------------

# This function removes a certain comment from a parameters QC comment column.
# It returns the updated data frame
remove_comment <- function(param, comment){
  param_flag <- paste0(param, "_flag")
  param_comm <- paste0(param, "_comm")
  
  for (i in 1:nrow(df)){
    # If a row contains the comment in question, create 'new_string' where 
    # this comment is removed
    if (grepl(comment, df[[param_comm]][i], fixed = TRUE)){
      new_string <- paste(setdiff(strsplit(df[[param_comm]][i], ";",
                                           fixed = TRUE)[[1]], comment),
                          sep = "", collapse = ";")
      # If the new string is empty,
      # set comment column to NA and change QC flag to 2
      if (new_string == ""){
        df[[param_comm]][i] <- NA
        df[[param_flag]][i] <- 2
      } else {
        df[[param_comm]][i] <- new_string
      }
    }
  }
  return(df)
}

# This function create and prints the QC summary of one parameter
QC_summary <- function(param_name){
  
  # Print the parameter name as subheader
  cat("\n------------------------------------------------------------\n")
  cat(param_name, ":\n", sep = "")
  cat("----------\n")
  
  # Filter out rows with nan for the current paramter
  df_filter <- df %>% 
    filter(!is.na(as.numeric(!!as.symbol(param_name))))
  
  cat("  Total rows: ", nrow(df_filter), "\n\n", sep = "")
  
  # Get and print flag frequency
  flag_counts <- df_filter %>%
    group_by(flags = !!as.symbol(paste0(param_name, "_flag"))) %>%
    summarize(n = n())
  
  for (flag in flag_counts$flags){
    count <- flag_counts$n[which(flag_counts$flags == flag)]
    percent <- round((count/nrow(df_filter))*100, 2)
    cat("  Rows with flag ", flag, ": ", count, " (", percent, "%)\n", sep = "")
  } 
  
  # Get and print the QC message frequency
  untidy_message_counts <- df_filter %>%
    group_by(messages = !!as.symbol(paste0(param_name, "_comm"))) %>%
    summarize(n = n()) %>%
    filter(!is.na(messages))
  
  if (nrow(untidy_message_counts) != 0) {
    message_counts <- untidy_message_counts %>%
      mutate(messages = strsplit(messages, ";")) %>%
      unnest(messages) %>%
      filter(messages != "") %>%
      group_by(messages) %>%
      summarize(n = sum(n)) %>%
      arrange(desc(n))
    
    cat("\n  QC Messages\n")
    for (message in message_counts$messages) {
      count <- message_counts$n[which(message_counts$messages == message)]
      percent <- round((count/nrow(df_filter))*100, 2)
      cat("    '", message, "': ", count, " (", percent, "%)\n", sep = "")
    }
  }
}


#-------------------------------------------------------------------------------
# CREATE THE SUMMARY FILE
#-------------------------------------------------------------------------------

sink("output/QC_summary.txt")
cat("=================================================================\n")
cat("QC SUMMARY\n", sep="")
cat("===========\n")
cat("Total number of rows evaluated in QuinCe: ", nrow(df), "\n\n", sep = "")

# Extract the parameter(s) to create QC summaries for 
for (param in names(settings$qc_summary_params)){
  if(settings$qc_summary_params[[param]]$make_summary){
  
    # Remove comment(s) if specified in the settings file
    remove_comment_list <- settings$qc_summary_params[[param]]$remove_comment
    if (length(remove_comment_list) > 0) {
      for (comment in remove_comment_list) {
        df <- remove_comment(param, comment)
      }
    }
    
    # Create and print the QC summaries to output file
    QC_summary(param)
  }
}

sink()