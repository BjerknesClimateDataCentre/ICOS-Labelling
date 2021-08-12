################################################################################
### CREATE A SUMMARY OF QC MESSAGES FROM QUINCE
################################################################################

### Description:
# Sript makes a summary of how many and what kind of QC messages each 
# parameter got from the QC procedures in QuinCe.

### Requirements:
# The dataset as exported from Quince (with format 'ICOS OTC Labelling') must 
# be in the input folder in the same directory as this script.

### Output:
# A text file containing the QC summary in the output folder.


#-------------------------------------------------------------------------------
# INPUT PARAMETERS
#-------------------------------------------------------------------------------

# Specify the name of the parameters to create summary for:
raw_CO2_colname <- "S1_CO2w"

#summary_params <- c("xco2_wet_cal")

#-------------------------------------------------------------------------------
# INITIAL SETTINGS
#-------------------------------------------------------------------------------

library(readr)
library(jsonlite)
library(dplyr)
library(tidyr)


#-------------------------------------------------------------------------------
# IMPORT DATA AND CONFIG FILE
#-------------------------------------------------------------------------------

# Import data
datafile_name <- list.files("input",pattern="csv$")
datafile_path <- paste("input/",datafile_name, sep="")
df <- read_csv(datafile_path)

# Import config file and store the header converter as data frame
config <- read_json(path="config.json",format="json")

# Update the column names using the names in the config file
for (header in names(config$header_converter)){
  if (header %in% names(df)) {
    colnames(df)[which(names(df) == header)] <- 
      config$header_converter[[header]]
  }
}

# Update column names related to the raw CO2
colnames(df)[which(names(df) == raw_CO2_colname)] <- "raw_CO2"
colnames(df)[which(names(df) == paste(raw_CO2_colname," QC Flag",sep=""))] <-
  "raw_CO2_flag"
colnames(df)[which(names(df) == paste(raw_CO2_colname," QC Comment",sep=""))] <-
  "raw_CO2_comm"

# Store data object in output folder (use this when fix data import issue (#7))
# saveRDS(df,file='input/exported_data.rds')


#-------------------------------------------------------------------------------
# CREATE SUMMARY
#-------------------------------------------------------------------------------

#summaryfile_path <- paste("output/QC_summary_", datafile_name, ".txt", sep="")
#sink(summaryfile_path)
#cat("QC SUMMARY FOR datafile_name \n\n\n", sep="")


#-------------------------------
# FOR EACH PARAMETER:


# Filter out rows with nan for the current paramter
df_filter <- df %>% 
  filter(!is.na(as.numeric(raw_CO2)))

# Store the total number of measurements
n_meas <- nrow(df_filter)

# Store the flag frequency
flag_counts <- df_filter %>%
  group_by(raw_CO2_flag) %>%
  summarize(n=n())

# Store the QC message frequency
untidy_message_counts <- df_filter %>%
  group_by(raw_CO2_comm) %>%
  summarize(n=n()) %>%
  filter(!is.na(raw_CO2_comm))

message_counts <- untidy_message_counts %>%
  mutate(raw_CO2_comm = strsplit(raw_CO2_comm,";")) %>%
  unnest(raw_CO2_comm) %>%
  filter(raw_CO2_comm != "") %>%
  group_by(raw_CO2_comm) %>%
  summarize(count=sum(n))




#for (i in 1:length(message_names)) {
#	cat(message_names[i], ": ", message_counts[i], " (", format(round(message_counts[i] / nrow(data) * 100, 2), nsmall=2), "%)\n", sep="")
#}

#sink()



# Exrtract values from counts tibbles:
# flag_counts$n[which(flag_counts$xco2_wet_cal_flag==2)]



#---------------------------
# Use fco2 columns in the end to kind of sum up the results (since these are
# accumulated)

#cat("Total rows: ", total_row, "\n", sep="")
#cat("Rows with messages: ", message_rows," (" ,perc,"%)","\n\n", sep="")                  
#cat("QC Messages\n")
#cat("===========\n")





##########################
### OLD
#cat("\r", input_files, "               ")

# Get the message counts
#message_names <- vector(mode="character", length=0)
#message_counts <- vector(mode="numeric", length=0)
#message_rows <- 0


#for (row in 1:nrow(df)) {
#  messages <- as.character(data[["fCO2..uatm..QC.Comment"]][row])

#  if (nchar(messages) > 0) {
#    message_rows <- message_rows + 1
#    message_list <- unlist(strsplit(messages, ";"))

#    for (m in 1:length(message_list)) {
#      message <- as.character(message_list[m])

#      if (nchar(message) > 0) {
#        message_index <- which(message_names == message)

#          if (length(message_index) == 0) {
#            message_names[length(message_names) + 1] <- message
#            message_counts[length(message_counts) + 1] <- 1
#            } else {
#              message_counts <- replace(message_counts, message_index, message_counts[message_index] + 1)
#            }
#      }
#    }
#  }
#}

#total_row <- length(data[,1]) 
#perc <- round(((message_rows/total_row)*100),2)

#summaryfile_path <- paste("output/QC_summary_", datafile_name, ".txt", sep="")
#sink(summaryfile_path)
#cat("SUMMARY FOR FILE ", datafile_name, "\n\n\n", sep="")
#cat("Total rows: ", total_row, "\n", sep="")
#cat("Rows with messages: ", message_rows," (" ,perc,"%)","\n\n", sep="")                  
#cat("QC Messages\n")
#cat("===========\n")

#for (i in 1:length(message_names)) {
#	cat(message_names[i], ": ", message_counts[i], " (", format(round(message_counts[i] / nrow(data) * 100, 2), nsmall=2), "%)\n", sep="")
#}

#sink()