################################################################################
### CREATE A SUMMARY OF QC MESSAGES FROM QUINCE
################################################################################

### Description:
# Sript makes a summary of how many and what kind of QC messages for each 
# parameter got from the QC procedures in QuinCe..

### Requirements:
# One data file as exported from Quince (with format 'ICOS OTC Labelling') in 
# the input folder in the same directory as this script

### Output:
# A text document containing the summary in the output folder


#-------------------------------------------------------------------------------
# INPUT PARAMETERS
#-------------------------------------------------------------------------------

# None!


#-------------------------------------------------------------------------------
# INITIAL SETTINGS
#-------------------------------------------------------------------------------

library(readr)
library(jsonlite)
library(dplyr)


#-------------------------------------------------------------------------------
# IMPORT DATA AND CONFIG FILE
#-------------------------------------------------------------------------------

# Import data
filepath <- paste("input/",list.files("input",pattern="csv$"), sep="")
df <- read_csv(filepath)

# Import config file and store the header converter as data frame
config <- read_json(path="config.json",format="json")

# Update the column names using the names in the config file
for (header in names(config$header_converter)){
  if (header %in% names(df)) {
    colnames(df)[which(names(df) == header)] <- 
      config$header_converter[[header]]
  }
}

# Store data object in output folder
saveRDS(df,file='input/exported_data.rds')


#-------------------------------------------------------------------------------
# CREATE SUMMARY
#-------------------------------------------------------------------------------



##########################
### OLD
cat("\r", input_files, "               ")

# Get the message counts
message_names <- vector(mode="character", length=0)
message_counts <- vector(mode="numeric", length=0)
message_rows <- 0

for (row in 1:nrow(df)) {
	messages <- as.character(data[["fCO2..uatm..QC.Comment"]][row])


	#if (length(messages) > 0) {
 if (nchar(messages) > 0) {

   message_rows <- message_rows + 1
	 message_list <- unlist(strsplit(messages, ";"))

	 for (m in 1:length(message_list)) {
		message <- as.character(message_list[m])

		if (nchar(message) > 0) {
			message_index <- which(message_names == message)
			if (length(message_index) == 0) {
				message_names[length(message_names) + 1] <- message
				message_counts[length(message_counts) + 1] <- 1
			} else {
				message_counts <- replace(message_counts, message_index, message_counts[message_index] + 1)
			}
		 }
		}
	}
}

total_row <- length(data[,1]) 
perc <- round(((message_rows/total_row)*100),2)

summary_file <- paste(OUTPUT_DIR, "/", input_files, ".summary.txt", sep="")
sink(summary_file)
cat("SUMMARY FOR FILE ", input_files, "\n\n\n", sep="")
cat("Total rows: ", total_row, "\n", sep="")
cat("Rows with messages: ", message_rows," (" ,perc,"%)","\n\n", sep="")                  
cat("QC Messages\n")
cat("===========\n")

for (i in 1:length(message_names)) {
	cat(message_names[i], ": ", message_counts[i], " (", format(round(message_counts[i] / nrow(data) * 100, 2), nsmall=2), "%)\n", sep="")
}
sink()