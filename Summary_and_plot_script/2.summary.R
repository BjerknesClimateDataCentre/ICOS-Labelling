##################
## CREATE A SUMMARY OF QC MESSAGES FROM QUINCE
## For VOS and FOS

#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
# Input parameters:
# none!

#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------


library(ncdf4)

# File locations
INPUT_DIR <- "output"
OUTPUT_DIR <- "output"

# Get the files in the input directory
input_files <- list.files(INPUT_DIR, pattern="csv$")

for (file_loop in 1:length(input_files)) {
	# Load the data
	cat("\r", input_files[file_loop], "               ")
	in_file <- paste(INPUT_DIR, "/", input_files[file_loop], sep="")
	data <- read.csv(in_file,header=T, fileEncoding="UTF8")
	
	# Get the message counts
	message_names <- vector(mode="character", length=0)
	message_counts <- vector(mode="numeric", length=0)
	message_rows <- 0

	for (row in 1:nrow(data)) {
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
  
  summary_file <- paste(OUTPUT_DIR, "/", input_files[file_loop], ".summary.txt", sep="")
	sink(summary_file)
	cat("SUMMARY FOR FILE ", input_files[file_loop], "\n\n\n", sep="")
  cat("Total rows: ", total_row, "\n", sep="")
  cat("Rows with messages: ", message_rows," (" ,perc,"%)","\n\n", sep="")                  
	cat("QC Messages\n")
	cat("===========\n")
	

	for (i in 1:length(message_names)) {
		cat(message_names[i], ": ", message_counts[i], " (", format(round(message_counts[i] / nrow(data) * 100, 2), nsmall=2), "%)\n", sep="")
	}
	sink()

  
}