#--------------------------------------------------------------------------------
# Function for extracting from new_df to quince_df
extract_data <- function(from_df, to_array){
  
  # Loop through the data columns in current row
  for (i in 2:ncol(from_df)){
    if (!is.na(from_df[[i]])) {
      
      # Copy data from the "current_row" to the "to_print" array 
      to_array[i] <- as.character(from_df[[i]])
      
      # Copy the datetime from the "current_row" to the "to_print" array
      datetime_col <- length(from_df) + i - 1
      to_array[datetime_col] <- as.character(from_df[[1]])

    }
  }

# Find the correct location for the datetime column  
# co2_datetime_col <- length(current_row) + which(colnames(current_row)==co2_col) - 1

  return(to_array)
}



#--------------------------------------------------------------------------------


trigger_col <- which(colnames(new_df)==trigger)

data_finished <- 0
row_count <- 1
to_print <- rep(NA,length(colnames_quince_df))
state <- "look_for_trigger"


# Loop through lines in new_df
while (data_finished == 0) {
  current_row <- new_df[row_count,]

  ##-----------------------------
  ## STATE IS "LOOK FOR TRIGGER"
  if (state == "look_for_trigger") {

    if (!is.na(current_row[trigger_col])) { 
      # Function to extract trigger and other data on current row to array
      to_print <- extract_data(current_row,to_print)
      # Save the date.time when this measurement cycle starts
      measure_cycle_start <- as.POSIXct(current_row[[1]], tz="UTC", format="%Y-%m-%d %H:%M:%S")
      state <- "find_other_fields"
    }


  ##-----------------------------
  ## STATE IS "FIND OTHER FIELDS"
  } else if (state == "find_other_fields") {
     
     measure_cycle_datetime <- as.POSIXct(current_row[[1]], tz="UTC", format="%Y-%m-%d %H:%M:%S")
     time_diff <- abs(difftime(measure_cycle_datetime, measure_cycle_start, units="s"))


     # If a new trigger is reached: write data, clear array and extract data from trigger row
     if (!is.na(current_row[trigger_col])) {
      cat(to_print, sep="\t")
      cat("\n")
      to_print <- rep(NA,length(colnames_quince_df))
      
      to_print <- extract_data(current_row, to_print)
      measure_cycle_start <- as.POSIXct(current_row[[1]], tz="UTC", format="%Y-%m-%d %H:%M:%S")

     # Else if timegap is to large: write data, clear array, and set back state
     } else if (time_diff > max_time_ofset) {
      cat(to_print, sep="\t")
      cat("\n")
      to_print <- rep(NA,length(colnames_quince_df))
      state <- "look_for_trigger"
     
     # Or if non of the above: extract data from current row into array
     } else {
      # Function to extract all data from row into array
      to_print <- extract_data(current_row, to_print) #cat("Extract all data on current row. ")
     }
       
  }


  row_count <- row_count + 1
  if (row_count > nrow(new_df)) {
     data_finished <- 1
  }

}
# Print last row
cat(to_print, sep="\t")
cat("\n")
sink()