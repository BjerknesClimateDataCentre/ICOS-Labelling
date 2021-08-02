#####################################################################################
### SCRIPT FOR MERGING TWO DATASETS (LEFT MERGE)
###########################

Sys.setlocale("LC_ALL", "English"); 

##-----------------------------------------------------------------------------
### INPUT PARAMETERS


# Name of files to merge (the 'pri' file is the "most important one"- all rows are kept in
# the output merged file e.g. the co2 file, while the sst rows are added if there is a match
filename_pri <- "34FM20200815-ICOS OTC Labelling_losGatos_wDOY_2020_SHORT.txt"  
filename_sec <- "mea_all_stdval_wDOY_2020_SHORT.txt"

# Specify the separator
#sepp_pri <- "\t"
#sepp_sec <- "\t"      # e.g. "\t"

# The files must contain the date and time in DOY format (if does not: use the 'convert_to_doy' script) 
# Specify the column storing the DOY
doy_col_pri <- c(1)           
doy_col_sec <- c(1)             

# What should be added to the columns in order to separate their source files ('_' needed)
suffix_pri <- "_FromProcessedFile"          
suffix_sec <- "_FromRawFile"       

# Maximum allowed time difference in hours (usually use 5)
max_time_diff <- 1/(60)     

# The output filename
output_filename <- "Finnmaid_merged_raw_and_processed.txt"

# Columns which are not numbers (any text and date/time columns) need to be specified so that 
# these are not converted to numbers in the output file. Use many cols in array if needed.
#char_cols_pri <-  c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)   
#char_cols_sec <- c(2)


##-----------------------------------------------------------------------------
# Import data to merge, and store in separate data frames
library(readr)
library(dplyr)
#library(fuzzyjoin)

##-----------------------------------------------------------------------------
# Import data and store in separate tibles

filepath_pri <- paste("input/", filename_pri, sep="")
filepath_sec <- paste("input/", filename_sec, sep="")

df_pri <- read_tsv(filepath_pri)
df_sec <- read_tsv(filepath_sec)

#----------
# Chronology check. 

# Find number of non-chronological rows and write result to console. Non chornology
# needs to be fixed manually before can continue sctipt.

# Primary file:
n_non_chron_pri <- sum(diff(df_pri$DOY) < 0)
if (n_non_chron_pri != 0) {
  non_chron_row_pri <- which(diff(df_pri$DOY) < 0)
  cat("Primary file: Row(s) not in chronologic order:","\n",non_chron_row_pri,"\n")
  stop()
}

# Secondary file:
n_non_chron_sec <- sum(diff(df_sec$DOY) < 0)
if (n_non_chron_sec != 0) {
  non_chron_row_sec <- which(diff(df_sec$DOY) < 0)
  cat("Secondary file: Row(s) not in chronologic order:","\n",non_chron_row_sec,"\n")
  stop()
}

cat("Both files are in chronological order!","\n")


#----------
# Change the class of the date and time columns (and other non-numeric cols) so that they are not changed to numbers in the output file

#for (i in 1:length (main_char_cols)){
#  df_main[,main_char_cols[i]] <- as.character(df_main[,main_char_cols[i]])
#}

#for (j in 1:length (toAdd_char_cols)){
#  df_toAdd[,toAdd_char_cols[j]] <- as.character(df_toAdd[,toAdd_char_cols[j]])
#}


#------------

# Create new empty data frame with number of columns equal the sum of columns in the two datasets
#new_df <- data.frame(matrix(ncol=ncol(df_main) + ncol(df_toAdd),nrow=nrow(df_main)+nrow(df_toAdd)))
#colnames(new_df) <- c(paste(colnames(df_main),main_colName,sep=""),
#                      paste(colnames(df_toAdd),toAdd_colName,sep=""))

# Divide with 24 to get the same unit on the max hour and the doy (max hour is in hour, while doy is the day of year).
#max_time_diff <- max_time_diff/24

# Define counter for df_toAdd
#df_toAdd_row_count <- 1

# Set start value for the while loop inside the for loop
#df_toAdd_finished <- 0

#for (k in 1:nrow(df_main)) {
  
  # Calculate the current diff, BUT only if there are more rows left in toAdd file. 
#  if (df_toAdd_finished == 0) {
#    current_diff <- abs(df_main[k,doy_col_main] - df_toAdd[df_toAdd_row_count,doy_col_toAdd])
#  } else {
#    current_diff <- 999
#  }
  
#  found_best_match <- 0  
#  while (found_best_match == 0 && df_toAdd_finished == 0) {
#    next_diff <- abs(df_main[k,doy_col_main] - df_toAdd[df_toAdd_row_count + 1 ,doy_col_toAdd])
    
#    # If the next difference is smaller (a better match) or equal, we need to keep comapring with the next row.
#    if (current_diff >= next_diff) {
#      current_diff <- next_diff 
#      df_toAdd_row_count <- df_toAdd_row_count + 1 
    
    # If the next difference is larger (a worse match) we have found the best match (row number: df_toAdd_row_count)
#    } else {
#      found_best_match <- 1
#    }
    
    # Close the while loop if df_toAdd has no more rows
 #   if (df_toAdd_row_count >= nrow(df_toAdd)) {
#      df_toAdd_finished <- 1
#    }
#  }
  
  # Write the df_main row. Add the df_toAdd row if there is an acceptable match, add NaN if not.
#  if (current_diff <= max_time_diff) {
#    new_df[k,] <- c(df_main[k,], df_toAdd[df_toAdd_row_count,])
#    percent <- round((k/nrow(df_main))*100,0)
#    cat("\rProgress - merge: ", percent,"%", ". (At row nr", k, ").")
#  } else {
#    new_df[k,] <- c(df_main[k,], rep("NaN",ncol(df_toAdd)))
#  } 
  
#}
#cat("\nMerge complete!")


# Write the new merged data frame
#out_file <- paste(output_dir, "/", out_put_file_name, sep="")
#write.table(new_df, file=out_file, quote=FALSE, row.names=FALSE, fileEncoding="UTF8", sep="\t")