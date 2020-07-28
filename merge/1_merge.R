#####################################################################################
### SCRIPT FOR MERGING TWO DATASETS
###########################

Sys.setlocale("LC_ALL", "English"); 

##-----------------------------------------------------------------------------
### INPUT PARAMETERS


# Name of files to merge (the 'main' file is the "most important one"- all rows are kept in
# the output merged file e.g. the co2 file, while the sst rows are added if there is a match
file_main <- "KW40_dat.txt_wDOY.txt"  
file_toAdd <- "KW40_ENV_DShip.dat_wDOY.txt"

# Specify the separator
main_sepp <- "\t"
toAdd_sepp <- "\t"      # e.g. "\t"

# The files must contain the date and time in DOY format (if does not: use the 'convert_to_doy' script) 
# Specify the column storing the DOY
doy_col_main <- c(1)           
doy_col_toAdd <- c(1)             

# What should be added to the columns in order to spearate their sorce file ('_' needed)
main_colName <- ""          
toAdd_colName <- ""       

# Maximum allowed time difference in hours (usually use 5)
max_time_diff <- 2/(60)     

# The output file name
out_put_file_name <- "Polarstern_2019_KW40.txt"

# Columns which are not numbers (any text and date/time columns) need to be specified so that 
# these are not converted to numbers in the outputfile. Use many cols in array if needed.
main_char_cols <-  c(2,4,5,6,7)   
toAdd_char_cols <- c(2)



##-----------------------------------------------------------------------------
# Import data to merge, and store in separate data frames
input_dir<-"input"
output_dir<-"output"

in_main <- paste(input_dir, "/", file_main, sep="")
in_toAdd <- paste(input_dir, "/", file_toAdd, sep="")

df_main <- read.table(in_main,header=T, sep=main_sepp, fileEncoding="UTF8")
df_toAdd <- read.table(in_toAdd,header=T, sep=toAdd_sepp, fileEncoding="UTF8")



#----------
# Check if date is chronological (DOY shoyld not decrease!).
# If it is non-chronological, this needs to be fixed before can run this merge script!

# Main file:
for (g in 1:(nrow(df_main)-1)) {
  main_doy_diff <- df_main[g+1,doy_col_main] - df_main[g,doy_col_main]
  if (main_doy_diff < 0) {
    stop("Main file does not have chronological DOY")
  }
}

# ToAdd file:
for (h in 1:(nrow(df_toAdd)-1)) {
  toAdd_doy_diff <- df_toAdd[h+1,doy_col_toAdd] - df_toAdd[h,doy_col_toAdd]
  if (toAdd_doy_diff < 0) {
    stop(cat("ToAdd file does not have chronological DOY. Stop at row ",h,sep=""))
  }
}


#----------
# Change the class of the date and time columns (and other non-numeric cols) so that they are not changed to numbers in the output file

for (i in 1:length (main_char_cols)){
  df_main[,main_char_cols[i]] <- as.character(df_main[,main_char_cols[i]])
}

for (j in 1:length (toAdd_char_cols)){
  df_toAdd[,toAdd_char_cols[j]] <- as.character(df_toAdd[,toAdd_char_cols[j]])
}


#------------
# Create new empty data frame with number of columns equal the sum of columns in the two datasets
nCol_new_df <- ncol(df_main) + ncol(df_toAdd)
new_df <- data.frame(matrix(ncol=nCol_new_df,nrow=0))
colname_1 <- paste(colnames(df_main),main_colName,sep="")
colname_2 <- paste(colnames(df_toAdd),toAdd_colName,sep="")
colnames(new_df) <- c(colname_1, colname_2)

# Divide with 24 to get the same unit on the max hour and the doy (max hour is in hour, while doy is the day of year).
max_time_diff <- max_time_diff/24

# Define counter for df_toAdd
df_toAdd_row_count <- 1

# Set start value for the while loop inside the for loop
df_toAdd_finished <- 0

for (k in 1:nrow(df_main)) {
  
  # Calculate the current diff, BUT only if there are more rows left in toAdd file. 
  if (df_toAdd_finished == 0) {
  current_diff <- abs(df_main[k,doy_col_main] - df_toAdd[df_toAdd_row_count,doy_col_toAdd])
  } else {
    current_diff <- 999
  }
  
  found_best_match <- 0  
  while (found_best_match == 0 && df_toAdd_finished == 0) {
    next_diff <- abs(df_main[k,doy_col_main] - df_toAdd[df_toAdd_row_count + 1 ,doy_col_toAdd])
    
    # If the next difference is smaller (a better match) or equal, we need to keep comapring with the next row.
    if (current_diff >= next_diff) {
      current_diff <- next_diff 
      df_toAdd_row_count <- df_toAdd_row_count + 1 
    
    # If the next difference is larger (a worse match) we have found the best match (row number: df_toAdd_row_count)
    } else {
      found_best_match <- 1
    }
    
    # Close the while loop if df_toAdd has no more rows
    if (df_toAdd_row_count >= nrow(df_toAdd)) {
      df_toAdd_finished <- 1
    }
  }
  
  # Write the df_main row. Add the df_toAdd row if there is an acceptable match, add NaN if not.
  if (current_diff <= max_time_diff) {
    new_df[k,] <- c(df_main[k,], df_toAdd[df_toAdd_row_count,])
  } else {
    new_df[k,] <- c(df_main[k,], rep("NaN",ncol(df_toAdd)))
  } 
  
}


# Write the new merged data frame
out_file <- paste(output_dir, "/", out_put_file_name, sep="")
write.table(new_df, file=out_file, quote=FALSE, row.names=FALSE, fileEncoding="UTF8", sep="\t")