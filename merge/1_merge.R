#####################################################################################
### Function for checking if equilibrium is reached for the raw FOS data
###########################

Sys.setlocale("LC_ALL", "English"); 

##-----------------------------------------------------------------------------

### Input parameters:



max_time_diff <- 5               # In hours (usually use 5 unless get told something else)

time_col_file1 <- c(2)              # The format must be day of year. If other format add this to script.
time_col_file2 <- c(2)              # File 1 and File 2 are defined alfabetically

file1_colName <- "_1m"             # What should be added to the columns in order 
file2_colName <- "_30m"              # to separate between the two files afterwards.

file1_sepp <- ","
file2_sepp <- ","


##-----------------------------------------------------------------------------

# Import data to merge and store in data frames
input_dir<-"input"
output_dir<-"output"

input_files <- list.files(input_dir)

in_file <- paste(input_dir, "/", input_files[1], sep="")
in_file2 <- paste(input_dir, "/", input_files[2], sep="")

df_1 <- read.table(in_file,header=T, sep=file1_sepp, fileEncoding="UTF8")
df_2 <- read.table(in_file2,header=T, sep=file2_sepp, fileEncoding="UTF8")


#----------

# Fix date and time 
#date.time_1m <- as.POSIXct(df_1m$date.time, tz="UTC", format="%Y-%m-%d %H:%M:%S" )
#df_1m$date.time <- date.time_1m

#date.time_30m <- as.POSIXct(df_30m$date.time, tz="UTC", format="%Y-%m-%d %H:%M:%S" )
#df_30m$date.time <- date.time_30m


#------------

# Create new empty data frame with enough space (cols) for both datasets.
nCol_new_df <- ncol(df_1) + ncol(df_2)
new_df <- data.frame(matrix(ncol=nCol_new_df,nrow=0))
colname_1 <- paste(colnames(df_1),file1_colName,sep="")
colname_2 <- paste(colnames(df_2),file2_colName,sep="")
colnames(new_df) <- c(colname_1, colname_2)


# USe a while loop to merge the two datasets. 
row_count_1 <- 1
row_count_2 <- 1
row_count_new <- 1

data_1_finished <- 0
data_2_finished <- 0

time_diff <- max_time_diff/24

state <- "find_match"

dummy_match_count <- 0

#------------
while (data_1_finished == 0 && data_2_finished == 0) {
  
  current_time_1 <- df_1[row_count_1, time_col_file1]
  current_time_2 <- df_2[row_count_2, time_col_file2]
  
  #------------ 
  # FIND MATCH
  if (state == "find_match") {
    
    # Compare the current file1 time with the current file2 time has 3 outcomes:
    # Outcome 1: file2 time is more than the acepted number of hours EARLIER -> start loop again and compare with next file2 row
    if (current_time_1 - current_time_2 > time_diff) {
      row_count_2 <- row_count_2 + 1
    
    # Outcome 2: file2 time is more than the acepted number of hours LATER -> jump to next row in file1 data
    } else if (current_time_2 - current_time_1 > time_diff) {
      row_count_1 <- row_count_1 + 1
      
    # Outcome 3: file2 time is between 5 hours earlier and 5 hours later than file1 time -> we have a timestamp match
    } else { 
      state <- "write_match"
    }
  
  }
  
  #------------
  # WRITE MATCH
  if(state == "write_match") {
  
    new_df[row_count_new,] <- c(df_1[row_count_1,], df_2[row_count_2,])

    # Increase all row counts and change state back to find match
    row_count_1 <- row_count_1 + 1
    row_count_2 <- row_count_2 + 1
    row_count_new <- row_count_new + 1
    state <- "find_match"
  } 
  
  
  # Stop loop if fall of data frames
  if(row_count_1 > nrow(df_1)) {
    data_1_finished <- 1
  }
  if(row_count_2 > nrow(df_2)) {
    data_2_finished <- 1
  }
  
}


out_file <- paste(output_dir, "/", "merged.txt", sep="")
write.table(new_df, file=out_file, row.names=FALSE, fileEncoding="UTF8", sep="\t")















