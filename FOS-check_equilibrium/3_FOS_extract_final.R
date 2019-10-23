#####################################################################################
### Function for checking extracting only the final equilibrated measurement
###########################

Sys.setlocale("LC_ALL", "English"); 

##-----------------------------------------------------------------------------
### The script has some input parameters:

#xco2_col_name <- "xCO2"

#CO2_col <- c(19)                        # This is for xCO2 or pCO2
#additional_CO2_col <- c(20) 

extract_final <- FALSE                   # If FALSE then an average of the final "number_of_meas" measurements are extracted 
                                         # instead of the final one.

# Inout from previous script used here:
# - the actual data frame, with proper date time (posix class)
# - max_seq_diff
# - CO2_col
# - numb_of_meas

#------------------------------------------------------------------------------
# Import new dataset

#output_dir<- "output"

#input_file <- list.files(output_dir, pattern = "txt")
#in_file <- paste(output_dir, "/", input_file, sep="")
#df_sub <- read.csv(in_file,header=T, sep = ",", strip.white=TRUE, fileEncoding="UTF8")
#df_sub$date.time <- as.POSIXct(df_sub$date.time, tz="UTC", format="%Y-%m-%d %H:%M:%S")


#-------------------------------------------------
# Extract the last value (or average of values) in each measurement cycle

# Create new empty data frame
new_df <- data.frame(matrix(ncol=ncol(df), nrow=0))
colnames(new_df) <- colnames(df)

# Fill data frame with while loop- Here are some start values for the loop
data_finished <- 0
df_row <- 1
new_df_row <-1
state <- "find_seq_end"


  while (data_finished == 0) {
    current_diff <- df[df_row + 1,time_diff_col] #abs(difftime(df[df_row,ncol(df)],df[df_row+1,ncol(df)],unit="secs"))    #PS: ncol(df) is the last column number where the dateTime are stored
    
    if(state=="find_seq_end") {
      if(current_diff>max_seq_diff){
        state <- "write_row"
      }
    }

    if(state=="write_row") {
      new_df[new_df_row,] <- df[df_row,]
      
      if(extract_final==FALSE){
        CO2_vec <- rep(NA,numb_of_meas)
        
        for (p in 1:numb_of_meas) {
          CO2_vec[p] <- df[(df_row-p)+1,CO2_col]
        }
        
        mean_CO2 <- sum(na.omit(CO2_vec))/length(na.omit(CO2_vec))
        new_df[new_df_row,CO2_col] <- mean_CO2
      }
      
      #new_df$date.time[new_df_row] <- toString(df$date.time[df_row])
      new_df_row <- new_df_row + 1
      state <- "find_seq_end"
      
      print(new_df_row)
    } 

    
    
    
    df_row <- df_row + 1
    if(df_row == nrow(df) - 1) {
      data_finished <- 1
    }

  }




out_file <- paste(output_dir, "/", "co2_raw_edited.txt", sep="")
write.table(new_df, file=out_file, row.names=FALSE, fileEncoding="UTF8", sep=sepp)






