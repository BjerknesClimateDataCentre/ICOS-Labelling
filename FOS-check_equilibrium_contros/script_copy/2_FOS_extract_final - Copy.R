#####################################################################################
### Function for checking extracting only the final equilibrated measurement
###########################

Sys.setlocale("LC_ALL", "English"); 

##-----------------------------------------------------------------------------
### The script has some input parameters:

pco2_calc_col_name <- "pCO2"
pco2_meas_col_name <- "pCO2_corr"

flush_col_name <- "Flush"
zero_col_name <- "Zero"


#-------------------------------------------------
# Extract the avera of pco2 measuremens values in each cycle


data_finished <- 0
count_df <- 1
count_new_df <- 1
summ <- 0

new_df <- data.frame(matrix(ncol=ncol(df), nrow=0))
colnames(new_df) <- colnames(df)

state <-"find_new_sequence"


while (data_finished == 0) {

  if (state == "find_new_sequence"){   
    if (df[[zero_col_name]][count_df] == 0 & df[[flush_col_name]][count_df] == 0) {
      seq_start <- count_df
      state <- "calculate_average"
    }
  }
  
  if (state == "calculate_average") {
    if (df[[zero_col_name]][count_df] == 0 & df[[flush_col_name]][count_df] == 0) {
      summ <- summ + df[[pco2_col_name]][count_df]
   
    } else {
      average <- summ / (count_df - seq_start) 
      
      new_df[count_new_df,] <- df[count_df-1,] 
      new_df$date.time[count_new_df] <- toString(df$date.time[count_df-1])
      new_df[[pco2_col_name]][count_new_df] <- average
      count_new_df <- count_new_df + 1
      
      summ <- 0
      state <- "find_new_sequence"
      
    }
  }


  count_df <- count_df + 1
  if (count_df > nrow(df)) {
    data_finished <- 1
    
    # In order to include the last line of data:
    if(summ > 0) {
      average <- summ / (count_df - seq_start) 
      
      new_df[count_new_df,] <- df[count_df-1,] 
      new_df$date.time[count_new_df] <- toString(df$date.time[count_df-1])
      new_df[[pco2_col_name]][count_new_df] <- average
      count_new_df <- count_new_df + 1
      
      summ <- 0
    }
  
  }

}


out_file <- paste(output_dir, "/", "co2_raw_edited.txt", sep="")
write.table(new_df, file=out_file, row.names=FALSE, fileEncoding="UTF8", sep=sepp)






