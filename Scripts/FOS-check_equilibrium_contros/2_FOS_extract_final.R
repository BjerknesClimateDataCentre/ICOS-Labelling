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
# Extract the row with the final measurement value, but replace the measured and calculaed pco2 with 
# the average over the equilibrated measurements (when zero and flush is zero). 

data_finished <- 0
count_df <- 1
count_new_df <- 1
summ_calc <- 0
summ_meas <-0

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
      summ_calc <- summ_calc + df[[pco2_calc_col_name]][count_df]
      summ_meas <- summ_meas + df[[pco2_meas_col_name]][count_df]
   
    } else {
      average_calc <- summ_calc / (count_df - seq_start) 
      average_meas <- summ_meas / (count_df - seq_start)
      
      new_df[count_new_df,] <- df[count_df-1,] 
      new_df$date.time[count_new_df] <- toString(df$date.time[count_df-1]) 
      new_df[[pco2_calc_col_name]][count_new_df] <- average_calc
      new_df[[pco2_meas_col_name]][count_new_df] <- average_meas
      
      count_new_df <- count_new_df + 1
      
      summ_calc <- 0
      summ_meas <- 0
     
      state <- "find_new_sequence"
      
    }
  }


  count_df <- count_df + 1
  if (count_df > nrow(df)) {
    data_finished <- 1
    
    # In order to include the last line of data:
    if(summ_calc > 0) {
      average_calc <- summ_calc / (count_df - seq_start) 
      average_meas <- summ_meas / (count_df - seq_start)
      
      new_df[count_new_df,] <- df[count_df-1,] 
      new_df$date.time[count_new_df] <- toString(df$date.time[count_df-1])
      new_df[[pco2_calc_col_name]][count_new_df] <- average_calc
      new_df[[pco2_meas_col_name]][count_new_df] <- average_meas
      
      count_new_df <- count_new_df + 1
      
      summ_calc <- 0
      summ_meas <- 0
    }
  
  }

}


out_file <- paste(output_dir, "/", "co2_raw_edited.txt", sep="")
write.table(new_df, file=out_file, row.names=FALSE, fileEncoding="UTF8", sep=sepp)






