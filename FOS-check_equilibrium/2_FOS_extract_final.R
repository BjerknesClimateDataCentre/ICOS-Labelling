#####################################################################################
### Function for checking extracting only the final equilibrated measurement
###########################

Sys.setlocale("LC_ALL", "English"); 

##-----------------------------------------------------------------------------
### The script has some input parameters:

#xco2_col_name <- "xCO2"

#CO2_col <- c(19)                        # This is for xCO2 or pCO2
#additional_CO2_col <- c(20) 



#------------------------------------------------------------------------------
# Import new dataset

#output_dir<- "output"

#input_file <- list.files(output_dir, pattern = "txt")
#in_file <- paste(output_dir, "/", input_file, sep="")
#df_sub <- read.csv(in_file,header=T, sep = ",", strip.white=TRUE, fileEncoding="UTF8")
#df_sub$date.time <- as.POSIXct(df_sub$date.time, tz="UTC", format="%Y-%m-%d %H:%M:%S")


#-------------------------------------------------
# Extract the last values in each cycle


data_finished <- 0
current_row <- 1
count <-1

new_df <- data.frame(matrix(ncol=ncol(df), nrow=0))
colnames(new_df) <- colnames(df)


while (data_finished == 0) {

current_diff <- abs(difftime(df$date.time[current_row],df$date.time[current_row+1],unit="secs"))

if(current_diff<time_limit){
  current_row <- current_row+1
} else {
    new_df[count,] <- df[current_row,]
    new_df$date.time[count] <- toString(df$date.time[current_row])
    count <- count + 1
    current_row <- current_row +1 
}

if(current_row == nrow(df)-1) {
  data_finished <- 1
}

}

out_file <- paste(output_dir, "/", "co2_raw_edited.txt", sep="")
write.table(new_df, file=out_file, row.names=FALSE, fileEncoding="UTF8", sep=sepp)






