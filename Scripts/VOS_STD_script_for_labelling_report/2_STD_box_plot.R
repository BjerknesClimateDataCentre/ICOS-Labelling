########################################################################################
### Function for creating STD box plots to be used for the labellign step 2 reports
###########################
### FOR VOS only

### Run the script in a directory which contains an input folder and an output folder.
### The Input folder needs to contain one raw data file .
### The output folder will contain one boxplot showing the standard measurements anomalies.


##-----------------------------------------------------------------------------
##-----------------------------------------------------------------------------
# Input params to be assigned:
#none!

##-----------------------------------------------------------------------------
##-----------------------------------------------------------------------------

# Remove old figure produced by this script from the output directory
image <- paste("output/",list.files("output", pattern="raw.jpg$",), sep="")
file.remove(image)


input_dir<- "input"
output_dir<-"output"

# List all files in input directory
input_file <- list.files(input_dir)

# Get the path to file and read the data 
in_file <- paste(input_dir, "/", input_file, sep="")
data <- read.table(in_file,header=T, sep = "\t", strip.white=TRUE, fileEncoding="UTF8")



# Identify date and time
# (The if statement is related to whether there are one or two date/time columns in raw file)
if (date_col == time_col) {
  date.time <- as.POSIXct(data[,date_col], tz="UTC", format=dt_format)
} else {
  date.time <- as.POSIXct(paste(data[,date_col], data[,time_col]), tz="UTC", format=dt_format)          
}

# Add the date time column to the data frame
data$date.time <- date.time


# Extract data needed for plotting (those in cols) from each standard and store as separate list elements 
df_list <- list()
cols <- c(ncol(data), run_type_col, CO2_col, std_val_col)

for (i in 1:length(std_names)) {                                                                                            
  type <- std_names[i]
  data_sub <- data[data[,run_type_col]==type,cols]   
  data_sub$diff <- data_sub[[CO2_name]] - data_sub[[std_val_name]]     
  df_list[[i]] <- data_sub
  
}

# Find the max and min y-values for the plots
#diffs <-list()    
#for (j in 1:length(std_names)) {
#  diffs[[j]] <- df_list[[j]]$diff                                                  
#}   
#yrange <- range(c(diffs)) 

# Manually edit yrange for plot:
#if (yrange[1] < -10) {
#  yrange[1] <- -10
#}    
#if (yrange[2] > 10) {
#  yrange[2] <- 10
#}



## Make the plots as output
out_file <- paste(output_dir, "/", input_file, sep="")

# Make output path and filename 
filename <- paste((paste((sub("^([^.]*).*", "\\1", out_file)), sep="_")), "jpg", sep=".")

# Make the plot:
jpeg(filename)


boxplot(df_list[[1]]$diff, df_list[[2]]$diff, df_list[[3]]$diff, df_list[[4]]$diff, names = std_names, outline=FALSE, boxcol = c("green", "blue", "red", "purple"), medcol = c("green", "blue", "red", "purple"), ylab="Calibration anomaly [ppm]")
abline(h=0)

dev.off()


