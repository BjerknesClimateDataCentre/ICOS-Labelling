########################################################################################
### Function for creating STD anomaly plots to be used for the labellign step 2 reports
###########################

### Run the script in a directory which contains an input folder and an output folder.
### The Input folder needs to contain raw data file(s) (they need to have the same format and standards).
### Output folder will one plot (per file) showing the standard measurements anomalies.

### The script has 7 input paramters that needs to be assign before the script is ran:
### - date_col
### - time_col
### - dt_format
### - CO2_col
### - run_type_col
### - std_names
### - std_val_col


##-----------------------------------------------------------------------------
# Input params to be assigned:

date_col <-5
time_col <- 6
dt_format <- "%d/%m/%y %H:%M:%S"            # e.g. "%d/%m/%y %H:%M:%S"
CO2_col <- 14
run_type_col <- 3
std_names <- c("STD1","STD2","STD3","STD4")
std_val_col <- 12

#------------------------------------------------------------------------------
# Set work directory
wd <- "D:/MyFiles/Projects/ICOS/Labelling/Labelling Step 2/Reports/STD_script_for_labelling_report"
setwd(wd)

##-----------------------------------------------------------------------------

  input_dir<- "input"
  output_dir<-"output"
  
  # List all files in input directory
  input_files <- list.files(input_dir)
 
  
  # Loop through the input files
  	for (file_loop in 1:length(input_files)) {
     
	    # Get the path to file and read the data 
	    in_file <- paste(input_dir, "/", input_files[file_loop], sep="")
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
	    
	    ## Make the plots as output
	    out_file <- paste(output_dir, "/", input_files[file_loop], sep="")
	    
	    # Make output path and filename 
	    filename <- paste((paste((sub("^([^.]*).*", "\\1", out_file)),"time", sep="_")), "jpg", sep=".")
	      
	    # Make the plot:
	    jpeg(filename, width=1000)
          
        
        # Extract data needed for plotting (those in cols) from each standard and store as separate list elements 
        df_list <- list()
        cols <- c(ncol(data), run_type_col, CO2_col, std_val_col)
        
        for (i in 1:length(std_names)) {                                                                                              
	      
	        type <- std_names[i]
	        data_sub <- data[data[,run_type_col]==type,cols]   
	        data_sub$diff <- data_sub$CO2.um.m - data_sub$std.val     
	        df_list[[i]] <- data_sub
	     
	    }
      

        # Find the max and min y-values for the plots
	    diffs <-list()    
	    for (j in 1:length(std_names)) {
	        diffs[[j]] <- df_list[[j]]$diff                                                  
	    }   
	    yrange <- range(c(diffs)) 
	    
      # Manually edit yrange for plot:
	    if (yrange[1] < -5) {
	    	yrange[1] <- -5
	    }    
        if (yrange[2] > 5) {
        	yrange[2] <- 5
        }


	    # Make plot 
	    #plot_name <-paste((sub("^([^.]*).*", "\\1", input_files[file_loop])), sep="_")
	    color <- c("green", "blue", "red", "purple")
        
        zeros <- rep(0, length(data$date.time))
        plot (data$date.time, zeros, col="black", lwd=1, xlab="Date", ylab="CO2 amonaly [unit]", type="l", ylim = c(yrange[1], yrange[2]))

	    for (k in 1:length(std_names)) {
            points(df_list[[k]]$date.time, df_list[[k]]$diff, col=color[k])
		}
  
	    legend("topleft", legend = c("STD1", "STD2", "STD3", "STD4", "Base line"), pch, col=c(color,"black"), lwd=c(2,2,2,2,2))
	    dev.off()
    
    
  	
  	
  	}
