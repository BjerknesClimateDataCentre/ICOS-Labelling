################################################################################
### PLOT ALL MEASUREMETNS
################################################################################

### Description:
# 

### Requirements:
# 

### Output:
# 


#-------------------------------------------------------------------------------
# INITIAL SETTINGS
#-------------------------------------------------------------------------------

library(readr)
library(jsonlite)
library(ggplot2)
#library(extrafont) Uninstall this package
#library(ggpubr) Uninstall this package!!!

Sys.setlocale("LC_ALL", "English");

windowsFonts(Times=windowsFont("Times New Roman"))

#-------------------------------------------------------------------------------
# IMPORT DATA AND CONFIG FILE
#-------------------------------------------------------------------------------

# Import data
datafile_name <- list.files("input",pattern="csv$")
datafile_path <- paste("input/",datafile_name, sep="")
df <- read_csv(datafile_path)

# Import header config and change the headers of the data frame
header_config <- read_json(path="header_config.json",format="json")
for (header in names(header_config$header_converter)){
  if (header %in% names(df)) {
    colnames(df)[which(names(df) == header)] <- 
      header_config$header_converter[[header]]
  }
}

# Import the input parameters
settings <- read_json(path="settings.json", format="json")

# Update column names related to the raw CO2
colnames(df)[which(names(df) == settings$raw_CO2_colname)] <- "raw_CO2"
colnames(df)[which(names(df) == paste(settings$raw_CO2_colname," QC Flag",sep=""))] <-
  "raw_CO2_flag"
colnames(df)[which(names(df) == paste(settings$raw_CO2_colname," QC Comment",sep=""))] <-
  "raw_CO2_comm"


#-------------------------------------------------------------------------------
# FUNCTION(S)
#-------------------------------------------------------------------------------

create_plot <- function(param, plot_count, y_lab, y_lims, letter_position) {

  letter_text <- paste(letters[plot_count],")",sep="")
  
  filename <- paste("output/",plot_count,"_",param,".png", sep="")
  png(filename)
  ret <- ggplot(df, aes(x = datetime, y = as.numeric(df[[param]]))) +
    geom_point() +
    xlab("Time") + ylab(y_lab) + 
    scale_x_datetime(date_breaks="1 month", date_labels = '%b') +
    theme_bw() +
    theme(axis.text=element_text(size=rel(1.5)),
          axis.title=element_text(size=rel(1.7)),
          text=element_text(family="Times"))  +#, face="bold"))  +
    annotate("text",
             x = letter_position[[1]],
             y = letter_position[[2]], 
             label = letter_text,
             hjust = letter_position[[3]],
             vjust = letter_position[[4]],
             size=9)
  
  if (!is.na(y_lims[1])){
    ret <- ret + ylim(y_lims[1], y_lims[2])
  }
  
  print(ret)
  dev.off()
}


#Create function that returns the the letter location
create_letter_position <- function(letter_position_name, param) {
  
  position_index <- which(positions$location==letter_position_name)
  
  if (positions$ypos[position_index] > 0) {
    ypos <- max(na.omit(as.numeric(df[[param]])))
  } else {
    ypos <- min(na.omit(as.numeric(df[[param]])))
  }

  xpos <- positions$xpos[position_index] 
  hjustvar <- positions$hjustvar[position_index]
  vjustvar <- positions$vjustvar[position_index]
  
  letter_position <- list(xpos,ypos,hjustvar,vjustvar)
  
  return(letter_position)
}


#-------------------------------------------------------------------------------
# CREATE THE PLOTS
#-------------------------------------------------------------------------------

# Create positions template
positions <- data.frame(
  xpos = c(min(df$datetime),min(df$datetime),max(df$datetime),max(df$datetime)),
  ypos =  c(-Inf,Inf,-Inf,Inf),
  hjustvar = c(-1,-1,1,1),
  vjustvar = c(-1,1,-1,1),
  location = c("bottomleft","topleft","bottomright","topright"))

# Loop through the parameter list and create the plots
plot_count <- 1

sink(file = "output/out_of_range.txt")
for (parameter in settings$parameter_list){
  
  if (parameter$make_plot) {
    
    # Extract all these in a loop?
    param <- parameter$param_name
    y_lab <- parameter$y_lab
    y_lims <- parameter$y_lims
    letter_position_name <- parameter$letter_position_name
  
    # Change ylim to vector
    y_lims <- as.numeric(unlist(strsplit(y_lims, ",")))
    
    # Get the letter position details
    letter_position <- create_letter_position(letter_position_name, param)
  
    # Create the plot
    create_plot(param, plot_count, y_lab, y_lims, letter_position)
    
    # Print outliers to screen if there are any
    if (!is.na(y_lims[1])){

      n_meas <- length(na.omit(as.numeric(df$peq)))
      
      outlier_low <- sum(na.omit(as.numeric(df[[param]])) < y_lims[1])
      percent_low <- round((outlier_low/n_meas)*100,1)
      cat("\nNumber of ", param, " measurements lower than ", y_lims[1], ": ",
          outlier_low, " (", percent_low, "%)", sep="")
      
      outlier_high <- sum(na.omit(as.numeric(df[[param]])) > y_lims[2])
      percent_high <- round((outlier_high/n_meas)*100,1)
      cat("\nNumber of ", param, " measurements higher than ", y_lims[2], ": ",
          outlier_high, " (", percent_high, "%)", sep="")
    }
    
  }
  
  plot_count <- plot_count + 1
  
}
sink()