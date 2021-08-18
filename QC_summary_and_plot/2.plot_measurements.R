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
# INPUT PARAMETERS
#-------------------------------------------------------------------------------


#letter_position_name <- "topright"

#parameter_list <- c("temp", "teq","sal","peq","raw_CO2")

#axis_labels <- c("Intake Temperature","Temperature at Equilibrator")


#-------------------------------------------------------------------------------
# INITIAL SETTINGS
#-------------------------------------------------------------------------------

library(readr)
library(jsonlite)
library(ggplot2)

Sys.setlocale("LC_ALL", "English");


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

create_plot <- function(param, plot_count, x_lab, y_lab,letter_position) {

  letter_text <- paste(letters[plot_count],")",sep="")
  
  filename <- paste("output/",plot_count,"_",param,".png", sep="")
  png(filename)
  ret <- ggplot(df, aes(x = datetime, y = as.numeric(df[[param]]))) +
    geom_point() +
    xlab(x_lab) + ylab(y_lab) + 
    scale_x_datetime(date_breaks="1 month", date_labels = '%b') +
    theme_bw() +
    theme(axis.text=element_text(size=rel(1.5)),
          axis.title=element_text(size=rel(1.7)))  +
    annotate("text",
             x = letter_position[[1]],
             y = letter_position[[2]], 
             label = letter_text,
             hjust = letter_position[[3]],
             vjust = letter_position[[4]],
             size=9)
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
x_lab <- "Time"

for (parameter in settings$parameter_list){
  
  if (parameter$make_plot) {
    param <- parameter$param_name
    y_lab <- parameter$axis_label
    letter_position_name <- parameter$letter_position_name
    
    letter_position <- create_letter_position(letter_position_name, param)
  
    create_plot(param, plot_count, x_lab, y_lab, letter_position)
  }
  
  plot_count <- plot_count + 1
  
}