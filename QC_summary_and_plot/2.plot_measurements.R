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

# Original column name of the measured CO2
raw_CO2_colname <- "S1_CO2w"


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

# Import config file and store the header converter as data frame
config <- read_json(path="config.json",format="json")

# Update the column names using the names in the config file
for (header in names(config$header_converter)){
  if (header %in% names(df)) {
    colnames(df)[which(names(df) == header)] <- 
      config$header_converter[[header]]
  }
}

# Update column names related to the raw CO2
colnames(df)[which(names(df) == raw_CO2_colname)] <- "raw_CO2"
colnames(df)[which(names(df) == paste(raw_CO2_colname," QC Flag",sep=""))] <-
  "raw_CO2_flag"
colnames(df)[which(names(df) == paste(raw_CO2_colname," QC Comment",sep=""))] <-
  "raw_CO2_comm"


#-------------------------------------------------------------------------------
# FUNCTION(S)
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# CREATE THE PLOTS
#-------------------------------------------------------------------------------

annotations <- data.frame(
  xpos = c(min(df$datetime),min(df$datetime),max(df$datetime),max(df$datetime)),
  ypos =  c(min(df$temp),max(df$temp),min(df$temp),max(df$temp)),
  annotateText = c(rep("a)",4)),
  hjustvar = c(-1,-1,1,1) ,
  vjustvar = c(-1,1,-1,1))



png("output/temp_plot.png")

ggplot(df, aes(x = datetime, y = temp)) +
  geom_point() + 
  xlab("Time") + ylab(expression(paste("Intake Temperature [ ",degree,"C]"))) + 
  scale_x_datetime(date_breaks="1 month", date_labels = '%b') + 
  theme_bw() +
  theme(axis.text=element_text(size=rel(1.5)),
        axis.title=element_text(size=rel(1.7))
  ) + 
  annotate("text", x = annotations$xpos[4], y = annotations$ypos[4], 
           label = annotations$annotateText[4], hjust = annotations$hjustvar[4],
           vjust = annotations$vjustvar[4], size=9)
  
  
dev.off()