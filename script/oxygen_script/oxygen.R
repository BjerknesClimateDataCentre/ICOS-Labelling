################################################################################
### ATMOSPHERIC CO2 PLOTS AND STATS
################################################################################

#-------------------------------------------------------------------------------
# INITIAL SETTINGS
#-------------------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Load packages
library(readr)
library(dplyr)
library(jsonlite)
library(ggplot2)

# Set the locale (needed for correct spelling of months in plots)
Sys.setlocale("LC_ALL", "English");

# Change the plot font (subscript 2 does not work in the png with default font)
windowsFonts(Times = windowsFont("Times New Roman"))


#-------------------------------------------------------------------------------
# IMPORT DATA AND SETTINGS
#-------------------------------------------------------------------------------

# Import data
dataname <- list.files("input", pattern = ".txt")
datapath <- paste0("input/", dataname)
df <- read_tsv(datapath)

# Import settings
settings <- read_json(path = "settings.json", format = "json")


#-------------------------------------------------------------------------------
# FUNCTIONS
#-------------------------------------------------------------------------------

# This function returns information about where in the plot to put the letter
# string. Required inputs are the name of the parameter, and in which corner
# of the plot the letter should be added.
create_letter_position <- function(letter_position_name, y_name, x_name,
                                   y_lims, x_lims) {
  
  # Create a data frame with location information about each corner in a
  # plot. This data frame is later used to determine where to place the letter 
  # string in plots.
  positions_template <- data.frame(
    xpos = c(-Inf, -Inf, Inf, Inf),
    ypos = c(-Inf, Inf, -Inf, Inf),
    hjustvar = c(-1, -1, 1, 1),
    vjustvar = c(-1, 1, -1, 1),
    location = c("bottomleft", "topleft", "bottomright", "topright"))
  
  # Get the row number in the positions template data frame where information 
  # about the requested corner is given
  position_index <- which(positions_template$location == letter_position_name)
  
  # The x and y-positions are either the max or min of the parameter. Which one
  # is determined by the sign in the xpos and ypos column in the position 
  # template. (!! This is confusing code - improve how this is done !!)
  if (positions_template$ypos[position_index] > 0) {
    
    if (is.na(y_lims[2])){
      ypos <- max(na.omit(as.numeric(df_mod[[y_name]])))
    } else {
      ypos <- na.omit(y_lims[2])
    }
    
  } else {
    
    if (is.na(y_lims[1])) {
      ypos <- min(na.omit(as.numeric(df_mod[[y_name]])))
    } else {
      ypos <- na.omit(y_lims[1])
    }
    
  }
  
  if (positions_template$xpos[position_index] > 0){
    
    if (is.na(x_lims[2])) {
      xpos <- max(na.omit(as.numeric(df_mod[[x_name]])))
    } else {
      xpos <- na.omit(x_lims[2])
    }
    
  } else {
    
    if (is.na(x_lims[1])) {
      xpos <- min(na.omit(as.numeric(df_mod[[x_name]])))
    } else {
      xpos <- na.omit(x_lims[1])
    }
    
  }
  
  # Change xpos class back to posixct if x axis data is date time
  if (x_name == "datetime") {
    xpos <- as.POSIXct(xpos, origin = '1970-01-01')
  }
  
  # The remaining letter position information is extracted straight from 
  # the position template data frame
  hjustvar <- positions_template$hjustvar[position_index]
  vjustvar <- positions_template$vjustvar[position_index]
  
  # Store all position details in a vector and return it
  letter_position <- list(xpos, ypos, hjustvar, vjustvar)
  return(letter_position)
}


#-------------------------------------------------------------------------------
# MODIFY THE DATA 
#-------------------------------------------------------------------------------

# Extract the column names from the settings
for (column_key in names(settings$required_columns)) {
  assign(column_key, settings$required_columns[[column_key]])
}

# Extract the plot settings
for (plot_key in names(settings$plot_settings)) {
  assign(plot_key, settings$plot_settings[[plot_key]])
}

# Create a datetime column; only select and rename the required columns; remove
# rows with missing value
df_mod <- df %>%
  mutate(datetime = as.POSIXct(paste(df[[date_colname]], df[[time_colname]]),
                               format = settings$datetime_format)) %>%
  {if (add_saturation) 
    select(., datetime, all_of(o2_colname), all_of(saturation_colname))
    else select(., datetime, all_of(o2_colname))} %>%
  {if (add_saturation)
    rename(., o2 = all_of(o2_colname), saturation = all_of(saturation_colname))
    else rename(., o2 = all_of(o2_colname))} %>%
  {if (settings$remove_missing)
    filter(., o2 != as.numeric(settings$missing_value)) 
    else .}


#-------------------------------------------------------------------------------
# PLOT ATMOSPHERIC CO2 VS TIME 
#-------------------------------------------------------------------------------

# Make the plot ranges numeric
o2_ylims <- as.numeric(unlist(strsplit(o2_ylims, ",")))
sat_ylims <- as.numeric(unlist(strsplit(sat_ylims, ",")))

# Get the letter position details
letter_position <- create_letter_position(letter_position_name, y_name = "o2", 
                                          x_name = "datetime", 
                                          y_lims = c(o2_ylims[1], o2_ylims[2]),
                                          x_lims = c(NA, NA))

# Set up the image file
png("output/1.o2_vs_time.png")

# Create the plot
plot <-  ggplot(df_mod, aes(x = datetime, y = o2)) +
  geom_point() +
  xlab("Time") + ylab(y_lab) + 
  # Specify monthly ticks with short month names as label
  scale_x_datetime(date_breaks = "1 month", date_labels = '%b') +
  # Change plot layout to another theme and so some adjustments to the theme
  theme_bw() +
  theme(text = element_text(family = "Times"),
        axis.text = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.7))) +
  # Add the plot letter string
  annotate("text",
           x = letter_position[[1]],
           y = letter_position[[2]], 
           label = letter_string,
           hjust = letter_position[[3]],
           vjust = letter_position[[4]],
           size = 9)

# Change the range in plot if specified in settings
if (!is.na(o2_ylims[1])){
  plot <- plot + ylim(o2_ylims[1], o2_ylims[2])
}

# Create the image
print(plot)
dev.off()