################################################################################
### CHECK EQUILIBRATION/STANDARD DEVIATION OF RAPID CO2 MEASUREMENTS
################################################################################

#-------------------------------------------------------------------------------
# INITIAL CONFIGS
#-------------------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Load packages
library(readr)
library(dplyr)
library(jsonlite)
library(ggplot2)
library(ggpubr)
library(gridExtra)

# Set the locale (needed for correct spelling of months in plots)
Sys.setlocale("LC_ALL", "English");

# Change the plot font (subscript 2 does not work in the png with default font)
windowsFonts(Times = windowsFont("Times New Roman"))


#-------------------------------------------------------------------------------
# FUNCTIONS
#-------------------------------------------------------------------------------

extract_settings <- function(settings){
  for (key in names(settings)) {
    assign(key, settings[[key]], envir = .GlobalEnv)
  }
}

# This function returns information about where in the plot to put the letter
# string. Required inputs are the name of the parameter, and in which corner
# of the plot the letter should be added.
create_letter_position <- function(df, letter_position_name, y_name, x_name,
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
      ypos <- max(as.numeric(na.omit(df[[y_name]])))
    } else {
      ypos <- na.omit(y_lims[2])
    }
    
  } else {
    
    if (is.na(y_lims[1])) {
      ypos <- min(as.numeric(na.omit(df[[y_name]])))
    } else {
      ypos <- na.omit(y_lims[1])
    }
    
  }
  
  if (positions_template$xpos[position_index] > 0){
    
    if (is.na(x_lims[2])) {
      xpos <- max(as.numeric(na.omit(df[[x_name]])))
    } else {
      xpos <- na.omit(x_lims[2])
    }
    
  } else {
    
    if (is.na(x_lims[1])) {
      xpos <- min(as.numeric(na.omit(df[[x_name]])))
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
# IMPORT DATA AND SETTINGS
#-------------------------------------------------------------------------------

dataname <- list.files("input", pattern = ".txt")
datapath <- paste0("input/", dataname)
df <- read_tsv(datapath)

# Store settings in variables
settings <- read_json(path = "settings.json", format = "json")
for (setting_name in names(settings)){
  extract_settings(settings[[setting_name]])
}


#-------------------------------------------------------------------------------
# AGGREGATE DATA
#-------------------------------------------------------------------------------

# Add datetime column
if (date_colname & time_colname){
  df <- df %>%
    mutate(datetime = as.POSIXct(paste(df[[date_colname]], df[[time_colname]]),
                                 format = settings$datetime_format))
} else {
  df <- df %>%
    mutate(datetime = as.POSIXct(paste0(
      df[[year_colname]], "-", df[[month_colname]], "-", df[[day_colname]], " ",
      df[[hour_colname]], ":", df[[minute_colname]], ":", df[[second_colname]]),
      format = "%Y-%m-%d %H:%M:%S"))
}

# Add measurement sequence number
df <- df %>% 
  mutate(new_sequence = 
           ifelse(is.na(abs(difftime(df$datetime,
                                     lag(df$datetime),
                                     units = "secs"))), TRUE,
                  ifelse(abs(difftime(df$datetime,
                                      lag(df$datetime),
                                      units = "secs")) > as.numeric(max_timediff), TRUE,
                         FALSE))) %>%
  mutate(seqnum = cumsum(new_sequence)) #%>%
  #rename(co2 = all_of(co2_colname))

# If specified in settings, only keep the final n measurements per sequence
if (check_whole_sequence){
  df_seqnum <- df
} else {
  df_seqnum <- df %>%
    group_by(seqnum) %>%
    do(tail(., n = as.numeric(check_last_n)))
}

# Calculate mean and standard deviation per sequence
df_stats <- df_seqnum %>%
  group_by(seqnum) %>%
  summarize(mean = mean(na.omit(!!as.symbol(co2_colname))),
            std = sd(na.omit(!!as.symbol(co2_colname))),
            std_hist = ifelse((std > 2), 2, std))


#-------------------------------------------------------------------------------
# CREATE FIGURES
#-------------------------------------------------------------------------------

# FIGURE 1: Plot random example sequences

# Extract random sequence numbers to use in the plots
random_seqnums <- round(
  runif(as.numeric(fig1_n_random_plots), 0, max(df_seqnum$seqnum)),
  0)

# Create the plot
filename <- paste("output/1.plot_random_sequences.png", sep = "")
png(filename)
plot_list <- list()
for (i in 1:length(random_seqnums)) {
  
  df_to_plot <- df_seqnum %>%
    filter(seqnum == random_seqnums[i])
  
  plot_list[[i]] <- ggplot(df_to_plot, aes(x = datetime, y = !!as.symbol(co2_colname))) +
    geom_point() +
    # Hide axis labels - will be added later
    xlab("") + ylab("") + 
    # Change to another layout theme
    theme_bw() +
    # Add axis label text and edit size of axis texts
    theme(text = element_text(family = "Times"),
          axis.text = element_text(size = rel(1.4)),
          axis.title = element_text(size = rel(1.7)))
}

# Create the axis labels to be shared by all plots in the figure
text_left <- text_grob(fig1_ylab, rot = 90, vjust = 1, size = 19,
                       family = "Times")
text_bottom <- text_grob("Seconds", size = 19, family = "Times")

# Arrange the plots in the figure and add the common axis labels
grid.arrange(grobs = plot_list, ncol = 1, left = text_left, 
             bottom = text_bottom, heights = rep(2,length(random_seqnums)))

dev.off()

#---
# FIGURE 2: Plot standard deviation vs sequence number

# Get the plot letter position details
letter_position <- create_letter_position(df_stats, fig2_letter_position, y_name = "std", 
                        x_name = "seqnum", y_lims = c(NA,NA), x_lims=c(NA,NA))

# Create the plot
png("output/2.plot_std_vs_time.png")
plot_2 <-  ggplot(df_stats, aes(x = seqnum, y = std)) +
  geom_point() +
  xlab("Sequence Number") + ylab("Standard Deviation [ppm]") + 
  # Change plot layout to another theme and so some adjustments to the theme
  theme_bw() +
  theme(text = element_text(family = "Times"),
        axis.text = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.7))) +
  # Add the plot letter string
  annotate("text",
           x = letter_position[[1]],
           y = letter_position[[2]], 
           label = "a)",
           hjust = letter_position[[3]],
           vjust = letter_position[[4]],
           size = 9)
print(plot_2)
dev.off()

#---
# FIGURE 3: Make histogram of standard deviations

png("output/3.histogram.png")
plot_3 <- ggplot(df_stats, aes(x = std_hist)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "grey") +
  scale_x_continuous(breaks = seq(0, 2, 0.5),
                     labels = c("> 0.5", "> 1", "> 1.5", "> 2", "< 2")) +
  stat_bin(binwidth = 0.5, geom = "text", aes(label = ..count..), vjust = -0.5,
           size = 5, family = "Times") + 
  labs(x = "Standard Deviation [ppm]", y = "Measurement Sequence Frequency") +
  theme_bw() +
  theme(text = element_text(family = "Times"),
        axis.text = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.7)))

plot_3 <- plot_3 +
  annotate("text",
           x = 1.75,
           y = max(ggplot_build(plot_3)$data[[1]]$count), 
           label = "b)",
           hjust = -1,
           vjust = 1,
           size = 9)

print(plot_3)
dev.off()


#-------------------------------------------------------------------------------
# EXTRACT ONE CO2 MEASUREMENT PER SEQUENCE
#-------------------------------------------------------------------------------

df_to_write <- df

if (extract_whole_sequence){  # This means calculate average from whole sequence
  
  # Loop though list of columns and replace data with sequence-vise averages
  colnames_average <- append(list(co2_colname), average_other_cols)
  for (col in colnames_average){
    df_to_write <- df_to_write %>%
      group_by(seqnum) %>%
      mutate({{col}} := round(mean(!!as.symbol(col)),2))
  }
  
  # Extract last value per sequence, and remove most added columns
  df_to_write <- df_to_write %>%
    group_by(seqnum) %>%
    do(tail(., n = 1)) %>%
    select(-datetime, -new_sequence)

} #else {
#  if (extract_last_n == 1) {
#    # Only extract the final co2 value as it is
#  } else {
#    # Use the average of the last 'n_meas_for_extraction' in each sequence
#  }
#} 

# Exclude the final column 'seqnum' (not possible inside pipe since this is the 
# group-by variable!)
df_to_write <- df_to_write[,1:length(df_to_write)-1]

# Write data to file
out_file <- paste0("output/", strsplit(dataname, '.txt'), "_extracted.txt")
write_tsv(df_to_write, file = out_file)