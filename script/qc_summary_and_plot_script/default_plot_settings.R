################################################################################
### RESET THE PLOT SETTINGS TO DEFAULT
################################################################################

#-------------------------------------------------------------------------------
# INITIAL SETTINGS
#-------------------------------------------------------------------------------

library(jsonlite)


#-------------------------------------------------------------------------------
# IMPORT SETTING FILES
#-------------------------------------------------------------------------------

# Import the settings
all_settings <- read_json(path="settings.json", format="json")

# Import the default settings
default_plot_settings <- read_json(
  path="default_plot_settings/default_plot_settings.json", format="json")


#-------------------------------------------------------------------------------
# RESET TO DEFAULT
#-------------------------------------------------------------------------------

# Get the default settings for the correct station type
if (all_settings$station_type == "SOOP") {
  default_plot_settings <- default_plot_settings$default_plots_SOOP
} else {
  default_plot_settings <- default_plot_settings$default_plots_FOS
}

# Overwrite the current settings with the default setting
all_settings$all_plots <- default_plot_settings

# Write the settings to the json file.
write_json(all_settings,"settings.json", pretty=TRUE, auto_unbox=TRUE)