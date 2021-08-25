################################################################################
### RESET THE PLOT SETTINGS TO DEFAULT
################################################################################

### Description:
# All plot settings can be edited, however the setting when starting out with 
# plotting new data is always the same. There is one default setting for SOOP
# and another default setting for FOS. This script resets the settings to this
# default.

### Requirements:
# - There must be a settings json file in the same directory as this script 
# ('settings.json'). Specify in this file if should reset to default for SOOP
# or FOS ("station_type": "SOOP"/"FOS").
# - There must be a default settings folder with a json file containing the 
# defaults plot settings ('..default_settings/default_settings.json').

### Output:
# The all_plot_settings section in the settings.json file will contain the 
# default settings for the chosen station type.


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