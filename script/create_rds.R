################################################################################
### CREATE RDS DATA OBJECTS
################################################################################

# Note that this script is not complete yet. It needs to deal with raw data as
# well..

### Description
# The script imports data (either raw or exported from QuinCe), reformats and
# cleans up the data, and creates .Rdata objects which can be used by the other
# labelling scripts


#-------------------------------------------------------------------------------
# INITIAL SETTINGS
#-------------------------------------------------------------------------------

# Load packages
library(readr)
library(jsonlite)

# Remove existing processed rds file in the data directory
if (file.exists("data/processed_data.rds")){
  file.remove("data/processed_data.rds")
}


#-------------------------------------------------------------------------------
# IMPORT DATA, HEADER CONFIG AND SETTINGS FILE
#-------------------------------------------------------------------------------

# Import data
datafile_name <- list.files("data") #,pattern="csv$")
datafile_path <- paste("data/", datafile_name, sep = "")
df <- read_csv(datafile_path)

# Import header config and settings file 
header_config <- read_json(path = "header_config.json", format = "json")
settings <- read_json(path = "settings.json", format = "json")


#-------------------------------------------------------------------------------
# FUNCTION(S)
#-------------------------------------------------------------------------------

# Rename columns using a converter consisting of the old and new colnames
rename_cols <- function(converter){
  for (new_colname in names(converter)){
    old_colname <- converter[[new_colname]]
    if (old_colname %in% names(df)){
      colnames(df)[which(names(df) == old_colname)] <- new_colname
    }
  }
  return(df)
}


#-------------------------------------------------------------------------------
# REFORMAT DATA
#-------------------------------------------------------------------------------

# Rename the fixed columns names from QuinCe
df <- rename_cols(header_config$fixed_colname_converter)

# Rename the column names from the raw file
df <- rename_cols(settings$raw_colname_converter)

# Save the df tibble object to an rds file
saveRDS(df, file = "data/processed_data.rds")