################################################################################
### CREATE RDATA OBJECTS
################################################################################

#-------------------------------------------------------------------------------
# INITIAL SETTINGS
#-------------------------------------------------------------------------------

# Load packages
library(readr)
library(jsonlite)


#-------------------------------------------------------------------------------
# IMPORT DATA, HEADER CONFIG AND SETTINGS FILE
#-------------------------------------------------------------------------------

# Import data
datafile_name <- list.files("data") #,pattern="csv$")
datafile_path <- paste("data/",datafile_name, sep="")
df <- read_csv(datafile_path)

# Import header config file and store the header converter as data frame
header_config <- read_json(path="header_config.json",format="json")


#-------------------------------------------------------------------------------
# FUNCTION(S)
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# REFORMAT DATA
#-------------------------------------------------------------------------------

# Update the column names using the names in the header config file
for (header in names(header_config$header_converter)){
  if (header %in% names(df)) {
    colnames(df)[which(names(df) == header)] <- 
      header_config$header_converter[[header]]
  }
}

# Save the df tibble object to and rds file
saveRDS(df, file = "data/processed_data.rds")