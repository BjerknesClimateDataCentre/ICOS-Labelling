#####################################################################################
### Function for finding out of range values for oxygen and ph
###########################


##-----------------------------------------------------------------------------
# Input params to be assigned:

press_min <- 2
press_max <- 4

QuinCe_timelag <- 0           # This sctipt compares dates in QuinCamilla exported files and raw files. QuinCamilla changes the time zone.                             # We therefore need to know the time difference (in hours).

QC_rows_old <- 8              # How many rows got QC message from QuinCamilla (see output from summary script)


### REMEMBER TO ADD THE exported file from QuinCe to the "exported_file" folder


#----------------
# Do not change
oxygen_min <- 50
oxygen_max <- 400

pH_min <- 7
pH_max <- 9


#---------------------------------------------------------------------------

Sys.setlocale("LC_ALL", "English"); 



#------
# OXYGEN RANGE CHECK


if(is_O2==TRUE) {

  # Find row numbers were oxygen is out of range
  if(O2_calculations == "no") {
   ox_row_above <- which(df_sub[[O2_name]]>oxygen_max)
   ox_row_below <- which(df_sub[[O2_name]]<oxygen_min)
  } else {
    ox_row_above <- which(df_sub$calc_O2>oxygen_max)
    ox_row_below <- which(df_sub$calc_O2<oxygen_min)
  }

  # Make output sentence with total number of ox measurements out of range
  ox_oor <- length(ox_row_above) + length(ox_row_below)
  ox_oor_perc <- round((ox_oor/nrow(df_sub)*100),2)
  cat("Oxygen out of range: ",ox_oor," (",ox_oor_perc,"%).", sep="")

  # Find data.time where oxygen is out of range
  ox_row_oor <- sort(c(ox_row_above,ox_row_below))
  ox_dates <- df_sub$date.time[ox_row_oor]
  ox_dates <- as.character(ox_dates)
}

#------------------------------------------------------------------------------
# pH RANGE CHECK (same prosedure as for oxygen)
if(is_pH==TRUE) {
  
  ph_row_above <- which(df_sub[[pH_name]]>pH_max)
  ph_row_below <- which(df_sub[[pH_name]]<pH_min)

  
  ph_oor <- length(ph_row_above) + length(ph_row_below)
  ph_oor_perc <- round((ph_oor/nrow(df_sub)*100),2)
  cat("\n","pH out of range: ",ph_oor," (",ph_oor_perc,"%).", sep="")
  
  ph_row_oor <- sort(c(ph_row_above,ph_row_below))
  ph_dates <- df_sub$date.time[ph_row_oor]
  ph_dates <- as.character(ph_dates)
  
}


#------------------------------------------------------------------------------
# pressure (depth) RANGE CHECK (same prosedure as for oxygen)

press_row_above <- which(df_sub[[pressure_name]] > press_max)
press_row_below <- which(df_sub[[pressure_name]] < press_min)


press_oor <- length(press_row_above) + length(press_row_below)
press_oor_perc <- round((press_oor/nrow(df_sub)*100),2)
cat("\n","pressure (depth) out of range: ",press_oor," (",press_oor_perc,"%).", sep="")

press_row_oor <- sort(c(press_row_above,press_row_below))
press_dates <- df_sub$date.time[press_row_oor]
press_dates <- as.character(press_dates)






#------------------------------------------------------------------------------
# FIND HOW OXYGEN, pH and PRESSURE RANGE CHECK CHANGES THE NUMBER OF ROWS WITH ERRORS FOR THIS STATION

# Combine the new dates we have (if nessesary) 
if(is_pH==TRUE && is_O2==TRUE) {
    new_dates <- c(ox_dates,ph_dates,press_dates)  
    } else if (is_pH==FALSE) {
    new_dates <- c(ox_dates,press_dates)
    } else {
    new_dates <-c(ph_dates,press_dates)
}
new_dates <- unique(new_dates)

# Import the exported file from QuinCe (this file contains the other error messages)
input_file2 <- list.files("exported_file")
in_file2 <- paste("exported_file/", input_file2, sep="")
df_exp <- read.csv(in_file2,header=T, sep=",", fileEncoding="UTF8")
df_exp$Date <- as.POSIXct(df_exp$Date, tz="UTC", format="%Y-%m-%d %H:%M:%S")

# Find date.time where exported file contains message
QC_message_rows <- which(df_exp$Automatic.QC.Message!="")
QC_dates <- df_exp$Date[QC_message_rows] 

# Adjust the exported dates for the timelag given in QuinCe 
QC_dates_adj <- QC_dates + (QuinCe_timelag*60*60)

QC_dates_adj <- as.character(QC_dates_adj)


# Find out if the new dates are in the exported file
compare <- new_dates %in% QC_dates_adj
additional_rows <- sum(compare==FALSE)

QC_rows_new <- QC_rows_old + additional_rows
QC_perc_new <- round((QC_rows_new/nrow(df_sub))*100,2)

# Pring new QC row results to screen
cat("\n","Rows with some kind of QC message when including all parameters: ", QC_rows_new, " (", QC_perc_new, "%).", sep="")



