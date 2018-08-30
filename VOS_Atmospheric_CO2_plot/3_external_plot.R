########################################################################################
### Plot station atm data vs. external data
###########################
### FOR VOS


### Run the script after the "Atm_CO2_plot.R" script (creates a data file in the output folder which is needed).

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Initial values:

if (!input_from_main) {

CO2_ext_name <- "analysis_value"

change_year_from <- NA   # The station data year will be changed in case this period is not available for the external station. If not nessesary assign "NA".
change_year_to <- NA     # If not nessesary assign "NA"
add_to_external <- NA       # In case we change years in data. This accounts for yearly increase. If not nessesary assign "NA" 

max_lon <- NA              # In case we want to compare a bounding box instead of all station data
min_lon <- NA
max_lat <- NA
min_lat <- NA

}

#-----------------
# Consider to change these axis ranges after viewing plots.
STvsExt_ylim_min <- NA
STvsExt_ylim_max <- NA


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Remove old figures produced by this script from the output directory
images <- list.files("output", pattern="^[3].*png$",)
for (image_loop in 1:length(images)) {
  image <- paste("output", "/", images[image_loop], sep="")
  file.remove(image)
}


external_dir<-"external_data"


output_file <- paste("output", "/", "3_external_info_correlation.txt", sep="")
sink(output_file)

#--------------------------------------
# EDIT STATION DATA BEFORE PLOTTING (save copies along the way)

# Change name of data.frame (to make a copy)
df_station <- df_mean_std

df_station <- df_station[!(df_station$mean==65535),]  # These were used for Nuka, they remone missing values
#df_station <- df_station[!(df_station$mean<100),]


# Remove lines (atm sequences) with standard value higher than 1
df_station <- df_station[!(df_station$std.dev > 1),] 
# Remove lines with standard value = NA
df_station <- df_station[!(is.na(df_station$std.dev)),] 
df_station2 <- df_station


#Remove lines with lat and lon =NA
df_station <- df_station[!(is.na(df_station$longitude)),]
df_station <- df_station[!(is.na(df_station$latitude)),]
df_station3 <- df_station


# Only include values with longitude betwen max and min
if(is.numeric(min_lon)) {
  df_station <- df_station[(df_station$longitude > min_lon),]
}
if (is.numeric(max_lon)) {
  df_station <- df_station[(df_station$longitude < max_lon),]
}
df_station4 <- df_station

# Only include values with latitude betwen max and min
if(is.numeric(max_lat)) {
  df_station <- df_station[(df_station$longitude < max_lat),] 
}
if (is.numeric(min_lat)){
  df_station <- df_station[(df_station$latitude > min_lat),]
}
df_station5 <- df_station


# Change year on station data if data from station year is not available in external data
if(is.numeric(change_year_from)) {
dummy_date <- df_station$date.time
dummy_date2 <- gsub(change_year_from, change_year_to, dummy_date)
dummy_date3 <- as.POSIXct(dummy_date2, tz="UTC", format="%Y-%m-%d %H:%M:%S")
df_station$date.time <- dummy_date3
}
df_station6 <- df_station



#--------------------------------------
# Import the external data
input_file2 <- list.files(external_dir, pattern = "txt")
in_file2 <- paste(external_dir, "/", input_file2, sep="")
df_external <- read.table(in_file2,header=T, sep = "\t", strip.white=TRUE, fileEncoding="UTF8")
df_external$date_external <- as.POSIXct(paste(df_external[,2], df_external[,3], df_external[,4], df_external[,5], df_external[,6], df_external[,7]), tz="UTC", format="%Y %m %d %H %M %S")


# Add constant value to external CO2 data if the same years are not compared
if(is.numeric(add_to_external)) {
  df_external$analysis_value <- df_external$analysis_value + add_to_external
}


#--------------------------------------
# Add headers to station data
external_col_header <- c("external_date", "external_CO2")

for (i in 1:length(external_col_header)) {
  df_station[,external_col_header[i]] <- NA
}

#-------------------------------------
# Will add external data to the station data, row by row.

station_finished <- 0
external_finished <- 0

current_station_row <- 1
current_external_row <- 1


while (station_finished == 0 && external_finished == 0) {
	found_match <- 0
	current_diff <- abs(difftime(df_station$date.time[current_station_row], df_external$date_external[current_external_row], unit="secs"))

	while (found_match == 0 && external_finished == 0) {
		current_diff <- abs(difftime(df_station$date.time[current_station_row], df_external$date_external[current_external_row], unit="secs"))

		if(current_external_row + 1 <= nrow(df_external)) {
			next_diff <- abs(difftime(df_station$date.time[current_station_row], df_external$date_external[current_external_row+1], unit="secs"))
			
			if (next_diff <= current_diff) {
				current_diff <- next_diff
				current_external_row <- current_external_row + 1
			} else {
				found_match <- 1
			}
		} else {
			external_finished <- 1
		}
	}

	#if(current_diff < 86400) {
		df_station$external_date[current_station_row] <- toString(df_external$date_external[current_external_row])
		df_station$external_CO2[current_station_row] <- df_external[[CO2_ext_name]][current_external_row]
	#}
	
	current_station_row <- current_station_row + 1
	if(current_station_row > nrow(df_station)) {
		station_finished <- 1
	}
  #print(current_station_row)
}



#-------------------------
## PLOT STATION  AND EXTERNAL DATA VS TIME:

# Find xlims based on the period where external and station data overlap
df_station$external_date <- as.POSIXct(df_station$external_date, tz="UTC", format="%Y-%m-%d %H:%M:%S")
xlim_min <- max(min(na.omit(df_station$date.time)), min(na.omit(df_station$external_date)))
xlim_max <- min(max(na.omit(df_station$date.time)), max(na.omit(df_station$external_date)))

# Find the row numbers where xlims occur
xlim_min_row <- min(which(grepl(xlim_min, df_station$external_date)),which(grepl(xlim_min, df_station$date.time)))
xlim_max_row <- min(which(grepl(xlim_max, df_station$external_date)),which(grepl(xlim_max, df_station$date.time)))


# Decide on ylims for plot:
if(is.numeric(STvsExt_ylim_min)) {
  output_file_name <- paste("output/", "3.station_and_external_CO2_vs_time", "plot_own-range.png", sep="")
  STvsExt_ylims <- c(STvsExt_ylim_min, STvsExt_ylim_max)
} else {
  output_file_name <- paste("output/", "3.station_and_external_CO2_vs_time", "plot.png", sep="")
  STvsExt_ylims <- c(max(na.omit(df_station$external_CO2[xlim_min_row:xlim_max_row]), na.omit(df_station$mean[xlim_min_row:xlim_max_row])), min(na.omit(df_station$external_CO2[xlim_min_row:xlim_max_row]), na.omit(df_station$mean[xlim_min_row:xlim_max_row]))) 
}


png(output_file_name)
par(mar=c(5,5,2,2))
plot(df_station$date.time, df_station$external_CO2, xlim =c(xlim_min,xlim_max), ylab = expression("Atmospheric xCO"[2]*" [ppm]"), xlab = "Time",  ylim=STvsExt_ylims, cex.lab=1.5,cex.axis=1.3)
points(df_station$date.time, df_station$mean, col = "red")
legend("topright", "c)", bty="n", cex=2.5)
dev.off()


# PLOT STATION VS EXTERNAL DATA (not in report!)
#png(paste("output/", "4.station_vs_external_CO2", "plot.png", sep=""))
#par(mar=c(5,5,2,2))
#plot(df_station$external_CO2, df_station$mean, xlab = expression("External xCO"[2]*" [ppm]"), ylab = expression("xCO"[2]*" from station [ppm]"), cex.lab=1.5,cex.axis=1.3)
#abline(0,1)
#legend("topleft", "c)", bty="n", cex=2.5)
#dev.off()


#-------------------------

## Find correlation coefficient between daily average Nuka and external xco2
cor_pearson <- cor.test(df_station$mean, df_station$external_CO2, method="pearson")
cat("Correlation is ", round(cor_pearson$estimate,2), sep="")


# FIND intercept and slope:
#plot(df_station$external_CO2, df_station$mean, ylim=c(200,500), xlim=c(200,500))
#abline(0,1, col="red")
model<- lm(df_station$external_CO2~df_station$mean)
#abline(model)
coefs <- coef(model)
cat("\n", "Intercept is: ", coefs[1],"\n", "Slope is: ", coefs[2], sep="")


sink()




#-------------------------
## OLDER PLOTS:
#-------------------------

## Plot station vs. external data
#png(paste("output/", "2.station_vs_external_CO2_", "plot.png", sep=""))
#par(mar=c(5,5,2,2))
#tryCatch(plot(df_station[[CO2_sta_name]], df_station$external_CO2, ylim = c(390,420), xlim = c(365,450), ylab = "External xCO2 [ppm]", xlab = "xCO2 from station [ppm]", cex.lab=1.5,cex.axis=1.3), error=function(e) {})
#legend("topleft", "a)", bty="n", cex=2.5)
#dev.off()


## Plot station AND external data vs. time
#png(paste("output/", "3.station_and_external_CO2_vs_time", "plot.png", sep=""))
#par(mar=c(5,5,2,2))
#tryCatch(plot(df_station$date.time, df_station$external_CO2, ylim = c(390,420), ylab = "Atmospheric xCO2 [ppm]", xlab = "Time", cex.lab=1.5,cex.axis=1.3), error=function(e) {})
#points(df_station$date.time, df_station[[CO2_sta_name]], col = "red")
#legend("topleft", "b)", bty="n", cex=2.5)
#dev.off()


#-------------------------
#-------------------------
# If we want to make plot with daily means instead . 

## Store the daily means in a separate column
#for (i in 1:length(df_station$CO2)) {
#  df_station$mean_co2[i] <- NA
#}

## Calculate the means with a while loop
#start <- as.Date(df_station$date.time[1], format=dt_format)
#end <- as.Date(df_station$date.time[length(df_station$date.time)], format=df_format)
#theDate <- start
#row_number <- 1

#while (theDate <= end) {
  
#  df_date <- subset(df_station, as.character(date.time) == theDate)
#  if (length(df_date$CO2) == 0) {
#    theDate <- theDate + 1
#  } else {
  
#    sum <- 0
#    for (i in 1:length(df_date$CO2)) {
#      sum <- sum + df_date$CO2[i]
#    }
#    mean <- round(sum/length(df_date$CO2),2)
  
#    if ((row_number+length(df_date$CO2)) > (length(df_station$CO2))) {
#      df_station$mean_co2[row_number:(row_number+length(df_date$CO2))-1] <- mean 
#    }
#    else { 
#      df_station$mean_co2[row_number:(row_number+length(df_date$CO2))] <- mean 
#    }
    
#    row_number <- row_number + length(df_date$CO2)
#    theDate <- theDate + 1

#  }  
 
#}

# Make a new subset of the data frame where only chose one measurement per day.
#df_daily <- data.frame(matrix(ncol=ncol(df_station), nrow=0))  
#names <- c("X", "date.time", "Type","CO2","external_date", "external_CO2", "mean_co2")
#colnames(df_daily) <- names
#df_daily$date.time <- as.POSIXct(df_daily$date.time, tz="UTC", format="%Y-%m-%d %H:%M:%S")

#count <- 1
#for (j in 1:(nrow(df_station)-1)) {
#  if (df_station$mean_co2[j] != df_station$mean_co2[j+1]) {
#    df_daily[count,] <- df_station[j,] 
#    count <- count + 1
#  }
#}

# Make first sub_set without extreme values:
#df_daily_sub <- subset(df_daily, mean_co2 > 300 &  mean_co2 < 500)

# Plot station vs. external data
#png(paste("output/", "4.daily_station_vs_external_CO2_", "plot.png", sep=""))
#par(mar=c(5,5,2,2))
#plot(df_daily_sub$mean_co2, df_daily_sub$external_CO2, ylim = c(390,415), xlim = c(390,415), ylab = expression("External xCO"[2]*" [ppm]"), xlab = expression("xCO"[2]*" from station [ppm]"), cex.lab=1.5,cex.axis=1.3)
#abline(0,1)
#legend("topleft", "a)", bty="n", cex=2.5)
#dev.off()

# Plot station AND external data vs. time
#png(paste("output/", "5.daily_station_and_external_CO2_vs_time", "plot.png", sep=""))
#par(mar=c(5,5,2,2))
#plot(df_daily$date.time, df_daily$external_CO2, ylim = c(390,415), ylab = expression("Atmospheric xCO"[2]*" [ppm]"), xlab = "Time", cex.lab=1.5,cex.axis=1.3)
#points(df_daily$date.time, df_daily$mean_co2, col = "red")
#legend("topleft", "b)", bty="n", cex=2.5)
#dev.off()

#-----------------------------
#-----------------------------



## Find correlation coefficient between daily average Nuka and external xco2
#cor_pearson <- cor.test(df_daily_sub$mean_co2, df_daily_sub$external_CO2, method="pearson")
#cat("Correlation is ", round(cor_pearson$estimate,2), sep="")

## Include in report how many values were exluded for this correlation
#excluded <- nrow(df_daily)-nrow(df_daily_sub)
#excl_percent <- round((excluded/nrow(df_daily))*100,2)

#cat("\n","Excluded ", excluded, " (", excl_percent, "%) values.", sep="")




