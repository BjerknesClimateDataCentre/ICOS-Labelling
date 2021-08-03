########################################################################################
### Plot station atm data vs. external data
###########################
### FOR VOS

# Clear plots
if (!is.null(dev.list()))
  dev.off()
# Clean workspace
rm(list = ls())


# Append timestamp to output produced, also free text (?)
timestamp <- function() {
  as.character(as.POSIXct(date(), tz = "UTC", format = "%a %b %d %H:%M:%S %Y"),
               format = "%Y%m%dT%H%M%S")
}


library(ggplot2)
library(maps)
library(mapdata)
library(sp)

load(file = "ATM_PS_20190207T092421.RData")

##############################################################################



### Run the script after the "Atm_CO2_plot.R" script (creates a data file in the output folder which is needed).

sink.reset <- function() {
  for (i in seq_len(sink.number())) {
    sink(NULL)
  }
}

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Initial values:

if (!exists("input_from_main")) {
  CO2_ext_name <- "analysis_value"
  
  change_year_from <-
    NA   # The station data year will be changed in case this period is not available for the external station. If not nessesary assign "NA".
  change_year_to <- NA     # If not nessesary assign "NA"
  add_to_external <-
    NA       # In case we change years in data. This accounts for yearly increase. If not nessesary assign "NA" +- 3 per year
  
  max_lon <-
    NA              # In case we want to compare a bounding box instead of all station data
  min_lon <- NA
  max_lat <- NA
  min_lat <- NA
  
}

#-----------------
# Consider to change these axis ranges after viewing plots.
STvsExt_ylim_min <- NA
STvsExt_ylim_max <- NA
listextst <- c("cpt", "psa", "hba")

#for (st in c(1:length(listextst)) ) {

#extst <- listextst[st]
appendtext <- paste("PS", sep = "")


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


df_station1 <- df_station#_noNA

# Only include values with longitude betwen max and min
if (is.numeric(min_lon)) {
  df_station1 <- df_station1[(df_station1$longitude > min_lon), ]
}
if (is.numeric(max_lon)) {
  df_station1 <- df_station1[(df_station1$longitude < max_lon), ]
}
df_station4 <- df_station1


# Only include values with latitude betwen max and min
if (is.numeric(max_lat)) {
  df_station1 <- df_station1[(df_station1$longitude < max_lat), ]
}
if (is.numeric(min_lat)) {
  df_station1 <- df_station1[(df_station1$latitude > min_lat), ]
}
df_station5 <- df_station1

# Only values within good data range
df_station1 <- df_station1[!(df_station1$mean > 500), ]
df_station1 <- df_station1[!(df_station1$mean < 300), ]



#-------------------------
## PLOT STATION  AND EXTERNAL DATA VS TIME:
externalmaxs <- c()
externalmins <- c()

for (st in c(1:length(listextst)) ) {
  extst <- listextst[st]
  
# Find xlims based on the period where external and station data overlap
#df_station1[[paste(extst,"_date",sep="")]] <- 
#  df_station1[[paste(extst,"_date",sep="")]]
 # It's not a string any longer   
#as.POSIXct(df_station1[[paste(extst,"_date",sep="")]], tz = "UTC", 
#             format = "%Y-%m-%d %H:%M:%S")
xlim_min <-
  max(min(na.omit(df_station1$date.time)), 
      min(na.omit(df_station1[[paste(extst,"_date",sep="")]])))
xlim_max <-
  min(max(na.omit(df_station1$date.time)), 
      max(na.omit(df_station1[[paste(extst,"_date",sep="")]])))

# Find the row numbers where xlims occur
xlim_min_row <-
  min(which(df_station1[[paste(extst,"_date",sep="")]]==xlim_min), 
      which(df_station1$date.time==xlim_min))
xlim_max_row <-
  min(which(df_station1[[paste(extst,"_date",sep="")]]==xlim_max), 
      which(df_station1$date.time==xlim_max))


# Decide on ylims for plot:
if (is.numeric(STvsExt_ylim_min)) {
  output_file_name <-
    paste(
      "output/",
      "3.station_and_external_CO2_vs_time",
      "plot_own-range",
      timestamp(),
      appendtext,
      ".png",
      sep = ""
    )
  STvsExt_ylims <- c(STvsExt_ylim_min, STvsExt_ylim_max)
} else {
  output_file_name <-
    paste(
      "output/",
      "3.station_and_external_CO2_vs_time_",
      "plot",
      timestamp(),
      appendtext,
      ".png",
      sep = ""
    )
  externalmaxs[st] <- max(na.omit(df_station1[[paste(extst,"_CO2",sep="")]]
                                  [xlim_min_row:xlim_max_row]))
  externalmins[st] <- min(na.omit(df_station1[[paste(extst,"_CO2",sep="")]]
                                  [xlim_min_row:xlim_max_row]))
  }
  STvsExt_ylims <-
    c(min(
#      na.omit(df_station1[[paste(extst,"_CO2",sep="")]][xlim_min_row:xlim_max_row]),
externalmins,
            na.omit(df_station1$mean[xlim_min_row:xlim_max_row])
    ), max(
#      na.omit(df_station1[[paste(extst,"_CO2",sep="")]][xlim_min_row:xlim_max_row]),
externalmaxs,
            na.omit(df_station1$mean[xlim_min_row:xlim_max_row])
    ))
}



png(filename=output_file_name, width=1600,height=1600, res=300)
par(mar = c(5, 5, 2, 2))
plot(
  df_station1$date.time,
  df_station1$mean, 
  col = "red",  
  pch = 19,
  xlim = c(xlim_min, xlim_max),
  ylab = expression("Atmospheric xCO"[2] * " [ppm]"),
  xlab = "Time",
  ylim = STvsExt_ylims,
  cex.lab = 1.5,
  cex.axis = 1.3
)
colours <- c("black","blue","green","orange")
for (st in c(1:length(listextst)) ) {
  extst <- listextst[st]
  points(df_station1$date.time, 
         df_station1[[paste(extst,"_CO2",sep="")]], 
         col = colours[st],
         pch=20)
  }
legend("topright", legend=c("Polarstern",listextst),bty = "n", cex = 1,
       col = c("red",colours), pch =c(19,20,20,20,20))

#legend("topright", "c)", bty = "n", cex = 2.5)

dev.off()


# PLOT STATION VS EXTERNAL DATA (not in report!)
png(
  paste(
    "output/",
    "4.station_vs_external_CO2",
    "plot",
    timestamp(),
    appendtext,
    ".png",
    sep = ""
  )
)

par(mar = c(5, 5, 2, 2))

for (st in c(1:length(listextst)) ) {
  extst <- listextst[st]
  if (st ==1){
  plot(df_station1[[paste(extst,"_CO2",sep="")]],
       df_station1$mean,
       col = colours[st],
       xlab = expression("External xCO"[2] * " [ppm]"),
       ylab = expression("xCO"[2] * " from station [ppm]"),
       cex.lab = 1.5,
       cex.axis = 1.3,
       pch=20)
  } else {
points(
  df_station1[[paste(extst,"_CO2",sep="")]],
  df_station1$mean,
  col = colours[st],
  pch=20
)
  }
abline(0, 1)
}
legend("topright", legend=c(listextst),bty = "n", cex = 1.5,
       col = c(colours), pch =c(20,20,20))
#legend("topleft", "c)", bty = "n", cex = 2.5)
dev.off()



#-------------------------


output_file <- paste("output",
                     "/",
                     "3_external_info_correlation",
                     timestamp(),
                     appendtext,
                     ".txt",
                     sep = "")
sink(output_file)

for (st in c(1:length(listextst)) ) {
  extst <- listextst[st]

  cat(extst, "\n")
## Find correlation coefficient between daily average Nuka and external xco2
cor_pearson <-
  cor.test(df_station1$mean, df_station1[[paste(extst,"_CO2",sep="")]], method = "pearson")
cat("Correlation is ", round(cor_pearson$estimate, 2), sep = "")


# FIND intercept and slope:
#plot(df_station1[[paste(extst,"_CO2",sep="")]], df_station1$mean, ylim=c(200,500), xlim=c(200,500))
#abline(0,1, col="red")
model <- lm(df_station1[[paste(extst,"_CO2",sep="")]] ~ df_station1$mean)
#abline(model)
coefs <- coef(model)
cat("\n", "Intercept is: ", coefs[1], "\n", "Slope is: ", coefs[2],"\n", sep =
      "")

}
sink()

#}

#-------------------------
## OLDER PLOTS:
#-------------------------

## Plot station vs. external data
#png(paste("output/", "2.station_vs_external_CO2_", "plot.png", sep=""))
#par(mar=c(5,5,2,2))
#tryCatch(plot(df_station1[[CO2_sta_name]], df_station1[[paste(extst,"_CO2",sep="")]], ylim = c(390,420), xlim = c(365,450), ylab = "External xCO2 [ppm]", xlab = "xCO2 from station [ppm]", cex.lab=1.5,cex.axis=1.3), error=function(e) {})

#legend("topleft", "a)", bty="n", cex=2.5)
#dev.off()


## Plot station AND external data vs. time
#png(paste("output/", "3.station_and_external_CO2_vs_time", "plot.png", sep=""))
#par(mar=c(5,5,2,2))
#tryCatch(plot(df_station1$date.time, df_station1[[paste(extst,"_CO2",sep="")]], ylim = c(390,420), ylab = "Atmospheric xCO2 [ppm]", xlab = "Time", cex.lab=1.5,cex.axis=1.3), error=function(e) {})
#points(df_station1$date.time, df_station1[[CO2_sta_name]], col = "red")

#legend("topleft", "b)", bty="n", cex=2.5)
#dev.off()


#-------------------------
#-------------------------
# If we want to make plot with daily means instead .

## Store the daily means in a separate column
#for (i in 1:length(df_station1$CO2)) {
#  df_station1$mean_co2[i] <- NA
#}

## Calculate the means with a while loop
#start <- as.Date(df_station1$date.time[1], format=dt_format)
#end <- as.Date(df_station1$date.time[length(df_station1$date.time)], format=df_format)

#theDate <- start
#row_number <- 1

#while (theDate <= end) {

#  df_date <- subset(df_station1, as.character(date.time) == theDate)
#  if (length(df_date$CO2) == 0) {
#    theDate <- theDate + 1
#  } else {


#    sum <- 0
#    for (i in 1:length(df_date$CO2)) {
#      sum <- sum + df_date$CO2[i]
#    }
#    mean <- round(sum/length(df_date$CO2),2)

#    if ((row_number+length(df_date$CO2)) > (length(df_station1$CO2))) {
#      df_station1$mean_co2[row_number:(row_number+length(df_date$CO2))-1] <- mean
#    }
#    else {
#      df_station1$mean_co2[row_number:(row_number+length(df_date$CO2))] <- mean
#    }

#    row_number <- row_number + length(df_date$CO2)
#    theDate <- theDate + 1

#  }

#}

# Make a new subset of the data frame where only chose one measurement per day.
#df_daily <- data.frame(matrix(ncol=ncol(df_station1), nrow=0))

#names <- c("X", "date.time", "Type","CO2","external_date", "external_CO2", "mean_co2")
#colnames(df_daily) <- names
#df_daily$date.time <- as.POSIXct(df_daily$date.time, tz="UTC", format="%Y-%m-%d %H:%M:%S")

#count <- 1
#for (j in 1:(nrow(df_station1)-1)) {
#  if (df_station1$mean_co2[j] != df_station1$mean_co2[j+1]) {
#    df_daily[count,] <- df_station1[j,]

#    count <- count + 1
#  }
#}

# Make first sub_set without extreme values:
#df_daily_sub <- subset(df_daily, mean_co2 > 300 &  mean_co2 < 500)

# Plot station vs. external data
#png(paste("output/", "4.daily_station_vs_external_CO2_", "plot.png", sep=""))
#par(mar=c(5,5,2,2))
#plot(df_daily_sub$mean_co2, df_daily_sub[[paste(extst,"_CO2",sep="")]], ylim = c(390,415), xlim = c(390,415), ylab = expression("External xCO"[2]*" [ppm]"), xlab = expression("xCO"[2]*" from station [ppm]"), cex.lab=1.5,cex.axis=1.3)
=======

#abline(0,1)
#legend("topleft", "a)", bty="n", cex=2.5)
#dev.off()

# Plot station AND external data vs. time
#png(paste("output/", "5.daily_station_and_external_CO2_vs_time", "plot.png", sep=""))
#par(mar=c(5,5,2,2))
#plot(df_daily$date.time, df_daily[[paste(extst,"_CO2",sep="")]], ylim = c(390,415), ylab = expression("Atmospheric xCO"[2]*" [ppm]"), xlab = "Time", cex.lab=1.5,cex.axis=1.3)

#points(df_daily$date.time, df_daily$mean_co2, col = "red")
#legend("topleft", "b)", bty="n", cex=2.5)
#dev.off()

#-----------------------------
#-----------------------------



## Find correlation coefficient between daily average Nuka and external xco2
#cor_pearson <- cor.test(df_daily_sub$mean_co2, df_daily_sub[[paste(extst,"_CO2",sep="")]], method="pearson")

#cat("Correlation is ", round(cor_pearson$estimate,2), sep="")

## Include in report how many values were exluded for this correlation
#excluded <- nrow(df_daily)-nrow(df_daily_sub)
#excl_percent <- round((excluded/nrow(df_daily))*100,2)

#cat("\n","Excluded ", excluded, " (", excl_percent, "%) values.", sep="")

