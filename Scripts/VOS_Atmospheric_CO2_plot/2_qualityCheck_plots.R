########################################################################################
## plot checks of quality for atmospheric CO2
## For VOS only

##-----------------------------------------------------------------------------
##-----------------------------------------------------------------------------
# Input params to be assigned:
# none!



#-----------------
# Consider to change these axis ranges after viewing plots.

stdPlot_ylim_min <- NA
stdPlot_ylim_max <- NA


##-----------------------------------------------------------------------------
##-----------------------------------------------------------------------------

# Remove old figures produced by this script from the output directory
images <- list.files("output", pattern="^[1-2].*png$",)
for (image_loop in 1:length(images)) {
  image <- paste("output", "/", images[image_loop], sep="")
  file.remove(image)
}


library(ggplot2)

output_file <- paste("output", "/", "2_qualityCheck_info_out-of-range_hist-values.txt", sep="")
sink(output_file)

#-------------------------------------
#Rename the df_sub (to make a copy) and calculate mean and standard deviation for each ATM section
df_sub2 <- df_sub
df_mean_std <- data.frame(matrix(,max(df_sub2$ATM_seq),6))
colnames(df_mean_std) <- c("date.time", "latitude", "longitude", "ATM_seq", "mean", "std.dev")


for (i in 1:max(df_sub2$ATM_seq)) {
  df_dummy <- subset(df_sub2, ATM_seq==i)   
  
  df_mean_std$mean[i] <- (sum(df_dummy$CO2))/nrow(df_dummy)
  df_mean_std$std.dev[i] <- sd(df_dummy$CO2)
  df_mean_std$date.time[i] <- toString(df_dummy$date.time[1])
  df_mean_std$latitude[i] <- df_dummy$latitude[1]
  df_mean_std$longitude[i] <- df_dummy$longitude[1]
  df_mean_std$ATM_seq[i] <- df_dummy$ATM_seq[1]  
}
df_mean_std$date.time <- as.POSIXct(df_mean_std$date.time, tz="UTC", format="%Y-%m-%d %H:%M:%S")


#-------------------------------------
# Plot mean and standard deviation vs longitude

if(is.numeric(stdPlot_ylim_min)){
  output_file_name <- paste("output/", "1.mean_stddev_vs_longitude", "_plot_own-range.png", sep="")
  stdPlot_ylims <- c(stdPlot_ylim_min, stdPlot_ylim_max)
} else if ((max(na.omit(df_mean_std$mean)) > atm_co2_max) || (min(na.omit(df_mean_std$mean)) < atm_co2_min)) {
  output_file_name <- paste("output/", "1.mean_stddev_vs_longitude", "_plot_bad-range.png", sep="")
  stdPlot_ylims <- c(atm_co2_min, atm_co2_max)
} else {
  output_file_name <- paste("output/", "1.mean_stddev_vs_longitude", "_plot.png", sep="")
  stdPlot_ylims <- c(min(na.omit(df_mean_std$mean)), max(na.omit(df_mean_std$mean)))
}
png(output_file_name, width = 680, height = 480)
par(mar=c(5,5,2,2))
plot(df_mean_std$longitude, df_mean_std$mean, xlab = "Longitude", ylab = expression("Atmospheric xCO"[2]*" [ppm]"), ylim = stdPlot_ylims,  cex.lab=1.5, cex.axis=1.3)
arrows(df_mean_std$longitude, (df_mean_std$mean - df_mean_std$std.dev), df_mean_std$longitude, (df_mean_std$mean + df_mean_std$std.dev), length=0.05, angle=90, code=3, col="red")
legend("topright", "a)", bty="n", cex=2.5)
dev.off()



# Print number of means outside of ylim plotting area 
outlier_tot <- sum(df_mean_std$mean > stdPlot_ylims[2], df_mean_std$mean < stdPlot_ylims[1])
outlier_percent <- round((outlier_tot/(nrow(df_mean_std)))*100,2)
cat("\n","Plot 1: number of outliers are ", outlier_tot, " (", outlier_percent, "%).", sep="")


#-------------------------------------
# Plot histogram (y axis = % (or numbers) of atm measurements, x axis = standard deviation in ppm (0.5 ppm, 1.0 ppm, 1.5 ppm etc.))

# First remove the extreme outliers
std.dev <- c(df_mean_std$std.dev)
std.dev <- std.dev[!is.na(std.dev)]

# Change all values higher than 2 to 2.2 so that these show up in the last bin in the plot
std.dev[std.dev > 2] <- 2.2

# Make histogram
png(paste("output/", "2.std-dev_histogram", ".png", sep=""), width = 480, height = 480)
par(mar=c(5,5,2,2))
h <- hist(std.dev, breaks=5, xlab = "Standard deviation [ppm]", ylab= "Atmospheric sequences", cex.lab=1.5, cex.axis=1.3, main = "", xaxt="n")
axis(1,at=seq(0,2.5,0.5), labels=c(0,0.5,1,1.5,">2", ""), cex.axis=1.3)
legend("topright", "b)", bty="n", cex=2.5)
dev.off()


# Print total number of measurements in first bin, and how many percentages this is in total
count1 <- h$counts[1]
count2 <- h$counts[2]
count3 <- h$counts[3]
count4 <- h$counts[4]
count5 <- h$counts[5]
total <- sum(h$counts)
perc1 <- round(count1/total*100,2)
perc2 <- round(count2/total*100,2)
perc3 <- round(count3/total*100,2)
perc4 <- round(count4/total*100,2)
perc5 <- round(count5/total*100,2)
cat("\n","Values of histogram: ", count1," (",perc1,"%), ", count2," (",perc2,"%), ", count3," (",perc3,"%), ", count4," (",perc4,"%), ", count5, " (",perc5,"%)", sep="")

sink()

#---------------------------------------------------------
# Make the datafile with mean and stds as output to be used for making external plots 
#write.csv(df_mean_std, file = "output/Mean_std_data.csv")




