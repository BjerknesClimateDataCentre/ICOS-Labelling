sal_mean <- mean(data$Salinity, na.rm=TRUE)
  sal_std <- sd(data$Salinity, na.rm=TRUE)
  temp_mean <- mean(data$Intake.Temperature, na.rm=TRUE)
  temp_std <- sd(data$Intake.Temperature, na.rm=TRUE)
  
  zsal <- (data$Salinity - sal_mean)/sal_std
  ztem <- (data$Intake.Temperature - temp_mean) /temp_std
  zco2 <- (data$CO2..measured. - mean(data$CO2..measured., na.rm=TRUE)) / sd(data$CO2..measured., na.rm=TRUE)

  dev.off()
  #plot(range(data$dates, na.rm=TRUE), c(-2.5,2.5), type='n', xlab='Date', ylab='Z-score')
  par(mar=c(5,5,2,2))
  plot(data$dates, zsal, type="l", ylim=c(-3,3),xlab='Date', ylab='Z-score')
 # par(new=TRUE)
  lines(data$dates, ztem, type="l", col="red")
#  par(new=TRUE)
  lines(data$dates, zco2, type="l", col="blue")
  legend("topleft", 
         legend= c("Sal","Temp","CO2"), 
         col=c("black", "red", "blue"), pch=c("l","l","l"), bty='n')
                                                                            
  