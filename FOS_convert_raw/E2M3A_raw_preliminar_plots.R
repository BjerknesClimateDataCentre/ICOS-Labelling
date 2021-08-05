
## Preliminary plots
# Clear plots
if (!is.null(dev.list()))
  dev.off()
# Clean workspace
rm(list = ls())

library(lubridate)

load(file="E2M3Araw_2015_2017.RData")

datearray <- vector()
co2array <- vector()
azpcarray <- vector()
sensarray <- vector()

for (ind in 1:NROW(new_df$datetime)) {
  if ( is.na(new_df$pco2_2_CO2[ind]) & is.na(new_df$pco2_3_CO2[ind]) ) {
    co2array[ind] <- NA
    azpcarray[ind] <- NA
    sensarray[ind] <- NA
  }
  else if (!is.na(new_df$pco2_2_CO2[ind])) {
    co2array[ind] <- new_df$pco2_2_CO2[ind]
    azpcarray[ind] <- new_df$pco2_2_instrumentZero[ind]
    sensarray[ind] <- 2
  }
    else  {
      co2array[ind] <- new_df$pco2_3_CO2[ind]
      azpcarray[ind] <- new_df$pco2_3_instrumentZero[ind]
      sensarray[ind] <- 3
    }
  }
  
datearray <- ymd_hms(new_df$datetime)
datearray[datearray>="2018-01-01"] <- NA

filter <- !is.na(co2array) & !is.na(azpcarray) & !is.na(sensarray) & !is.na(datearray)

co2array <- co2array[filter]
azpcarray <- azpcarray[filter]
sensarray <- sensarray[filter]
datearray <- datearray[filter]

co2_df <- data.frame(datearray, co2array, azpcarray, sensarray)
colnames(co2_df) <- c("datetime","xco2", "azpc","sensor")

#  rm(new_df)
save(co2_df, file="E2M3AxCO2raw_2015_2017.RData")
  
 azpcchange <- c(co2_df$azpc[1:length(co2_df$azpc)-1] 
                 - co2_df$azpc[2:length(co2_df$azpc)], 0)
 azpcchange[abs(azpcchange)<=0] <- NA
  
  outputplotname <- "./output/pco2e2m3a_all.png"
  png(outputplotname, width=1600,height=800)
  par(mar=c(5,5,2,2))
  plot(co2_df$datetime, co2_df$xco2, col=co2_df$sens, ylim=c(-100, 1000))
  dev.off()
  
  outputplotname <- "./output/pco2e2m3a_lastbeforeAZCPchange.png"
  png(outputplotname, width=1600,height=800)
  par(mar=c(5,5,2,2))
  plot(co2_df$datetime[!is.na(azpcchange)], co2_df$xco2[!is.na(azpcchange)], 
       col=co2_df$sens[!is.na(azpcchange)], ylim=c(-100, 1000))
  dev.off()
  
  length(co2_df$xco2[!is.na(azpcchange)&co2_df$sensor==2])/length(co2_df$xco2[co2_df$sensor==2])*100
  length(co2_df$xco2[!is.na(azpcchange)&co2_df$sensor==3])/length(co2_df$xco2[co2_df$sensor==3])*100
  
  outputplotname <- "./output/pco2e2m3a_pco22.png"
  png(outputplotname, width=1600,height=800)
  par(mar=c(5,5,2,2))
  plot(co2_df$datetime[co2_df$sensor==2], 
       co2_df$xco2[co2_df$sensor==2], 
       col="red", ylim=c(-100, 1000))
  points(co2_df$datetime[!is.na(azpcchange)&co2_df$sensor==2], 
          co2_df$xco2[!is.na(azpcchange)&co2_df$sensor==2], 
          col="blue", ylim=c(-100, 1000))
  dev.off()
    
    datemin <- "2016-06-28"
      datemax <- "2016-07-03"
      filter1 <-co2_df$sensor==2& co2_df$datetime>=datemin & co2_df$datetime <= datemax
      filter2 <- !is.na(azpcchange) & filter1
    outputplotname <- paste("./output/pco2e2m3a_pco22_",datemin,"_",datemax,".png", sep="")
    png(outputplotname, width=1600,height=800)
    par(mar=c(5,5,2,2))
    plot(co2_df$datetime[filter1], 
         co2_df$xco2[filter1], 
         col="red", ylim=c(-100, 1000))
    points(co2_df$datetime[filter2], 
           co2_df$xco2[filter2], 
           col="blue", ylim=c(-100, 1000))
    dev.off()
    
    
    ## Filter by sequence >1 hour between measurements -> new sequence; choose the last measurement of
    seqchange <- c(co2_df$datetime[1:length(co2_df$datetime)-1] 
                    - co2_df$datetime[2:length(co2_df$datetime)], 0)
    seqchange[abs(seqchange) < hours(1)] <- NA
    
    outputplotname <- "./output/pco2e2m3a_lastbeforetimegap.png"
    png(outputplotname, width=1600,height=800)
    par(mar=c(5,5,2,2))
    plot(co2_df$datetime[!is.na(seqchange)], co2_df$xco2[!is.na(seqchange)], 
         col=co2_df$sens[!is.na(seqchange)], ylim=c(-100, 1000))
    dev.off()
  ###-----
    dev.off()
    datemin <- "2016-06-30 00:00:00"
    datemax <- "2016-06-30 04:00:00"
    filter1 <-co2_df$sensor==2& co2_df$datetime>=datemin & co2_df$datetime <= datemax
    filter2 <- !is.na(seqchange) & filter1
    outputplotname <- paste("./output/pco2e2m3a_pco22_seqchange_",datemin,"_",datemax,".png", sep="")
    #png(outputplotname, width=1600,height=800)
    #par(mar=c(5,5,2,2))
    
    plot.new()
    plot(co2_df$datetime[filter1], 
         co2_df$xco2[filter1], 
         col="red", ylim=c(-100, 1000), xlab="")
    points(co2_df$datetime[filter2], 
           co2_df$xco2[filter2], 
           col="blue", ylim=c(-100, 1000))
    axis(1, co2_df$datetime[filter1], format(co2_df$datetime[filter1], "%H:%M:%S"))
    
   # dev.off()