library(lubridate)

pap2017 <- read.table("/Users/rpr061/Desktop/2017/PAP_Apr2017.co2", header=TRUE, 
                                sep="", stringsAsFactors = FALSE)
pap2017$datetime <- date_decimal(pap2017$TS_jday/365 + 2017)

azpcchange <- c(pap2017$AZPC[1:length(pap2017$AZPC)-1] 
                - pap2017$AZPC[2:length(pap2017$AZPC)], 0)
azpcchange[abs(azpcchange)<=0] <- NA
azpcindex <- which(!is.na(azpcchange))
azpcindex <- c(0,azpcindex)

#for (as in 20:23){#length(azpcindex)-1) {#length(azpcsequences)) {
#plot(pap2017$datetime[azpcindex[as]+1:azpcindex[as+1]],pap2017$CONC[azpcindex[as]+1:azpcindex[as+1]], 
#      ylim=c(100,600), xlab="date", ylab="xCO2")
#}

plot(pap2017$CONC[200:230]-pap2017$CONC[199:229], col=pap2017$AZPC[200:230],ylim=c(-1,1),
      xlab="index", ylab="xCO2 difference")
lines(c(0,32),c(0,0))
