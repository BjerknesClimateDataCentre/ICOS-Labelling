# Test map for atmospheric sequences STD. Run 1_AtmCO2plot and 2_quality check first!

##############################################################################

# Clear plots
if(!is.null(dev.list())) dev.off()
# Clean workspace
rm(list=ls())

timestamp <- function() {
  as.character(
    as.POSIXct(date(),tz="UTC", format="%a %b %d %H:%M:%S %Y"), 
    format="%Y%m%dT%H%M%S")
}
appendtext <- "TC"

library(ggplot2)
library(maps)
library(mapdata)
library(sp)

load(file = "ATM_PS_20190207T092421.RData")
df_mean_std <- df_mean_std

#extstationinfo <- data.frame("name" = c("ice", "pal", "zep","stm"),
#"latitude" = c(63.3998, 67.9700,78.9067,66.00),
#"longitude" = c(-20.2884, 24.1200, 11.883, 2.00),
#"color" = c("black", "green", "blue","orange")
# extstationinfo <- data.frame("name" = c("tac", "bal"),
#                              "latitude" = c(52.5177, 55.5000),
#                              "longitude" = c(1.1386, 16.6700),
#                              "color" = c("green", "blue"))

extstationinfo <- data.frame("name" = c("cpt", "psa", "hba"),
                             "latitude" = c(-34.3523, -64.9200,-75.5600),
                             "longitude" = c(18.4891, -64.0000,-27.0200),
                             "color" = c("green", "blue", "orange"))

##############################################################################



sizestd <- df_mean_std$std.dev
sizestd[sizestd>= 0 & sizestd<0.5 ] <- 0.1
sizestd[sizestd>= 1 & sizestd<1.5 ] <- 0.15
sizestd[sizestd>= 1.5 & sizestd<2 ] <- 0.2
sizestd[sizestd>= 2 ] <- 0.5

worldmap <- map_data("world")

#svg("~/Desktop/GOSars_AtmosphericSTD_map.svg")

#plot.new()
png(paste("./output/AtmosphericSTD_map_",
    timestamp(),"_",appendtext,".png",sep=""), width=3000,height=1600, res=300)
par(mar=c(5,5,2,2))
ggplot() +
  theme_void() +
  geom_polygon(data=worldmap, aes(x=long, y = lat, group = group), 
                        fill = NA, color = "darkgrey") + 
  coord_fixed(xlim = c(-90, 50), ylim = c(-90, -20), ratio = 2.0) +
  geom_point(data = df_mean_std, 
             aes(x=longitude, y=latitude, size=sizestd),shape=1) +
  scale_colour_gradientn(colours=rainbow(4)) +
  # Position of external stations
  geom_point(data= extstationinfo, 
             aes(x=longitude, y=latitude), size=6, shape=18, col=extstationinfo$color) +
  geom_text(data= extstationinfo, 
             aes(x=longitude+10, y=latitude-2, label=name, fontface="bold")) +
  scale_size(breaks=c(0.1,0.15,0.2,0.5), 
             labels=c("[0-0.5)","[0.5-1)","[1.5-2)", "\u2265 2")) +
  labs(size="Standard deviation \n of atmospheric \n CO2 sequence")
 #     col="Mean of \n atmospheric \n CO2 sequence")
dev.off()



## Time-std plot
png(paste("./output/AtmosphericSTDvsTime_",
    timestamp(),"_",appendtext,".png",sep=""), width=1600,height=1600, res=300)
par(mar=c(5,5,2,2))
plot (df_mean_std$date.time, df_mean_std$std.dev, 
      xlab="Time", ylab="Standard deviation of\n atmospheric CO2 sequence")
dev.off()

png(paste("./output/AtmosphericSTDvsTime_0-10range_",
    timestamp(),"_",appendtext,".png",sep=""), width=1600,height=1600, res=300)
par(mar=c(5,5,2,2))
plot (df_mean_std$date.time, df_mean_std$std.dev, 
      xlab="Time", ylab="Standard deviation of\n atmospheric CO2 sequence",
      ylim=c(0,10))
dev.off()

png(paste("./output/AtmosphericSTDvsTime_0-2range_",
    timestamp(),"_",appendtext,".png",sep=""), width=1600,height=1600, res=300)
par(mar=c(5,5,2,2))
plot (df_mean_std$date.time, df_mean_std$std.dev, 
      xlab="Time", ylab="Standard deviation of\n atmospheric CO2 sequence",
      ylim=c(0,2))
dev.off()
  # If using color, this is how to change palette
  #scale_color_distiller(palette="Spectral")


## Distance to coast
# Calculate distance to coast of all points
points <- matrix(c(df_mean_std$longitude, df_mean_std$latitude),ncol=2)
xy.coastworld <- cbind(worldmap$long, worldmap$lat)[!is.na(worldmap$long), ]
distances <- matrix(0, ncol = 1, nrow = nrow(points))

for (i in 1:nrow(points)) {
  distances[i, 1] <- min(spDistsN1(xy.coastworld,points[i,1:2], longlat = TRUE))
}

df_mean_std$distance.to.coast <- distances


## Distance to land-std plot


## Time-std plot
png(paste("./output/AtmosphericSTDvsDistance_",
    timestamp(),"_",appendtext,".png",sep=""), width=1600,height=1600, res=300)
par(mar=c(5,5,2,2))
plot (df_mean_std$distance.to.coast, df_mean_std$std.dev, 
      xlab="Distance to coast (km)", 
      ylab="Standard deviation of\n atmospheric CO2 sequence")
dev.off()

png(paste("./output/AtmosphericSTDvsDistance_0-10range_",
    timestamp(),"_",appendtext,".png",sep=""), width=1600,height=1600, res=300)
par(mar=c(5,5,2,2))
plot (df_mean_std$distance.to.coast, df_mean_std$std.dev, 
      xlab="Distance to coast (km)", 
      ylab="Standard deviation of\n atmospheric CO2 sequence",
      ylim=c(0,10))
dev.off()

png(paste("./output/AtmosphericSTDvsDistance_0-2range_",
    timestamp(),"_",appendtext,".png",sep=""), width=1600,height=1600, res=300)
par(mar=c(5,5,2,2))
plot (df_mean_std$distance.to.coast, df_mean_std$std.dev, 
      xlab="Distance to coast (km)", 
      ylab="Standard deviation of\n atmospheric CO2 sequence",
      ylim=c(0,2))
dev.off()


#--------------


#antmap < - ggplot(world, aes(x=long, y=lat, group=group, bg = 'white')) +
#geom_polygon(fill = 'grey') antmap + theme(panel.background = element_rect(fill
#= 'white'),  panel.border = element_rect(colour = 'black', fill = NA)) +
#coord_map("ortho", orientation=c(-90, 0, 0), ylim = c(-90,-50))


# 
# dev.off()
# map(database = "world", xlim = c(-15, 40), ylim = c(55, 78.5), fill=FALSE, 
#     resolution=0, mar = c(1,1,2,1))
# points(df_mean_std$longitude,df_mean_std$latitude, cex=log10(df_mean_std$std.dev))
# 
# lines(df_mean_std$longitude,df_mean_std$latitude)
# 
# dev.new()
# map(database = "world", xlim = c(-15, 40), ylim = c(55, 78.5), fill=FALSE, 
#     resolution=0, mar = c(1,1,2,1))
# points(df_mean_std$longitude,df_mean_std$latitude, cex=(df_mean_std$std.dev)/1000)