#Enrique Sánchez-Fabrés code. Environmental data from 3d currents product of Theme 2_3D currents and vertical motion for Sustainable Fisheries

#To download data in fillefile: ftp://eftp.ifremer.fr | login: w0323d4 | password: eprouve-rechargiez-agregez

#Data from 2010

library(mapdata)
library(maptools)
library(rnaturalearth)
library(raster)
library(gdata)
#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/ggpubr")
#install.packages("rlang")
library(ggpubr)



### MAPs and Histograms of environmental variables for locations of Atlantic Bluefin Tuna

setwd("C:/Theme 2_3D currents and vertical motion for Sustainable Fisheries/woc/tracks atun rojo/")

### Colours----

c1 <- rgb(216,238,192, max = 255, alpha = 120, names = "lt.green") #light colours for histogram
c2 <- rgb(255,100,100, max = 255, alpha = 100, names = "lt.red")
c3 <- rgb(0, 100, 255, max=255, alpha = 100, names = "lt.blue")
c4 <- rgb(100, 0, 100, max=255, alpha = 100, names = "lt.purple")
####----
env_track2010<-read.csv("2010 tracks and histograms/2010_env_tracks.csv")
#env_track2010<-env_track2010[,2:16]

env_track2011<-read.csv("2011 tracks and histograms/2011_env_tracks.csv")
table(env_track2011$Deploy.ID)

env_track2012<-read.csv("2012 tracks and histograms/2012_env_tracks.csv")
table(env_track2012$Deploy.ID)

env_track2013<-read.csv("2013_env_tracks.csv")
table(env_track2012$Deploy.ID)


#Map settings----
mapBuffer <-0.1
worldMap <- ne_download(scale = "medium", returnclass = "sp")

worldMapCroped <- crop(worldMap,extent(-40-mapBuffer, 0+mapBuffer,25-mapBuffer, 65+mapBuffer))  # based on geographical limits of oceanographical product and blueifn distribution

mainMap <- ggplot() + geom_polygon(data = worldMapCroped, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa general

#Map in and out 

mtlt<-read.csv("C:/Theme 2_3D currents and vertical motion for Sustainable Fisheries/woc/tracks atun rojo/Magic table long tracks Bluefin Tuna.csv")
mtlt<-subset(mtlt, !Year=="2009")
mt<-read.csv("Magic Table Tuna Tracks.csv")
lt<-read.csv("All Long tracks Bluefin Tuna.csv") #to show full tracks in the maps
lt<-subset(lt, !Year=="2009") 
lt<-subset(lt, Lon<0)

#pdf("full tuna tracks.pdf")
mainMap + geom_point(data = lt, aes(x = Lon, y = Lat,  color = factor(Year)), size = 0.5) + labs(title = "Tuna tracks") + labs(color = "Year", size = 12) +geom_rect(aes(xmin = -40, xmax = -6.075, ymin = 25, ymax = 50), color = "red",fill=NA, inherit.aes = FALSE)
#dev.off()

in_out<-read.csv("Magic Table Tuna Tracks.csv")
mainMap + geom_point(data =in_out, aes(x = Lon, y = Lat,  color = factor(Direction)), size = 0.8) + labs(title = "Tuna tracks") + labs(colour = "Direction", size = 12) 


#### MAP WITH ALL TRACKS 2010 ####

#pdf(file="2010_Env_histograms_tracks.pdf")

worldMapCroped <- crop(worldMap,extent(-25-mapBuffer, 0+mapBuffer,25-mapBuffer, 65+mapBuffer))  # based on geographical limits of oceanographical product and blueifn distribution

mainMap <- ggplot() + geom_polygon(data = worldMapCroped, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa general

lt10<-subset(lt, Year=="2010")

#png("2010 tuna tracks map.png", width = 500, height = 400 )
mainMap + geom_point(data = lt10, aes(x = Lon, y = Lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2010") + labs(colour = "Deploy.ID", size = 12) +geom_rect(aes(xmin = -40, xmax = -6.075, ymin = 25, ymax = 50), color = "red",fill=NA, inherit.aes = FALSE) 
dev.off()




#pdf(file="Long tuna tracks 2010.pdf")
#dev.off()

#Histograms for all tracks together----

ggplot(env_track2010, aes(x=cellvalue_sal_15)) + 
  geom_histogram(aes(y=..density..), fill="gray")+
  geom_density(alpha=.2, fill="#FF6666") +
  scale_color_grey() + theme_classic() 



par(mfrow=c(2,1))

hist(env_track2010$cellvalue_sal_15, breaks=50, xlim=c(35.4,36.7), main = "2010 - Salinity in tuna locations", sub="15m", xlab="Salinity (PSU)")
hist(env_track2010$cellvalue_sal_100,  breaks=50, xlim=c(35.4,36.7), main = "", sub="100m", xlab="Salinity (PSU)" )

par(mfrow=c(1,1))
hist(env_track2010$cellvalue_temp_15, breaks=25, ylim=c(0,80), xlim=c(284, 298), main = "2010 - Temperature in tuna locations ", col="red", xlab="Temperature (ºK)" )
hist(env_track2010$cellvalue_temp_100, breaks=25, main = "2010 - Temperature in tuna locations at 100m ", xlab="Temperature (ºK)", add=T)
legend("topright", c("15m", "100m"), fill=c("red", "gray"))

par(mfrow=c(2,2))
hist(env_track2010$cellvalue_u15, breaks=50, xlim=c(-0.2, 0.4), main = "2010 - Water velocity", sub ="15m", xlab= "Eastward water velocity (m/s)")
hist(env_track2010$cellvalue_u100, breaks=50, xlim=c(-0.2, 0.4), main = "",sub="100m", xlab= "Eastward water velocity (m/s)")

hist(env_track2010$cellvalue_v15, breaks=25, xlim=c(-0.2, 0.2), main = "",sub="15m", xlab= "Northward water velocity (m/s)")
hist(env_track2010$cellvalue_v100, breaks=50, xlim=c(-0.2, 0.2), main = "",sub="100m", xlab= "Northward water velocity (m/s)")

par(mfrow=c(1,1))
hist(env_track2010$cellvalue_w100, breaks=50, xlim=c(-0.000025, 0.00004), main = "2010 - Vertical velocity at 100m", xlab= "Vertical water velocity (m/s)")
dev.off()

#Histograms and maps by indvidual----
#id61125 ----
table(env_track2010$Deploy.ID)

id61125<-subset(env_track2010,Deploy.ID=="61125")

#pdf(file="2010_61125_Env_histograms_tracks.pdf")

mainMap + geom_point(data = id61125, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2010 |  ID61125 | Direction = out")  + theme(legend.position='none')

par(mfrow=c(2,1))

hist(id61125$cellvalue_sal_15, breaks=25, xlim=c(35.4,36.7), main = "2010 - Salinity in id61125 locations", xlab="Salinity (PSU)", sub="15m",)
hist(id61125$cellvalue_sal_100,  breaks=25, xlim=c(35.4,36.7), main = "", sub="100m", xlab="Salinity (PSU)" )

par(mfrow=c(1,1))
hist(id61125$cellvalue_temp_15, breaks=50, xlim=c(284, 296), ylim=c(0, 40), main = "2010 -  id61125 Temperature", col="red", xlab="Temperature (ºK)" )
hist(id61125$cellvalue_temp_100, breaks=25, main = "", add=T)
legend("topright", c("15m", "100m"), fill=c("red", "gray"))

par(mfrow=c(2,2))
hist(id61125$cellvalue_u15, breaks=10, xlim=c(-0.2, 0.2), main = "2010 - Water velocity (id61125)", sub ="15m", xlab= "Eastward water velocity (m/s)")
hist(id61125$cellvalue_u100, breaks=10, xlim=c(-0.2, 0.2), main = "", sub ="100m",xlab= "Eastward water velocity (m/s)")

hist(id61125$cellvalue_v15, breaks=25, xlim=c(-0.2, 0.2), main = "",  sub ="15m", xlab= "Northward water velocity (m/s)")
hist(id61125$cellvalue_v100, breaks=25, xlim=c(-0.2, 0.2), main = "",  sub ="100m",xlab= "Northward water velocity (m/s)")

par(mfrow=c(1,1))
hist(id61125$cellvalue_w100, breaks=100, xlim=c(-0.000025, 0.00003), main = "2010 - Vertical velocity at 100m (id61125)", xlab= "Vertical water velocity (m/s)")

#dev.off()


#id61127 ----
table(env_track2010$Deploy.ID)

id61127<-subset(env_track2010,Deploy.ID=="61127")

#pdf(file="2010_61127_Env_histograms_tracks.pdf")

mainMap + geom_point(data = id61127, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2010 |  ID61127 | Direction = out")  + theme(legend.position='none')

par(mfrow=c(2,1))

hist(id61127$cellvalue_sal_15, breaks=25, xlim=c(35.4,36.7), main = "2010 - Salinity in id61127 locations", sub="15m",xlab="Salinity (PSU)")
hist(id61127$cellvalue_sal_100,  breaks=25, xlim=c(35.4,36.7), main = "", sub="100m", xlab="Salinity (PSU)" )

par(mfrow=c(1,1))
hist(id61127$cellvalue_temp_15, breaks=25, xlim=c(284, 296), ylim=c(0, 18), main = "2010 - Temperature in id61127 locations", col="red", xlab="Temperature (ºK)" )
hist(id61127$cellvalue_temp_100, breaks=25, main = "", add=T)
legend("topright", c("15m", "100m"), fill=c("red", "gray"))

par(mfrow=c(2,2))
hist(id61127$cellvalue_u15, breaks=10, xlim=c(-0.2, 0.2), main = "2010 - Water velocity (id61127)", sub ="15m", xlab= "Eastward water velocity (m/s)")
hist(id61127$cellvalue_u100, breaks=10, xlim=c(-0.2, 0.2), main = "", sub ="100m",xlab= "Eastward water velocity (m/s)")

hist(id61127$cellvalue_v15, breaks=25, xlim=c(-0.2, 0.2), main = "",  sub ="15m", xlab= "Northward water velocity (m/s)")
hist(id61127$cellvalue_v100, breaks=25, xlim=c(-0.2, 0.2), main = "",  sub ="100m",xlab= "Northward water velocity (m/s)")


par(mfrow=c(1,1))
hist(id61127$cellvalue_w100, breaks=100, xlim=c(-0.000025, 0.00003), main = "2010 - Vertical velocity at 100m (id61127)", xlab= "Vertical water velocity (m/s)")

#dev.off()

#id86438----

table(env_track2010$Deploy.ID)

id86438<-subset(env_track2010,Deploy.ID=="86438")

#pdf(file="2010_86438_Env_histograms_tracks.pdf")

mainMap + geom_point(data = id86438, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2010 |  ID86438 | Direction = out")  + theme(legend.position='none')

par(mfrow=c(2,1))

hist(id86438$cellvalue_sal_15, breaks=25, xlim=c(35.4,36.7), main = "2010 - Salinity in id86438 locations", sub="15m", xlab="Salinity (PSU)")
hist(id86438$cellvalue_sal_100,  breaks=25, xlim=c(35.4,36.7), main = "", sub="100m", xlab="Salinity (PSU)" )

par(mfrow=c(1,1))
hist(id86438$cellvalue_temp_15, breaks=25, xlim=c(284, 296), ylim=c(0, 18), main = "2010 - Temperature in id86438 locations", col="red", xlab="Temperature (ºK)" )
hist(id86438$cellvalue_temp_100, breaks=25, main = "", add=T)
legend("topright", c("15m", "100m"), fill=c("red", "gray"))

par(mfrow=c(2,2))
hist(id86438$cellvalue_u15, breaks=50, xlim=c(-0.2, 0.25), main = "2010 - Water velocity (id86438)", sub ="15m", xlab= "Eastward water velocity (m/s)")
hist(id86438$cellvalue_u100, breaks=10, xlim=c(-0.2, 0.25), main = "", sub ="100m",xlab= "Eastward water velocity (m/s)")

hist(id86438$cellvalue_v15, breaks=25, xlim=c(-0.2, 0.25), main = "",  sub ="15m", xlab= "Northward water velocity (m/s)")
hist(id86438$cellvalue_v100, breaks=25, xlim=c(-0.2, 0.25), main = "",  sub ="100m",xlab= "Northward water velocity (m/s)")


par(mfrow=c(1,1))
hist(id86438$cellvalue_w100, breaks=50, xlim=c(-0.000025, 0.00003), main = "2010 - Vertical velocity at 100m (id86438)", xlab= "Vertical water velocity (m/s)")

#dev.off()


#id86440 ----
table(env_track2010$Deploy.ID)

id86440 <-subset(env_track2010,Deploy.ID=="86440")


#pdf(file="2010_86440_Env_histograms_tracks.pdf")

mainMap + geom_point(data = id86440, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2010 |  ID86440 | Direction = out")  + theme(legend.position='none')

par(mfrow=c(2,1))

hist(id86440$cellvalue_sal_15, breaks=25, xlim=c(35.4,36.7), main = "2010 - Salinity in id86440 locations", sub="15m", xlab="Salinity (PSU)")
hist(id86440$cellvalue_sal_100,  breaks=25, xlim=c(35.4,36.7), main = "", sub="100m", xlab="Salinity (PSU)" )

par(mfrow=c(1,1))
hist(id86440$cellvalue_temp_15, breaks=25, xlim=c(284, 296), ylim=c(0, 20), main = "2010 - Temperature in id86440 locations", col="red", xlab="Temperature (ºK)" )
hist(id86440$cellvalue_temp_100, breaks=50, main = "", add=T)
legend("topright", c("15m", "100m"), fill=c("red", "gray"))

par(mfrow=c(2,2))
hist(id86440$cellvalue_u15, breaks=50, xlim=c(-0.2, 0.25), main = "2010 - Water velocity (id86440)", sub ="15m", xlab= "Eastward water velocity (m/s)")
hist(id86440$cellvalue_u100, breaks=10, xlim=c(-0.2, 0.25), main = "", sub ="100m",xlab= "Eastward water velocity (m/s)")

hist(id86440$cellvalue_v15, breaks=25, xlim=c(-0.2, 0.25), main = "",  sub ="15m", xlab= "Northward water velocity (m/s)")
hist(id86440$cellvalue_v100, breaks=25, xlim=c(-0.2, 0.25), main = "",  sub ="100m",xlab= "Northward water velocity (m/s)")


par(mfrow=c(1,1))
hist(id86440$cellvalue_w100, breaks=50, xlim=c(-0.000025, 0.00003), main = "2010 - Vertical velocity at 100m (id86440)", xlab= "Vertical water velocity (m/s)")

#dev.off()



#2011----

#### MAP and histogram WITH ALL TRACKS 2011 ####



#pdf("2011_env_histograms_tracks.pdf")
#png("2011 tuna tracks map.png",width = 500, height = 400)
lt11<-subset(lt, Year=="2011")

mainMap + geom_point(data = lt11, aes(x = Lon, y = Lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2011") + labs(colour = "Deploy.ID", size = 12) +geom_rect(aes(xmin = -40, xmax = -6.075, ymin = 25, ymax = 50), color = "red",fill=NA, inherit.aes = FALSE) 
dev.off()

#Histograms 2011  table(env_track2011$Deploy.ID)


par(mfrow=c(2,1))

hist(env_track2011$cellvalue_sal_15, breaks=50, xlim=c(35.4,36.7), main = "2011 - Salinity in tuna locations", sub="15m", xlab="Salinity (PSU)")
hist(env_track2011$cellvalue_sal_100,  breaks=50, xlim=c(35.4,36.7), main = "", sub="100m", xlab="Salinity (PSU)" )

par(mfrow=c(1,1))
hist(env_track2011$cellvalue_temp_15, breaks=25, ylim=c(0,80), xlim=c(282, 298), main = "2011 - Temperature in tuna locations ", xlab="Temperature (ºK)" )
hist(env_track2011$cellvalue_temp_100, breaks=25, main = "2011 - Temperature in tuna locations at 100m ", xlab="Temperature (ºK)", col= c2, add=T)
legend("topright", c("15m", "100m"), fill=c("gray", c2))

par(mfrow=c(2,2))
hist(env_track2011$cellvalue_u15, breaks=50, xlim=c(-0.2, 0.4), main = "2011 - Water velocity", sub ="15m", xlab= "Eastward water velocity (m/s)")
hist(env_track2011$cellvalue_u100, breaks=50, xlim=c(-0.2, 0.4), main = "",sub="100m", xlab= "Eastward water velocity (m/s)")

hist(env_track2011$cellvalue_v15, breaks=25, xlim=c(-0.2, 0.2), main = "",sub="15m", xlab= "Northward water velocity (m/s)")
hist(env_track2011$cellvalue_v100, breaks=50, xlim=c(-0.2, 0.2), main = "",sub="100m", xlab= "Northward water velocity (m/s)")

par(mfrow=c(1,1))
hist(env_track2011$cellvalue_w100, breaks=50, xlim=c(-0.00004, 0.00004), main = "2011 - Vertical velocity at 100m", xlab= "Vertical water velocity (m/s)")
dev.off()


#72498----
table(env_track2011$Deploy.ID)

id72498<-subset(env_track2011,Deploy.ID=="72498")

pdf(file="2011_72498_Env_histograms_tracks.pdf")

mainMap + geom_point(data = id72498, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2011 |  ID72498 | Direction = out")  + theme(legend.position='none')

par(mfrow=c(2,1))

hist(id72498$cellvalue_sal_15, breaks=25, xlim=c(35.4,36.7), main = "2011 - Salinity in id72498 locations", xlab="Salinity (PSU)", sub="15m",)
hist(id72498$cellvalue_sal_100,  breaks=25, xlim=c(35.4,36.7), main = "", sub="100m", xlab="Salinity (PSU)" )

par(mfrow=c(1,1))
hist(id72498$cellvalue_temp_15, breaks=25, xlim=c(284, 296), ylim=c(0, 30), main = "2011 -  id72498 Temperature", col="red", xlab="Temperature (ºK)" )
hist(id72498$cellvalue_temp_100, breaks=25, main = "", add=T,col=c1)
legend("topright", c("15m", "100m"), fill=c("red", c1))

par(mfrow=c(2,2))
hist(id72498$cellvalue_u15, breaks=10, xlim=c(-0.2, 0.2), main = "2011 - Water velocity (id72498)", sub ="15m", xlab= "Eastward water velocity (m/s)")
hist(id72498$cellvalue_u100, breaks=10, xlim=c(-0.2, 0.2), main = "", sub ="100m",xlab= "Eastward water velocity (m/s)")

hist(id72498$cellvalue_v15, breaks=25, xlim=c(-0.2, 0.2), main = "",  sub ="15m", xlab= "Northward water velocity (m/s)")
hist(id72498$cellvalue_v100, breaks=25, xlim=c(-0.2, 0.2), main = "",  sub ="100m",xlab= "Northward water velocity (m/s)")

par(mfrow=c(1,1))
hist(id72498$cellvalue_w100, breaks=15, xlim=c(-0.000025, 0.00003), main = "2011 - Vertical velocity at 100m (id72498)", xlab= "Vertical water velocity (m/s)")

dev.off()


#72499----
table(env_track2011$Deploy.ID)


id72499<-subset(env_track2011,Deploy.ID=="72499")

pdf(file="2011_72499_Env_histograms_tracks.pdf")

mainMap + geom_point(data = id72499, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2011 |  ID72499 | Direction = out")  + theme(legend.position='none')

par(mfrow=c(2,1))

hist(id72499$cellvalue_sal_15, breaks=25, xlim=c(35.4,36.7), main = "2011 - Salinity in id72499 locations", xlab="Salinity (PSU)", sub="15m",)
hist(id72499$cellvalue_sal_100,  breaks=25, xlim=c(35.4,36.7), main = "", sub="100m", xlab="Salinity (PSU)" )

par(mfrow=c(1,1))
hist(id72499$cellvalue_temp_15, breaks=25, xlim=c(284, 296), ylim=c(0, 30), main = "2011 -  id72499 Temperature", col="red", xlab="Temperature (ºK)" )
hist(id72499$cellvalue_temp_100, breaks=25, main = "", add=T,col=c1)
legend("topright", c("15m", "100m"), fill=c("red", c1))

par(mfrow=c(2,2))
hist(id72499$cellvalue_u15, breaks=10, xlim=c(-0.2, 0.2), main = "2011 - Water velocity (id72499)", sub ="15m", xlab= "Eastward water velocity (m/s)")
hist(id72499$cellvalue_u100, breaks=10, xlim=c(-0.2, 0.2), main = "", sub ="100m",xlab= "Eastward water velocity (m/s)")

hist(id72499$cellvalue_v15, breaks=25, xlim=c(-0.2, 0.2), main = "",  sub ="15m", xlab= "Northward water velocity (m/s)")
hist(id72499$cellvalue_v100, breaks=25, xlim=c(-0.2, 0.2), main = "",  sub ="100m",xlab= "Northward water velocity (m/s)")

par(mfrow=c(1,1))
hist(id72499$cellvalue_w100, breaks=15, xlim=c(-0.000025, 0.00003), main = "2011 - Vertical velocity at 100m (id72499)", xlab= "Vertical water velocity (m/s)")

dev.off()

#86243----
table(env_track2011$Deploy.ID)


id86243<-subset(env_track2011,Deploy.ID=="86243")

pdf(file="2011_86243_Env_histograms_tracks.pdf")

mainMap + geom_point(data = id86243, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2011 |  ID86243 | Direction = out")  + theme(legend.position='none')

par(mfrow=c(2,1))

hist(id86243$cellvalue_sal_15, breaks=25, xlim=c(35.4,36.7), main = "2011 - Salinity in id86243 locations", xlab="Salinity (PSU)", sub="15m",)
hist(id86243$cellvalue_sal_100,  breaks=25, xlim=c(35.4,36.7), main = "", sub="100m", xlab="Salinity (PSU)" )

par(mfrow=c(1,1))
hist(id86243$cellvalue_temp_15, breaks=25, xlim=c(284, 296), ylim=c(0, 30), main = "2011 -  id86243 Temperature", col="red", xlab="Temperature (ºK)" )
hist(id86243$cellvalue_temp_100, breaks=25, main = "", add=T,col=c1)
legend("topright", c("15m", "100m"), fill=c("red", c1))

par(mfrow=c(2,2))
hist(id86243$cellvalue_u15, breaks=10, xlim=c(-0.2, 0.2), main = "2011 - Water velocity (id86243)", sub ="15m", xlab= "Eastward water velocity (m/s)")
hist(id86243$cellvalue_u100, breaks=10, xlim=c(-0.2, 0.2), main = "", sub ="100m",xlab= "Eastward water velocity (m/s)")

hist(id86243$cellvalue_v15, breaks=25, xlim=c(-0.2, 0.2), main = "",  sub ="15m", xlab= "Northward water velocity (m/s)")
hist(id86243$cellvalue_v100, breaks=25, xlim=c(-0.2, 0.2), main = "",  sub ="100m",xlab= "Northward water velocity (m/s)")

par(mfrow=c(1,1))
hist(id86243$cellvalue_w100, breaks=15, xlim=c(-0.000025, 0.00003), main = "2011 - Vertical velocity at 100m (id86243)", xlab= "Vertical water velocity (m/s)")

dev.off()

#97462----
table(env_track2011$Deploy.ID)

id97462<-subset(env_track2011,Deploy.ID=="97462")

pdf(file="2011_97462_Env_histograms_tracks.pdf")

mainMap + geom_point(data = id97462, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2011 |  ID97462 | Direction = out")  + theme(legend.position='none')

par(mfrow=c(2,1))

hist(id97462$cellvalue_sal_15, breaks=25, xlim=c(35.4,36.7), main = "2011 - Salinity in id97462 locations", xlab="Salinity (PSU)", sub="15m",)
hist(id97462$cellvalue_sal_100,  breaks=25, xlim=c(35.4,36.7), main = "", sub="100m", xlab="Salinity (PSU)" )

par(mfrow=c(1,1))
hist(id97462$cellvalue_temp_15, breaks=50, xlim=c(284, 296),  main = "2011 -  id97462 Temperature", col="red", xlab="Temperature (ºK)" )
hist(id97462$cellvalue_temp_100, breaks=25, main = "", add=T,col=c1)
legend("topright", c("15m", "100m"), fill=c("red", c1))

par(mfrow=c(2,2))
hist(id97462$cellvalue_u15, breaks=10, xlim=c(-0.3, 0.2), main = "2011 - Water velocity (id97462)", sub ="15m", xlab= "Eastward water velocity (m/s)")
hist(id97462$cellvalue_u100, breaks=10, xlim=c(-0.3, 0.2), main = "", sub ="100m",xlab= "Eastward water velocity (m/s)")

hist(id97462$cellvalue_v15, breaks=25, xlim=c(-0.2, 0.2), main = "",  sub ="15m", xlab= "Northward water velocity (m/s)")
hist(id97462$cellvalue_v100, breaks=25, xlim=c(-0.2, 0.2), main = "",  sub ="100m",xlab= "Northward water velocity (m/s)")

par(mfrow=c(1,1))
hist(id97462$cellvalue_w100, breaks= 50,xlim=c(-0.00003, 0.00002), main = "2011 - Vertical velocity at 100m (id97462)", xlab= "Vertical water velocity (m/s)")

dev.off()

#97466-----

table(env_track2011$Deploy.ID)

id97466<-subset(env_track2011,Deploy.ID=="97466")

pdf(file="2011_97466_Env_histograms_tracks.pdf")

mainMap + geom_point(data = id97466, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2011 |  ID97466 | Direction = out")  + theme(legend.position='none')

par(mfrow=c(2,1))

hist(id97466$cellvalue_sal_15, breaks=25, xlim=c(36,37), main = "2011 - Salinity in id97466 locations", xlab="Salinity (PSU)", sub="15m",)
hist(id97466$cellvalue_sal_100,  breaks=25, xlim=c(36,37), main = "", sub="100m", xlab="Salinity (PSU)" )

par(mfrow=c(1,1))
hist(id97466$cellvalue_temp_15, breaks=25, xlim=c(284, 300),  main = "2011 -  id97466 Temperature", col="red", xlab="Temperature (ºK)" )
hist(id97466$cellvalue_temp_100, breaks=25, main = "", add=T,col=c1)
legend("topright", c("15m", "100m"), fill=c("red", c1))

par(mfrow=c(2,2))
hist(id97466$cellvalue_u15, breaks=10, xlim=c(-0.3, 0.2), main = "2011 - Water velocity (id97466)", sub ="15m", xlab= "Eastward water velocity (m/s)")
hist(id97466$cellvalue_u100, breaks=10, xlim=c(-0.3, 0.2), main = "", sub ="100m",xlab= "Eastward water velocity (m/s)")

hist(id97466$cellvalue_v15, breaks=25, xlim=c(-0.2, 0.2), main = "",  sub ="15m", xlab= "Northward water velocity (m/s)")
hist(id97466$cellvalue_v100, breaks=25, xlim=c(-0.2, 0.2), main = "",  sub ="100m",xlab= "Northward water velocity (m/s)")

par(mfrow=c(1,1))
hist(id97466$cellvalue_w100, breaks= 50,xlim=c(-0.00004, 0.00004), main = "2011 - Vertical velocity at 100m (id97466)", xlab= "Vertical water velocity (m/s)")

dev.off()




#2012----

#97462 114006 114008 114009 118756 120084 120088 
# 14     49     67    163    152     37     31

#Main map tracks 2012 and histograms 2012 ----

#png("2012 tuna tracks map.png",width = 500, height = 400)
lt12<-subset(lt, Year=="2012")

mainMap + geom_point(data = lt12, aes(x = Lon, y = Lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2012") + labs(colour = "Deploy.ID", size = 12) +geom_rect(aes(xmin = -40, xmax = -6.075, ymin = 25, ymax = 50), color = "red",fill=NA, inherit.aes = FALSE) 
dev.off()

#Main histograms 2012



par(mfrow=c(2,1))

hist(env_track2012$cellvalue_sal_15, breaks=50, xlim=c(35,37.5), main = "2012 - Salinity in tuna locations", sub="15m", xlab="Salinity (PSU)")
hist(env_track2012$cellvalue_sal_100,  breaks=50, xlim=c(35,37.5), main = "", sub="100m", xlab="Salinity (PSU)" )

par(mfrow=c(1,1))
hist(env_track2012$cellvalue_temp_15, breaks=25, ylim=c(0,140), xlim=c(285, 300), main = "2012 - Temperature in tuna locations ", xlab="Temperature (ºK)" )
hist(env_track2012$cellvalue_temp_100, breaks=25, main = "2012 - Temperature in tuna locations at 100m ", xlab="Temperature (ºK)", col= c2, add=T)
legend("topright", c("15m", "100m"), fill=c("gray", c2))

par(mfrow=c(2,2))
hist(env_track2012$cellvalue_u15, breaks=100, xlim=c(-0.3, 0.4), main = "2012 - Water velocity", sub ="15m", xlab= "Eastward water velocity (m/s)")
hist(env_track2012$cellvalue_u100, breaks=25, xlim=c(-0.3, 0.4), main = "",sub="100m", xlab= "Eastward water velocity (m/s)")

hist(env_track2012$cellvalue_v15, breaks=100, xlim=c(-0.3, 0.3), main = "",sub="15m", xlab= "Northward water velocity (m/s)")
hist(env_track2012$cellvalue_v100, breaks=50, xlim=c(-0.2, 0.2), main = "",sub="100m", xlab= "Northward water velocity (m/s)")

par(mfrow=c(1,1))
hist(env_track2012$cellvalue_w100, breaks=50, xlim=c(-0.00004, 0.00004), main = "2012 - Vertical velocity at 100m", xlab= "Vertical water velocity (m/s)")

dev.off()



#114009-----
table(env_track2012$Deploy.ID)

id114009<-subset(env_track2012,Deploy.ID=="114009")

pdf(file="2012_114009_Env_histograms_tracks.pdf")

mainMap + geom_point(data = id114009, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2012 |  ID114009 | Direction = out")  + theme(legend.position='none')

par(mfrow=c(2,1))

hist(id114009$cellvalue_sal_15, breaks=25, xlim=c(35,37.5), main = "2012 - Salinity in id114009 locations", xlab="Salinity (PSU)", sub="15m",)
hist(id114009$cellvalue_sal_100,  breaks=25, xlim=c(35,37.5), main = "", sub="100m", xlab="Salinity (PSU)" )

par(mfrow=c(1,1))
hist(id114009$cellvalue_temp_15, breaks=25, xlim=c(285, 300), ylim=c(0, 35), main = "2012 -  id114009 Temperature", col="red", xlab="Temperature (ºK)" )
hist(id114009$cellvalue_temp_100, breaks=25, main = "", add=T,col=c1)
legend("topright", c("15m", "100m"), fill=c("red", c1))

par(mfrow=c(2,2))
hist(id114009$cellvalue_u15, breaks=100, xlim=c(-0.3, 0.4), main = "2012 - Water velocity (id114009)", sub ="15m", xlab= "Eastward water velocity (m/s)")
hist(id114009$cellvalue_u100, breaks=10, xlim=c(-0.3, 0.4), main = "", sub ="100m",xlab= "Eastward water velocity (m/s)")

hist(id114009$cellvalue_v15, breaks=100, xlim=c(-0.3, 0.4), main = "",  sub ="15m", xlab= "Northward water velocity (m/s)")
hist(id114009$cellvalue_v100, breaks=10, xlim=c(-0.3, 0.4), main = "",  sub ="100m",xlab= "Northward water velocity (m/s)")

par(mfrow=c(1,1))
hist(id114009$cellvalue_w100, breaks=25, xlim=c(-0.00004, 0.00004), main = "2012 - Vertical velocity at 100m (id114009)", xlab= "Vertical water velocity (m/s)")

dev.off()

#118756-----

table(env_track2012$Deploy.ID)

id118756<-subset(env_track2012,Deploy.ID=="118756")

pdf(file="2012_1118756_Env_histograms_tracks.pdf")

mainMap + geom_point(data = id118756, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2012 |  ID118756 | Direction = out")  + theme(legend.position='none')

par(mfrow=c(2,1))

hist(id118756$cellvalue_sal_15, breaks=25, xlim=c(35,37.5), main = "2012 - Salinity in id118756 locations", xlab="Salinity (PSU)", sub="15m",)
hist(id118756$cellvalue_sal_100,  breaks=25, xlim=c(35,37.5), main = "", sub="100m", xlab="Salinity (PSU)" )

par(mfrow=c(1,1))
hist(id118756$cellvalue_temp_15, breaks=25, xlim=c(285, 300), ylim=c(0, 30), main = "2012 -  id118756 Temperature", col="red", xlab="Temperature (ºK)" )
hist(id118756$cellvalue_temp_100, breaks=25, main = "", add=T,col=c1)
legend("topright", c("15m", "100m"), fill=c("red", c1))

par(mfrow=c(2,2))
hist(id118756$cellvalue_u15, breaks=100, xlim=c(-0.3, 0.4), main = "2012 - Water velocity (id118756)", sub ="15m", xlab= "Eastward water velocity (m/s)")
hist(id118756$cellvalue_u100, breaks=25, xlim=c(-0.3, 0.4), main = "", sub ="100m",xlab= "Eastward water velocity (m/s)")

hist(id118756$cellvalue_v15, breaks=100, xlim=c(-0.3, 0.4), main = "",  sub ="15m", xlab= "Northward water velocity (m/s)")
hist(id118756$cellvalue_v100, breaks=25, xlim=c(-0.3, 0.4), main = "",  sub ="100m",xlab= "Northward water velocity (m/s)")

par(mfrow=c(1,1))
hist(id118756$cellvalue_w100, breaks=15, xlim=c(-0.00004, 0.00004), main = "2012 - Vertical velocity at 100m (id118756)", xlab= "Vertical water velocity (m/s)")

dev.off()
#114006----

table(env_track2012$Deploy.ID)

id114006<-subset(env_track2012,Deploy.ID=="114006")

pdf(file="2012_114006_Env_histograms_tracks.pdf")

mainMap + geom_point(data = id114006, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2012 |  ID114006 | Direction = out")  + theme(legend.position='none')

par(mfrow=c(2,1))

hist(id114006$cellvalue_sal_15, breaks=25, xlim=c(35,37.5), main = "2012 - Salinity in id114006 locations", xlab="Salinity (PSU)", sub="15m",)
hist(id114006$cellvalue_sal_100,  breaks=25, xlim=c(35,37.5), main = "", sub="100m", xlab="Salinity (PSU)" )

par(mfrow=c(1,1))
hist(id114006$cellvalue_temp_15, breaks=25, xlim=c(285, 300), ylim=c(0, 10), main = "2012 -  id114006 Temperature", col="red", xlab="Temperature (ºK)" )
hist(id114006$cellvalue_temp_100, breaks=25, main = "", add=T,col=c1)
legend("topright", c("15m", "100m"), fill=c("red", c1))

par(mfrow=c(2,2))
hist(id114006$cellvalue_u15, breaks=10, xlim=c(-0.3, 0.4), main = "2012 - Water velocity (id114006)", sub ="15m", xlab= "Eastward water velocity (m/s)")
hist(id114006$cellvalue_u100, breaks=10, xlim=c(-0.3, 0.4), main = "", sub ="100m",xlab= "Eastward water velocity (m/s)")

hist(id114006$cellvalue_v15, breaks=10, xlim=c(-0.3, 0.4), main = "",  sub ="15m", xlab= "Northward water velocity (m/s)")
hist(id114006$cellvalue_v100, breaks=10, xlim=c(-0.3, 0.4), main = "",  sub ="100m",xlab= "Northward water velocity (m/s)")

par(mfrow=c(1,1))
hist(id114006$cellvalue_w100, breaks=15, xlim=c(-0.00004, 0.00004), main = "2012 - Vertical velocity at 100m (id114006)", xlab= "Vertical water velocity (m/s)")
dev.off()

#114008----
table(env_track2012$Deploy.ID)

id114008<-subset(env_track2012,Deploy.ID=="114008")

pdf(file="2012_114008_Env_histograms_tracks.pdf")

mainMap + geom_point(data = id114008, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2012 |  ID114008 | Direction = out")  + theme(legend.position='none')

par(mfrow=c(2,1))

hist(id114008$cellvalue_sal_15, breaks=25, xlim=c(35,37.5), main = "2012 - Salinity in id114008 locations", xlab="Salinity (PSU)", sub="15m",)
hist(id114008$cellvalue_sal_100,  breaks=25, xlim=c(35,37.5), main = "", sub="100m", xlab="Salinity (PSU)" )

par(mfrow=c(1,1))
hist(id114008$cellvalue_temp_15, breaks=10, xlim=c(285, 300), ylim=c(0, 20), main = "2012 -  id114008 Temperature", col="red", xlab="Temperature (ºK)" )
hist(id114008$cellvalue_temp_100, main = "", add=T,col=c1)
legend("topright", c("15m", "100m"), fill=c("red", c1))

par(mfrow=c(2,2))
hist(id114008$cellvalue_u15, breaks=100, xlim=c(-0.3, 0.4), main = "2012 - Water velocity (id114008)", sub ="15m", xlab= "Eastward water velocity (m/s)")
hist(id114008$cellvalue_u100, breaks=25, xlim=c(-0.3, 0.4), main = "", sub ="100m",xlab= "Eastward water velocity (m/s)")

hist(id114008$cellvalue_v15, breaks=100, xlim=c(-0.3, 0.4), main = "",  sub ="15m", xlab= "Northward water velocity (m/s)")
hist(id114008$cellvalue_v100, breaks=25, xlim=c(-0.3, 0.4), main = "",  sub ="100m",xlab= "Northward water velocity (m/s)")

par(mfrow=c(1,1))
hist(id114008$cellvalue_w100, breaks=15, xlim=c(-0.00004, 0.00004), main = "2012 - Vertical velocity at 100m (id114008)", xlab= "Vertical water velocity (m/s)")
dev.off()

#120084-----
table(env_track2012$Deploy.ID)

id120084<-subset(env_track2012,Deploy.ID=="120084")

pdf(file="2012_120084_Env_histograms_tracks.pdf")

mainMap + geom_point(data = id120084, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2012 |  ID120084 | Direction = out")  + theme(legend.position='none')

par(mfrow=c(2,1))

hist(id120084$cellvalue_sal_15, breaks=5, xlim=c(35,37.5), main = "2012 - Salinity in id120084 locations", xlab="Salinity (PSU)", sub="15m",)
hist(id120084$cellvalue_sal_100,  breaks=5, xlim=c(35,37.5), main = "", sub="100m", xlab="Salinity (PSU)" )

par(mfrow=c(1,1))
hist(id120084$cellvalue_temp_15, breaks=10, xlim=c(285, 300), ylim=c(0, 15), main = "2012 -  id120084 Temperature", col="red", xlab="Temperature (ºK)" )
hist(id120084$cellvalue_temp_100, main = "", add=T,col=c1)
legend("topright", c("15m", "100m"), fill=c("red", c1))

par(mfrow=c(2,2))
hist(id120084$cellvalue_u15, breaks=10, xlim=c(-0.3, 0.4), main = "2012 - Water velocity (id120084)", sub ="15m", xlab= "Eastward water velocity (m/s)")
hist(id120084$cellvalue_u100, breaks=10, xlim=c(-0.3, 0.4), main = "", sub ="100m",xlab= "Eastward water velocity (m/s)")

hist(id120084$cellvalue_v15, breaks=25, xlim=c(-0.3, 0.4), main = "",  sub ="15m", xlab= "Northward water velocity (m/s)")
hist(id120084$cellvalue_v100, breaks=25, xlim=c(-0.3, 0.4), main = "",  sub ="100m",xlab= "Northward water velocity (m/s)")

par(mfrow=c(1,1))
hist(id120084$cellvalue_w100, breaks=15, xlim=c(-0.00004, 0.00004), main = "2012 - Vertical velocity at 100m (id120084)", xlab= "Vertical water velocity (m/s)")
dev.off()

#120088 ----
table(env_track2012$Deploy.ID)

id120088<-subset(env_track2012,Deploy.ID=="120088")

#pdf(file="2012_120088_Env_histograms_tracks.pdf")

mainMap + geom_point(data = id120088, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2012 |  ID120088 | Direction = out")  + theme(legend.position='none')

par(mfrow=c(2,1))

hist(id120088$cellvalue_sal_15, breaks=5, xlim=c(35,37.5), main = "2012 - Salinity in id120088 locations", xlab="Salinity (PSU)", sub="15m",)
hist(id120088$cellvalue_sal_100,  breaks=5, xlim=c(35,37.5), main = "", sub="100m", xlab="Salinity (PSU)" )

par(mfrow=c(1,1))
hist(id120088$cellvalue_temp_15, breaks=10, xlim=c(285, 300), ylim=c(0, 15), main = "2012 -  id120088 Temperature", col="red", xlab="Temperature (ºK)" )
hist(id120088$cellvalue_temp_100, main = "", add=T,col=c1)
legend("topright", c("15m", "100m"), fill=c("red", c1))

par(mfrow=c(2,2))
hist(id120088$cellvalue_u15, breaks=10, xlim=c(-0.3, 0.4), main = "2012 - Water velocity (id120088)", sub ="15m", xlab= "Eastward water velocity (m/s)")
hist(id120088$cellvalue_u100, breaks=10, xlim=c(-0.3, 0.4), main = "", sub ="100m",xlab= "Eastward water velocity (m/s)")

hist(id120088$cellvalue_v15, breaks=25, xlim=c(-0.3, 0.4), main = "",  sub ="15m", xlab= "Northward water velocity (m/s)")
hist(id120088$cellvalue_v100, breaks=25, xlim=c(-0.3, 0.4), main = "",  sub ="100m",xlab= "Northward water velocity (m/s)")

par(mfrow=c(1,1))
hist(id120088$cellvalue_w100, breaks=15, xlim=c(-0.00004, 0.00004), main = "2012 - Vertical velocity at 100m (id120088)", xlab= "Vertical water velocity (m/s)")
dev.off()


#2013-----

#114009* 118756 118760 120446 130539 130546 *track 14009 starts in 2012
#  9      8      9      9     20     19

#Main map tracks 2013 and histograms 2013 ----
worldMapCroped <- crop(worldMap,extent(-25-mapBuffer, -4+mapBuffer,25-mapBuffer, 55+mapBuffer))  # based on geographical limits of oceanographical product and blueifn distribution

mainMap <- ggplot() + geom_polygon(data = worldMapCroped, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa general


#pdf("2013_env_histograms_tracks.pdf")

png("2013 tuna tracks map.png",width = 500, height = 400)
lt13<-subset(lt, Year=="2013")

mainMap + geom_point(data = lt13, aes(x = Lon, y = Lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2013") + labs(colour = "Deploy.ID", size = 12) +geom_rect(aes(xmin = -40, xmax = -6.075, ymin = 25, ymax = 50), color = "red",fill=NA, inherit.aes = FALSE) 
dev.off()

#Main histograms 2013----

par(mfrow=c(2,1))

hist(env_track2013$cellvalue_sal_15, breaks=50, xlim=c(35,37.5), main = "2013 - Salinity in tuna locations", sub="15m", xlab="Salinity (PSU)")
hist(env_track2013$cellvalue_sal_100,  breaks=50, xlim=c(35,37.5), main = "", sub="100m", xlab="Salinity (PSU)" )

par(mfrow=c(1,1))
hist(env_track2013$cellvalue_temp_15, breaks=25, ylim=c(0,40), xlim=c(285, 300), main = "2013 - Temperature in tuna locations ", xlab="Temperature (ºK)", col="blue" )
hist(env_track2013$cellvalue_temp_100, breaks=25, main = "2013 - Temperature in tuna locations at 100m ", xlab="Temperature (ºK)", col= c2, add=T)
legend("topright", c("15m", "100m"), fill=c("blue", c2))

par(mfrow=c(2,2))
hist(env_track2013$cellvalue_u15, breaks=25, xlim=c(-0.3, 0.4), main = "2013 - Water velocity", sub ="15m", xlab= "Eastward water velocity (m/s)")
hist(env_track2013$cellvalue_u100, breaks=25, xlim=c(-0.3, 0.4), main = "",sub="100m", xlab= "Eastward water velocity (m/s)")

hist(env_track2013$cellvalue_v15, breaks=25, xlim=c(-0.3, 0.3), main = "",sub="15m", xlab= "Northward water velocity (m/s)")
hist(env_track2013$cellvalue_v100, breaks=50, xlim=c(-0.2, 0.2), main = "",sub="100m", xlab= "Northward water velocity (m/s)")

par(mfrow=c(1,1))
hist(env_track2013$cellvalue_w100, breaks=25, xlim=c(-0.00004, 0.00004), main = "2013 - Vertical velocity at 100m", xlab= "Vertical water velocity (m/s)")

dev.off()



#114009 track of interes, join with track from 2012 ----

id114009_12<-subset(env_track2012,Deploy.ID=="114009")
id114009_13<-subset(env_track2013,Deploy.ID=="114009")

id114009<-rbind(id114009_12,id114009_13)
id114009$year<- year(id114009$date)

#pdf("id114009_env_histograms_tracks.pdf")

mainMap + geom_point(data = id114009, aes(x = lon, y = lat,  color = factor(year)), size = 0.8) + labs(title = "Tuna tracks |  ID114009 | 2012-05-16 / 2013-01-19") 

par(mfrow=c(2,1))

hist(id114009$cellvalue_sal_15, breaks=25, xlim=c(35,37.5), main = "2012/2013 - Salinity in id114009 locations", xlab="Salinity (PSU)", sub="15m",)
hist(id114009$cellvalue_sal_100,  breaks=25, xlim=c(35,37.5), main = "", sub="100m", xlab="Salinity (PSU)" )

par(mfrow=c(1,1))
hist(id114009$cellvalue_temp_15, breaks=25, xlim=c(285, 300), ylim=c(0, 35), main = "2012/2013 -  id114009 Temperature", col="red", xlab="Temperature (ºK)" )
hist(id114009$cellvalue_temp_100, breaks=25,main = "", add=T,col=c1)
legend("topright", c("15m", "100m"), fill=c("red", c1))

par(mfrow=c(2,2))
hist(id114009$cellvalue_u15, breaks=50, xlim=c(-0.3, 0.4), main = "2012/2013 - Water velocity (id114009)", sub ="15m", xlab= "Eastward water velocity (m/s)")
hist(id114009$cellvalue_u100, breaks=25, xlim=c(-0.3, 0.4), main = "", sub ="100m",xlab= "Eastward water velocity (m/s)")

hist(id114009$cellvalue_v15, breaks=50, xlim=c(-0.3, 0.4), main = "",  sub ="15m", xlab= "Northward water velocity (m/s)")
hist(id114009$cellvalue_v100, breaks=25, xlim=c(-0.3, 0.4), main = "",  sub ="100m",xlab= "Northward water velocity (m/s)")

par(mfrow=c(1,1))
hist(id114009$cellvalue_w100, breaks=15, xlim=c(-0.00004, 0.00004), main = "2012/2013 - Vertical velocity at 100m (id114009)", xlab= "Vertical water velocity (m/s)")
dev.off()


#118756-----

id118756<-subset(env_track2013,Deploy.ID=="118756")



#òpdf("id118756_env_histograms_tracks.pdf")

mainMap + geom_point(data = id118756, aes(x = lon, y = lat, col="red"), size = 0.8) + labs(title = "2013 Tuna tracks | ID118756 | Direction = out | ")  + theme(legend.position='none')



#118760-----

id118760<-subset(env_track2013,Deploy.ID=="118760")

#pdf("id118760_env_histograms_tracks.pdf")

mainMap + geom_point(data = id118760, aes(x = lon, y = lat, col="red"), size = 1.2) + labs(title = "2013 Tuna tracks | ID118760 | Direction = out | ")  + theme(legend.position='none')

par(mfrow=c(2,1))

hist(id118760$cellvalue_sal_15, breaks=25, xlim=c(35,37.5), main = "2013 - Salinity in id118760 locations", xlab="Salinity (PSU)", sub="15m",)
hist(id118760$cellvalue_sal_100,  breaks=25, xlim=c(35,37.5), main = "", sub="100m", xlab="Salinity (PSU)" )

par(mfrow=c(1,1))
hist(id118760$cellvalue_temp_15, breaks=25, xlim=c(285, 300), ylim=c(0, 10), main = "2013 -  id118760 Temperature", col="red", xlab="Temperature (ºK)" )
hist(id118760$cellvalue_temp_100, breaks=25,main = "", add=T,col=c1)
legend("topright", c("15m", "100m"), fill=c("red", c1))

par(mfrow=c(2,2))
hist(id118760$cellvalue_u15, breaks=10, xlim=c(-0.3, 0.4), main = "2013 - Water velocity (id118760)", sub ="15m", xlab= "Eastward water velocity (m/s)")
hist(id118760$cellvalue_u100, breaks=10, xlim=c(-0.3, 0.4), main = "", sub ="100m",xlab= "Eastward water velocity (m/s)")

hist(id118760$cellvalue_v15, breaks=10, xlim=c(-0.3, 0.4), main = "",  sub ="15m", xlab= "Northward water velocity (m/s)")
hist(id118760$cellvalue_v100, breaks=10, xlim=c(-0.3, 0.4), main = "",  sub ="100m",xlab= "Northward water velocity (m/s)")

par(mfrow=c(1,1))
hist(id118760$cellvalue_w100, breaks=10, xlim=c(-0.00004, 0.00004), main = "2013 - Vertical velocity at 100m (id118760)", xlab= "Vertical water velocity (m/s)")
dev.off()



#120446----

id120446<-subset(env_track2013,Deploy.ID=="120446")

#òpdf("id118756_env_histograms_tracks.pdf")

mainMap + geom_point(data = id120446, aes(x = lon, y = lat, col="red"), size = 1.2) + labs(title = "2013 Tuna tracks | ID120446 | Direction = out | ")  + theme(legend.position='none')

#130539----

id130539<-subset(env_track2013,Deploy.ID=="130539")

#pdf("id130539_env_histograms_tracks.pdf")

mainMap + geom_point(data = id130539, aes(x = lon, y = lat, col="red"), size = 1.2) + labs(title = "2013 Tuna tracks | ID130539 | Direction = out | ")  + theme(legend.position='none')


par(mfrow=c(2,1))

hist(id130539$cellvalue_sal_15, breaks=25, xlim=c(35,37.5), main = "2013 - Salinity in id130539 locations", xlab="Salinity (PSU)", sub="15m",)
hist(id130539$cellvalue_sal_100,  breaks=25, xlim=c(35,37.5), main = "", sub="100m", xlab="Salinity (PSU)" )

par(mfrow=c(1,1))
hist(id130539$cellvalue_temp_15, breaks=25, xlim=c(285, 300), ylim=c(0, 6), main = "2013 -  id130539 Temperature", col="red", xlab="Temperature (ºK)" )
hist(id130539$cellvalue_temp_100, breaks=25,main = "", add=T,col=c1)
legend("topright", c("15m", "100m"), fill=c("red", c1))

par(mfrow=c(2,2))
hist(id130539$cellvalue_u15, breaks=10, xlim=c(-0.3, 0.4), main = "2013 - Water velocity (id130539)", sub ="15m", xlab= "Eastward water velocity (m/s)")
hist(id130539$cellvalue_u100, breaks=10, xlim=c(-0.3, 0.4), main = "", sub ="100m",xlab= "Eastward water velocity (m/s)")

hist(id130539$cellvalue_v15, breaks=10, xlim=c(-0.3, 0.4), main = "",  sub ="15m", xlab= "Northward water velocity (m/s)")
hist(id130539$cellvalue_v100, breaks=10, xlim=c(-0.3, 0.4), main = "",  sub ="100m",xlab= "Northward water velocity (m/s)")

par(mfrow=c(1,1))
hist(id130539$cellvalue_w100, breaks=10, xlim=c(-0.00004, 0.00004), main = "2013 - Vertical velocity at 100m (id130539)", xlab= "Vertical water velocity (m/s)")
dev.off()


#130546------


id130546<-subset(env_track2013,Deploy.ID=="130546")

#pdf("id130546_env_histograms_tracks.pdf")

mainMap + geom_point(data = id130546, aes(x = lon, y = lat, col="red"), size = 1.2) + labs(title = "2013 Tuna tracks | ID130546 | Direction = out | ")  + theme(legend.position='none')


par(mfrow=c(2,1))

hist(id130546$cellvalue_sal_15, breaks=25, xlim=c(35,37.5), main = "2013 - Salinity in id130546 locations", xlab="Salinity (PSU)", sub="15m",)
hist(id130546$cellvalue_sal_100,  breaks=25, xlim=c(35,37.5), main = "", sub="100m", xlab="Salinity (PSU)" )

par(mfrow=c(1,1))
hist(id130546$cellvalue_temp_15, breaks=25, xlim=c(285, 300), ylim=c(0, 6), main = "2013 -  id130546 Temperature", col="red", xlab="Temperature (ºK)" )
hist(id130546$cellvalue_temp_100, breaks=25,main = "", add=T,col=c1)
legend("topright", c("15m", "100m"), fill=c("red", c1))

par(mfrow=c(2,2))
hist(id130546$cellvalue_u15, breaks=10, xlim=c(-0.3, 0.4), main = "2013 - Water velocity (id130546)", sub ="15m", xlab= "Eastward water velocity (m/s)")
hist(id130546$cellvalue_u100, breaks=10, xlim=c(-0.3, 0.4), main = "", sub ="100m",xlab= "Eastward water velocity (m/s)")

hist(id130546$cellvalue_v15, breaks=10, xlim=c(-0.3, 0.4), main = "",  sub ="15m", xlab= "Northward water velocity (m/s)")
hist(id130546$cellvalue_v100, breaks=10, xlim=c(-0.3, 0.4), main = "",  sub ="100m",xlab= "Northward water velocity (m/s)")

par(mfrow=c(1,1))
hist(id130546$cellvalue_w100, breaks=15, xlim=c(-0.00004, 0.00004), main = "2013 - Vertical velocity at 100m (id130546)", xlab= "Vertical water velocity (m/s)")
dev.off()



#COMPARATION OF ENVIRONMENTAL VARAIBILITY AMONG INDIVIDUALS----

#Classification of routes: North peninsula (NP), North peninsula + north atlantic (NPAN), Canarias and central atlantic (CAC), Azores and central Atlantic (AAC)

# North Iberic peninsula (NP), North peninsula + north atlantic (NPAN) ----
#Tracks: 
id61125<-id61125[, 2:16]
id61127<-id61127[, 2:16]
id97462n<-subset(id97462, lon>-20)

npan<-rbind(id61125, id61127,id72499, id72498, id97462n)

c2 <- crop(worldMap,extent(-20-mapBuffer, -1.075+mapBuffer,35-mapBuffer, 50+mapBuffer))  # based on geographical limits of oceanographical product and blueifn distribution

mainMap3 <- ggplot() + geom_polygon(data = c2, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa general

mapnpan<- mainMap3 + geom_point(data = npan, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 1.2) + labs(title = "")  +geom_vline(xintercept = -6, color = "red",fill=NA, inherit.aes = FALSE) + guides(colour = guide_legend(override.aes = list(size=4))) + theme(legend.title = element_text(face = "bold")) +labs(colour = "ID")


npansal15<-ggplot(npan, aes(x=cellvalue_sal_15, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3) + xlim(35,36.8) + theme_classic()+labs(x = "Salinity (PSU) at 15m", y = "") + theme(plot.title = element_text(face = "bold")) +labs(title = " North Iberic Peninsula and Noth Atlantic tracks (NPAN)")  + theme(legend.text = element_text(size = 8), legend.title = element_text(face = "bold")) +labs(y = NULL, fill = "IDs") + theme(legend.position = "bottom") + theme(legend.key.size = unit(0.5, 'cm')) + theme(axis.title = element_text(size = 9), plot.title = element_text(size = 10))
#+ scale_fill_brewer(palette="Set1")
npansal100<-ggplot(npan, aes(x=cellvalue_sal_100, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3) + xlim(35,36.8) + theme_classic()+labs(x = "Salinity (PSU) at 15m", y = "") + theme(plot.title = element_text(face = "bold")) +labs(title = "")  + theme(legend.text = element_text(size = 8), legend.title = element_text(face = "bold")) +labs(y = NULL, fill = "IDs") + theme(legend.position = "none") +theme(axis.title = element_text(size = 9))

ggarrange(npansal15, npansal100,  ncol = 1, nrow = 2)

#table with dates
mindate<-c(min(id61125$date), min(id61127$date), min(id72498$date), min(id72499$date), min(id97462n$date))

maxdate<-c(max(id61125$date), max(id61127$date), max(id72498$date), max(id72499$date), max(id97462n$date))

nºtracks<-c(length(id61125$date), length(id61127$date), length(id72498$date), length(id72499$date), length(id97462n$date))

ID<-c("61125", "61127", "72498", "72499", "97462")

tabledates<-data.frame(ID, nºtracks, mindate, maxdate)
tab<-ggtexttable(tabledates, rows = NULL)

maptab<-ggarrange( mapnpan, tab,  ncol = 2, nrow = 1)

ggarrange(npansal15, maptab,  ncol = 1, nrow = 2)


#Canarias and central atlantic (CAC)------

#86243, 97466, 120088, 120084, 114009


#join tracks----
id86243<-subset(env_track2011,Deploy.ID=="86243")
ncol(id86243)#15

id97466<-subset(env_track2011,Deploy.ID=="97466")
ncol(id97466)#15


id120088<-subset(env_track2012,Deploy.ID=="120088")
id120088<-id120088[, 1:15]

#id120084<-subset(env_track2012,Deploy.ID=="120084") 
#id120084<-id120084[, 1:15]

#id114009_12<-subset(env_track2012,Deploy.ID=="114009")
#id114009_13<-subset(env_track2013,Deploy.ID=="114009")

#id114009<-rbind(id114009_12,id114009_13)
#id114009<-id114009[, 1:15]
id114009n<-subset(id114009, date<"2012-09-30")
mainMap4 + geom_point(data = id114009n, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 1.2) + labs(title = "") + guides(colour = guide_legend(override.aes = list(size=4))) + theme(legend.title = element_text(face = "bold")) +labs(colour = "ID")


cac<-rbind(id86243, id97466,id120088, id114009n)


#plots CAC----

#map
c3 <- crop(worldMap,extent(-45-mapBuffer, -1.075+mapBuffer,25-mapBuffer, 50+mapBuffer))  # based on geographical limits of oceanographical product and blueifn distribution

mainMap4 <- ggplot() + geom_polygon(data = c3, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa general

mapcac<- mainMap4 + geom_point(data = cac, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 1.2) + labs(title = "") + guides(colour = guide_legend(override.aes = list(size=4))) + theme(legend.title = element_text(face = "bold")) +labs(colour = "ID")

#hist
cacsal15<-ggplot(cac, aes(x=cellvalue_sal_15, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3) + xlim(35.7,37.5) + theme_classic()+labs(x = "Salinity (PSU) at 15m", y = "") + theme(plot.title = element_text(face = "bold")) +labs(title = "Canary Islands (CAC)")  + theme(legend.text = element_text(size = 8), legend.title = element_text(face = "bold")) +labs(y = NULL, fill = "IDs") + theme(legend.position = "bottom") + theme(legend.key.size = unit(0.5, 'cm')) + theme(axis.title = element_text(size = 9), plot.title = element_text(size = 10))
#+ scale_fill_brewer(palette="Set1")
cacsal100<-ggplot(cac, aes(x=cellvalue_sal_100, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3) + xlim(35.7,37.5) + theme_classic()+labs(x = "Salinity (PSU) at 15m", y = "") + theme(plot.title = element_text(face = "bold")) +labs(title = "")  + theme(legend.text = element_text(size = 8), legend.title = element_text(face = "bold")) +labs(y = NULL, fill = "IDs") + theme(legend.position = "none") +theme(axis.title = element_text(size = 9))

ggarrange(cacsal15, cacsal100, mapcac, ncol = 1, nrow = 3)

#table


mindate<-c(min(id86243$date), min(id97466$date), min(id120088$date), min(id114009n$date))

maxdate<-c(max(id86243$date), max(id97466$date), max(id120088$date), max(id114009n$date))

nºtracks<-c(length(id86243$date), length(id97466$date), length(id120088$date), length(id114009n$date))

ID<-c("86243", "97466", "120088", "114009n")

tabledates2<-data.frame(ID, nºtracks, mindate, maxdate)
tab2<-ggtexttable(tabledates2, rows = NULL)

maptab2<-ggarrange( mapcac, tab2,  ncol = 2, nrow = 1)

ggarrange(cacsal15, cacsal100, mapcac, tab2,  ncol = 2, nrow = 2)
