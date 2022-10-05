#Enrique code. Analysis of depths registered in tuna tracks and enviornmental data


library(mapdata)
library(maptools)
library(rnaturalearth)
library(raster)
library(gdata)
library(ggplot2)


setwd("C:/Theme 2_3D currents and vertical motion for Sustainable Fisheries/woc/tracks atun rojo/")


#Map settings----
mapBuffer <-0.1
worldMap <- ne_download(scale = "medium", returnclass = "sp")

worldMapCroped <- crop(worldMap,extent(-40-mapBuffer, +20+mapBuffer,25-mapBuffer, 65+mapBuffer))  # based on geographical limits of oceanographical product and blueifn distribution

mainMap <- ggplot() + geom_polygon(data = worldMapCroped, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa general



### Colours----

c1 <- rgb(216,238,192, max = 255, alpha = 230, names = "lt.green") #light colours for histogram
c2 <- rgb(255,100,100, max = 255, alpha = 100, names = "lt.red")
c3 <- rgb(0, 100, 255, max=255, alpha = 100, names = "lt.blue")
c4 <- rgb(100, 0, 100, max=255, alpha = 100, names = "lt.purple")
####----

mtlt<-read.csv("Magic table long tracks Bluefin Tuna.csv")
mtlt$Depth<-abs(mtlt$Depth)
mtlt<-subset(mtlt, !Year=="2009")

#pdf("Bluefin tuna depths.pdf")
hist(mtlt$Depth, breaks=100, xlab="Depth (m)", main= "Bluefin tuna depths")
boxplot(mtlt$Depth, main= "Bluefin tuna depths", ylab = "Depth",ylim=c(1500,0))
axis(side=2, at=seq(0,1000, 100), labels=seq(0,1000,100))
boxplot(mtlt$Depth~mtlt$Year, las=2,ylab = "Depth (m)", xlab="Year", ylim=c(1500,0),main = "Bluefin Tuna depths")

par(mar=c(6,4,2,1))
boxplot(mtlt$Depth~mtlt$Deploy.ID, las=2,ylab = "Depth (m)", xlab="", ylim=c(750,0),main = "Bluefin Tuna depths")
#stripchart(mtlt$Depth~mtlt$Deploy.ID, vertical = TRUE, method = "jitter",pch = 21, add = TRUE, col = "red")
#axis(side=2, at=seq(0,1000, 100), labels=seq(0,1000,100))
dev.off()


#2010----
t2010<-subset(mtlt, Year=="2010")

summary(t2010$Depth)


#pdf("2010 tracks and histograms/Depths 2010.pdf")
hist(t2010$Depth, breaks=100, xlab="Depth (m)", main= "Bluefin tuna depths 2010")
boxplot(t2010$Depth, main= "Bluefin tuna depths 2010", ylab = "Depth",ylim=c(750,0))
axis(side=2, at=seq(0,1000, 100), labels=seq(0,1000,100))

boxplot(t2010$Depth~t2010$Deploy.ID, ylab = "Depth", xlab="ID", ylim=c(750,0),main = "Bluefin Tuna depths 2010")
stripchart(t2010$Depth~t2010$Deploy.ID, vertical = TRUE, method = "jitter",
           pch = 21, add = TRUE, col = "red")
axis(side=2, at=seq(0,1000, 100), labels=seq(0,1000,100))
#abline(15,0, lwd = 3, col = c4)
#abline(100,0, lwd = 3, col = c4)

dev.off()

mainMap + geom_point(data = t2010, aes(x = Lon, y = Lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2010") + labs(colour = "Deploy.ID", size = 12) 

#2011-------
t2011<-subset(mtlt, Year=="2011")
t2011$Depth<-abs(t2011$Depth)

summary(t2011$Depth)

#pdf("2011 tracks and histograms/Depths 2011.pdf")

hist(t2011$Depth, breaks=100, xlab="Depth (m)", main= "Bluefin tuna depths 2011")
boxplot(t2011$Depth, main= "Bluefin tuna depths 2011", ylab = "Depth", ylim=c(1700,0))
boxplot(t2011$Depth~t2011$Deploy.ID, ylab = "Depth (m)", xlab="ID", ylim=c(1700,0),main = "Bluefin Tuna depths 2011")
stripchart(t2011$Depth~t2011$Deploy.ID, vertical = TRUE, method = "jitter",
           pch = 21, add = TRUE, col = "red")

dev.off()

#axis(side=2, at=c(15,100))


#
t2012<-subset(mtlt, Year=="2012")

summary(t2012$Depth)

#pdf("2012 tracks and histograms/Depths 2012.pdf")

hist(t2012$Depth, breaks=150, xlab="Depth (m)", main="Bluefin Tuna depths 2012")

boxplot(t2012$Depth, main="Bluefin Tuna depths 2012", ylab="Depth (m)", ylim=c(1300,0))

boxplot(t2012$Depth~t2012$Deploy.ID, ylab = "Depth (m)", ylim=c(1300,0),main = "Bluefin Tuna depths 2012", xlab="ID")
stripchart(t2012$Depth~t2012$Deploy.ID, vertical = TRUE, method = "jitter",
           pch = 21, add = TRUE, col = "red")

dev.off()

#
t2013<-subset(mtlt, Year=="2013")


#pdf("2013 tracks and histograms/Depths 2013.pdf")

summary(t2013$Depth)

hist(t2013$Depth, breaks=150, xlab="Depth (m)", main="Bluefin Tuna depths 2013")
boxplot(t2013$Depth, main="Bluefin Tuna depths 2012", ylab="Depth (m)", ylim=c(1300,0))

boxplot(t2013$Depth~t2013$Deploy.ID, ylab = "Depth (m)", ylim=c(1250,0),main = "Bluefin Tuna depths 2012", xlab="ID")
stripchart(t2013$Depth~t2013$Deploy.ID, vertical = TRUE, method = "jitter",
           pch = 21, add = TRUE, col = "red")
dev.off()
