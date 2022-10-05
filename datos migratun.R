#### Elaboration of Magic Table with data from electronic tagging of Atlantic Bluefintuna ####

library(stringr)
library(reshape2)
library(ggplot2)
library(ggrepel)
library(mapdata)
library(maptools)
library(rnaturalearth)
library(raster)
library(gdata)
#library(scatterpie)
library (openxlsx)
library(dplyr)
library(lubridate)

setwd("C:/Theme 2_3D currents and vertical motion for Sustainable Fisheries/woc/tracks atun rojo/")

#Map settings
mapBuffer <-0.1
worldMap <- ne_download(scale = "medium", returnclass = "sp")
worldMapCroped <- crop(worldMap,extent(-50-mapBuffer, 20+mapBuffer,20-mapBuffer, 60+mapBuffer))  # extended eastwards and northwards to include the Tracks in the mediterranean and Northeastern Atlantic (out of the study area)

#####Data tracks ABASCAL ------------------

AllGPE <- read.xlsx("marcas_migratun.xlsx")
head(AllGPE, n=1)
table(AllGPE$Deployd.ID)

#dates change
AllGPE$Date<-parse_date_time(AllGPE$Date, orders="dmy hms")
AllGPE$yyyymmdd <- as.Date(AllGPE$Date)
AllGPE$Year <- year(AllGPE$yyyymmdd)

tapply(AllGPE$DeployID, paste(AllGPE$DeployID, AllGPE$ddmmyy, sep = ""), length)

#####Data tracks OSPINA -----------------------------------------------------------------

tracks <- read.csv("Tuna Tracks (original Ospina).csv")
tracks <-  tracks[, 2:24]

str(tracks)
summary(tracks)
table(tracks$id) 

table(duplicated(tracks$id)) #many Ids with only one track... better to remove?

tracks2<-subset(tracks, duplicated(tracks$id))
table(tracks2$Year)

id114006full<-subset(tracks2, id=="114006")

mapBuffer <-0.1
worldMap <- ne_download(scale = "medium", returnclass = "sp")

worldMapCroped <- crop(worldMap,extent(-20-mapBuffer, 10+mapBuffer,25-mapBuffer, 50+mapBuffer))  # based on geographical limits of oceanographical product and blueifn distribution

mainMap <- ggplot() + geom_polygon(data = worldMapCroped, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa general


mainMap + geom_point(data = id114006full, aes(x = Long, y = Lat,  color = factor(Month)), size = 1.2) + labs(title = "Track 114006") + guides(colour = guide_legend(override.aes = list(size=4))) + theme(legend.title = element_text(face = "bold"), legend.position = "bottom") +labs(colour = "Month") + theme(legend.position = "right") +labs(shape = "Year")




#### JOINING TWO TRACKS DATA SOURCES ----

# Magic Table: ID | DeployID | Lon | Lat | Date | YYYMMDD | Year | Month | Day | Yday | Hour | Depth | 

AllGPE$Month <- month(AllGPE$Date)
AllGPE$Day <- day(AllGPE$Date)
AllGPE$Hour <- hour(AllGPE$Date)
AllGPE$Minute <- minute(AllGPE$Date)
AllGPE$yday <- yday(AllGPE$Date)

t1 <- AllGPE[,c(1,4,3,15,2,14,16,17,18,19,20,8)]

names(t1) <- c("Deploy.ID", "Lon", "Lat", "Year", "Date", "YYYMMMDD","Month","Day","Hour", "Minute","yday","Depth")


#Setting sample number
tracks$ddmmyy <- as.Date(tracks$df_dates)
tracks$hour <- hour(tracks$df_dates)
tracks$minute <- minute(tracks$df_dates)

t2 <- tracks[,c(1,11,12,3,21,24,4,5,25,26,23,9)]# depth = Max.depth...?
names(t2) <- c("Deploy.ID", "Lon", "Lat", "Year", "Date",  "YYYMMMDD","Month","Day","Hour","Minute","yday","Depth")

t3 <- rbind(t1, t2)

ID <- paste(t3$Deploy.ID,"_",t3$Year,t3$Month,t3$Day,t3$Hour,t3$Minute,sep = "") #trakcs ids based on individual id and date and hour of the track
t3 <-cbind(ID,t3)

write.csv(t3, "All Tuna Tracks.csv")


mt<- subset(t3,Lon > -75.975 & Lon < -6.075) #Central/North Atlantic Ocean (20°N-50°N, 76°W-6°W).
mt<- subset(mt, Lat > 20.025 & Lat < 49.925) #area where there is info from current 3d product


#Adding vectors of direction
#mt<-mt[,1:13]
mt=t1

mt$Dx<-rep(NA, times=nrow(mt))
mt$Dy<-rep(NA, times=nrow(mt))

#Calculating vector directions of individuals
for (i in 1:nrow(mt)) {
  
  i2=i+1
  a1<-mt[i,]
  a2<-mt[i2,]
  
  if ((a1$Deploy.ID==a2$Deploy.ID)==TRUE) { #& (a1$Month<=a2$Month)==TRUE & (a1$Day<=a2$Day)==TRUE & (a1$Hour<a2$Hour)==TRUE)
  
    
    dx=mt$Lon[i2]-mt$Lon[i]
    dy=mt$Lat[i2]-mt$Lat[i]
    
    mt$Dx[i]<-dx
    mt$Dy[i]<-dy
    
  
  }
    
  }
  

#calculating module of vector directions
#formula: |v|=sqtr(a^2+b^2)

mt$module<-sqrt((mt$Dx^2)+(mt$Dy^2))


#Classification of tracks based on directions: towards Mediterranean (in), outwards Mediterranean (out) or Non-defined (nd)----

t3<-read.csv("All Tuna Tracks.csv") #to see full tracks (also out of study area)
#t3 <-read.csv("Magic Table Tuna Tracks.csv") tu subset from mt the long tracks 
mt<-read.csv("Magic Table Tuna Tracks.csv")

table(mt$Deploy.ID)

mt$Direction<-rep("nd", times=nrow(mt))

td <- mt %>% group_by(Deploy.ID) %>% filter (duplicated(Deploy.ID)) #to select only individuals with more than 1 track

table(td$Deploy.ID) #individuals with several tracks to identify direction
##------
#114006
id114006<-subset(mt, Deploy.ID=="114006")
plot(id114006$Lon, id114006$Lat, add=T)
plot(worldMapCroped, add=T)
id114006[1,]
id114006[97,] # = OUT

mt[mt$Deploy.ID=="114006", "Direction"] <-"out"
id114006[, "Direction"] <-"out"

#114008 
id114008<-subset(mt, Deploy.ID=="114008")
plot(id114008$Lon, id114008$Lat, add=T)
plot(worldMapCroped, add=T)
id114008[1,]
id114008[67,] 

mt[mt$Deploy.ID=="114008","Direction"] <-"out"
id114008[, "Direction"] <-"out" #to create table with long tracks

#114009
id114009<-subset(mt, Deploy.ID=="114009")
plot(id114009$Lon, id114009$Lat)
plot(worldMapCroped, add=T)
id114009[1,]
id114009[173,] 

mt[mt$Deploy.ID=="114009","Direction"] <- "out"
id114009[, "Direction"] <-"out"

#118756
id118756<-subset(mt, Deploy.ID=="118756")
plot(id118756$Lon, id118756$Lat)
plot(worldMapCroped, add=T)
id118756[1,]
id118756[161,] 

mt[mt$Deploy.ID=="118756","Direction"] <- "out"
id118756[, "Direction"] <-"out"


#118758
id118758<-subset(mt, Deploy.ID=="118758")
plot(id118758$Lon, id118758$Lat, ylim=c(30,40))
plot(worldMapCroped, add=T)
id118758[1,]
id118758[24,] 

mt[mt$Deploy.ID=="118758","Direction"] <- "out"

#118758B
id118758B<-subset(mt, Deploy.ID=="118758B")
plot(id118758B$Lon, id118758B$Lat, ylim=c(30,40))
plot(worldMapCroped, add=T)
id118758B[1,]
id118758B[17,] 

mt[mt$Deploy.ID=="118758B","Direction"] <- "out"


#118760
id118760<-subset(mt, Deploy.ID=="118760")
plot(id118760$Lon, id118760$Lat)
plot(worldMapCroped, add=T)
id118760[1,]
id118760[55,] 

mt[mt$Deploy.ID=="118760","Direction"] <- "out"
id118760[, "Direction"] <-"out"

#120084
id120084<-subset(mt, Deploy.ID=="120084")
plot(id120084$Lon, id120084$Lat,  ylim=c(30,40))
plot(worldMapCroped, add=T)
id120084[1,]
id120084[37,] 

mt[mt$Deploy.ID=="120084","Direction"] <- "out"
id120084[, "Direction"] <-"out"

#120088
id120088<-subset(mt, Deploy.ID=="120088")
plot(id120088$Lon, id120088$Lat,  ylim=c(20,40))
plot(worldMapCroped, add=T)
id120088[1,]
id120088[31,] 

mt[mt$Deploy.ID=="120088","Direction"] <- "out"
id120088[, "Direction"] <-"out"

#120446
id120446<-subset(mt, Deploy.ID=="120446")
plot(id120446$Lon, id120446$Lat)
plot(worldMapCroped, add=T)
id120446[1,]
id120446[45,] 

mt[mt$Deploy.ID=="120446","Direction"] <- "out"
id120446[, "Direction"] <-"out"

#130539 
table(mt$Deploy.ID)

id130539 <-subset(mt, Deploy.ID=="130539")
plot(id130539$Lon, id130539$Lat)
plot(worldMapCroped, add=T)
id130539[1,]
id130539[28,] 

mt[mt$Deploy.ID=="130539","Direction"] <- "out"
id130539[, "Direction"] <-"out"

#130543
table(mt$Deploy.ID)

id130543 <-subset(mt, Deploy.ID=="130543")
plot(id130543$Lon, id130543$Lat)
plot(worldMapCroped, add=T)
id130543[1,]
id130543[12,]
id130543[24,] 

mt[mt$Deploy.ID=="130543","Direction"] <- "in"

#130545
table(td$Deploy.ID)

id130545 <-subset(mt, Deploy.ID=="130545")
plot(id130545$Lon, id130545$Lat, ylim=c(20,40), xlim=c(-15,0))
plot(worldMapCroped, add=T)
id130545[1,]
id130545[4,] #direction = nd

#130546
table(td$Deploy.ID)

id130546 <-subset(mt, Deploy.ID=="130546")
plot(id130546$Lon, id130546$Lat, ylim=c(20,60), xlim=c(-20,-6))
plot(worldMapCroped, add=T)
id130546[1,]
id130546[52,]

mt[mt$Deploy.ID=="130546","Direction"] <- "out"
id130546[, "Direction"] <-"out"

#130548
table(td$Deploy.ID)

id130548 <-subset(mt, Deploy.ID=="130548")
plot(id130548$Lon, id130548$Lat,  ylim=c(34,38), xlim=c(-10,-3))
plot(worldMapCroped, add=T)
id130548[1,]
id130548[5,]

mt[mt$Deploy.ID=="130548","Direction"] <- "in"

#130552
table(td$Deploy.ID)

id130552 <-subset(mt, Deploy.ID=="130552")
plot(id130552$Lon, id130552$Lat)
plot(worldMapCroped, add=T)
id130552[1,]
id130552[5,]

mt[mt$Deploy.ID=="130552","Direction"] <- "in"

#61125
table(td$Deploy.ID)

id61125 <-subset(mt, Deploy.ID=="61125")
plot(id61125$Lon, id61125$Lat)
plot(worldMapCroped, add=T)
id61125[1,]
id61125[317,]

mt[mt$Deploy.ID=="61125","Direction"] <- "out"
id61125[, "Direction"] <-"out"

#61127
table(td$Deploy.ID)

id61127 <-subset(mt, Deploy.ID=="61127")
plot(id61127$Lon, id61127$Lat)
plot(worldMapCroped, add=T)
id61127[1,]
id61127[389,]

mt[mt$Deploy.ID=="61127","Direction"] <- "out"
id61127[, "Direction"] <-"out"

#72498
table(td$Deploy.ID)

id72498 <-subset(mt, Deploy.ID=="72498")
plot(id72498$Lon, id72498$Lat)
plot(worldMapCroped, add=T)
id72498[1,]
id72498[288,]

mt[mt$Deploy.ID=="72498","Direction"] <- "out"
id72498[, "Direction"] <-"out"

#72499
table(td$Deploy.ID)

id72499 <-subset(mt, Deploy.ID=="72499")
plot(id72499$Lon, id72499$Lat)
plot(worldMapCroped, add=T)
id72499[1,]
id72499[579,]

mt[mt$Deploy.ID=="72499","Direction"] <- "out"
id72499[, "Direction"] <-"out"

#86238 
table(td$Deploy.ID)

id86238 <-subset(mt, Deploy.ID=="86238")
table(id86238$Year)#tracks from different years 2009 and 2012

id862382009<-subset(id86238, Year=="2009")
plot(id862382009$Lon, id862382009$Lat)
plot(worldMapCroped, add=T) #out of study atrea

id862382012<-subset(id86238, Year=="2012")
plot(id862382012$Lon, id862382012$Lat)
plot(worldMapCroped, add=T) 
id862382012[1,]
id862382012[4,]

mt[mt$Deploy.ID=="86238","Direction"] <- "out"

#86243
table(td$Deploy.ID)

id86243 <-subset(mt, Deploy.ID=="86243")
plot(id86243$Lon, id86243$Lat)
plot(worldMapCroped, add=T)
id86243[1,]
id86243[33,]

mt[mt$Deploy.ID=="86243","Direction"] <- "out"
id86243[, "Direction"] <-"out"

#86432
table(td$Deploy.ID)

id86432<-subset(mt, Deploy.ID=="86432")
plot(id86432$Lon, id86432$Lat)
plot(worldMapCroped, add=T)
id86432[1,]
id86432[463,]

mt[mt$Deploy.ID=="86432","Direction"] <- "out"
id86432[, "Direction"] <-"out"

#86437B
table(td$Deploy.ID)

id86437B <-subset(mt, Deploy.ID=="86437B")
plot(id86437B$Lon, id86437B$Lat)
plot(worldMapCroped, add=T)
id86437B[1,]
id86437B[190,]

mt[mt$Deploy.ID=="86437B","Direction"] <- "out"

#86438
table(td$Deploy.ID)

id86438 <-subset(mt, Deploy.ID=="86438")
plot(id86438$Lon, id86438$Lat)
plot(worldMapCroped, add=T)
id86438[1,]
id86438[340,]

mt[mt$Deploy.ID=="86438","Direction"] <- "out"
id86438[, "Direction"] <-"out"

#86440
table(td$Deploy.ID)

id86440 <-subset(mt, Deploy.ID=="86440")
plot(id86440$Lon, id86440$Lat)
plot(worldMapCroped, add=T)
id86440[1,]
id86440[375,]

mt[mt$Deploy.ID=="86440","Direction"] <- "out"
id86440[, "Direction"] <-"out"

#97462
table(td$Deploy.ID)

id97462 <-subset(mt, Deploy.ID=="97462")
plot(id97462$Lon, id97462$Lat)
plot(worldMapCroped, add=T)
id97462[1,]
id97462[106,]

mt[mt$Deploy.ID=="97462","Direction"] <- "out"
id97462[, "Direction"] <-"out"

#97466
table(td$Deploy.ID)

id97466 <-subset(mt, Deploy.ID=="97466")
plot(id97466$Lon, id97466$Lat)
plot(worldMapCroped, add=T)
id97466[1,]
id97466[39,]

mt[mt$Deploy.ID=="97466","Direction"] <- "nd" #track starts and ends in Atlantic 
id97466[, "Direction"] <-"out"

#
write.csv(mt, "Magic Table Tuna Tracks.csv",row.names = F)

#selecting long tracks--------
longtracks<-rbind(id114006,id114008,id114009,id118756,id118760,id120084,id120088,id120446,id130539,id130546,id61125,id61127,id72498, id72499,id86243,id86432,id86438,id86440,id97462,id97466)



#longtracks<-longtracks[,2:14]

#write.csv(longtracks,"Long tracks Bluefin Tuna.csv",row.names = F) 
write.csv(longtracks,"Magic table long tracks Bluefin Tuna.csv",row.names = F) 

mtlt<-read.csv("Magic table long tracks Bluefin Tuna.csv")
aaa<-read.csv("Magic Table Tuna Tracks.csv")
#mt <- read.csv("Magic Table Tuna Tracks.csv")
#mt<-mt[,2:16]

####Here ends the construction of the Magic Table--------


##extracting dates of tracks-----
tdates <- mt$Date
tdates <- unique(tdates)
tdates<-as.Date(tdates)
tdates <- data.frame(tdates, year(tdates))
tdates$month <- month(tdates$tdates)
tdates$day <- day(tdates$tdates)
names(tdates) <- c("Date","Year", "Month", "Day" )
tdates$yday <- yday(tdates$Date)
write.csv(tdates, "Dates of Tuna tracks.csv")

dates2010 <- subset(tdates, Year== "2010")
dates2011 <- subset(tdates, Year== "2011")
#
range(dates2010$yday) 
range(dates2011$yday) 


#checking number of tracks and individuals per year
mt2008 <- subset(mt, Year=="2008") #probably neligible
table(mt2008$ID)

mt2009 <- subset(mt, Year=="2009") 
table(mt2009$ID)

mt2010 <- subset(mt, Year=="2010") 
table(mt2010$ID)

mt2011 <- subset(mt, Year=="2011")
table(mt2011$ID)

mt2012 <- subset(mt, Year=="2012")
table(mt2012$ID)

mt2013 <- subset(mt, Year=="2013")
table(mt2013$ID)

#### MAP WITH ALL TRACKS ####

worldMapCroped <- crop(worldMap,extent(-50-mapBuffer, 20+mapBuffer,20-mapBuffer, 60+mapBuffer))  # extended eastwards and northwards to include the Tracks in the mediterranean and Northeastern Atlantic (out of the study area)

mainMap <- ggplot() + geom_polygon(data = worldMapCroped, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa general

mainMap + geom_point(data = mt, aes(x = Lon, y = Lat,  color = factor(ID)), size = 0.8) + labs(title = "Tuna tracks") + labs(colour = "id") + theme(legend.position='none')


#year maps

#pdf(file= "Yearly Tuna tracks.pdf" )

mainMap + geom_point(data = mt2008, aes(x = Lon, y = Lat,  color = factor(ID)), size = 0.8) + labs(title = "Tuna tracks 2008") + labs(colour = "id") + theme(legend.position='none')

mainMap + geom_point(data = mt2009, aes(x = Lon, y = Lat,  color = factor(ID)), size = 0.8) + labs(title = "Tuna tracks 2009") + labs(colour = "id") + theme(legend.position='none')

mainMap + geom_point(data = mt2010, aes(x = Lon, y = Lat,  color = factor(ID)), size = 0.8) + labs(title = "Tuna tracks 2010") + labs(colour = "id") + theme(legend.position='none')

mainMap + geom_point(data = mt2011, aes(x = Lon, y = Lat,  color = factor(ID)), size = 0.8) + labs(title = "Tuna tracks 2011") + labs(colour = "id") + theme(legend.position='none')

mainMap + geom_point(data = mt2012, aes(x = Lon, y = Lat,  color = factor(ID)), size = 0.8) + labs(title = "Tuna tracks 2012") + labs(colour = "id") + theme(legend.position='none')

mainMap + geom_point(data = mt2013, aes(x = Lon, y = Lat,  color = factor(ID)), size = 0.8) + labs(title = "Tuna tracks 2013") + labs(colour = "id") + theme(legend.position='none')

#dev.off()
   


#Map with all GPE tracks----

#creating the map



worldMapCroped <- crop(worldMap,extent(min(AllGPE$Most.Likely.Longitude)-mapBuffer,max(AllGPE$Most.Likely.Longitude)+mapBuffer,min(AllGPE$Most.Likely.Latitude)-mapBuffer,max(AllGPE$Most.Likely.Latitude)+mapBuffer)) 

mainMap <- ggplot() + geom_polygon(data = worldMapCroped, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa general


GPEtracksmap <- mainMap + geom_point(data = AllGPE, aes(x = Most.Likely.Longitude, y = Most.Likely.Latitude,  color = factor(AllGPE$Deployd.ID)), size = 0.1) + labs(title = "Tuna GPE tracks") + labs(colour = "Id")

#Maps per year (2008 - 2011)

#2008

gpe8<- subset(AllGPE, Year== "2008")
mainMap + geom_point(data = gpe8, aes(x = gpe8$Most.Likely.Longitude, y = gpe8$Most.Likely.Latitude,  color = factor(gpe8$Deployd.ID)), size = 0.1) + labs(title = "Tuna tracks 2008") + labs(colour = "Id") 



#only one individual, our of study area

#2009

gpe9<- subset(AllGPE, Year== "2009")
mainMap + geom_point(data = gpe9, aes(x = gpe9$Most.Likely.Longitude, y = gpe9$Most.Likely.Latitude,  color = factor(gpe9$Deployd.ID)), size = 0.1) + labs(title = "Tuna tracks 2009") + labs(colour = "Id") 

#2010

gpe10<- subset(AllGPE, Year== "2010")
mainMap + geom_point(data = gpe10, aes(x = Most.Likely.Longitude, y = Most.Likely.Latitude,  color = factor(Deployd.ID)), size = 0.1) + labs(title = "Tuna tracks 2010") + labs(colour = "Id") 

mainMap + geom_point(data = g10, aes(x = Most.Likely.Longitude, y = Most.Likely.Latitude,  color = factor(Deployd.ID)), size = 0.1) + labs(title = "Tuna tracks 2010") + labs(colour = "Id")

#2011

gpe11<- subset(AllGPE, Year== "2011")
mainMap + geom_point(data = gpe11, aes(x = Most.Likely.Longitude, y = Most.Likely.Latitude,  color = factor(Deployd.ID)), size = 0.1) + labs(title = "Tuna tracks 2011") + labs(colour = "Id") 


# MAPS Ospina ------

#study area = lat 20 50, long -80 -5 --> area covered by oceanographic products from WOC theme 2 

worldMapCroped <- crop(worldMap,extent(-80-mapBuffer, 20+mapBuffer,20-mapBuffer,+60+mapBuffer))  # extended eastwards and northwards to include the Tracks in the mediterranean and Northeastern Atlantic (out of the study area)

mainMap <- ggplot() + geom_polygon(data = worldMapCroped, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa general

#2008 ---

tracks8 <- subset(tracks2, Year == "2008")

tmap8 <- mainMap + geom_point(data = tracks8, aes(x = tracks8$Long, y = Lat,  color = factor(tracks8$id)), size = 0.1) + labs(title = "") + labs(colour = "id") 

#NO TRACKS IN THE STUDY AREA FOR 2008

#2009 ---

tracks9 <- subset(tracks2, Year == "2009")

tmap9 <- mainMap + geom_point(data = tracks9, aes(x = tracks9$Long, y = Lat,  color = factor(tracks9$id)), size = 0.1) + labs(title = "") + labs(colour = "id") 

#NO TRACKS IN THE STUDY AREA FOR 2009

#2010 ---

tracks10 <- subset(tracks2, Year == "2010")

tmap10 <- mainMap + geom_point(data = tracks10, aes(x = tracks10$Long, y = Lat,  color = factor(tracks10$id)), size = 0.1) + labs(title = "") + labs(colour = "id") 

#NO TRACKS IN THE STUDY AREA FOR 2010

#2011 ---

tracks11 <- subset(tracks2, Year == "2011")

tmap11 <- mainMap + geom_point(data = tracks11, aes(x = Long, y = Lat,  color = factor(id)), size = 1.2) + labs(title = "Tuna tracks 2011") + labs(colour = "id") 

# 2012 ---

tracks12 <- subset(tracks2, Year == "2012")

tmap12 <- mainMap + geom_point(data = tracks12, aes(x = Long, y = Lat,  color = factor(id)), size = 0.8) + labs(title = "Tuna tracks 2012") + labs(colour = "id") 


# 2013 ---

tracks13 <- subset(tracks2, Year == "2013")

tmap13 <- mainMap + geom_point(data = tracks13, aes(x = Long, y = Lat,  color = factor(id)), size = 0.8) + labs(title = "Tuna tracks 2013") + labs(colour = "id") 

