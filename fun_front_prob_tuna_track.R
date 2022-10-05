#Funcion para extraer datos de probabilidad de frente entorno a las localizaciones de de atun rojo

library(rnaturalearth) #to  download world maps 
library(mapdata)
library(maptools)
library(dismo)
library(raster)
library(fields)
library(lubridate)
library(RNetCDF)
library(ggplot2)

worldMap <- ne_download(scale = "medium", returnclass = "sp")



#track data----
tmay<- read.csv("tablas/tracks_May.csv")
table(tmay$year)
tmay11<-subset(tmay, year=="2011")
range(tmay11$date)
tmay11_1<-subset(tmay11, date>"2011-05-15" & date<="2011-05-31")

tjun11_2<-subset(tmay11, date>="2011-06-01" & date<"2011-06-16")
tjun11_3<-subset(tmay11, date>="2011-06-16" & date<="2011-06-31")
tjul11_2<-subset(tmay11, date>="2011-07-01" & date<"2011-07-16")

tmay12<-subset(tmay, year=="2012")




#July----
tracksjuly<-read.csv("tablas/tracks_July.csv")

tracksjuly$year<- year(tracksjuly$date)
table(tracksjuly$Deploy.ID)

range(tracksjuly$date)

#adjust track data to area covered by of front data lat -> 15.0 to 44.5 | lon= -20.0 to 4.5
tracksjuly<-subset(tracksjuly, lat<=44.5 & lat>=15)
tracksjuly<-subset(tracksjuly, lon<=4.5 & lon>=-20)

#selecting year and pair of weeks, corresponding with data of front prob.
tracksjuly12<-subset(tracksjuly, year=="2012")
range(tracksjuly12$date)# "2010-07-17" "2010-08-25"
#july10<-subset(july10, !Deploy.ID=="114006")
july12<-subset(tracksjuly12, date>="2012-07-01" & date<="2012-07-15")
range(july12$date)
table(july12$Deploy.ID)

july12_1<-subset(tracksjuly12, date>"2012-07-15" & date<="2012-07-31")
range(july12_1$date)
table(july12_1$Deploy.ID)


aug12<-subset(tracksjuly12, date>="2012-08-01" & date<"2012-08-16")
range(aug12$date)
table(aug12$Deploy.ID)

aug12_2<-subset(tracksjuly12, date>="2012-08-16" & date<"2012-08-31")
range(aug12_2$date)
table(aug12_2$Deploy.ID)

sept12_1 <-subset(tracksjuly12, date>="2012-09-01" & date<"2012-09-16")
sept12_2 <- subset(tracksjuly12, date>"2012-09-15" & date<="2012-09-31")


#Function to extract front data at tuna location for periods os 15 days-----

##To test function
#2010-07-23 
#latsample=37.15 
#longsample=-9.125

rm(ncpath)
ncpath= "mwoi_50km_15days/mwoi_binned_50km_2012-08-16T00_00_00Z_2012-08-31T00_00_00Z.nc" #select file of the 15 days of interest

getfrontnc=function(latsample,longsample,ncpath) 
{
  rm(nc)
  nc=open.nc(ncpath)
  #print.nc(nc)
  
  lat = var.get.nc( nc, "lat")          # coordinate variable
  lon = var.get.nc( nc, "lon")          # coordinate variable
  #time=var.get.nc( nc, "time")
  proba_front= var.get.nc( nc, "proba_front", start=c(1,1,1), count=c(length(lon),length(lat),1)) 
  proba_front_norm= var.get.nc( nc, "proba_front_normalized", start=c(1,1,1), count=c(length(lon),length(lat),1))
  dir_front <- var.get.nc( nc, "dir_front", start=c(1,1,1), count=c(length(lon),length(lat),1))
  dir_front_std <- var.get.nc( nc, "dir_front_std", start=c(1,1,1), count=c(length(lon),length(lat),1)) 
  
  #Flip matrix
  proba_front2= t(proba_front)[nrow(t(proba_front)):1,] 
  proba_front_norm2= t(proba_front_norm)[nrow(t(proba_front_norm)):1,] 
  dir_front2= t(dir_front)[nrow(t(dir_front)):1,] 
  dir_front_std2= t(dir_front_std)[nrow(t(dir_front_std)):1,] 
  
  #plot map
  rfront<-raster(proba_front2, xmn=min(lon), xmx=max(lon), ymn=min(lat),ymx=max(lat),crs="+proj=longlat +datum=WGS84")
  plot(rfront,col=tim.colors(64),  main="Front probability 2012 | ID114006 | 15-31st th July")
  plot(worldMap,col="gray",add=TRUE) 
  points(july12_1$lon, july12_1$lat, pch = 20, lwd=1)
  
  
  rfront<-raster(proba_front2, xmn=min(lon), xmx=max(lon), ymn=min(lat),ymx=max(lat),crs="+proj=longlat +datum=WGS84")
  plot(rfront,col=tim.colors(64),  main="Front probability 2012 | ID114006 | 01-15th August")
  plot(worldMap,col="gray",add=TRUE) 
  points(aug12$lon, aug12$lat, pch = 20, lwd=1)
  
  
  plot(rfront,col=tim.colors(64),  main="Front probability 2012 | ID114006 | 15-31st August")
  plot(worldMap,col="gray",add=TRUE) 
  points(aug12_2$lon, aug12_2$lat, pch = 20, lwd=1)
  
  
  #extracting  CELLVALUES or values at tuna locations pixels----
  x=lon
  y=lat
  
  difx=abs(x-longsample) 
  dify=abs(y-latsample)
  
  idx <- which(difx == min(difx), arr.ind = TRUE)[1,1];idx
  idy <- which(dify == min(dify), arr.ind = TRUE)[1,1];idy
  
  if(min(difx)>0.5 | min(dify>0.5)) {cellvalue_front_prob=-999; cellvalue_front_prob_norm=-999; cellvalue_front_dir=-999; cellvalue_front_dir_std=-999} else {
    cellvalue_front_prob=proba_front[idx,idy]#####funciona
    cellvalue_front_prob_norm=proba_front_norm[idx,idy]
    cellvalue_front_dir=dir_front[idx,idy]
    cellvalue_front_dir_std=dir_front_std[idx,idy]
    }
  
  
}


#Loop to extract data of front probability from all positions of tuna tracks----



#######make output table with environmental variable

#table to be filled by loop
rm(tout)
tout<-data.frame(
  Deploy.ID=numeric(0),#
  ID=numeric(0), #identificator number/text unic in magic table # Sample
  date=as.Date(character()),
  lat=numeric(0),
  lon=numeric(0),
  direction=character(0),
  
  cellvalue_front_prob=numeric(0),  #VALUE OF THE EN.	 VARAIBLE AT LOCATION/DATE
  cellvalue_front_prob_norm=numeric(0),	
  cellvalue_front_dir=numeric(0),
  cellvalue_front_dir_std=numeric(0))


#i=1 loop test
rm(i)

t2= aug10_2
 
for(i in 1:nrow(t2))  {
  
  sample<-t2[i,]
  yyyymmdd=sample$date
  latsample=sample$lat
  longsample=sample$lon
  yday=sample$yday
  
  rm(proces)
  proces=getfrontnc(latsample,longsample,ncpath)
  #dev.off()
  
  cellvalue_front_prob=proces[1]
  cellvalue_front_prob_norm= proces[2]
  cellvalue_front_dir=proces[3]
  cellvalue_front_dir_std=proces[4] 
  
  #ojo filas,columnas
  tout[i,1]=sample$Deploy.ID
  tout[i,2]=sample$ID
  tout[i,3]=yyyymmdd
  tout[i,4]=sample$lat
  tout[i,5]=sample$lon
  tout[i,6]=sample$direction
  
  tout[i,7]=cellvalue_front_prob
  tout[i,8]=cellvalue_front_prob_norm
  tout[i,9]=cellvalue_front_dir
  tout[i,10]=cellvalue_front_dir_std
  
  
}

#front_tracks_july_1 <- tout
#front_tracks_july_2 <- tout
#front_tracks_july_3 <- tout
front_tracks_july_4 <- tout

t3<-rbind(front_tracks_july_1, front_tracks_july_2, front_tracks_july_3)

range(t3$date)

write.csv(t3, "tablas/2012 front_prob july tracks_ID114006.csv", row.names=FALSE)


frontp10<-read.csv("tablas/2010 front_prob july tracks.csv")
range(frontp10$date)

ftaug10<-tout
ftaug10_2<-tout

fronts2010<-rbind(frontp10, ftaug10, ftaug10_2)

write.csv(fronts2010, "tablas/2010 front_prob july tracks.csv")




## time series----

tjul<-read.csv("tablas/2010 front_prob july tracks.csv")

tjul$date<-as.Date(tjul$date)

tjul$cellvalue_front_prob[tjul$cellvalue_front_prob== "-999"] <- NA

df$Tem[df$Tem == "-9999"] <- "NA"

table(tjul$cellvalue_front_prob)




ggplot(tjul) + geom_smooth(aes(x=yday(date),y=cellvalue_front_prob,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) +   theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Front prob. in tracks starting in July 2010", x = "Yday", y = "Front probability",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 0))+ theme(plot.title = element_text(size = 13, hjust = 0.5, face="bold")) 


#Map settings----
mapBuffer <-0.1
worldMap <- ne_download(scale = "medium", returnclass = "sp")

worldMapCroped <- crop(worldMap,extent(-20-mapBuffer, 5+mapBuffer,25-mapBuffer, 50+mapBuffer))  # based on geographical limits of oceanographical product and blueifn distribution

mainMap <- ggplot() + geom_polygon(data = worldMapCroped, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa general


mainMap + geom_point(data = tjul, aes(x = lon, y = lat,  color = factor(Deploy.ID))) + labs(title = "Tracks starting in July")  + guides(colour = guide_legend(override.aes = list(size=4))) + theme(legend.title = element_text(face = "bold")) +labs(colour = "ID")+theme(legend.position = "right")+labs(shape = "Year")
                      