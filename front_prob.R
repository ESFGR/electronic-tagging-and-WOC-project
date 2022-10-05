#Enrique Sánchez-Fabrés
#WOC project, environmental variability at tuna migratory routes. 
#Analisys of front probability at tuna track´s locations


library(rnaturalearth) #to  download world maps 
library(mapdata)
library(maptools)
library(dismo)
library(raster)
library(fields)
library(lubridate)
library(RNetCDF)

#map
worldMap <- ne_download(scale = "medium", returnclass = "sp")
worldMapc <-crop(worldMap, e)


#setwd("C:/Users/Usuario/Desktop/WOC theme 2 Bluefin tuna/") #no hace falta si se abre dsde archivo  de la carpeta r.data 

rm(list=ls(all=TRUE))

#point to where you nc files are located
ncpath="mwoi_50km_15days/"

#read the nc
myncname="mwoi_binned_50km_2010-07-16T00_00_00Z_2010-07-31T00_00_00Z.nc" #nc correspondiente al fechas 16-31 Julio 2010

rm(nc)
nc=open.nc(paste(ncpath,myncname,sep=""), readunlimi=FALSE)
print.nc(nc)

lat = var.get.nc( nc, "lat")          # coordinate variable
lon = var.get.nc( nc, "lon")          # coordinate variable
time=var.get.nc( nc, "time")
proba_front= var.get.nc( nc, "proba_front", start=c(1,1,1), count=c(length(lon),length(lat),1))  #50 es el numero maximo que valores de las dimensiones lat y lon. time solo tiene 1


rm(proba_front2)
proba_front2= t(proba_front)[nrow(t(proba_front)):1,] #flip matrix


#temp = var.get.nc(nc, "to", start=c(1,1,15,1), count=c(length(lon),length(lat),1,1),unpack=TRUE) #Sea water temeperature

rfront<-raster(proba_front2, xmn=min(lon), xmx=max(lon), ymn=min(lat),ymx=max(lat),crs="+proj=longlat +datum=WGS84")
plot(rfront,col=tim.colors(64),  main="Front probability 2010 16-31 July")
plot(worldMap,col="gray",add=TRUE) 


table(proba_front)

range(lat)
range(lon)
summary(lat)
summary(lon)

#extracting  CELLVALUES or values at tuna locations pixels----
x=lon
y=lat

longsample= 39.593894
latsample= -13.215945

difx=abs(x-longsample) 
dify=abs(y-latsample)

idx <- which(difx == min(difx), arr.ind = TRUE)[1,1];idx
idy <- which(dify == min(dify), arr.ind = TRUE)[1,1];idy

proba_front_test<- proba_front[idx, idy]

plot(proba_front)


#add tracks of bluefin tuna

tenv<-read.csv("C:/Theme 2_3D currents and vertical motion for Sustainable Fisheries/woc/tracks atun rojo/Magic_table_env_var.csv") #table with all tracks and evironmental data

tracksjuly<-read.csv("C:/Theme 2_3D currents and vertical motion for Sustainable Fisheries/woc/tracks atun rojo/tracks_July.csv")

tracksjuly$year<- year(tracksjuly$date)

range(tracksjuly$date)


july10<-subset(tracksjuly, year=="2010")

points(july10$lon, july10$lat, pch = 20, lwd=1, add=T)
?points

july10[10,]
