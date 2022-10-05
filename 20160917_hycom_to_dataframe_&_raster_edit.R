# code by Diego Alvarez-Berastegui
# 2016/09/08
#Edit by Enrique Sánchez-Fabrés 
# V2: netdcf files are readen by the package "RNetCDF" R (= 2.5.0)
# Acknowledge use of this code to:
# the Bluefin tuna project (SOCIB/IEO) and the OOSTOP task group from CLIOTOP

library(rnaturalearth) #to  download world maps 
library(mapdata)
library(maptools)
library(dismo)
library(raster)
library(fields)

library(RNetCDF)


rm(list=ls(all=TRUE))

#point to where you hycom nc file is located
ncpath="C:/Theme 2_3D currents and vertical motion for Sustainable Fisheries/woc/theme2/ocean_currents/woc-l4-cur-natl3d_rep-1d/v2.0/2010/191/"


#point to a shape file (polygon) with a world map
#country <- readShapePoly("D:/AAD_2/sig_Med/ESRIDATA/WORLD/country.shp",proj4string=(CRS("+proj=longlat +datum=WGS84")))


#map settings
#download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_coastline.zip", destfile = 'coastlines.zip')
#unzip(zipfile = "coastlines.zip", exdir = 'ne-coastlines-10m')
#coastline <- readOGR("ne-coastlines-10m/ne_10m_coastline.shp")

e <- extent(-40, -6.075, 20, 50) #study area based on extension from product data and bluefin tuna distribution
ccoast<-crop(coastline, e)


m <- map(,fill=T)

raster(m)

coastlinelamb=spTransform(ccoast,"+proj=laea +lon_0=-31")##3 working ok at leifs computer
#plot(coastlinelamb)

worldMap <- ne_download(scale = "medium", returnclass = "sp")
worldMapc <-crop(worldMap, e)

#read the nc
myncname="20100710120000_WOC-L4-CUR-NATL3D_REP-1D-v1.0-fv2.0.nc" #nc correspondiente al 10/07/2010

rm(nc)
nc=open.nc(paste(ncpath,myncname,sep=""), readunlimi=FALSE)
print.nc(nc)
depth = var.get.nc( nc, "depth")
lat = var.get.nc( nc, "lat")          # coordinate variable
lon = var.get.nc( nc, "lon")          # coordinate variable
time=var.get.nc( nc, "time")
sal= var.get.nc( nc,"so",start=c(1,1,15,1), count=c(length(lon),length(lat),1,1),unpack=TRUE) # read salinity at that date
evel = var.get.nc( nc,"uo",start=c(1,1,15,1), count=c(length(lon),length(lat),1,1),unpack=TRUE) #eastward velocity"
nvel = var.get.nc( nc,"vo",start=c(1,1,15,1), count=c(length(lon),length(lat),1,1),unpack=TRUE) #northward velocity
temp = var.get.nc(nc, "to", start=c(1,1,15,1), count=c(length(lon),length(lat),1,1),unpack=TRUE) #Sea water temeperature


summary(lat)
summary(lon)
summary(sal) #values = 0, must be sbstitued by NAs (later)
sal[sal==0] <- NA #para eliminar valores de 0 en el reaster


#flip left the data matrix... a classic problem 
rm(sal2)
sal2= t(sal)[nrow(t(sal)):1,]
rm(sal)

rm(temp2)
temp2= t(temp)[nrow(t(temp)):1,]
rm(temp)

rm(evel2)
evel2= t(evel)[nrow(t(evel)):1,]
rm(nvel2)

nvel2= t(nvel)[nrow(t(nvel)):1,]
rm(nvel)

# now make the raster for the matrix.
rsal <- raster(sal2, xmn=min(lon), xmx=max(lon), ymn=min(lat),ymx=max(lat),crs="+proj=longlat +datum=WGS84")
rtemp <- raster(temp2, xmn=min(lon), xmx=max(lon), ymn=min(lat),ymx=max(lat),crs="+proj=longlat +datum=WGS84")

plot(rsal,col=tim.colors(64),  main="Salinity 10/07/2010")
plot(worldMap,col="gray",add=TRUE)


plot(rtemp,col=tim.colors(64),  main="")

# now it is much easier to crop for plotting and processig specific areas
# palette can be improved by filtering low values

xmax=-6.075
xmin=-40
ymax=50
ymin=25

e <- extent(xmin, xmax,ymin,ymax)
#e <- extent(-40, -6.075, 25, 50)
rsalc <- crop(rsal, e)# ojo aqui transformacion a logaritmo para ver mejor paleta

hist(rsalc)
#rsalc[rsalc==0] <- NA #para eliminar valores de 0 en el reaster
plot(rsalc,col=tim.colors(64))
plot(worldMap,col="gray",add=TRUE)
#plot(coastline,col="gray",add=TRUE)

rtempc<-crop(rtemp, e)
rtempc[rtempc==273.15]<- NA
rtempc <- subs(rtempc, subsWithNA=T)
###

plot(rtempc,col=rev(rainbow(150, end = 0.9)))


masktempr <- mask(rtempc, rtempc, inverse=FALSE, 
     maskvalue=273.15, updatevalue=NA, updateNA=FALSE)



?mask# there are many ways to comoute the gradients... here is one (not the one I used on ly paper), but maybe more appropriated

# just convinient for my
h=rsalc
h=rtempc

# get the resolution of images in x and y directions in degrees
dxgrad=xres(h)
dygrad=yres(h)



#dx y dy in meters (transforming from degrees to meters)...
# important... this formula is ok only for areas near the 40 degrees latitude
# see comments on http://stackoverflow.com/questions/4102520/how-to-transform-a-distance-from-degrees-to-metres

dy=dygrad*60*1853 # 
dx=dxgrad*60*1853*cos(40*2*pi/360)


#get values at cells for computation of the gradients
p2 <- focal(h, w=matrix(c(0,0,0,1,0,0,0,0,0),nrow=3),fun=sum, pad=FALSE, padValue=NA)
p8 <- focal(h, w=matrix(c(0,0,0,0,0,1,0,0,0),nrow=3),fun=sum, pad=FALSE, padValue=NA)

p4 <- focal(h, w=matrix(c(0,1,0,0,0,0,0,0,0),nrow=3),fun=sum, pad=FALSE, padValue=NA)
p6 <- focal(h, w=matrix(c(0,0,0,0,0,0,0,1,0),nrow=3),fun=sum, pad=FALSE, padValue=NA)


dzdx=(p6-p4)/(2*dx)
dzdy=(p2-p8)/(2*dy)
#gradient aproxiimated by 4 cells (also 8cells are possible)
grad4c=sqrt((dzdx^2)+(dzdy^2))


plot(grad4c, main= "Salinty gradient 10/07/2010")
plot(grad4c, main= "Tempperature gradient 10/07/2010")
plot(worldMap,col="gray",add=TRUE)


# now we can reduce the "spatial deffinition" of the gradient calculatoin by smooting the result..
#this is tricky as smoothing is not a real modification of the spatila resolution.


#here we deduce by a 6x6 matryx window
smooth1 <- focal(grad4c, w=matrix(1, 5, 5), mean)
plot(smooth1,useRaster=TRUE, axes=TRUE, main="Salinity gradients 10/07/2010",legend=TRUE, las=1)
plot(worldMap,col="black",add=TRUE)




















































