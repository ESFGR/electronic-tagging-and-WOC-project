#Extrac values from nc at given coordinates
#Enrique Sánchez-Fabrés
# 
# date must be as yyyymmdd
# getsstvalnc("2003-01-01",39.6,2.8,0.15)
#
##Required libraries
# library(RNetCDF)  # For netcdf
# library(maptools) # For shapefiles
# library(fields)   # For shapefiles
# library(rgdal)    # For shapefiles
library(lubridate)

#PARAMETROS PARA test

#mydate="2011-08-08"  ;  latsample=40.018 ;  longsample=-9.884 


#ncpath="C:/Theme 2_3D currents and vertical motion for Sustainable Fisheries/woc/theme2/ocean_currents/woc-l4-cur-natl3d_rep-1d/v2.0/2011/"


file.exists('C:/Theme 2_3D currents and vertical motion for Sustainable Fisheries/woc/Códigos/function formula density.R') #####

#function to calculate density
source('C:/Theme 2_3D currents and vertical motion for Sustainable Fisheries/woc/Códigos/function formula density.R')



getvarsvalnc=function(mydate,latsample,longsample,ncpath) 
  {
  #library(ncdf)
  library(RNetCDF)
  #.............................................
  
  #read sampling dates
  sampledate=as.Date(mydate)#
  year=format(sampledate, "%Y") ; year
  month=format(sampledate, "%m") ; month
  day=format(sampledate, "%d") ; day
  yday= yday(sampledate) ; yday 
  yday<-stringr::str_pad(yday, 3, pad = "0")
  
  
  # from the date to number in R 
  Rdate=as.double(sampledate)
  
  # convert from number to date since 1981-01-01 00:00:00
  # reference time used by hycom
  matdatecero=as.double(as.Date("1981-01-01")) # Create the zero day in R
  
  
  #Determining the sampling date from the matlab zero date
  
  datenum= 24*((Rdate-matdatecero)) #time in hours since 2000-01-01 00:00:00
  
  # open the nc file 
  
  #name of the nc file with the data:
  namenc=paste(yday,"/",year,month,day,"120000_WOC-L4-CUR-NATL3D_REP-1D-v1.0-fv2.0.nc",sep="")
  
  rm(nc)
  nc=open.nc(paste(ncpath,namenc,sep=""), readunlimi=FALSE)
  #print.nc(nc)
  lat = var.get.nc( nc, "lat")     # coordinate variable
  lon = var.get.nc( nc, "lon")
  date= var.get.nc( nc, "time")
  depth = var.get.nc(nc, "depth")
  
  mydate=which(date==datenum,arr.ind=TRUE) #Make sure we can find the date we want in the netcdf file
  #variables at 16.25 (7th depth level in nc file)
  
  # read salinity at that date
  sal15= var.get.nc( nc,"so",start=c(1,1,7,mydate), count=c(length(lon),length(lat),1,1),unpack=TRUE)
  #eastward velocity"
  u15= var.get.nc( nc,"uo",start=c(1,1,7,mydate), count=c(length(lon),length(lat),1,1),unpack=TRUE)
  #northward velocity
  v15 = var.get.nc( nc,"vo",start=c(1,1,7,mydate), count=c(length(lon),length(lat),1,1),unpack=TRUE)
  #Sea water temeperature
  temp.015 = var.get.nc(nc, "to", start=c(1,1,7,mydate), count=c(length(lon),length(lat),1,1),unpack=TRUE)
  
 
  #variables at 100.25m (19th depth level in nc file)
  sal100= var.get.nc( nc,"so",start=c(1,1,19,mydate), count=c(length(lon),length(lat),1,1),unpack=TRUE) 
  u100= var.get.nc( nc,"uo",start=c(1,1,19,mydate), count=c(length(lon),length(lat),1,1),unpack=TRUE)  
  v100 = var.get.nc( nc,"vo",start=c(1,1,19,mydate), count=c(length(lon),length(lat),1,1),unpack=TRUE) 
  w100 =var.get.nc( nc,"wo",start=c(1,1,19,mydate), count=c(length(lon),length(lat),1,1),unpack=TRUE)#vertical velocity
  temp.b = var.get.nc(nc, "to", start=c(1,1,19,mydate), count=c(length(lon),length(lat),1,1),unpack=TRUE) 
  
  
  #filtering fillvalues 
  sal15[sal15==0] <- NA 
  sal100[sal100==0] <- NA
 
  temp15=temp.015
  temp15[temp15== "273.149993896484"] <- NA
 
  temp100=temp.b
  temp100[temp100== "273.149993896484"] <- NA

  #rsal <- raster(sal15, xmn=min(lon), xmx=max(lon), ymn=min(lat),ymx=max(lat),crs="+proj=longlat +datum=WGS84")
  
  #plot(rsal)
  #points(c(latsample, longsample))
  #extract(rsal)
  
  #flip left the data matrix... a classic problem ----
  #rm(sal2)
  #sal15= t(sal15)[nrow(t(sal15)):1,]
  #sal100= t(sal100)[nrow(t(sal100)):1,]
  
  #dim(sal15)
  
  #rm(sal)
  
  #rm(temp2)
  #temp15= t(temp15)[nrow(t(temp15)):1,]
  #temp100= t(temp100)[nrow(t(temp100)):1,]
  #rm(temp)
  
  #rm(evel2)
  #evel2= t(evel)[nrow(t(evel)):1,]
  #rm(nvel2)
  
  #nvel2= t(nvel)[nrow(t(nvel)):1,]
  #rm(nvel)
 #histograms
  #hist()
  # identifica la fila y coulmna correspondiente a la poscion geo deseada 
 #longsample=-15
 #latsample= 35
  
  #calculating density----
  
  P= 10.13 #pressure at sea surface in decibars
  #P=2.516 #assuming a densisty of 1022kg/m3
  
  sal0<-var.get.nc( nc,"so",start=c(1,1,1,mydate), count=c(length(lon),length(lat),1,1),unpack=TRUE) 
  temp0<-var.get.nc( nc,"to",start=c(1,1,1,mydate), count=c(length(lon),length(lat),1,1),unpack=TRUE) 
  
  den0<-densatp(sal0, temp0, P)
 
  
  #extracting  CELLVALUES or values at tuna locations pixels----
  x=lon
  y=lat
  
  difx=abs(x-longsample) 
  dify=abs(y-latsample)
    
  idx <- which(difx == min(difx), arr.ind = TRUE)[1,1];idx
  idy <- which(dify == min(dify), arr.ind = TRUE)[1,1];idy

     
    if(min(difx)>0.5 | min(dify>0.5)) {cellvalue_sal_15=-999; cellvalue_sal_100=-999; cellvalue_temp_15=-999; cellvalue_temp_100=-999; cellvalue_u15=-999;   cellvalue_u100=-999; cellvalue_v15= -999; cellvalue_v100 =-999; cellvalue_w100= -999} else {
    cellvalue_sal_15=sal15[idx,idy]#####funciona
    cellvalue_sal_100=sal100[idx,idy]
    cellvalue_temp_15=temp15[idx,idy]
    cellvalue_temp_100=temp100[idx,idy]
    cellvalue_u15=u15[idx,idy]
    cellvalue_u100=u100[idx,idy]
    cellvalue_v15=v15[idx,idy]
    cellvalue_v100=v100[idx,idy]
    cellvalue_w100=w100[idx,idy]
    cellvalue_den0= den0[idx,idy]
    rm(nc) 
    } # fin else

  
  #test to confirm position and values from cellvalue and sourranding cells/pixels
  #which(difx == min(difx)) #201
  #which(dify == min(dify)) #662
  #delta=10#pixels of distance
  #lon[662];lat[201] #coordinates of cellvalue --> -9.985, 40.025  
  #lon[662-delta];lon[662+delta] #-9.975, -9.775 
  #lat[200];lat[202] #39.925, 40.125
  
  #salf= t(sal15)[nrow(t(sal15)):1,] #flip to left
  #rm(rsal)
  #rsal<-raster::raster(salf,xmn=min(lon), xmx=max(lon), ymn=min(lat),ymx=max(lat), crs="+proj=longlat +datum=WGS84")
  
 # library(raster)
  #e<-c(-11.4, -8.5, 38.5, 41.5)
  #rsalc<-crop(rsal,e)
  #plot(rsalc)
  #text(lon[662],lat[201],labels="px", cex=0.7, font=2);text(lon[662+delta],lat[201],labels="p6", cex=0.7, font=2);text(lon[661-delta],lat[201],labels="p4", cex=0.7, font=2);text(lon[662],lat[201+delta],labels="p2", cex=0.7, font=2);text(lon[662],lat[201-delta],labels="p8", cex=0.7, font=2)
  
  #rp4=extract(rsal, data.frame(lon[662-delta],lat[201])); rp4 ;rp6=extract(rsal, data.frame (lon[662+delta],lat[201])); rp6 ;rpx=extract(rsal, data.frame(lon[662],lat[201])); rpx ;rp2=extract(rsal, data.frame (-9.875, 40.125)); rp2 ;rp8=extract(rsal, data.frame (-9.875, 39.925)); rp8
 
  
  ##Calculating coef.var in area----
  
  #extract data from the area around track sample
  
  deltacv=1 # area diameter to extract env data arpund tracks cordinates
  
  lonrange=c(idx-deltacv,idx+deltacv); latrange=c(idy-deltacv, idy+deltacv)
  
  if(is.na(lon[idx+deltacv])|is.na(lon[idx-deltacv])|is.na(lat[idy+deltacv])|is.na(lat[idy-deltacv])){cv_sal15=NA  ;cv_sal100=NA; cv_temp15=NA;cv_temp100=NA;cv_u15=NA;cv_u100=NA;cv_v15=NA;cv_v100=NA;cv_w100=NA;cv_den0=NA } else {  
    #salinity
  
  CV <- function(x){ 
    (sd(x, na.rm=T)/mean(x, na.rm=T))*100
  } #coef of variation
  
  cv_sal15 <- CV(sal15[lonrange, latrange])
  cv_sal100 <- CV(sal100[lonrange, latrange])
  
  cv_temp15 <- CV(temp15[lonrange, latrange])
  cv_temp100 <- CV(temp100[lonrange, latrange])
  
  cv_u15 <- CV(u15[lonrange, latrange])
  cv_u100 <- CV(u100[lonrange, latrange])
  
  cv_v15 <- CV(v15[lonrange, latrange])
  cv_v100 <- CV(v100[lonrange, latrange])

  cv_w100 <- CV(w100[lonrange, latrange])
  
  cv_den0 <- CV(den0)
  
   }
 
 #Calculating Gradients----

 delta= 10 #pixeles de distancia
 #distance between pixels = horizontal resolution = 1/10º
 dxdist= 1 #distance between the central and a 10-pixel-far pixels
 dydist = 1 
 
 
 #gradient aproximated by 4 cells (also 8cells are possible)
 
 if(is.na(lon[idx+delta])|is.na(lon[idx-delta])|is.na(lat[idy+delta])|is.na(lat[idy-delta])){cellvalue_grad_sal15=NA  ;cellvalue_grad_sal100=NA; cellvalue_grad_temp15=NA;cellvalue_grad_temp100=NA;cellvalue_grad_u15=NA;cellvalue_grad_u100=NA;cellvalue_grad_v15=NA;cellvalue_grad_v100=NA;cellvalue_grad_w100=NA;cellvalue_grad_den0=NA}else{  
 #salinity
 
 #15
 p2_sal15=sal15[idx,idy+delta] #pixel de arriba
 p4_sal15=sal15[idx-delta,idy] #pixel de la izq
 p6_sal15=sal15[idx+delta,idy] #pixel derecha
 p8_sal15=sal15[idx,idy-delta] #pixel debajo
 
 # compute the gradient
 dzdx_sal15=(p6_sal15-p4_sal15)/(2*dxdist)
 dzdy_sal15=(p2_sal15-p8_sal15)/(2*dydist)
 
 
 rm(cellvalue_grad_sal15)  
 cellvalue_grad_sal15=sqrt((dzdx_sal15^2)+(dzdy_sal15^2))
 
 #100
 p2_sal100=sal100[idx,idy+delta] #pixel de arriba
 p4_sal100=sal100[idx-delta,idy] #pixel de la izquierda
 p6_sal100=sal100[idx+delta,idy] #pixel derecha
 p8_sal100= sal100[idx,idy-delta] #pixel debajo
 
 # compute the gradient
 dzdx_sal100=(p6_sal100-p4_sal100)/(2*dxdist)
 dzdy_sal100=(p2_sal100-p8_sal100)/(2*dydist)
 
 rm(cellvalue_grad_sal100) 
 cellvalue_grad_sal100=sqrt((dzdx_sal100^2)+(dzdy_sal100^2)) 
 
 #temperature
 
 #15
 p2_temp15=temp15[idx,idy+delta]  
 p4_temp15=temp15[idx-delta,idy]  
 p6_temp15=temp15[idx+delta,idy]  
 p8_temp15= temp15[idx,idy-delta]  
 
 # compute the gradient
 dzdx_temp15=(p6_temp15-p4_temp15)/(2*dxdist)
 dzdy_temp15=(p2_temp15-p8_temp15)/(2*dydist)
 
 
 rm(cellvalue_grad_temp15) 
 cellvalue_grad_temp15=sqrt((dzdx_temp15^2)+(dzdy_temp15^2)) 
 
 #100
 p2_temp100=temp100[idx,idy+delta] #pixel de arriba
 p4_temp100=temp100[idx-delta,idy] #pixel de la izquierda
 p6_temp100=temp100[idx+delta,idy] #pixel derecha
 p8_temp100= temp100[idx,idy-delta] #pixel debajo
 
 # compute the gradient
 dzdx_temp100=(p6_temp100-p4_temp100)/(2*dxdist)
 dzdy_temp100=(p2_temp100-p8_temp100)/(2*dydist)
 
 rm(cellvalue_grad_temp100) 
 cellvalue_grad_temp100=sqrt((dzdx_temp100^2)+(dzdy_temp100^2)) 
 
 #water velocities
 
 # Eastward veloticy
 #15
 p2_u15=u15[idx,idy+delta] #pixel de arriba
 p4_u15=u15[idx-delta,idy] #pixel de la izquierda
 p6_u15=u15[idx+delta,idy] #pixel derecha
 p8_u15= u15[idx,idy-delta] #pixel debajo
 
 # compute the gradient
 dzdx_u15=(p6_u15-p4_u15)/(2*dxdist)
 dzdy_u15=(p2_u15-p8_u15)/(2*dydist)
 
 
 rm(cellvalue_grad_u15) 
 cellvalue_grad_u15=sqrt((dzdx_u15^2)+(dzdy_u15^2)) 
 
 #100
 p2_u100=u100[idx,idy+delta] #pixel de arriba
 p4_u100=u100[idx-delta,idy] #pixel de la izquierda
 p6_u100=u100[idx+delta,idy] #pixel derecha
 p8_u100= u100[idx,idy-delta] #pixel debajo
 
 # compute the gradient
 dzdx_u100=(p6_u100-p4_u100)/(2*dxdist)
 dzdy_u100=(p2_u100-p8_u100)/(2*dydist)
 
 rm(cellvalue_grad_u100) 
 cellvalue_grad_u100=sqrt((dzdx_u100^2)+(dzdy_u100^2)) 
 
#northward veloticy
 #15
 p2_v15=v15[idx,idy+delta] #pixel de arriba
 p4_v15=v15[idx-delta,idy] #pixel de la izqvierda
 p6_v15=v15[idx+delta,idy] #pixel derecha
 p8_v15= v15[idx,idy-delta] #pixel debajo
 
 # compvte the gradient
 dzdx_v15=(p6_v15-p4_v15)/(2*dxdist)
 dzdy_v15=(p2_v15-p8_v15)/(2*dydist)
 
 
 rm(cellvalue_grad_v15) 
 cellvalue_grad_v15=sqrt((dzdx_v15^2)+(dzdy_v15^2)) 
 
 #100
 p2_v100=v100[idx,idy+delta] #pixel de arriba
 p4_v100=v100[idx-delta,idy] #pixel de la izqvierda
 p6_v100=v100[idx+delta,idy] #pixel derecha
 p8_v100= v100[idx,idy-delta] #pixel debajo
 
 # compvte the gradient
 dzdx_v100=(p6_v100-p4_v100)/(2*dxdist)
 dzdy_v100=(p2_v100-p8_v100)/(2*dydist)
 
 rm( cellvalue_grad_v100) 
 cellvalue_grad_v100=sqrt((dzdx_v100^2)+(dzdy_v100^2)) 
 
 #vertical velocity
 #100
 p2_w100=w100[idx,idy+delta] #pixel de arriba
 p4_w100=w100[idx-delta,idy] #pixel de la izqwierda
 p6_w100=w100[idx+delta,idy] #pixel derecha
 p8_w100= w100[idx,idy-delta] #pixel debajo
 
 # compwte the gradient
 dzdx_w100=(p6_w100-p4_w100)/(2*dxdist)
 dzdy_w100=(p2_w100-p8_w100)/(2*dydist)
 
 rm(cellvalue_grad_w100) 
 cellvalue_grad_w100=sqrt((dzdx_w100^2)+(dzdy_w100^2)) 
 
 #density
 p2_den0=den0[idx,idy+delta] #pixel de arriba
 p4_den0=den0[idx-delta,idy] #pixel de la izquierda
 p6_den0=den0[idx+delta,idy] #pixel derecha
 p8_den0= den0[idx,idy-delta] #pixel debajo
 
 # compute the gradient
 dzdx_den0=(p6_den0-p4_den0)/(2*dxdist)
 dzdy_den0=(p2_den0-p8_den0)/(2*dydist)
 
 
 rm(cellvalue_grad_den0) 
 cellvalue_grad_den0=sqrt((dzdx_den0^2)+(dzdy_den0^2)) #grad4cmatrix
 
 } #end coordinates limit condition
 
 #print----
 #cat("cellvalue_sal_15=",cellvalue_sal_15,"cellvalue_sal_100=",cellvalue_sal_100,"cellvalue_temp_15=",cellvalue_temp_15,"cellvalue_temp_100=",cellvalue_temp_100,"cellvalue_u15=",cellvalue_u15,"cellvalue_u100=",cellvalue_u100,"cellvalue_v15=",cellvalue_v15,"cellvalue_v100=",cellvalue_v100,"cellvalue_w100=", cellvalue_w100,"cv_sal15=",cv_sal15,"cv_temp15=",cv_temp15,"cv_sal100=",cv_sal100,"cv_temp100=",cv_temp100,"grad_sal15=", cellvalue_grad_sal15,"grad_sal100=", cellvalue_grad_sal100,"grad_temp15=", cellvalue_grad_temp15,"grad_temp100=", cellvalue_grad_temp100,"cellvalue_denisty", cellvalue_den0,"cv_den0", cv_den0)
 
  return(c(cellvalue_sal_15, 
           cellvalue_sal_100, 
           cellvalue_temp_15, 
           cellvalue_temp_100, 
           cellvalue_u15, 
           cellvalue_u100, 
           cellvalue_v15, 
           cellvalue_v100, 
           cellvalue_w100, 
           cellvalue_den0, 
           cv_sal15, 
           cv_sal100, 
           cv_temp15, 
           cv_temp100, 
           cv_u15,
           cv_u100,
           cv_v15,
           cv_v100,
           cv_w100,
           cv_den0, 
           cellvalue_grad_sal15, 
           cellvalue_grad_sal100, 
           cellvalue_grad_temp15, 
           cellvalue_grad_temp100, 
           cellvalue_grad_u15,
           cellvalue_grad_u100,
           cellvalue_grad_v15,
           cellvalue_grad_v100,
           cellvalue_grad_w100,
           cellvalue_grad_den0))# meter todas las vars
    }
    

#getvarsvalnc(mydate, latsample,longsample, dirnc) #funciona -> meter en loop para usar con magic table







