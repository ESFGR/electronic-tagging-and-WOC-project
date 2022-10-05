#Extract values from nc for given AREA sourrounding tracks of bluefin tuna

library(lubridate)

#PARAMETROS PARA test

# mydate="2013-01-02"  ;  latsample=35 ;  longsample=-15 

#ncpath="C:/Theme 2_3D currents and vertical motion for Sustainable Fisheries/woc/theme2/ocean_currents/woc-l4-cur-natl3d_rep-1d/v2.0/2013/"

#namenc="20100710120000_WOC-L4-CUR-NATL3D_REP-1D-v1.0-fv2.0.nc"

#getsstvalnc(mydate,latsample,longsample,dirnc,namenc)   estos son los daraos de donde quieres sacar los valores

library(RNetCDF)
library(stringr)
library(lubridate)



getvarsvalnc=function(mydate,latsample,longsample,ncpath) #only for salinity?
{
  #libraries
 
  
  
  #read sampling dates
  sampledate=as.Date(mydate)#
  year=format(sampledate, "%Y") ; year
  month=format(sampledate, "%m") ; month
  day=format(sampledate, "%d") ; day
  yday= yday(sampledate) ; yday 
  yday<-str_pad(yday, 3, pad = "0") ; yday 
  
  
  # from the date to number in R 
  Rdate=as.double(sampledate)
  
  # convert from number to date since 01-01-1981 00:00:00
  # reference time used by hycom
  matdatecero=as.double(as.Date("1981-01-01")) # Create the zero day in R
  
  #Determining the sampling date from the matlab zero date
  
  datenum= 24*((Rdate-matdatecero)) #time in hours since 1981-01-01 00:00:00
  
  #open the nc file--
  
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
  
#selecting range of lon and lat to extract data from the area around tracksample
 
  delta=0.25 # area diameter to extract env data arpund tracks cordinates
  
  lonrange=c(longsample-delta,longsample+delta); latrange=c(latsample-delta, latsample+delta)
  x=lon
  y=lat
  
  # read salinity at that date
  sal15= var.get.nc( nc,"so",start=c(1,1,7,mydate), count=c(length(lon),length(lat),1,1),unpack=TRUE)   
  sal15[sal15==0] <- NA 
  mean_sal15 <- mean(sal15[lonrange, latrange], na.rm=TRUE) # sal15 [lonrange, latrange]
  
  #eastward velocity"
  u15= var.get.nc( nc,"uo",start=c(1, 1,7,mydate), count=c(length(lon),length(lat),1,1),unpack=TRUE)  
  mean_u15 <- mean(u15[lonrange, latrange], na.rm=TRUE)
  
  #northward velocity
  v15 = var.get.nc( nc,"vo",start=c(1,1,7,mydate), count=c(length(lon),length(lat),1,1),unpack=TRUE)
  mean_v15 <- mean(v15[lonrange, latrange], na.rm=TRUE)
  
  #Sea water temeperature
  temp.015 = var.get.nc(nc, "to", start=c(1,1,7,mydate), count=c(length(lon),length(lat),1,1),unpack=TRUE)
  temp15=temp.015
  temp15[temp15=="273.149993896484"] <- NA
  mean_temp15 <- mean(temp15[lonrange, latrange], na.rm=TRUE)
  
  
  
  #variables at 100.25m (19th depth level in nc file)
  sal100= var.get.nc( nc,"so",start=c(1,1,19,mydate), count=c(length(lon),length(lat),1,1),unpack=TRUE) 
  sal100[sal100==0] <- NA 
  mean_sal100 <- mean(sal100[lonrange, latrange], na.rm=TRUE)
  
  u100= var.get.nc( nc,"uo",start=c(1,1,19,mydate), count=c(length(lon),length(lat),1,1),unpack=TRUE)  
  mean_u100 <- mean(u100[lonrange, latrange], na.rm=TRUE)
  
  
  v100 = var.get.nc(nc,"vo",start=c(1,1,19,mydate), count=c(length(lon),length(lat),1,1),unpack=TRUE) 
  mean_v100 <- mean(v100[lonrange, latrange], na.rm=TRUE)
  
   w100 =var.get.nc( nc,"wo",start=c(1,1,19,mydate), count=c(length(lon),length(lat),1,1),unpack=TRUE)
   mean_w100 <- mean(w100[lonrange, latrange], na.rm=TRUE) #vertical velocity
   
  temp.b = var.get.nc(nc, "to", start=c(1,1,19,mydate), count=c(length(lon),length(lat),1,1),unpack=TRUE) 
  temp100=temp.b
  temp100[temp100=="273.149993896484"] <- NA
  mean_temp100 <- mean(temp100[lonrange, latrange], na.rm=TRUE)
  
  #cvs
  
  cv_sal15<-cv(na.omit(sal15))
  cv_temp15<-cv(na.omit(temp15))
  cv_sal100<-cv(na.omit(sal100))
  cv_temp100<-cv(na.omit(temp100))
  
  
  
  
  x=lon
  y=lat
 
  difx=abs(x-longsample) 
  dify=abs(y-latsample)
  
  idx <- which(difx == min(difx), arr.ind = TRUE)[1,1];idx
  idy <- which(dify == min(dify), arr.ind = TRUE)[1,1];idy
  
  
  if(min(difx)>0.5 | min(difx>0.5)) {cellvalue_sal_15=-999; cellvalue_sal_100=-999; cellvalue_temp_15=-999; cellvalue_temp_100=-999; cellvalue_u15=-999; cellvalue_u100=-999; cellvalue_v15= -999; cellvalue_v100 =-999; cellvalue_w100= -999} else {
    
    cellvalue_sal_15=sal15[idx,idy]#####funciona
    cellvalue_sal_100=sal100[idx,idy]
    cellvalue_temp_15=temp15[idx,idy]
    cellvalue_temp_100=temp100[idx,idy]
    cellvalue_u15=u15[idx,idy]
    cellvalue_u100=u100[idx,idy]
    cellvalue_v15=v15[idx,idy]
    cellvalue_v100=v100[idx,idy]
    cellvalue_w100=w100[idx,idy]
    
    rm(nc) 
  } # fin else
  
  print(paste("cellvalue_sal_15=",cellvalue_sal_15,
              "cellvalue_sal_100=",cellvalue_sal_100, 
              "cellvalue_temp_15=",cellvalue_temp_15,
              "cellvalue_temp_100=",cellvalue_temp_100, 
              "cellvalue_u15=",cellvalue_u15,
              "cellvalue_u100=",cellvalue_u100,
              "cellvalue_v15=",cellvalue_v15,
              "cellvalue_v100=",cellvalue_v100,
              "cellvalue_w100=", cellvalue_w100,
              "mean_sal15=",  mean_sal15,
              "mean_sal100=", mean_sal100,
              "mean_temp15=", mean_temp15,
              "mean_temp100=", mean_temp100,
              "mean_u15=", mean_u15,
              "mean_u100=", mean_u100,
              "mean_v15=", mean_v15,
              "mean_v100=", mean_v100,
              "mean_w100=", mean_w100,
              "cv_sal15=",cv_sal15,
              "cv_temp15",cv_temp15,
              "cv_sal100=",cv_sal100,
              "cv_temp100=",cv_temp100
              
  )) 
  
  return(c(cellvalue_sal_15, cellvalue_sal_100, cellvalue_temp_15, cellvalue_temp_100, cellvalue_u15, cellvalue_u100, cellvalue_v15, cellvalue_v100, cellvalue_w100,  mean_sal15, mean_sal100, mean_temp15, mean_temp100,mean_u15,mean_u100,mean_v15,mean_v100,mean_w100, cv_sal15, cv_sal100, cv_temp15, cv_temp15, cv_temp100))# meter todas las vars
}
 
 
#getvarsvalnc(mydate, latsample,longsample, ncpath) #funciona -> meter en loop para usar con magic table
