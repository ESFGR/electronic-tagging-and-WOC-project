#2016/0927; diego; cpopy of the codes for bleufin tables, to be adapted to magic table 
# V2. Adapted to have blocks at the beginning for Leif or Diego's Computer
# V3. For loop adjusted to run in parallel, optimized a couple of lines, have output files generate date from computer


#Enrique Edition, data from 3d currents product of Theme 2_3D currents and vertical motion for Sustainable Fisheries

#To download data in fillefile: ftp://eftp.ifremer.fr | login: w0323d4 | password: eprouve-rechargiez-agregez

rm(list=ls(all=TRUE))

#Cheking dates for which there is tracks data each year to download environmental data
#env_track2010<-read.csv("2010_env_tracks.csv")

mtlt<-read.csv("C:/Users/Desktop/tracks atun rojo/Magic table long tracks Bluefin Tuna.csv")



table(mtlt$Year, mtlt$Deploy.ID)

t10<-subset(mtlt, Year=="2010")
table(t10$yday) #from yday 198 to 237

t11<-subset(mtlt, Year=="2012")
 
tapply(t11$Deploy.ID, t11$yday, length)# from yday  146 to 236 and  334 to 360 (mas o menos)

t12<-subset(mtlt, Year=="2012")
tapply(t12$Deploy.ID, t12$yday, length) #yday 

t13<-subset(mtlt, Year=="2013")
tapply(t13$Deploy.ID, t13$yday, length) #yday 
table(t13$Deploy.ID)
# Make sure code is present, read table, set source, set ncpath

#Library to allow us to use a for loop across many processors
library(foreach)
library(RNetCDF)  # For netcdf
library(maptools) # For shapefiles
library(fields)   # For shapefiles
library(rgdal)    # For shapefiles
library(lubridate)
library(DescTools)

#Section to set file?s locations
setwd("C:/Theme 2_3D currents and vertical motion for Sustainable Fisheries/woc/")

file.exists('Códigos/extract values from nc for given lat lon.R') #####

t1=read.csv("C:/Theme 2_3D currents and vertical motion for Sustainable Fisheries/woc/tracks atun rojo/Magic table long tracks Bluefin Tuna.csv")

source('Códigos/extract values from nc for given lat lon.R')
source('Códigos/function formula density.R')

ncpath= "C:/Theme 2_3D currents and vertical motion for Sustainable Fisheries/woc/theme2/ocean_currents/woc-l4-cur-natl3d_rep-1d/v2.0/2011/"

#Make Sure Table looks good

nrow(t1)
names(t1)

tapply(t1$ID,t1$Year,length)


############
# select scale of smoothing in kilometers of radius (this is for the gradient)
#delta=10

########### Either creates t2 from a filter or just uses t1
t2 <- t1[t1$Year=="2011",]
#t2=t1
table(t2$yday)

library(stringr)

table(t1$Year)

t2$yday<-str_pad(t2$yday, 3, pad = "0") #to add 0 to ydays with 2 digits so it fits the format of the environmental data files



##nrow(t2)
##names(t2)
##summary(t2)

CV <- function(x){ 
  (sd(x, na.rm=T)/mean(x, na.rm=T))*100
} #coef of variation


#######make output table with environmental variable


rm(tout)
tout<-data.frame(
  Deploy.ID=numeric(0),#
  ID=numeric(0), #identificator number/text unic in magic table # Sample
  date=as.Date(character()),
  lat=numeric(0),
  lon=numeric(0),
  direction=character(0),
  
  cellvalue_sal_15=numeric(0),  #VALUE OF THE EN.	 VARAIBLE AT LOCATION/DATE
  cellvalue_sal_100=numeric(0),	
  cellvalue_temp_15=numeric(0),
  cellvalue_temp_100=numeric(0),
  cellvalue_u15 =numeric(0),
  cellvalue_u100 =numeric(0),
  cellvalue_v15 =numeric(0),
  cellvalue_v100 =numeric(0),
  cellvalue_w100 =numeric(0),
  cellvalue_den0=numeric(0), 
  
  cv_sal15=numeric(0),
  cv_sal100=numeric(0),
  cv_temp15=numeric(0),
  cv_temp100=numeric(0),
  cv_u15=numeric(0),
  cv_u100=numeric(0),
  cv_v15=numeric(0),
  cv_v100=numeric(0),
  cv_w100=numeric(0),
  cv_den0=numeric(0),
  
  cellvalue_grad_sal15=numeric(0),
  cellvalue_grad_sal100=numeric(0),
  cellvalue_grad_temp15=numeric(0),
  cellvalue_grad_temp100=numeric(0),
  cellvalue_grad_u15=numeric(0),
  cellvalue_grad_u100=numeric(0),
  cellvalue_grad_v15=numeric(0),
  cellvalue_grad_v100=numeric(0),
  cellvalue_grad_w100=numeric(0),
  cellvalue_grad_den0=numeric(0),
  
  stringsAsFactors = FALSE)
str(tout)



typeof(tout)
class(tout)
head(tout)

#t2=t2[1,]

###### Read rows iteratively and  use code 2 to calculate gradients
################################################################################### loop 1
## Loop changed to allow for multiple processors
# If this a run on a single core processor might need to change it to be %do% instead of %dopar%

for(i in 1:nrow(t2))  {
  
  sample<-t2[i,]
  
  # Put if here to address year
  
  #yyyymmdd=paste(sample$Year,"-",sample$month,"-",sample$day,sep="")
  yyyymmdd=sample$YYYMMMDD
  
  
  latsample=sample$Lat
  longsample=sample$Lon
  yday=sample$yday
  
  rm(proces)
  proces=getvarsvalnc(yyyymmdd,latsample,longsample,ncpath)
  #dev.off()
  
  cellvalue_sal_15=proces[1]
  cellvalue_sal_100= proces[2]
  cellvalue_temp_15=proces[3]
  cellvalue_temp_100=proces[4]
  cellvalue_u15=proces[5]
  cellvalue_u100=proces[6]
  cellvalue_v15=proces[7]
  cellvalue_v100=proces[8]
  cellvalue_w100=proces[9]
  cellvalue_den0=proces[10]
  cv_sal15=proces[11]
  cv_sal100=proces[12]
  cv_temp15=proces[13]
  cv_temp100=proces[14]
  cv_u15=proces[15]
  cv_u100=proces[16]
  cv_v15=proces[17]
  cv_v100=proces[18]
  cv_w100=proces[19]
  cv_den0=proces[20]
  cellvalue_grad_sal15=proces[21]
  cellvalue_grad_sal100=proces[22]
  cellvalue_grad_temp15=proces[23]
  cellvalue_grad_temp100=proces[24]
  cellvalue_grad_u15=proces[25]
  cellvalue_grad_u100=proces[26]
  cellvalue_grad_v15=proces[27]
  cellvalue_grad_v100=proces[28]
  cellvalue_grad_w100=proces[29]
  cellvalue_grad_den0=proces[30]
  
  
  
  #ojo filas,columnas
  tout[i,1]=sample$Deploy.ID
  tout[i,2]=sample$ID
  tout[i,3]=yyyymmdd
  tout[i,4]=sample$Lat
  tout[i,5]=sample$Lon
  tout[i,6]=sample$Direction
  
  tout[i,7]=cellvalue_sal_15
  tout[i,8]=cellvalue_sal_100
  tout[i,9]=cellvalue_temp_15
  tout[i,10]=cellvalue_temp_100
  tout[i,11]=cellvalue_u15
  tout[i,12]=cellvalue_u100
  tout[i,13]=cellvalue_v15
  tout[i,14]=cellvalue_v100
  tout[i,15]=cellvalue_w100
  tout[i,16]=cellvalue_den0 
  
  tout[i,17]=cv_sal15
  tout[i,18]=cv_sal100
  tout[i,19]=cv_temp15
  tout[i,20]=cv_temp100
  tout[i,21]=cv_u15
  tout[i,22]=cv_u100
  tout[i,23]=cv_v15
  tout[i,24]=cv_v100
  tout[i,25]=cv_w100
  tout[i,26]=cv_den0
  
  tout[i,27]=cellvalue_grad_sal15
  tout[i,28]=cellvalue_grad_sal100
  tout[i,29]=cellvalue_grad_temp15
  tout[i,30]=cellvalue_grad_temp100
  tout[i,31]=cellvalue_grad_u15
  tout[i,32]=cellvalue_grad_u100
  tout[i,33]=cellvalue_grad_v15
  tout[i,34]=cellvalue_grad_v100
  tout[i,35]=cellvalue_grad_w100
  tout[i,36]=cellvalue_grad_den0

  
}  

#fix(tout)


write.csv(tout, "C:/Theme 2_3D currents and vertical motion for Sustainable Fisheries/woc/tracks atun rojo/2011_env_tracks_2904.csv", row.names = F)


#continuar  aqui#############
#########################################################################

Sys.time()#para poner en el nombre del fichero la hora y que no se sobreescriba

# CAMBIAR VARIABLE QUE ESTEN EN "env2"
write.table(tout,paste("C:/Theme 2_3D currents and vertical motion for Sustainable Fisheries/woc/tracks atun rojo/",Sys.time(),"_2012_env_tracks.csv",sep=""), quote = FALSE, dec = ".", sep=";",row.names=FALSE) 
write.csv(tout,paste(Sys.Date(),"_hycom_sal_surf_",delta,".txt",sep=""), quote = FALSE, dec = ".", sep=";",row.names=FALSE) 
#write.xlsx(tout,paste(Sys.Date(),"_hycom_sal_surf_",delta,".xls",sep=""),sheetName="Sheet1",showNA=TRUE)
