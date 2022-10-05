#en julio van al sur y luego al norte. coger los 10 primeros días de todos los tracks y hacer boxplot para sur y para norte, PERO excluyendo el track de mayo que va al sur. hay un patron coincidente de ese track con los de julio? 


library(mapdata)
library(maptools)
library(rnaturalearth)
library(raster)
library(gdata)
library(ggpubr)
library(lubridate)


### Colours----

c1 <- rgb(216,238,192, max = 255, alpha = 230, names = "lt.green") #light colours for histogram
c2 <- rgb(255,100,100, max = 255, alpha = 100, names = "lt.red")
c3 <- rgb(0, 100, 255, max=255, alpha = 100, names = "lt.blue")
c4 <- rgb(100, 0, 100, max=255, alpha = 100, names = "lt.purple")


### Loading track´s environmental data----

setwd("C:/Theme 2_3D currents and vertical motion for Sustainable Fisheries/woc/tracks atun rojo/")

tenv<-read.csv("Magic_table_env_var.csv") #table with all tracks and evironmental data

#Tracks starting in may------

id130539<-subset(tenv, Deploy.ID=="130539")
id130539<- id130539[1:10,]

id130546<-subset(tenv, Deploy.ID=="130546")
id130546<- id130546[1:10,]

id114008 <-subset(tenv, Deploy.ID=="114008")
id114008<-id114008[1:10,]

id86243<-subset(tenv, Deploy.ID=="86243")
id86243<-id86243[1:10,]

id97466<-subset(tenv, Deploy.ID=="97466")
id97466<-id97466[1:10,]

id120088<-subset(tenv, Deploy.ID=="120088")
id120088<-id120088[1:10,]

#id120084<-subset(tenv, Deploy.ID=="120084")
id114009<-subset(tenv, Deploy.ID=="114009")
id114009n<-subset(id114009, date<"2012-07-30")
id114009n <-id114009n[1:10,]


maytracks<-rbind(id86243, id97466,id120088, id114009n, id130539, id130546, id114008)
maytracks$year<-year(maytracks$date)
maytracks$start<-"May"




ggplot(maytracks) + 
  geom_point(mapping = aes(x = as.factor(Deploy.ID), y = maytracks$cellvalue_sal_15, color = as.factor(Deploy.ID)))

ggplot(maytracks) + 
  geom_boxplot(mapping = aes(x = as.factor(Deploy.ID), y = maytracks$cellvalue_sal_15, color = as.factor(Deploy.ID)))




#tracks staring in july------
#Tracks: 

id61125<-subset(tenv,Deploy.ID=="61125")
id61125<-id61125[1:10,]

id61127<-subset(tenv,Deploy.ID=="61127")
id61127<-id61127[1:10,]

id72499<-subset(tenv,Deploy.ID=="72499")
id72499<-id72499[1:10,]

id72498<-subset(tenv,Deploy.ID=="72498")
id72498<-id72498[1:10,]

id86438<-subset(tenv,Deploy.ID=="86438")
id86438<-id86438[1:10,]

id86440<-subset(tenv,Deploy.ID=="86440")
id86440<-id86440[1:10,]

id97462<-subset(tenv,Deploy.ID=="97462") ## not included, locations too spread in time and space
id97462n <-subset(id97462, lon>-20)


id114006 <-subset(tenv, Deploy.ID=="114006")
mainMap4 + geom_point(data = id114006, aes(x = lon, y = lat, color=as.factor(month(date))))
id114006<-subset(id114006, date>"2012-05-30") #the first location at may corresponds to the entry to Mediterranean
id114006<-id114006[1:10,]
id114006$year<- year(id114006$date)
id114006$start <- "id114006"


#id114006 not included to analyse differences between migratrory routes


julytracks<-rbind(id61125, id61127,id72499, id72498, id86438, id86440)
julytracks$year<-year(julytracks$date)
julytracks$start<-"July"

ggplot(julytracks) + 
  geom_boxplot(mapping = aes(x = as.factor(Deploy.ID), y = cellvalue_sal_15, color = as.factor(Deploy.ID)))


#Boxplot for the two migraotry and time trends in ten first days:

t10<-rbind(maytracks, id114006, julytracks)

ggplot(t10) + 
  geom_boxplot(mapping = aes(x = as.factor(start), y = cellvalue_sal_15, color = as.factor(start))) + theme(plot.title = element_text(face = "bold"), 
    panel.background = element_rect(fill = "gray94"), 
    plot.background = element_rect(fill = "white")) +labs(title = "Salinity at 15m comparing tracks starting in May and July", 
    x = NULL, y = "Salinity", colour = NULL)


