#Author: Enrique Sánchez-Fabrés
#Time series of environmental variables at tuna tracks

library(mapdata)
library(maptools)
library(rnaturalearth)
library(raster)
library(gdata)
library(ggpubr)
library(lubridate)


### Loading track´s environmental data----

setwd("C:/Theme 2_3D currents and vertical motion for Sustainable Fisheries/woc/tracks atun rojo/")

#### join yearly tables with data from tracks and from environmental variables
#env_track2010<-read.csv("2010_env_tracks_2904.csv")
#env
#env_track2011<-read.csv("2011_env_tracks_2904.csv")
#table(env_track2011$Deploy.ID)

#env_track2012<-read.csv("2012_env_tracks_2904.csv")
#table(env_track2012$Deploy.ID)

#env_track2013<-read.csv("2013_env_tracks_2904.csv")
#table(env_track2013$Deploy.ID)

#tenv<-rbind(env_track2010, env_track2011, env_track2012, env_track2013)

#transform ºk to ºC
#cellvalue_temp15<-rep(NA, nrow(tenv))
#cellvalue_temp100<-rep(NA, nrow(tenv))

#for (i in 1:nrow(tenv)) { if(is.na(tenv$cellvalue_temp_15[i])){ cellvalue_temp15[i]=NA  }else{  cellvalue_temp15[i]<-tenv$cellvalue_temp_15[i]-273.15}}
#for (i in 1:nrow(tenv)) { if(is.na(tenv$cellvalue_temp_100[i])){ cellvalue_temp100[i]=NA }else{ cellvalue_temp100[i]<-tenv$cellvalue_temp_100[i]-273.15} }

#tenv$cellvalue_temp_15<-cellvalue_temp15
#tenv$cellvalue_temp_100<-cellvalue_temp100

#write.csv(tenv,"Magic_table_env_var.csv",row.names = F)

tenv<-read.csv("Magic_table_env_var.csv") #table with all tracks and evironmental data

#Map settings----
mapBuffer <-0.1
worldMap <- ne_download(scale = "medium", returnclass = "sp")

worldMapCroped <- crop(worldMap,extent(-30-mapBuffer, 0+mapBuffer,33-mapBuffer, 60+mapBuffer))  # based on geographical limits of oceanographical product and blueifn distribution

mainMap <- ggplot() + geom_polygon(data = worldMapCroped, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa general



# North Iberic peninsula (NP), North peninsula + north atlantic (NPAN) ----
#Tracks: 

id61125<-subset(tenv,Deploy.ID=="61125")
id61127<-subset(tenv,Deploy.ID=="61127")
id72499<-subset(tenv,Deploy.ID=="72499")
id72498<-subset(tenv,Deploy.ID=="72498")
id86438<-subset(tenv,Deploy.ID=="86438")
id86440<-subset(tenv,Deploy.ID=="86440")
id97462<-subset(tenv,Deploy.ID=="97462") ## not included, locations too spread in time and space
id97462n<-subset(id97462, lon>-20)


npan<-rbind(id61125, id61127,id72499, id72498, id86438, id86440)

c2 <- crop(worldMap,extent(-20-mapBuffer, -1.075+mapBuffer,35-mapBuffer, 50+mapBuffer))  # based on geographical limits of oceanographical product and blueifn distribution

mainMap3 <- ggplot() + geom_polygon(data = c2, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa general

mapnpan<- mainMap3 + geom_point(data = npan, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 1.2) + labs(title = "North Atlantic tracks (NA)")  +geom_vline(xintercept = -6, color = "red",fill=NA, inherit.aes = FALSE) + guides(colour = guide_legend(override.aes = list(size=4))) + theme(legend.title = element_text(face = "bold")) +labs(colour = "ID")+theme(legend.position = "bottom")


#salinity
nsal15<- ggplot(npan) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_sal_15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Salinity", x = "", y = "PSU (15m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 0))+ theme(plot.title = element_text(size = 13, hjust = 0.5, face="bold")) 




nsal100<-ggplot(npan) + 
  geom_smooth(aes(x=yday(npan$date),y=cellvalue_sal_100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face = "bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the Year (yday)", y = "PSU (100m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))+theme(plot.margin = margin(t=0,b = 5))

ggsal<-ggarrange(nsal15, nsal100,  ncol = 1, nrow = 2, common.legend = TRUE,legend = "bottom", labels = c("a", "b")) + theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 10)) +labs(title = "")

ggarrange(ggsal,mapnpan,  ncol = 2, nrow = 1, labels = c("" ,"c"))

#temperature
ntemp15<- ggplot(npan) + 
  geom_smooth(aes(x=yday(npan$date),y=cellvalue_temp_15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Temperature", x = "", y = "ºC (15m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 0)) + theme(plot.title = element_text(size = 13, hjust = 0.5, face="bold"))

ntemp100<- ggplot(npan) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_temp_100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face = "bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the Year (yday)", y = "ºC (100m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))

#plot sal + temp

#ggtemp<- ggarrange(ntemp15, ntemp100,  ncol = 1, nrow = 2, common.legend = TRUE,legend = "bottom", labels = c("c", "d")) + theme(plot.title = element_text(face = "bold", hjust = 0.5, size=10)) +labs(title = "Temperature") 



#annotate_figure(ggsal, top = text_grob("Salinity", color = "black", face = "bold", size = 14))

saltemnpan<- ggarrange(nsal15, ntemp15, nsal100, ntemp100, common.legend = TRUE, legend = "right", labels=c("a", "c", "b", "d"),ncol=2, nrow=2)

#png("environmental variation in tracks/northward_saltemp.png", width = 972, height = 558)

nsaltempmap<-ggarrange(saltemnpan, mapnpan, ncol=2, nrow=1, labels = c("","e"), common.legend = F)

annotate_figure(saltemnpan, top = text_grob("North Atlantic tracks (NA)", color = "black", face = "bold", size = 15))


#dev.off()


#density

nden0<- ggplot(npan) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_den0,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "North Atlantic (NA)", x = "Day of the Year", y = "Density (kg/m3)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) + theme(plot.title = element_text(size = 10, face="bold"))

ngraden<- ggplot(npan) + 
  geom_smooth(aes(x=yday(npan$date),y=npan$cellvalue_grad_den0,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the Year", y = "Grad. Dens.",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) 


ndenisty<- ggarrange(nden0, ngraden, common.legend = T, legend = "bottom", labels = "Density at sea surface (NA)")
#png("environmental variation in tracks/DensityNA.png", width = 972, height = 558)
dev.off()

#vertical velocity


nw100<- ggplot(npan) + 
  geom_smooth(aes(x=yday(npan$date),y=cellvalue_w100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "NA", x = "Day of the Year", y = "Vertical velocity (m/s)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) 


#gradients

#salinity grad
ngradsal15<- ggplot(npan) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_grad_sal15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Sal. gradient", x = "", y = "Grad. sal. (15m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))  + theme(plot.title = element_text(size = 13, hjust = 0.5, face="bold"))

ngradsal100<- ggplot(npan) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_grad_sal100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the year (Yday)", y = "Grad. sal. (100m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) 


ngradtemp15<-ggplot(npan) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_grad_temp15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Temp. gradient", x = "", y = "Grad. temp. (15m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) + theme(plot.title = element_text(size = 13, hjust = 0.5, face="bold"))

ngradtemp100<- ggplot(npan) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_grad_temp100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the year (Yday)", y = "Grad. temp. (100m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) 

gradientsnpan<- ggarrange(ngradsal15,ngradtemp15,ngradsal100, ngradtemp100, common.legend = T, legend = "right", labels = c("a", "c", "b", "d"))

ggarrange(gradientsnpan, mapnpan, labels = c("", "e"))


annotate_figure(gradientsnpan, top = text_grob("North Atlantic tracks (NA)", color = "black", face = "bold", size = 15))



#Canarias and central atlantic (CAC)------

#86243, 97466, 120088, 120084, 114009

id86243<-subset(tenv, Deploy.ID=="86243")
id97466<-subset(tenv, Deploy.ID=="97466")
id120088<-subset(tenv, Deploy.ID=="120088")
#id120084<-subset(tenv, Deploy.ID=="120084")
id114009<-subset(tenv, Deploy.ID=="114009")
id114009n<-subset(id114009, date<"2012-07-30")



cac<-rbind(id86243, id97466,id120088, id114009n)


#map
c3 <- crop(worldMap,extent(-45-mapBuffer, -1.075+mapBuffer,25-mapBuffer, 50+mapBuffer))  # based on geographical limits of oceanographical product and blueifn distribution

mainMap4 <- ggplot() + geom_polygon(data = c3, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa general

mapcac<- mainMap4 + geom_point(data = cac, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 1.2) + labs(title = "South-western tracks (SW)") + guides(colour = guide_legend(override.aes = list(size=4))) + theme(legend.title = element_text(face = "bold"), legend.position = "bottom") +labs(colour = "ID")




#salinity
csal15<- ggplot(cac) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_sal_15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Salinity", x = "", y = "PSU (15m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) + theme(plot.title = element_text(hjust = 0.5)) +labs(x = NULL) 


csal100<- ggplot(cac) + 
  geom_smooth(aes(x=yday(cac$date),y=cellvalue_sal_100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face = "bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the Year (yday)", y = "PSU (100m)",colour = "ID") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))+theme(plot.margin = margin(t=0,b = 5))

cggsal<-ggarrange(csal15, csal100,  ncol = 1, nrow = 2, common.legend = TRUE,legend = "none", labels = c("a", "b"))

ggarrange(cggsal,mapcac,  ncol = 2, nrow = 1, labels = c("" ,"c"))

#temperature
ctemp15<-ggplot(cac) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_temp_15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Temperature", x = "", y = "ºC (15m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) + theme(plot.title = element_text(hjust = 0.5)) +labs(x = NULL) 

ctemp100<-ggplot(cac) + 
  geom_smooth(aes(x=yday(cac$date),y=cellvalue_temp_100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face = "bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the Year (yday)", y = "ºC (100m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))+theme(plot.margin = margin(t=0,b = 5))


#plot sal + temp
ggtemp<- ggarrange(ctemp15, ctemp100,  ncol = 1, nrow = 2, common.legend = TRUE,legend = "none", labels = c("a", "b")) 

saltemcac<- ggarrange(csal15, ctemp15, csal100, ctemp100, common.legend = TRUE, legend = "right", labels=c("d", "e", "f", "g"),ncol=2, nrow=2)

#png("environmental variation in tracks/westward_saltemp.png", width = 972, height = 558)
ctempsalmap<- ggarrange(saltemcac, mapcac, ncol=2, nrow=1, labels = c("","e"))
#dev.off()


annotate_figure(saltemcac, top = text_grob("South-Western tracks (SW)", color = "black", face = "bold", size = 15))

#density

cden0<- ggplot(cac) + 
  geom_smooth(aes(x=yday(cac$date),y=cellvalue_den0,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "South-western (SW)", x = "Day of the year (Yday)", y = "Density (kg/m3)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) + theme(plot.title = element_text(size = 10, face="bold"))

cdengrad0 <-ggplot(cac) + 
  geom_smooth(aes(x=yday(cac$date),y=cac$cellvalue_grad_den0,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "", y = "Grad. density",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))



cw100<- ggplot(cac) + 
  geom_smooth(aes(x=yday(cac$date),y=cellvalue_w100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "SW", x = "Day of the Year", y = "Vertical velocity (m/s)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 1))


#gradientsss
#salinity grad
cgradsal15<- ggplot(cac) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_grad_sal15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "", y = "Grad. salinity (15m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 0)) +labs(title = "Sal. gradient", x = NULL) + theme(plot.title = element_text(size = 13, hjust = 0.5, face="bold"))


cgradsal100<- ggplot(cac) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_grad_sal100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the Year (Yday)", y = "Grad. salinity (100m)",colour = "ID",  linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))


cgradtemp15<- ggplot(cac) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_grad_temp15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "", y = "Grad. temp (15m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 0))+labs(title = "Temp. gradient", x = NULL) + theme(plot.title = element_text(size = 13, hjust = 0.5, face="bold"))



cgradtemp100<- ggplot(cac) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_grad_temp100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the year (Yday)", y = "Grad. temp (100m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 0))

gradientscac<- ggarrange(cgradsal15,cgradtemp15,cgradsal100, cgradtemp100, common.legend = T, legend = "right", labels = c("a", "c", "b", "d"))

ggarrange(gradientscac, mapcac, labels = c("","e"))



annotate_figure(gradientscac, top = text_grob("South-Western tracks (SW)", color = "black", face = "bold", size = 15))


amapnpan
nsaltempmap
saltemnpan
gradientsnpan
cden0
dev.off()


 ggarrange(nsal15 + rremove("xlab"), nsal100 + rremove("ylab"), labels = NULL, ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom", align = "hv", 
font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

annotate_figure(figure, left = textGrob("Common y-axis", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Common x-axis", gp = gpar(cex = 1.3)))


?pdf

#west tracks 130539, 130546, 114006, 118756, 120084----
id130539<-subset(tenv, Deploy.ID=="130539")
id130546<-subset(tenv, Deploy.ID=="130546")
id114006<-subset(tenv, Deploy.ID=="114006")
id114008<-subset(tenv, Deploy.ID=="114008")

aaa<-subset(tenv, Deploy.ID=="118756")
id118756<-subset(aaa, lon>-20)

#id120084<-subset(tenv, Deploy.ID=="120084") removed

westracks<-rbind(id130539, id130546, id114006, id114008, id118756)



mapW <- mainMap4 + geom_point(data = westracks, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 1.2) + labs(title = "West-North tracks (WN)") + guides(colour = guide_legend(override.aes = list(size=4))) + theme(legend.title = element_text(face = "bold"), legend.position = "bottom") +labs(colour = "ID")

#salinity
wsal15<-ggplot(westracks) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_sal_15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Salinity", x = "", y = "PSU (15m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))  + theme(plot.title = element_text(hjust = 0.5)) +labs(x = NULL)

wsal100<- ggplot(westracks) + 
  geom_smooth(aes(x=yday(westracks$date),y=cellvalue_sal_100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face = "bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the Year (yday)", y = "PSU (100m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))+theme(plot.margin = margin(t=0,b = 5))

wggsal<-ggarrange(wsal15, wsal100,  ncol = 1, nrow = 2, common.legend = TRUE,legend = "none", labels = c("a", "b"))

ggarrange(wggsal,mapW,  ncol = 2, nrow = 1, labels = c("" ,"c"))

#temperature
wtemp15<-ggplot(westracks) + 
  geom_smooth(aes(x=yday(westracks$date),y=cellvalue_temp_15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Temperature", x = "", y = "ºC (15m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))  + theme(plot.title = element_text(hjust = 0.5)) +labs(x = NULL)

wtemp100<-ggplot(westracks) + 
  geom_smooth(aes(x=yday(westracks$date),y=cellvalue_temp_100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face = "bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the Year (yday)", y = "ºC (100m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))+theme(plot.margin = margin(t=0,b = 5))


#plot sal + temp
wggtemp<- ggarrange(wtemp15, wtemp100,  ncol = 1, nrow = 2, common.legend = TRUE,legend = "none", labels = c("a", "b")) 

saltemw<- ggarrange(wsal15, wtemp15, wsal100, wtemp100, common.legend = TRUE, legend = "right", labels=c("a", "b", "c", "d"),ncol=2, nrow=2)

#png("environmental variation in tracks/westward_saltemp.png", width = 972, height = 558)
mwtempsalmap<-ggarrange(saltemw, mapW, ncol=2, nrow=1, labels = c("","e"))



annotate_figure(saltemw, top = text_grob("Western-North tracks (WN)", color = "black", face = "bold", size = 15))
###

#salinity grad
wgradsal15<- ggplot(westracks) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_grad_sal15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "", y = "Grad. salinity (15m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 0)) +labs(title = "Sal. gradient", x = NULL)+ theme(plot.title = element_text(size = 13, hjust = 0.5, face="bold"))

wgradsal100<- ggplot(westracks) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_grad_sal100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the Year (Yday)", y = "Grad. salinity (100m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 0))


wgradtemp15<-ggplot(westracks) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_grad_temp15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "", y = "Grad. temp (15m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 0))+labs(title = "Temp. gradient", x = NULL)+ theme(plot.title = element_text(size = 13, hjust = 0.5, face="bold"))


wgradtemp100<-ggplot(westracks) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_grad_temp100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the year (Yday)", y = "Grad. temp (100m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 0))

gradientsw <-ggarrange(wgradsal15,wgradtemp15,wgradsal100, wgradtemp100, common.legend = T, legend = "right", labels = c("a", "c", "b", "d"))

annotate_figure(gradientsw, top = text_grob("Western-North tracks (WN)", color = "black", face = "bold", size = 15))


ggarrange(gradientsw, mapW, labels = c("","e"))

#density

wden0<- ggplot(westracks) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_den0,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Western-north (WN)", x = "Day of the year (Yday)", y = "Density (kg/m3)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) + theme(plot.title = element_text(size = 10, face="bold"))

wgraden0<- ggplot(westracks) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_grad_den0,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "", y = "Grad. density",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) 

ggarrange(wden0, wgraden0, ncol = 1, nrow = 2, common.legend = T, legend = "bottom", labels = c("a","b"))


densities <-ggarrange(nden0, cden0, wden0, ncol = 1, nrow = 3, legend="bottom", labels = c("a", "b", "c"))
annotate_figure(densities, top = text_grob("Denisty at ABFT locations", color = "black", face = "bold", size = 15))



den.grad <-ggarrange(ngraden,  cdengrad0, wgraden0, ncol = 1, nrow = 3, legend="bottom", labels = c("a", "b", "c"))

annotate_figure(den.grad, top = text_grob("Denisty gradients at ABFT locations", color = "black", face = "bold", size = 15))


#vertical velocity


wnw100<- ggplot(westracks) +  geom_smooth(aes(x=yday(date),y=cellvalue_w100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+ labs(title = "WN", x = "Day of the Year", y = "Vertical velocity (m/s)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) 


w100<-ggarrange(nw100, cw100, wnw100, ncol = 1, nrow = 3, legend="bottom", labels = c("a", "b", "c"))


annotate_figure(w100, top = text_grob("Vertical velocity at ABFT locations", color = "black", face = "bold", size = 15))


#Undefined  114006, 118756, 1120084-------

id114006<-subset(tenv, Deploy.ID=="114006")
id118756<-subset(tenv, Deploy.ID=="118756")
id120084<-subset(tenv, Deploy.ID=="120084")

utracks<-rbind(id114006, id118756, id120084)

mapU<- mainMap4 + geom_point(data = utracks, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 1.2) + labs(title = "Tuna tracks West-North (WN)") + guides(colour = guide_legend(override.aes = list(size=4))) + theme(legend.title = element_text(face = "bold"), legend.position = "bottom") +labs(colour = "ID")
mapW

#salinity
wsal15<-ggplot(westracks) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_sal_15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Salinity", x = "", y = "PSU (15m)",colour = "ID") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 0))


wsal100<-ggplot(westracks) + 
  geom_smooth(aes(x=yday(westracks$date),y=cellvalue_sal_100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face = "bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the Year (yday)", y = "PSU (100m)",colour = "ID") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))+theme(plot.margin = margin(t=0,b = 5))

wggsal<-ggarrange(wsal15, wsal100,  ncol = 1, nrow = 2, common.legend = TRUE,legend = "none", labels = c("a", "b"))

ggarrange(wggsal,mapW,  ncol = 2, nrow = 1, labels = c("" ,"c"))

#temperature
wtemp15<-ggplot(westracks) + 
  geom_smooth(aes(x=yday(westracks$date),y=cellvalue_temp_15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Temperature", x = "", y = "ºC (15m)",colour = "ID") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) 

wtemp100<-ggplot(westracks) + 
  geom_smooth(aes(x=yday(westracks$date),y=cellvalue_temp_100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face = "bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the Year (yday)", y = "ºC (100m)",colour = "ID") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))


#plot sal + temp
wggtemp<- ggarrange(wtemp15, wtemp100,  ncol = 1, nrow = 2, common.legend = TRUE,legend = "none", labels = c("a", "b")) 

saltemw<- ggarrange(wsal15, wtemp15, wsal100, wtemp100, common.legend = TRUE, legend = "bottom", labels=c("a", "b", "c", "d"),ncol=2, nrow=2)

#png("environmental variation in tracks/westward_saltemp.png", width = 972, height = 558)
ntempsalmap<-ggarrange(saltemw, mapW, ncol=2, nrow=1, labels = c("","e"))











#id114009----


c114 <- crop(worldMap,extent(-40-mapBuffer, -1.075+mapBuffer,25-mapBuffer, 50+mapBuffer))  # based on geographical limits of oceanographical product and blueifn distribution

mainMap114 <- ggplot() + geom_polygon(data = c114, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa general




map114<- mainMap114 + geom_point(data = id114009, aes(x = lon, y = lat,  color = factor(year(id114009$date))), size = 1.2) + labs(title = "Tuna tracks South-western (SW)") + guides(colour = guide_legend(override.aes = list(size=4))) + theme(legend.title = element_text(face = "bold"), legend.position = "bottom") +labs(colour = "ID")

id114009<-subset(tenv, Deploy.ID=="114009")
id114009$days<-rep(NA, nrow(id114009))

for (i in 1:nrow(id114009)) {
  
 a=i-1
 id114009$days[i] <- a
 
}

#salinity


year(id114009$date)



sal15_114<- ggplot(id114009) + 
  geom_smooth(aes(x=id114009$days,y=cellvalue_sal_15,group=as.factor(year(id114009$date)),color=as.factor(year(id114009$date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+ labs(title = "", x = "", y = "PSU (15m)",colour = "ID") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 0))


csal100<-ggplot(d114009) + 
  geom_smooth(aes(x=yday(d114009$date),y=cellvalue_sal_100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face = "bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the Year (yday)", y = "PSU (100m)",colour = "ID") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))+theme(plot.margin = margin(t=0,b = 5))

cggsal<-ggarrange(csal15, csal100,  ncol = 1, nrow = 2, common.legend = TRUE,legend = "none", labels = c("a", "b"))

ggarrange(cggsal,mapd114009,  ncol = 2, nrow = 1, labels = c("" ,"c"))

#temperature
ctemp15<-ggplot(id114009) + 
  geom_smooth(aes(x=yday(id114009$date),y=cellvalue_temp_15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "", y = "ºC (15m)",colour = "ID") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 0))

ctemp100<-ggplot(id114009) + 
  geom_smooth(aes(x=yday(id114009$date),y=cellvalue_temp_100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face = "bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the Year (yday)", y = "ºC (100m)",colour = "ID") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))+theme(plot.margin = margin(t=0,b = 5))


#plot sal + temp
ggtemp<- ggarrange(ctemp15, ctemp100,  ncol = 1, nrow = 2, common.legend = TRUE,legend = "none", labels = c("a", "b")) 

saltemid114009<- ggarrange(csal15, ctemp15, csal100, ctemp100, common.legend = TRUE, legend = "bottom", labels=c("a", "b", "c", "d"),ncol=2, nrow=2)

#png("environmental variation in tracks/eastward_saltemp.png", width = 972, height = 558)
ntempsalmap<-ggarrange(saltemid114009, mapid114009, ncol=2, nrow=1, labels = c("","e"))
#dev.off()









#map and histogram with all tracks by groups------

cac$direction<-rep("SW", nrow(cac))
npan$direction<-rep("NA", nrow(npan))
westracks$direction<-rep("WN", nrow(westracks))

group.track<-rbind(npan,cac,westracks)

c5 <- crop(worldMap,extent(-45-mapBuffer, -1.075+mapBuffer,25-mapBuffer, 55+mapBuffer))  # based on geographical limits of oceanographical product and blueifn distribution

mainMap5 <- ggplot() + geom_polygon(data = c5, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa general

alltracksmap<-mainMap5 + geom_point(data = group.track, aes(x = lon, y = lat,  color = factor(direction)), size = 1.2) + labs(title = "Atlantic Bluefin Tuna tracks") + guides(colour = guide_legend(override.aes = list(size=4))) + theme(legend.title = element_text(face = "bold"), legend.position = "right") +labs(colour = "ID")+geom_rect(aes(xmin = -40, xmax = -6.075, ymin = 25, ymax = 50), color = "red",fill=NA, inherit.aes = FALSE)


ggarrange(alltracksmap,mapnpan,mapcac, mapW, common.legend = F, legend = "right", labels = c("a", "b", "c", "d"))


hist_sal15<- ggplot(group.track, aes(x=cellvalue_sal_15, fill=as.factor(direction))) +  geom_density(alpha=.3) + xlim(34.5,37.5) + theme_classic()+labs(title = "Salinity", x = "Salinity (PSU) at 15m", y = "Density")+labs(colour = "", fill = "")+ theme(legend.position = "top")+ theme(plot.title = element_text(hjust = 0.5))

hist_sal00<- ggplot(group.track, aes(x=cellvalue_sal_100, fill=as.factor(direction))) +  geom_density(alpha=.3) + xlim(34.5,37.5) + theme_classic()+labs(x = "Salinity (PSU) at 100m", y = "Density")+labs(colour = "", fill = "")+ theme(legend.position = "top")

hist_temp15<- ggplot(group.track, aes(x=cellvalue_temp_15, fill=as.factor(direction))) +  geom_density(alpha=.3) + xlim(10,26)  + theme_classic()+labs(title="Temperature", x = "Temp (ºC) at 15m", y = "Density")+labs(colour = "", fill = "")+ theme(legend.position = "top")+ theme(plot.title = element_text(hjust = 0.5))

hist_temp100<- ggplot(group.track, aes(x=cellvalue_temp_100, fill=as.factor(direction))) +  geom_density(alpha=.3) + xlim(10,26)  + theme_classic()+labs(x = "Temp (ºC) at 100m", y = "Density")+labs(colour = "", fill = "")+ theme(legend.position = "top")


ggarrange(hist_sal15,  hist_temp15, hist_sal00, hist_temp100, labels = c("a", "c", "b", "d"), common.legend = T, legend = "bottom")


#Variation of salinty along tuna tracks in 2010----

sal15_10<-ggplot(env_track2010) + 
  geom_smooth(aes(x=yday(env_track2010$date),y=cellvalue_sal_15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "", y = "PSU (15m)",colour = "ID") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 0))

sal100_10<-ggplot(env_track2010) + 
  geom_smooth(aes(x=yday(env_track2010$date),y=cellvalue_sal_100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face = "bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the Year (yday)", y = "PSU (100m)",colour = "ID") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))+theme(plot.margin = margin(t=0,b = 5))

ggsal<-ggarrange(sal15_10, sal100_10,  ncol = 1, nrow = 2, common.legend = TRUE,legend = "none", labels = c("a", "b"))

map10<- mainMap + geom_point(data = env_track2010, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 1.2) + labs(title = "") + guides(colour = guide_legend(override.aes = list(size=4))) + theme(legend.position = "bottom")+labs(title = NULL, colour = "ID")

ggarrange(ggsal,map10,  ncol = 2, nrow = 1, labels = c("" ,"c"))


temp15_10<- ggplot(env_track2010) + 
  geom_smooth(aes(x=yday(env_track2010$date),y=cellvalue_temp_15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "", y = "ºC (15m)",colour = "ID") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 0))

temp100_10<-ggplot(env_track2010) + 
  geom_smooth(aes(x=yday(env_track2010$date),y=cellvalue_temp_100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face = "bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the Year (yday)", y = "ºC (100m)",colour = "ID") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))+theme(plot.margin = margin(t=0,b = 5))

ggtemp<- ggarrange(temp15_10, temp100_10,  ncol = 1, nrow = 2, common.legend = TRUE,legend = "none", labels = c("a", "b")) 

saltem10<- ggarrange(sal15_10, temp15_10, sal100_10, temp100_10, common.legend = TRUE, legend = "bottom", labels=c("a", "b", "c", "d"),ncol=2, nrow=2)

png("environmental variation in tracks//2010_saltemp.png", width = 972, height = 558)

ggarrange(saltem10, map10, ncol=2, nrow=1, labels = c("","e"))

dev.off()

#draw_plot_label(label = c("A", "B", "C"), size = 15, x = c(0, 0.5, 0), y = c(1, 1, 0.5))


#histograms by groups



ggplot(tenv, aes(x=cellvalue_sal_15, fill="", color=as.factor(year))) +  geom_density(alpha=.3) + xlim(34.5,37.5) + theme_classic()+labs(x = "Salinity (PSU) at 15m", y = "Density")+labs(colour = "", fill = "")+ theme(legend.position = "top")

