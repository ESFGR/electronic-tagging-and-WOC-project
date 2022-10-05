##Author: Enrique Sánchez-Fabrés
#Anslysis of the variation of environmental variables along tuna tracks with time series plots

load("C:/Theme 2_3D currents and vertical motion for Sustainable Fisheries/woc/tracks atun rojo/.RData")

library(mapdata)
library(maptools)
library(rnaturalearth)
library(raster)
library(gdata)
library(ggpubr)
library(lubridate)

### Loading track´s environmental data----

setwd("C:/Theme 2_3D currents and vertical motion for Sustainable Fisheries/woc/tracks atun rojo/")

tenv<-read.csv("Magic_table_env_var.csv") #table with all tracks and evironmental data

#Map settings----
mapBuffer <-0.1
worldMap <- ne_download(scale = "medium", returnclass = "sp")

worldMapCroped <- crop(worldMap,extent(-30-mapBuffer, 0+mapBuffer,33-mapBuffer, 60+mapBuffer))  # based on geographical limits of oceanographical product and blueifn distribution

mainMap <- ggplot() + geom_polygon(data = worldMapCroped, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa general


#Classifying by dates beggining of migration from strait of gibraltar


#Migrations starting in May ------

id130539<-subset(tenv, Deploy.ID=="130539")
id130546<-subset(tenv, Deploy.ID=="130546")

id114008 <-subset(tenv, Deploy.ID=="114008")
id86243<-subset(tenv, Deploy.ID=="86243")
id97466<-subset(tenv, Deploy.ID=="97466")
id120088<-subset(tenv, Deploy.ID=="120088")
#id120084<-subset(tenv, Deploy.ID=="120084")
id114009<-subset(tenv, Deploy.ID=="114009")
id114009n<-subset(id114009, date<"2012-07-30")


maytracks<-rbind(id86243, id97466,id120088, id114009n, id130539, id130546, id114008)
maytracks$year<-year(maytracks$date)


###



#map
c3 <- crop(worldMap,extent(-45-mapBuffer, -1.075+mapBuffer,25-mapBuffer, 50+mapBuffer))  # based on geographical limits of oceanographical product and blueifn distribution

mainMap4 <- ggplot() + geom_polygon(data = c3, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa general

mapmaytracks<- mainMap4 + geom_point(data = maytracks, aes(x = lon, y = lat,  color = factor(Deploy.ID), shape=factor(maytracks$year)), size = 1.2) + labs(title = "Tracks starting in MAY") + guides(colour = guide_legend(override.aes = list(size=4))) + theme(legend.title = element_text(face = "bold"), legend.position = "bottom") +labs(colour = "ID") + theme(legend.position = "right") +labs(shape = "Year")




#salinity
csal15<- ggplot(maytracks) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_sal_15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Sal. in tracks starting in May", x = "", y = "PSU (15m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) + theme(plot.title = element_text(hjust = 0.5)) +labs(x = NULL) 


csal100<- ggplot(maytracks) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_sal_100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face = "bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the Year (yday)", y = "PSU (100m)",colour = "ID") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))

cggsal<-ggarrange(csal15, csal100,  ncol = 1, nrow = 2, common.legend = TRUE, legend = "bottom", labels = c("a", "b"))

ggarrange(cggsal,mapmaytracks,  ncol = 2, nrow = 1, labels = c("" ,"c"))

#temperature
ctemp15<-ggplot(maytracks) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_temp_15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Temp. in tracks starting May", x = "", y = "ºC (15m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) + theme(plot.title = element_text(hjust = 0.5)) +labs(x = NULL) 

ctemp100<-ggplot(maytracks) + 
  geom_smooth(aes(x=yday(maytracks$date),y=cellvalue_temp_100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face = "bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the Year (yday)", y = "ºC (100m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))+theme(plot.margin = margin(t=0,b = 5))


#plot sal + temp
ggtemp<- ggarrange(ctemp15, ctemp100,  ncol = 1, nrow = 2, common.legend = TRUE,legend = "bottom", labels = c("a", "b")) 

saltemmaytracks <- ggarrange(csal15, ctemp15, csal100, ctemp100, common.legend = TRUE, legend = "right", labels=c("a", "b", "c", "d"),ncol=2, nrow=2)

#png("environmental variation in tracks/westward_saltemp.png", width = 972, height = 558)
ctempsalmap<- ggarrange(saltemmaytracks, mapmaytracks, ncol=2, nrow=1, labels = c("","e"))
#dev.off()


annotate_figure(saltemmaytracks, top = text_grob("South-Western tracks (SW)", color = "black", face = "bold", size = 15))

#density

cden0 <- ggplot(maytracks) + 
  geom_smooth(aes(x=yday(maytracks$date),y=cellvalue_den0,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Dens. may tracks", x = "Day of the year (Yday)", y = "Density (kg/m3)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) + theme(plot.title = element_text(size = 10, face="bold"))

cdengrad0 <-ggplot(maytracks) + 
  geom_smooth(aes(x=yday(maytracks$date),y=maytracks$cellvalue_grad_den0,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Grad. density May", x = "Day of the Year (Yday)", y = "Grad. density",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))

ggarrange(cden0, cdengrad0, labels = c("a","b"), common.legend = T, legend = "bottom")




cw100 <- ggplot(maytracks) + 
  geom_smooth(aes(x=yday(maytracks$date),y=cellvalue_w100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Vertical velocity at 100m of May tracks", x = "Day of the Year", y = "Vertical velocity (m/s)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 1))


#gradientsss
#salinity grad
cgradsal15<- ggplot(maytracks) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_grad_sal15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "", y = "Grad. salinity (15m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 0)) +labs(title = "Sal. gradient May", x = NULL) + theme(plot.title = element_text(size = 13, hjust = 0.5, face="bold"))


cgradsal100<- ggplot(maytracks) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_grad_sal100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the Year (Yday)", y = "Grad. salinity (100m)",colour = "ID",  linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))


cgradtemp15<- ggplot(maytracks) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_grad_temp15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "", y = "Grad. temp (15m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 0))+labs(title = "Temp. gradient May", x = NULL) + theme(plot.title = element_text(size = 13, hjust = 0.5, face="bold"))



cgradtemp100<- ggplot(maytracks) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_grad_temp100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the year (Yday)", y = "Grad. temp (100m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 0))

gradientsmaytracks <-ggarrange(cgradsal15,cgradtemp15,cgradsal100, cgradtemp100, common.legend = T, legend = "right", labels = c("a", "c", "b", "d"))

ggarrange(gradientsmaytracks, mapmaytracks, labels = c("","e"))



annotate_figure(gradientsmaytracks, top = text_grob("may tracks", color = "black", face = "bold", size = 15))


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


#Migrations starting in July = North Iberic peninsula (NP), North peninsula + north atlantic (NPAN) ----
#Tracks: 

id61125<-subset(tenv,Deploy.ID=="61125")
id61127<-subset(tenv,Deploy.ID=="61127")
id72499<-subset(tenv,Deploy.ID=="72499")
id72498<-subset(tenv,Deploy.ID=="72498")
id86438<-subset(tenv,Deploy.ID=="86438")
id86440<-subset(tenv,Deploy.ID=="86440")
id97462<-subset(tenv,Deploy.ID=="97462") ## not included, locations too spread in time and space
id97462n <-subset(id97462, lon>-20)
id114006 <-subset(tenv, Deploy.ID=="114006")
mainMap4 + geom_point(data = id114006, aes(x = lon, y = lat, color=as.factor(month(date))))
id114006<-subset(id114006, date>"2012-05-30") #the first location at may corresponds to the entry to Mediterranean




npan<-rbind(id61125, id61127,id72499, id72498, id86438, id86440, id114006)

c2 <- crop(worldMap,extent(-20-mapBuffer, -1.075+mapBuffer,30-mapBuffer, 50+mapBuffer))  # based on geographical limits of oceanographical product and blueifn distribution

mainMap3 <- ggplot() + geom_polygon(data = c3, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa general

npan$year<-year(npan$date)

mapnpan <- mainMap3 + geom_point(data = npan, aes(x = lon, y = lat,  color = factor(Deploy.ID), shape=factor(year)), size = 1.2) + labs(title = "Tracks starting in July")  +geom_vline(xintercept = -6, color = "red",fill=NA, inherit.aes = FALSE) + guides(colour = guide_legend(override.aes = list(size=4))) + theme(legend.title = element_text(face = "bold")) +labs(colour = "ID")+theme(legend.position = "right")+labs(shape = "Year")


#salinity
nsal15<- ggplot(npan) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_sal_15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Sal. in tracks from July", x = "", y = "PSU (15m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 0))+ theme(plot.title = element_text(size = 13, hjust = 0.5, face="bold")) 




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
  labs(title = "Temp. in tracks from July", x = "", y = "ºC (15m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 0)) + theme(plot.title = element_text(size = 13, hjust = 0.5, face="bold"))

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
  labs(title = "Density tracks July (surface)", x = "Day of the Year", y = "Density (kg/m3)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) + theme(plot.title = element_text(size = 10, face="bold"))

ngraden<- ggplot(npan) + 
  geom_smooth(aes(x=yday(npan$date),y=npan$cellvalue_grad_den0,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Density July track", x = "Day of the Year", y = "Grad. Dens.",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) 


ndenisty <- ggarrange(nden0, ngraden, common.legend = T, legend = "bottom")
#png("environmental variation in tracks/DensityNA.png", width = 972, height = 558)
#dev.off()

#vertical velocity


nw100<- ggplot(npan) + 
  geom_smooth(aes(x=yday(npan$date),y=cellvalue_w100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Vertical velocity July tracks", x = "Day of the Year", y = "Vertical velocity (m/s)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) 

#gradients

#salinity grad
ngradsal15<- ggplot(npan) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_grad_sal15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Sal. gradient July", x = "", y = "Grad. sal. (15m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))  + theme(plot.title = element_text(size = 13, hjust = 0.5, face="bold"))

ngradsal100<- ggplot(npan) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_grad_sal100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the year (Yday)", y = "Grad. sal. (100m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) 


ngradtemp15<-ggplot(npan) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_grad_temp15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Temp. gradient July", x = "", y = "Grad. temp. (15m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) + theme(plot.title = element_text(size = 13, hjust = 0.5, face="bold"))

ngradtemp100<- ggplot(npan) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_grad_temp100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the year (Yday)", y = "Grad. temp. (100m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) 

gradientsnpan <-ggarrange(ngradsal15,ngradtemp15,ngradsal100, ngradtemp100, common.legend = T, legend = "right", labels = c("a", "c", "b", "d"))

ggarrange(gradientsnpan, mapnpan, labels = c("", "e"))


annotate_figure(gradientsnpan, top = text_grob("North Atlantic tracks (NA)", color = "black", face = "bold", size = 15))


#By year --------


#2011-----
tenv$year<-year(tenv$date)

tenv2011 <-subset(tenv, year=="2011")
tenv2011<-subset(tenv2011, !Deploy.ID=="97462")

map2011 <- mainMap3 + geom_point(data = tenv2011, aes(x = lon, y = lat,  color = factor(Deploy.ID))) + labs(title = "Tracks 2011") + geom_vline(xintercept = -6, color = "red",fill=NA, inherit.aes = FALSE) + guides(colour = guide_legend(override.aes = list(size=4))) + theme(legend.title = element_text(face = "bold")) +labs(colour = "ID")+theme(legend.position = "right")+labs(shape = "Year")

#salinity
sal15_11<- ggplot(tenv2011) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_sal_15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Sal. in tracks from 2011", x = "", y = "PSU (15m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 0))+ theme(plot.title = element_text(size = 13, hjust = 0.5, face="bold")) 


sal100_11<-ggplot(tenv2011) + 
  geom_smooth(aes(x=yday(tenv2011$date),y=cellvalue_sal_100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face = "bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the Year (yday)", y = "PSU (100m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))+theme(plot.margin = margin(t=0,b = 5))

ggsal<-ggarrange(nsal15, nsal100,  ncol = 1, nrow = 2, common.legend = TRUE,legend = "bottom", labels = c("a", "b")) + theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 10)) +labs(title = "")

ggarrange(ggsal,maptenv2011,  ncol = 2, nrow = 1, labels = c("" ,"c"))

#temperature
temp15_11<- ggplot(tenv2011) + 
  geom_smooth(aes(x=yday(tenv2011$date),y=cellvalue_temp_15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Temp. in tracks from 2011", x = "", y = "ºC (15m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 0)) + theme(plot.title = element_text(size = 13, hjust = 0.5, face="bold"))

temp100_11<- ggplot(tenv2011) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_temp_100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face = "bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the Year (yday)", y = "ºC (100m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))



saltem2011 <- ggarrange( sal15_11, temp15_11, sal100_11, temp100_11, common.legend = TRUE, legend = "right", labels=c("a", "c", "b", "d"),ncol=2, nrow=2)


#2010----
tenv2010 <-subset(tenv, year=="2010")


map2010 <- mainMap3 + geom_point(data = tenv2010, aes(x = lon, y = lat,  color = factor(Deploy.ID))) + labs(title = "Tracks 2010") + geom_vline(xintercept = -6, color = "red",fill=NA, inherit.aes = FALSE) + guides(colour = guide_legend(override.aes = list(size=4))) + theme(legend.title = element_text(face = "bold")) +labs(colour = "ID")+theme(legend.position = "right")+labs(shape = "Year")

#salinity
sal15_10<- ggplot(tenv2010) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_sal_15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Sal. in tracks from 2010", x = "", y = "PSU (15m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 0))+ theme(plot.title = element_text(size = 13, hjust = 0.5, face="bold")) 


sal100_10<-ggplot(tenv2010) + 
  geom_smooth(aes(x=yday(tenv2010$date),y=cellvalue_sal_100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face = "bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the Year (yday)", y = "PSU (100m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))+theme(plot.margin = margin(t=0,b = 5))

ggsal<-ggarrange(nsal15, nsal100,  ncol = 1, nrow = 2, common.legend = TRUE,legend = "bottom", labels = c("a", "b")) + theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 10)) +labs(title = "")

ggarrange(ggsal,maptenv2010,  ncol = 2, nrow = 1, labels = c("" ,"c"))

#temperature
temp15_10<- ggplot(tenv2010) + 
  geom_smooth(aes(x=yday(tenv2010$date),y=cellvalue_temp_15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Temp. in tracks from 2010", x = "", y = "ºC (15m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 0)) + theme(plot.title = element_text(size = 13, hjust = 0.5, face="bold"))

temp100_10<- ggplot(tenv2010) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_temp_100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face = "bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the Year (yday)", y = "ºC (100m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))



saltem2010 <- ggarrange( sal15_10, temp15_10, sal100_10, temp100_10, common.legend = TRUE, legend = "right", labels=c("a", "c", "b", "d"),ncol=2, nrow=2)

#2012----

maytracks12<- subset(maytracks, year=="2012")
npan12<- subset(npan, year=="2012")

tenv2012 <- rbind(maytracks12, npan12)


map2012 <- mainMap3 + geom_point(data = tenv2012, aes(x = lon, y = lat,  color = factor(Deploy.ID))) + labs(title = "Tracks 2012") + geom_vline(xintercept = -6, color = "red",fill=NA, inherit.aes = FALSE) + guides(colour = guide_legend(override.aes = list(size=4))) + theme(legend.title = element_text(face = "bold")) +labs(colour = "ID")+theme(legend.position = "right")+labs(shape = "Year")

#salinity
sal15_12<- ggplot(tenv2012) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_sal_15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Sal. in tracks from 2012", x = "", y = "PSU (15m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 0))+ theme(plot.title = element_text(size = 13, hjust = 0.5, face="bold")) 


sal100_12<-ggplot(tenv2012) + 
  geom_smooth(aes(x=yday(tenv2012$date),y=cellvalue_sal_100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face = "bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the Year (yday)", y = "PSU (100m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))+theme(plot.margin = margin(t=0,b = 5))


#temperature
temp15_12<- ggplot(tenv2012) + 
  geom_smooth(aes(x=yday(tenv2012$date),y=cellvalue_temp_15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"), legend.title = element_text(face ="bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "Temp. in tracks from 2012", x = "", y = "ºC (15m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid")) +theme(plot.margin = margin(b = 0)) + theme(plot.title = element_text(size = 13, hjust = 0.5, face="bold"))

temp100_12<- ggplot(tenv2012) + 
  geom_smooth(aes(x=yday(date),y=cellvalue_temp_100,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID), linetype = as.factor(year(date)))) + 
  theme(panel.grid.major = element_line(colour = "gray97",size = 0.6), panel.grid.minor = element_line(colour = "gray97"),plot.title = element_text(face = "bold"), legend.title = element_text(face = "bold"), panel.background = element_rect(fill = "gray98",size = 0))+
  labs(title = "", x = "Day of the Year (yday)", y = "ºC (100m)",colour = "ID", linetype = "Year") + theme(axis.line = element_line(size = 0.6, linetype = "solid"), axis.ticks = element_line(colour = "gray14", size = 0.7), panel.background = element_rect(fill = "gray99", linetype = "solid"))



saltem2012 <- ggarrange( sal15_12, temp15_12, sal100_12, temp100_12, common.legend = TRUE, legend = "right", labels=c("a", "c", "b", "d"),ncol=2, nrow=2)




#map and histogram with all tracks by groups------

maytracks$Month<-rep("May", nrow(maytracks))

npan$year<-year(npan$date)
npan$Month<-rep("July", nrow(npan))


group.track<-rbind(npan,maytracks)

c5 <- crop(worldMap,extent(-45-mapBuffer, -1.075+mapBuffer,25-mapBuffer, 55+mapBuffer))  # based on geographical limits of oceanographical product and blueifn distribution

mainMap5 <- ggplot() + geom_polygon(data = c5, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa general

group.track$month<-month(group.track$date)

alltracksmap1 <-mainMap5 + geom_point(data = group.track, aes(x = lon, y = lat,  color = factor(month), shape=factor(year)), size = 1.2) + labs(title = "Atlantic Bluefin Tuna tracks by month") + guides(colour = guide_legend(override.aes = list(size=4))) + theme(legend.title = element_text(face = "bold"), legend.position = "right") +labs(colour = "Month")+geom_rect(aes(xmin = -40, xmax = -6.075, ymin = 25, ymax = 50), color = "red",fill=NA, inherit.aes = FALSE)+labs(shape = "Year")

alltracksmap <-mainMap5 + geom_point(data = group.track, aes(x = lon, y = lat,  color = factor(Month), shape=factor(year)), size = 1.2) + labs(title = "Atlantic Bluefin Tuna tracks by month") + guides(colour = guide_legend(override.aes = list(size=4))) + theme(legend.title = element_text(face = "bold"), legend.position = "right") +labs(colour = "Month")+geom_rect(aes(xmin = -40, xmax = -6.075, ymin = 25, ymax = 50), color = "red",fill=NA, inherit.aes = FALSE)+labs(shape = "Year")

#temperature and salinity
hist_sal15<- ggplot(group.track, aes(x=cellvalue_sal_15, fill=as.factor(Month))) +  geom_density(alpha=.3) + xlim(34.5,37.5) + theme_classic()+labs(title = "Salinity", x = "Salinity (PSU) at 15m", y = "Density")+labs(colour = "", fill = "")+ theme(legend.position = "top")+ theme(plot.title = element_text(hjust = 0.5))

hist_sal00<- ggplot(group.track, aes(x=cellvalue_sal_100, fill=as.factor(Month))) +  geom_density(alpha=.3) + xlim(34.5,37.5) + theme_classic()+labs(x = "Salinity (PSU) at 100m", y = "Density")+labs(colour = "", fill = "")+ theme(legend.position = "top")

hist_temp15<- ggplot(group.track, aes(x=cellvalue_temp_15, fill=as.factor(Month))) +  geom_density(alpha=.3) + xlim(10,26)  + theme_classic()+labs(title="Temperature", x = "Temp (ºC) at 15m", y = "Density")+labs(colour = "", fill = "")+ theme(legend.position = "top")+ theme(plot.title = element_text(hjust = 0.5))

hist_temp100<- ggplot(group.track, aes(x=cellvalue_temp_100, fill=as.factor(Month))) +  geom_density(alpha=.3) + xlim(10,26)  + theme_classic()+labs(x = "Temp (ºC) at 100m", y = "Density")+labs(colour = "", fill = "")+ theme(legend.position = "top")


ggarrange(hist_sal15,  hist_temp15, hist_sal00, hist_temp100, labels = c("a", "c", "b", "d"), common.legend = T, legend = "bottom")

#density

hist_den <- ggplot(group.track, aes(x=group.track$cellvalue_den0, fill=as.factor(Month))) +  geom_density(alpha=.3) + xlim(1.0240,1.0270) + theme_classic()+labs(title = "Water density", x = "surface water denisty (Hg/m3)", y = "Density")+labs(colour = "", fill = "")+ theme(legend.position = "right")+ theme(plot.title = element_text(hjust = 0.5))

#id114006--------

mainMap3 + geom_point(data = id114006, aes(x = lon, y = lat,  color = factor(month)), size = 1.2) + labs(title = "Track 114006") + guides(colour = guide_legend(override.aes = list(size=4))) + theme(legend.title = element_text(face = "bold"), legend.position = "bottom") +labs(colour = "Month") + theme(legend.position = "right") +labs(shape = "Year")


id114006$month<- month(id114006$date)

#temperature and salinity
id114006sal15<- ggplot(id114006, aes(x=cellvalue_sal_15, fill=as.factor(id114006$month))) +  geom_density(alpha=.3) + xlim(34.5,37.5) + theme_classic()+labs(title = "Salinity", x = "Salinity (PSU) at 15m", y = "Density")+labs(colour = "", fill = "")+ theme(legend.position = "top")+ theme(plot.title = element_text(hjust = 0.5))

id114006sal00<- ggplot(id114006, aes(x=cellvalue_sal_100, fill=as.factor(month))) +  geom_density(alpha=.3) + xlim(34.5,37.5) + theme_classic()+labs(x = "Salinity (PSU) at 100m", y = "Density")+labs(colour = "", fill = "")+ theme(legend.position = "top")

id114006temp15<- ggplot(id114006, aes(x=cellvalue_temp_15, fill=as.factor(month))) +  geom_density(alpha=.3) + xlim(10,26)  + theme_classic()+labs(title="Temperature", x = "Temp (ºC) at 15m", y = "Density")+labs(colour = "", fill = "")+ theme(legend.position = "top")+ theme(plot.title = element_text(hjust = 0.5))

id114006temp100<- ggplot(id114006, aes(x=cellvalue_temp_100, fill=as.factor(month))) +  geom_density(alpha=.3) + xlim(10,26)  + theme_classic()+labs(x = "Temp (ºC) at 100m", y = "Density")+labs(colour = "", fill = "")+ theme(legend.position = "top")


ggarrange(id114006sal15,  id114006temp15, id114006sal00, id114006temp100, labels = c("a", "c", "b", "d"), common.legend = T, legend = "bottom")

