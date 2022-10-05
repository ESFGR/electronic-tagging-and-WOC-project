library(mapdata)
library(maptools)
library(rnaturalearth)
library(raster)
library(gdata)
#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/ggpubr")
#install.packages("rlang")
library(ggpubr)
library(lubridate)


### MAPs and Histograms of environmental variables for locations of Atlantic Bluefin Tuna

setwd("C:/Theme 2_3D currents and vertical motion for Sustainable Fisheries/woc/tracks atun rojo/")

### Colours----
c1 <- rgb(216,238,192, max = 255, alpha = 120, names = "lt.green") #light colours for histogram
c2 <- rgb(255,100,100, max = 255, alpha = 100, names = "lt.red")
c3 <- rgb(0, 100, 255, max=255, alpha = 100, names = "lt.blue")
c4 <- rgb(100, 0, 100, max=255, alpha = 100, names = "lt.purple")

####----
env_track2010<-read.csv("2010_env_tracks_2504.csv")
#env_track2010<-env_track2010[,2:16]

env_track2011<-read.csv("2011_env_tracks_2504.csv")
table(env_track2011$Deploy.ID)

env_track2012<-read.csv("2012_env_tracks_2504.csv")
table(env_track2012$Deploy.ID)

env_track2013<-read.csv("2013_env_tracks_2504.csv")
table(env_track2013$Deploy.ID)

ggplot(env_track2010) + 
  geom_smooth(aes(x=yday(env_track2010$date),y=cellvalue_sal_15,group=as.factor(Deploy.ID),color=as.factor(Deploy.ID))) + theme(panel.grid.major = element_line(colour = "gray97", 
    size = 0.6), panel.grid.minor = element_line(colour = "gray97"), 
    plot.title = element_text(face = "bold"), 
    legend.title = element_text(face = "bold"), 
    panel.background = element_rect(fill = "gray98", 
        size = 0)) +labs(title = "2010 Salinity variation along tuna tracks", 
    x = "Day of the Year (yday)", y = "Salininty at 15m", 
    colour = "ID") + theme(axis.line = element_line(size = 0.6, 
    linetype = "solid"), axis.ticks = element_line(colour = "gray14", 
    size = 0.7), panel.background = element_rect(fill = "gray99", 
    linetype = "solid"))

#Map settings----
mapBuffer <-0.1
worldMap <- ne_download(scale = "medium", returnclass = "sp")

worldMapCroped <- crop(worldMap,extent(-40-mapBuffer, +20+mapBuffer,25-mapBuffer, 65+mapBuffer))  # based on geographical limits of oceanographical product and blueifn distribution

mainMap <- ggplot() + geom_polygon(data = worldMapCroped, aes(x = long, y = lat, group = group)) + coord_fixed() + theme(axis.ticks=element_blank()) + ylab("Latitude") + xlab("Longitude")#mapa general

#Map in and out 

mtlt<-read.csv("C:/Theme 2_3D currents and vertical motion for Sustainable Fisheries/woc/tracks atun rojo/Magic table long tracks Bluefin Tuna.csv")
mtlt<-subset(mtlt, !Year=="2009")
mt<-read.csv("Magic Table Tuna Tracks.csv")



#--------
a<-ggplot(env_track2010, aes(x=cellvalue_sal_15)) + 
  geom_histogram(aes(y=..density..), fill="gray")+ xlim(35,36.7) +
  geom_density(alpha=.2, fill=c2) +
  scale_color_grey() + theme_classic() +labs(x = "Salinity (PSU) at 15m", y = "Density")

b<-ggplot(env_track2010, aes(x=cellvalue_sal_100)) + 
  geom_histogram(aes(y=..density..), fill="gray")+ xlim(35,36.7) +
  geom_density(alpha=.2, fill=c2) + 
  scale_color_grey() + theme_classic() +labs(x = "Salinity (PSU) at 100m", y = "Density")


c<-ggplot(env_track2010, aes(x=cellvalue_temp_15)) + 
  geom_histogram(aes(y=..density..), fill="gray")+ xlim(284,298)+
  geom_density(alpha=.2, fill=c4) + 
  scale_color_grey() + theme_classic() +labs(x = "water temperature (ºK) at 15m", y = "Density")

d<-ggplot(env_track2010, aes(x=cellvalue_temp_100)) + 
  geom_histogram(aes(y=..density..),bins = 25, fill="gray")+ xlim(284,298)  + 
  geom_density(alpha=.2, fill=c4) + 
  scale_color_grey() + theme_classic() +labs(x = "water temperature (ºK) at 100m", y = "Density")

u15<-ggplot(env_track2010, aes(x=cellvalue_u15)) + 
  geom_histogram(aes(y=..density..),bins = 25, fill="gray")+
  geom_density(alpha=.2, fill=c4) + 
  scale_color_grey() + theme_classic() +labs(x = "Eastward velocity (m/s) at 15m", y = "Density")

u100<-ggplot(env_track2010, aes(x=cellvalue_u100)) + 
  geom_histogram(aes(y=..density..),bins = 25, fill="gray")+
  geom_density(alpha=.2, fill=c4) + 
  scale_color_grey() + theme_classic() +labs(x = "Eastward velocity (m/s) at 100m", y = "Density")


v15<-ggplot(env_track2010, aes(x=cellvalue_v15)) + 
  geom_histogram(aes(y=..density..),bins = 25, fill="gray")+
  geom_density(alpha=.2, fill=c1) + xlim(-0.3,0.3) +
  scale_color_grey() + theme_classic() +labs(x = "Northward velocity (m/s) at 15m", y = "Density")


v100<-ggplot(env_track2010, aes(x=cellvalue_v100)) + 
  geom_histogram(aes(y=..density..),bins = 25, fill="gray")+
  geom_density(alpha=.2, fill=c1) + xlim(-0.3,0.3) +
  scale_color_grey() + theme_classic() +labs(x = "Northward velocity (m/s) at 100m", y = "Density")


w100<-ggplot(env_track2010, aes(x=cellvalue_w100)) + 
  geom_histogram(aes(y=..density..),bins = 25, fill="gray")+
  geom_density(alpha=.2, fill=c3) +
  scale_color_grey() + theme_classic() +labs(x = "Vertical velocity (m/s) at 100m", y = "Density")

#ggplot(data=env_track2010) + geom_histogram(aes(x=cellvalue_temp_15, y=..density..), fill="gray")+ xlim(284,298) + geom_density(aes(x=cellvalue_temp_15),alpha=.2, fill=c4) + geom_histogram(aes(x=cellvalue_temp_100, y=..density..), fill="gray")+ xlim(284,298) + geom_density(aes(x=cellvalue_temp_100),alpha=.2, fill=c2) + scale_color_grey() + theme_classic() +labs(x = "water temperature (ºK)", y = "Density")+  scale_color_manual(name='Depth',breaks=c('15m', '100m'), values=c('15m'=c4, '100m'=c2))
  
  
a1<-ggarrange(a, b, labels = c("A", "B"), ncol = 1, nrow = 2)
a2<-ggarrange(c, d,labels = c("C", "D"), ncol = 1, nrow = 2)


figab<-annotate_figure(a1, left = text_grob("Salinity",face = "bold", rot = 90),right = "", fig.lab = "")


figcd<-annotate_figure(a2,left = text_grob("Temperature", face = "bold",rot = 90), right = "", fig.lab = "")

figure<-ggarrange(figab, figcd, ncol = 2, nrow = 1)

#annotate_figure(figure, top = text_grob("",  face = "bold", size = 14),left = text_grob("", rot = 90), right = "", fig.lab = "")

u15100<-ggarrange(u15, u100, labels = c("A", "B"), ncol = 2, nrow = 1)

u15100<-annotate_figure(u15100, top = text_grob("", size = 14),right = text_grob("", rot = -90), fig.lab = "")


v15100<- ggarrange(v15, v100,labels = c("C", "D"), ncol = 2, nrow = 1)

v15100<-annotate_figure(v15100, top = text_grob("", size = 14), right = text_grob("", rot = -90),  fig.lab = "")


w100<-ggarrange(w100, labels = c("E"), ncol = 2, nrow = 1)
w100<-annotate_figure(w100, top = text_grob("", size = 14), right = text_grob("", rot = -90), fig.lab = "")


vuw<-ggarrange(u15100,v15100,w100,ncol=1, nrow=3)

annotate_figure(vuw, 
                top = text_grob("",  face = "bold", size = 14),
                left = text_grob("", face = "bold",rot = 90), right = "", fig.lab = "")


####------

#todos los tracks de salida en 1 histograma y todos juntos con colores por año.

env_track2010<-env_track2010[,2:16]
env_track2012<-env_track2012[,1:15]

tenv<-rbind(env_track2010, env_track2011, env_track2012, env_track2013)
tenv$year<-year(tenv$date)
tenv

#ggplot(tenv, aes(x=cellvalue_sal_15, fill=as.factor(tenv$year),color=as.factor(tenv$year))) + geom_histogram(aes(y=..density..),alpha=.2)+ xlim(35,38) + scale_color_grey() + theme_classic()+labs(x = "Salinity (PSU) at 15m", y = "Density")

#ggplot()+geom_histogram(data=env_track2010, aes(x=cellvalue_sal_15, y=..density.., fill="2010"),alpha=.3) + geom_histogram(data=env_track2011, aes(x=cellvalue_sal_15, y=..density..,fill= "2011"),alpha=.3) + geom_histogram(data=env_track2012, aes(x=cellvalue_sal_15, y=..density..,fill= "2012"),alpha=.3) + geom_histogram(data=env_track2013, aes(x=cellvalue_sal_15, y=..density..,fill= "2013"),alpha=.3)

#density plots for all dataset


all_plotsal15<-ggplot(tenv, aes(x=cellvalue_sal_15)) +  geom_density(alpha=.3, fill="blue") + xlim(34.5,37.5) + theme_classic()+labs(x = "Salinity (PSU) at 15m", y = "")+labs(colour = "", fill = "")+ theme(legend.position = "top")
all_plotsal100<-ggplot(tenv, aes(x=cellvalue_sal_100)) +  geom_density(alpha=.3, fill="blue") + xlim(34.5,37.5) + theme_classic()+labs(x = "Salinity (PSU) at 100m", y = "")+labs(colour = "", fill = "")+ theme(legend.position = "top")

a1<-ggarrange(all_plotsal15, all_plotsal100, labels = c("A", "B"), ncol = 1, nrow = 2)


all_plottemp15<-ggplot(tenv, aes(x=cellvalue_temp_15)) +  geom_density(alpha=.3, fill="red") + xlim(282.5,298.5) + theme_classic()+labs(x = "temperature (ºK) at 15m", y = "")+labs(colour = "", fill = "")+ theme(legend.position = "top")
all_plottemp100<-ggplot(tenv, aes(x=cellvalue_temp_100)) +  geom_density(alpha=.3, fill="red") + xlim(282.5,298.5)+ theme_classic()+labs(x = "temperature (ºK) at 100m", y = "")+labs(colour = "", fill = "")+ theme(legend.position = "top")

a2<-ggarrange(all_plottemp15, all_plottemp100, labels = c("C", "D"), ncol = 1, nrow = 2)


require(gridExtra)
library(gridExtra)
grid.arrange(a1, a2, ncol=2)

ggplot(tenv, aes(x=cellvalue_sal_15, fill="", color=as.factor(year))) +  geom_density(alpha=.3) + xlim(34.5,37.5) + theme_classic()+labs(x = "Salinity (PSU) at 15m", y = "Density")+labs(colour = "", fill = "")+ theme(legend.position = "top")


#Yearly salinity for all tuna positions
plotsal15<- ggplot(tenv, aes(x=cellvalue_sal_15, fill=as.factor(year),color=as.factor(year))) +  geom_density(alpha=.3) + xlim(34.5,37.5) + theme_classic()+labs(x = "Salinity (PSU) at 15m", y = "")+labs(colour = "", fill = "")+ theme(legend.position = "top")

plotsal100<-ggplot(tenv, aes(x=cellvalue_sal_100, fill=as.factor(tenv$year),color=as.factor(tenv$year))) +  geom_density(alpha=.3) + xlim(34.5,37.5) + theme_classic()+labs(x = "Salinity (PSU) at 100m", y = "")+labs(colour = "", fill = "")+ theme(legend.position = "none")

histsal<-ggarrange(plotsal15, plotsal100, labels = c("A", "B"), ncol = 1, nrow = 2)

#Yearly temperature for all tuna positions
plottemp15<-ggplot(tenv, aes(x=cellvalue_temp_15, fill=as.factor(tenv$year),color=as.factor(tenv$year))) +  geom_density(alpha=.3) + xlim(282.5,298.5) + theme_classic()+labs(x = "Temperature (ºK) at 15m", y = "")+labs(colour = "", fill = "")+ theme(legend.position = "top")

plottemp100<-ggplot(tenv, aes(x=cellvalue_temp_100, fill=as.factor(tenv$year),color=as.factor(tenv$year))) +  geom_density(alpha=.3) + xlim(282.5,298.5) + theme_classic()+labs(x = "Temperature (ºK) at 100m", y = "")+labs(colour = "", fill = "")+ theme(legend.position = "none")

histemp<-ggarrange(plottemp15, plottemp100, labels = c("A", "B"), ncol = 1, nrow = 2)


#Salinity by years----

#2010
mt10<-subset(mt, Year=="2010")

mainMap + geom_point(data = mt10, aes(x = Lon, y = Lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2010") + labs(colour = "Deploy.ID", size = 12)+geom_rect(aes(xmin = -40, xmax = -6.075, ymin = 25, ymax = 50), color = "red",fill=NA, inherit.aes = FALSE)

sal15_2010<-ggplot(env_track2010, aes(x=cellvalue_sal_15, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3) + xlim(35,36.5) + theme_classic()+labs(x = "Salinity (PSU) at 15m", y = "") + theme(plot.title = element_text(face = "bold")) +labs(title = 2010) + theme(legend.position = "none")  + scale_fill_brewer(palette="Set1")


sal100_2010<-ggplot(env_track2010, aes(x=cellvalue_sal_100, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3) + xlim(35,36.5) + theme_classic()+labs(x = "Salinity (PSU) at 100m", y = "")+labs(colour = "ID", fill = "ID") + theme(legend.position = "bottom")  + theme(legend.title = element_text(face = "bold")) + scale_fill_brewer(palette="Set1") +
  theme(legend.key.size = unit(4, 'mm'))

sal2010<-ggarrange(sal15_2010, sal100_2010,labels = c("A", "B"), ncol = 1, nrow = 2)

sal2010

#2011

mainMap + geom_point(data = env_track2011, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2011") + labs(colour = "Deploy.ID", size = 12) 

sal15_2011<-ggplot(env_track2011, aes(x=cellvalue_sal_15, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3) + xlim(35,37.5) + theme_classic()+labs(x = "Salinity (PSU) at 15m", y = "")+ theme(plot.title = element_text(face = "bold")) +labs(title = 2011) + theme(legend.position = "none") + scale_fill_brewer(palette="Set1")

sal100_2011<-ggplot(env_track2011, aes(x=cellvalue_sal_100, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3) + xlim(35,37.5) + theme_classic()+labs(x = "Salinity (PSU) at 100m", y = "")+labs(colour = "ID", fill = "ID") + theme(legend.position = "bottom")  + theme(legend.title = element_text(face = "bold")) + scale_fill_brewer(palette="Set1") +
  theme(legend.key.size = unit(4, 'mm'))

sal2011<-ggarrange(sal15_2011, sal100_2011,labels = c("C", "D"), ncol = 1, nrow = 2)

sal2011


#2012

mainMap + geom_point(data = env_track2012, aes(x = lon, y = lat,  colour = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2012") + labs(colour = "Deploy.ID", size = 12) + scale_fill_brewer(palette="Set1")

sal15_2012<-ggplot(env_track2012, aes(x=cellvalue_sal_15, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3) + xlim(35,37.5) + theme_classic()+labs(x = "Salinity (PSU) at 15m", y = "")+ theme(plot.title = element_text(face = "bold")) +labs(title = 2012) + theme(legend.position = "none") + scale_fill_brewer(palette="Set1")

sal100_2012<-ggplot(env_track2012, aes(x=cellvalue_sal_100, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3) + xlim(35,37.5) + theme_classic()+labs(x = "Salinity (PSU) at 100m", y = "")+labs(colour = "ID", fill = "ID") + theme(legend.position = "bottom")  + theme(legend.title = element_text(face = "bold")) + scale_fill_brewer(palette="Set1") +
  theme(legend.key.size = unit(4, 'mm'))

sal2012<-ggarrange(sal15_2012, sal100_2012,labels = c("E", "F"), ncol = 1, nrow = 2)

sal2012

#2013

map2013<-mainMap + geom_point(data = env_track2013, aes(x = lon, y = lat,  colour = factor(Deploy.ID)), size = 1.8) + labs(title = "Tuna tracks 2013") + labs(colour = "Deploy.ID", size = 12) + scale_fill_brewer(palette="Set1") + theme(legend.position = "none")

sal15_2013<-ggplot(env_track2013, aes(x=cellvalue_sal_15, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3) + xlim(35,37.5) + theme_classic()+labs(x = "Salinity (PSU) at 15m", y = "")+ theme(plot.title = element_text(face = "bold")) +labs(title = 2013) + theme(legend.position = "none") + scale_fill_brewer(palette="Set1")

sal100_2013<-ggplot(env_track2013, aes(x=cellvalue_sal_100, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3) + xlim(35,37.5) + theme_classic()+labs(x = "Salinity (PSU) at 100m", y = "")+labs(colour = "ID", fill = "ID") + theme(legend.position = "bottom")  + theme(legend.title = element_text(face = "bold")) + scale_fill_brewer(palette="Set1") +
  theme(legend.key.size = unit(4, 'mm'))

sal2013<-ggarrange(sal15_2013, sal100_2013,labels = c("G", "H"), ncol = 1, nrow = 2)

sal2013

ggarrange(sal2010, sal2011, sal2012, sal2013, ncol = 2, nrow = 2)


#temperature by years----

#2010

mainMap + geom_point(data = env_track2010, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2010") + labs(colour = "Deploy.ID", size = 12) 

temp15_2010<- ggplot(env_track2010, aes(x=cellvalue_temp_15, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3) + xlim(284,298.5) + theme_classic()+labs(x = "Temperature (ºK) at 15m", y = "") + theme(plot.title = element_text(face = "bold")) +labs(title = 2010) + theme(legend.position = "none")  + scale_fill_brewer(palette="Set1")


temp100_2010<- ggplot(env_track2010, aes(x=cellvalue_temp_100, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3) + xlim(284,298.5) + theme_classic()+labs(x = "Temperature (ºK) at 100m", y = "")+labs(colour = "ID", fill = "ID") + theme(legend.position = "bottom")  + theme(legend.title = element_text(face = "bold")) + scale_fill_brewer(palette="Set1") +
  theme(legend.key.size = unit(4, 'mm'))

temp2010<-ggarrange(temp15_2010, temp100_2010,labels = c("A", "B"), ncol = 1, nrow = 2)

temp2010

#2011

mainMap + geom_point(data = env_track2011, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2011") + labs(colour = "Deploy.ID", size = 12) 

temp15_2011<- ggplot(env_track2011, aes(x=cellvalue_temp_15, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3) + xlim(284,298.5) + theme_classic()+labs(x = "Temperature (ºK) at 15m", y = "") + theme(plot.title = element_text(face = "bold")) +labs(title = 2011) + theme(legend.position = "none")  + scale_fill_brewer(palette="Set1")


temp100_2011<- ggplot(env_track2011, aes(x=cellvalue_temp_100, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3) + xlim(284,298.5) + theme_classic()+labs(x = "Temperature (ºK) at 100m", y = "")+labs(colour = "ID", fill = "ID") + theme(legend.position = "bottom")  + theme(legend.title = element_text(face = "bold")) + scale_fill_brewer(palette="Set1") +
  theme(legend.key.size = unit(4, 'mm'))

temp2011<-ggarrange(temp15_2011, temp100_2011,labels = c("A", "B"), ncol = 1, nrow = 2)

temp2011

#2012

mainMap + geom_point(data = env_track2012, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2012") + labs(colour = "Deploy.ID", size = 12) 

temp15_2012<- ggplot(env_track2012, aes(x=cellvalue_temp_15, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3) + xlim(284,298.5) + theme_classic()+labs(x = "Temperature (ºK) at 15m", y = "") + theme(plot.title = element_text(face = "bold")) +labs(title = 2012) + theme(legend.position = "none")  + scale_fill_brewer(palette="Set1")

temp100_2012<- ggplot(env_track2012, aes(x=cellvalue_temp_100, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3) + xlim(284,298.5) + theme_classic()+labs(x = "Temperature (ºK) at 100m", y = "")+labs(colour = "ID", fill = "ID") + theme(legend.position = "bottom")  + theme(legend.title = element_text(face = "bold")) + scale_fill_brewer(palette="Set1") +
  theme(legend.key.size = unit(4, 'mm'))

temp2012<-ggarrange(temp15_2012, temp100_2012,labels = c("A", "B"), ncol = 1, nrow = 2)

temp2012

#2013

mainMap + geom_point(data = env_track2013, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2013") + labs(colour = "Deploy.ID", size = 12) 

temp15_2013<- ggplot(env_track2013, aes(x=cellvalue_temp_15, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3) + xlim(284,298.5) + theme_classic()+labs(x = "Temperature (ºK) at 15m", y = "") + theme(plot.title = element_text(face = "bold")) +labs(title = 2013) + theme(legend.position = "none")  + scale_fill_brewer(palette="Set1")

temp100_2013<- ggplot(env_track2013, aes(x=cellvalue_temp_100, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3) + xlim(284,298.5) + theme_classic()+labs(x = "Temperature (ºK) at 100m", y = "")+labs(colour = "ID", fill = "ID") + theme(legend.position = "bottom")  + theme(legend.title = element_text(face = "bold")) + scale_fill_brewer(palette="Set1") +
  theme(legend.key.size = unit(4, 'mm'))

temp2013<-ggarrange(temp15_2013, temp100_2013,labels = c("A", "B"), ncol = 1, nrow = 2)

temp2013


#module water currents -----

tenv$vel15<- sqrt((tenv$cellvalue_u15^2)+(tenv$cellvalue_v15^2))
tenv$vel100<- sqrt((tenv$cellvalue_u100^2)+(tenv$cellvalue_v100^2)+(tenv$cellvalue_w100^2))

#Yearly temperature for all tuna positions
plotvel15<-ggplot(tenv, aes(x=vel15, fill=as.factor(tenv$year),color=as.factor(tenv$year))) +  geom_density(alpha=.3) + xlim(-0.1,+0.5) + theme_classic()+labs(x = "Module 2D velocity (m/s) at 15m", y = "")+labs(colour = "", fill = "")+ theme(legend.position = "top")

plotvel100<-ggplot(tenv, aes(x=vel100, fill=as.factor(tenv$year),color=as.factor(tenv$year))) +  geom_density(alpha=.3) + xlim(-0.1,+0.5) + theme_classic()+labs(x = "Module 3D velocity at 100m", y = "")+labs(colour = "", fill = "")+ theme(legend.position = "none")

hisvel<-ggarrange(plotvel15, plotvel100, labels = c("A", "B"), ncol = 1, nrow = 2)


#2010

env_track2010<-subset(tenv, year=="2010")

mainMap + geom_point(data = env_track2010, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2010") + labs(colour = "Deploy.ID", size = 12) 

vel15_2010<- ggplot(env_track2010, aes(x=vel15, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3)+ xlim(-0.1,+0.5) + theme_classic()+labs(x = "Module 2D velocity (m/s) at 15m", y = "") + theme(plot.title = element_text(face = "bold")) +labs(title = 2010) + theme(legend.position = "none")  + scale_fill_brewer(palette="Set1")


vel100_2010<- ggplot(env_track2010, aes(x=vel100, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3) + xlim(-0.1,+0.5) + theme_classic()+labs(x = "Module 3D velocity (m/s) at 100m", y = "")+labs(colour = "ID", fill = "ID") + theme(legend.position = "bottom")  + theme(legend.title = element_text(face = "bold")) + scale_fill_brewer(palette="Set1") +
  theme(legend.key.size = unit(4, 'mm'))

vel2010<-ggarrange(vel15_2010, vel100_2010,labels = c("A", "B"), ncol = 1, nrow = 2)

vel2010

#2011

env_track2011<-subset(tenv, year=="2011")

mainMap + geom_point(data = env_track2011, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2011") + labs(colour = "Deploy.ID", size = 12) 

vel15_2011<- ggplot(env_track2011, aes(x=vel15, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3)+ xlim(-0.1,+0.5) + theme_classic()+labs(x = "Module 2D velocity (m/s) at 15m", y = "") + theme(plot.title = element_text(face = "bold")) +labs(title = 2011) + theme(legend.position = "none")  + scale_fill_brewer(palette="Set1")


vel100_2011<- ggplot(env_track2011, aes(x=vel100, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3) + xlim(-0.1,+0.5) + theme_classic()+labs(x = "Module 3D velocity (m/s) at 100m", y = "")+labs(colour = "ID", fill = "ID") + theme(legend.position = "bottom")  + theme(legend.title = element_text(face = "bold")) + scale_fill_brewer(palette="Set1") +
  theme(legend.key.size = unit(4, 'mm'))

vel2011<-ggarrange(vel15_2011, vel100_2011,labels = c("A", "B"), ncol = 1, nrow = 2)

vel2011

#2012

env_track2012<-subset(tenv, year=="2012")

mainMap + geom_point(data = env_track2012, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2012") + labs(colour = "Deploy.ID", size = 12) 

vel15_2012<- ggplot(env_track2012, aes(x=vel15, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3)+ xlim(-0.1,+0.5) + theme_classic()+labs(x = "Module 2D velocity (m/s) at 15m", y = "") + theme(plot.title = element_text(face = "bold")) +labs(title = 2012) + theme(legend.position = "none")  + scale_fill_brewer(palette="Set1")


vel100_2012<- ggplot(env_track2012, aes(x=vel100, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3) + xlim(-0.1,+0.5) + theme_classic()+labs(x = "Module 3D velocity (m/s) at 100m", y = "")+labs(colour = "ID", fill = "ID") + theme(legend.position = "bottom")  + theme(legend.title = element_text(face = "bold")) + scale_fill_brewer(palette="Set1") +
  theme(legend.key.size = unit(4, 'mm'))

vel2012<-ggarrange(vel15_2012, vel100_2012,labels = c("A", "B"), ncol = 1, nrow = 2)

vel2012

#2013

env_track2013<-subset(tenv, year=="2013")

mainMap + geom_point(data = env_track2013, aes(x = lon, y = lat,  color = factor(Deploy.ID)), size = 0.8) + labs(title = "Tuna tracks 2013") + labs(colour = "Deploy.ID", size = 12) 

vel15_2013<- ggplot(env_track2013, aes(x=vel15, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3)+ xlim(-0.1,+0.5) + theme_classic()+labs(x = "Module 2D velocity (m/s) at 15m", y = "") + theme(plot.title = element_text(face = "bold")) +labs(title = 2013) + theme(legend.position = "none")  + scale_fill_brewer(palette="Set1")


vel100_2013<- ggplot(env_track2013, aes(x=vel100, fill=as.factor(Deploy.ID))) +  geom_density(alpha=.3) + xlim(-0.1,+0.5) + theme_classic()+labs(x = "Module 3D velocity (m/s) at 100m", y = "")+labs(colour = "ID", fill = "ID") + theme(legend.position = "bottom")  + theme(legend.title = element_text(face = "bold")) + scale_fill_brewer(palette="Set1") +
  theme(legend.key.size = unit(4, 'mm'))

vel2013<-ggarrange(vel15_2013, vel100_2013,labels = c("A", "B"), ncol = 1, nrow = 2)

vel2013

#direction of currents

tenv

for (i in 1:nrow(tenv)) {
  
  i2=i+1
  a1<-tenv[i,]
  a2<-tenv[i2,]
  
  if ((a1$Deploy.ID==a2$Deploy.ID)==TRUE) { #& (a1$Month<=a2$Month)==TRUE & (a1$Day<=a2$Day)==TRUE & (a1$Hour<a2$Hour)==TRUE)
    
    
    dx=tenv$lon[i2]-tenv$lon[i]
    dy=tenv$lat[i2]-tenv$lat[i]
    
    tenv$Dx[i]<-dx
    tenv$Dy[i]<-dy
    
    
  }
  
}

#direction vector (a, b)=> angulo = arctan(a/b)
atan()
