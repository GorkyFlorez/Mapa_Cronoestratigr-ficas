
library(raster)
library(tidyverse)
library(ggplot2)
library(sf)

SurAmerica = st_read("SHP/SurAmerica.geojson")  %>% st_as_sf()
SurAmeric  <- st_transform(SurAmerica  ,
                           crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Bosquebradas = st_read("SHP/Bosquebradas.shp")  %>% st_as_sf()
Geologia     = st_read("SHP/Geologia.shp")  %>% st_as_sf()
Peru          <- getData('GADM', country='Peru', level=0) %>% st_as_sf()
colombia   <- getData('GADM', country='colombia', level=2) %>% st_as_sf()
Risaralda      <- subset(colombia , NAME_1  == "Risaralda")


ggplot()+
  geom_sf(data = Bosquebradas)+
  geom_sf(data = Risaralda , fill="red")+
coord_sf(xlim = c(-82, -66), ylim = c(-5,13))

ggplot()+
  geom_sf(data = Risaralda )+
  geom_sf_text(data = Risaralda, aes(label=NAME_2))
  
  
coord_sf(xlim = c(-82, -66), ylim = c(-5,13))






library(ggspatial)
library(elevatr)
elev = get_elev_raster(Bosquebradas, z=13)

plot(elev)
Poligo_alt    <- crop(elev, Bosquebradas)                           #   
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Bosquebradas)
plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope") 
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)

colores = c( 
  "#fefae0", "#faedcd", "#d4a373"
)#amarillo pastel

Geo_data       <-  rasterToPoints(Poligo_alt)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "alt")


col=c("#fb8500", "#2a9d8f", "#8ac926", 
      "#7209b7", "#5e503f", "#be95c4",
      "#ffff3f", "#f95738", "#720026"
)

SurA= ggplot()+
  geom_sf(data = SurAmeric, fill="white", color="black", size=0.01)+
  geom_sf(data = Peru , fill="white", color="black", size=0.05)+
  geom_sf(data = colombia , fill="black", color="black", size=0.05)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -60, y = 10, hjust = 0, vjust = 1, 
           label = "a) Sur America",size = 3, family="serif", color = 
             "black",  fontface="italic", face = "bold")+
  annotate(geom = "text", x = -80, y = -40, hjust = 0, vjust = 1, 
           label = "Pacific ocean",size = 3, family="serif", color = 
             "black",  fontface="italic", angle=90)+
  annotate(geom = "text", x = -55, y = -50, hjust = 0, vjust = 1, 
           label = "Atlantic ocean",size = 3, family="serif", color = 
             "black",  fontface="italic")

ColomGG= ggplot()+
  geom_sf(data = colombia , fill="white", color="black", size=0.01)+
  geom_sf(data = Risaralda , fill="black", color="black", size=0.01)+
  coord_sf(xlim = c(-80, -67), ylim = c(-5 , 13)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))

Local =ggplot()+
  geom_sf(data = Risaralda  , fill="white", color="black", size=0.01)+
  geom_sf(data = Geologia , fill="black", color="black", size=0.01)+
  coord_sf(xlim = c(-76.7, -75.2), ylim = c(4.670201 , 5.475137)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))

library(ggnewscale) 
Mapa =ggplot()  +
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_sf(data = Bosquebradas, fill=NA, color="black")+
  geom_sf(data = Geologia, aes(fill=SimboloUC), alpha=0.4, color=NA)+
  scale_fill_manual(values = col,name='Cronoestratigráficas',
                    guide = guide_legend( direction = "horizontal",
                                          keyheight = unit(0.3, units = "cm"), 
                                          keywidth=unit(0.001, units = "cm"), 
                                          label.position = "bottom", title.position = 'top', nrow=1))+
  
  
  geom_sf_text(data = Geologia, aes(label=SimboloUC))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme_classic()+
  theme(
    legend.position = c(0.25, 0.10),
    axis.text.x  = element_text(face="bold", color="black", size=10,
                                family="serif"),
    axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                family="serif",size=10),
    axis.title = element_text(face="bold", color="black"),
    legend.background = element_rect(fill = "white"),
    legend.text=element_text(size=11, family="serif"),
    legend.title = element_text(size=11, family="serif", face='bold',hjust=0.5),
    legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
    legend.key.width = unit(0.6,"cm"), #ancho de cuadrados de referencia 
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  labs(title = '', fill = 'Densidad \n(miles)',  x = 'Longitud', y = 'Latitud')
  
D= ggplot()  +
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_sf(data = Bosquebradas, fill=NA, color="black")+
  geom_sf(data = Geologia, aes(fill=Descripcio), alpha=0.4, color=NA)+
  scale_fill_manual(values = col,name='Cronoestratigráficas',
                    guide = guide_legend( direction = "horizontal",
                                          keyheight = unit(0.3, units = "cm"), 
                                          keywidth=unit(0.001, units = "cm"), 
                                          label.position = "bottom", title.position = 'top', nrow=1))+

  geom_sf_text(data = Geologia, aes(label=Descripcio))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme_classic()+
  theme(
    legend.position = c(0.25, 0.10),
    axis.text.x  = element_text(face="bold", color="black", size=10,
                                family="serif"),
    axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                family="serif",size=10),
    axis.title = element_text(face="bold", color="black"),
    legend.background = element_rect(fill = "white"),
    legend.text=element_text(size=6, family="serif", angle=270),
    legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5),
    legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
    legend.key.width = unit(0.6,"cm"), #ancho de cuadrados de referencia 
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  labs(title = '', fill = 'Densidad \n(miles)',  x = 'Longitud', y = 'Latitud')

legend <- get_legend(D)



library(cowplot)
Fin=ggdraw() +
  coord_equal(xlim = c(0, 29), ylim = c(0, 21), expand = FALSE) +
  draw_plot(Mapa , width = 21, height = 21,x = -0.5, y = 0)+
  draw_plot(SurA , width = 5, height = 5,x = 20, y = 15)+
  draw_plot(ColomGG , width = 5, height = 5,x = 23.5, y = 15)+
  draw_plot(Local , width = 7, height = 7,x = 21, y = 9.5)+
  
  draw_plot(legend , width = 7, height = 7,x = 23, y = 5)+
  #draw_plot(ggAgru  , width = 20, height = 20,x = 14.5, y = 1)+
  
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "black", fill = NA, size = 1))+
  annotate(geom = "text", x = -75, y = -17, hjust = 0, vjust = 1, angle=45,
           label = "Gorky Florez Castillo            Gorky Florez Castillo        Gorky Florez Castillo",
           size = 7, family="serif", color = "grey20",
           alpha=0.2)


ggsave(plot=Fin,"Mapa de Cronoestratigráficas.png",units = "cm",width = 29, #alto
       height = 21, #ancho
       dpi=1200)























































