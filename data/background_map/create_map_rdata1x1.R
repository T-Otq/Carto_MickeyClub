# -----------------------------------------
# rm(list=ls())
# -----------------------------------------
library(RPostgreSQL)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(sf)
# library(ggmap)
library(rgdal)
library(RColorBrewer)
library(colorRamps)
library(scales)
library(grid)
drv <- dbDriver("PostgreSQL")
liaison <- dbConnect(drv, host="halieut.agrocampus-ouest.fr",user="vianney", password="*************", dbname="vianney_db")

load("~/ownCloud/Thèse/Script/script for create background map/mapping.RData")
geo_data<-st_read(liaison, query ="select distinct A.geom_st as geometry, A.cell_no,lati as lat,longi as lon
                  from geo.world_grid1x1_v3 A")
geo_borders<-st_read(liaison, query ="select distinct geom from geo.world_borders_ggplot")
NE_box_2<- st_sfc(st_polygon(list(cbind(
  c(rep(180,1801),rep(-180,1801),180),
  c(rev(seq(-90,90,by=0.1)),seq(-90,90,by=0.1),90)))),crs=st_crs(geo_data))
PROJ <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
geo_data <- sf::st_transform(geo_data, PROJ)
geo_borders<-sf::st_transform(geo_borders, PROJ)
box_rob<-sf::st_transform(NE_box_2, PROJ)
NE_graticules<-st_as_sf(NE_graticules)
graticules_rob <- st_transform(NE_graticules, PROJ)
# project long-lat coordinates for graticule label data frames 
# (two extra columns with projected XY are created)
prj.coord <- project(cbind(lbl.Y$lon, lbl.Y$lat), proj=PROJ)
lbl.Y.prj <- cbind(prj.coord, lbl.Y)
names(lbl.Y.prj)[1:2] <- c("X.prj","Y.prj")
# position label 
lbl.Y.prj$X.prj <- (-(lbl.Y.prj$X.prj))
lbl.Y.prj$X.prj2<-lbl.Y.prj$X.prj#-1.10e6
# X
prj.coord <- project(cbind(lbl.X$lon, lbl.X$lat), proj=PROJ)
lbl.X.prj <- cbind(prj.coord, lbl.X)
names(lbl.X.prj)[1:2] <- c("X.prj","Y.prj")
lbl.X.prj<-subset(lbl.X.prj, Y.prj<0)


# save.image('mappping_world_operationel_test.RData') 
# rm(geo_data)
# # test
# ggplot() +
#   geom_sf(data=graticules_rob, linetype="dotted", color="grey40", size = 0.25) +
#   # add graticules projected to Robinson
#   geom_sf(data=geo_borders, colour=NA, fill="gray70") +
#   # add Natural Earth box projected to Robinson
#   geom_sf(data=box_rob,colour="black", fill=NA, size = 0.2) +
#   # add label lat and lon
#   geom_text(data = lbl.Y.prj, aes(x = X.prj2, y = Y.prj, label = lbl), color="grey20", size=0.8) + #0.9
#   geom_text(data = lbl.X.prj, aes(x = X.prj, y = Y.prj, label = lbl), color="black", size=0.6) + #0.7
#   # theme
#   theme(panel.grid.major.x = element_line(color = NA),panel.background = element_blank(),
#         axis.text = element_blank(),axis.ticks = element_blank(), axis.title = element_blank(),
#         legend.position = "none")+
#   guides(fill =guide_colorbar(barwidth = 0.8, barheight =8,label.position = "right",title.position = "top"))
