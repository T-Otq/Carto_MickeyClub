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
liaison <- dbConnect(drv, host="sirs.agrocampus-ouest.fr",user="vianney", password="******", dbname="world_mapping")

load("data/MHW_raw.RData")
geo_data<-st_read(liaison, query ="select distinct A.geom_st as geometry, A.cell_no,B.eco_type,lati as lat,longi as lon
                  from public.world_grid1x1_v3 A inner join geo.biogeo3 B using (cell_no)")
geo_borders<-st_read(liaison, query ="select distinct geom from geo.world_borders_ggplot")
NE_box_2<- st_sfc(st_polygon(list(cbind(
  c(rep(180,1801),rep(-180,1801),180),
  c(rev(seq(-90,90,by=0.1)),seq(-90,90,by=0.1),90)))),crs=st_crs(geo_data))
dbDisconnect(liaison)