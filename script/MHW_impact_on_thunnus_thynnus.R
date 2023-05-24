MHW_thunnus_exec <- function(){
  
  library(shiny)
  library(dplyr)
  library(ggplot2)
  library(DT)
  library(plotly)
  library(kableExtra)
  library(shinythemes)
  library(tidyverse)
  library(RPostgreSQL)
  library(rpostgis)
  library(heatwaveR)
  library(ggpubr)
  library(ncdf4)
  library(sf)
  library(rgdal)
  
  #------------------------------------------------------------
  
  ### Historical Period###
  nc_meso<-nc_open("data/MHW_Thunnus.nc")
  prez <- ncvar_get(nc_meso, "probability")
  dim(prez)
  lon <- ncvar_get(nc_meso, "longitude")-0.25
  lat <- ncvar_get(nc_meso, "latitude")+0.25 
  
  length(lon)
  length(lat)
  #------------------------------------------------------------
  tcb_model<-data.frame()
  
  tcb_model<-as.data.frame(prez[,]) %>%
    setNames(lat) %>%
    bind_cols(lon=lon) %>%
    gather(lat, prez, -lon) %>%
    mutate(prez=as.numeric(prez),
           lat=as.numeric(lat),
           lon=as.numeric(lon)) %>%
    bind_rows(tcb_model)
  
  head(tcb_model)
  dim(tcb_model)
  
  # linking mt database
  drv <- dbDriver("PostgreSQL")
  liaisons_db_via <- dbConnect(drv, dbname="vianney_db",host="halieut.agrocampus-ouest.fr",port=5432,user="vianney",password="***")
  #------------------------------------------------------------
  cell_world05<-as.data.frame(dbGetQuery(liaisons_db_via,("select distinct cell_no, lati as lat, longi as lon from geo.world_grid05x05")))
  head(cell_world05)
  dim(cell_world05)
  
  dt_tcb05<-inner_join(tcb_model,cell_world05, by=c("lon","lat"))
  head(dt_tcb05)
  dim(dt_tcb05)
  #select cell_no with biomass data
  dt_tcb_ok<-filter(dt_tcb05,prez!=0)
  dim(dt_tcb_ok)
  cell_to_keep85<-unique(dt_tcb_ok$cell_no)
  length(cell_to_keep85)
  dt_tcb_05ok<-filter(dt_tcb05,cell_no%in%cell_to_keep85)
  dim(dt_tcb_05ok)
  cell_world01<-as.data.frame(dbGetQuery(liaisons_db_via,("select distinct id_1x1 cell_no1x1, id_05x05 as cell_no from geo.cross_0505_11")))
  head(cell_world01)
  dim(cell_world01)
  dt_tcb2<-inner_join(dt_tcb_05ok,cell_world01, by="cell_no")
  head(dt_tcb2)
  dim(dt_tcb2)
  length(unique(dt_tcb2$cell_no1x1))
  # --------------------------------------------------------------------
  # test MAP
  # --------------------------------------------------------------------
  cell_world<-as.data.frame(dbGetQuery(liaisons_db_via,("select distinct cell_no as cell_no1x1  from geo.world_grid1x1_v3")))
  head(cell_world)
  dim(cell_world)
  dt_tcb_ocean<-inner_join(dt_tcb2,cell_world, by="cell_no1x1")
  head(dt_tcb_ocean)
  dim(dt_tcb_ocean)
  length(unique(dt_tcb_ocean$cell_no1x1))
  # test MAP
  # --------------------------------------------------------------------
  # --------------------------------------------------------------------
  thunnus_prez<-dt_tcb_ocean%>%
    group_by(cell_no1x1) %>%
    summarise(presence=mean(prez)) %>%
    as.data.frame()
  head(thunnus_prez)
  
  
  thunnus_prez<-rename(thunnus_prez, cell_no = cell_no1x1)
  head(thunnus_prez)
  #------------------------------------------------------------------------
  #dbWriteTable(liaisons_db_via,c("vianney","thon_rouge_presense2"),thunnus_prez, row.names=FALSE, append=TRUE) # append = T pour ajouter des lignes 
  
  #'* then 
  thunnus_cells<-unique(thunnus_prez$cell_no)
  length(thunnus_prez$cell_no)
  length(unique(thunnus_prez$cell_no))
  length(thunnus_cells)
  
  #---------------------------------------------------------------------
  
  event_thunnus<-dbGetQuery(liaisons_db_via,"select * from daily_sst_from_avhrr.fix_seasonal_baseline_threshold_clim_90th A
inner join vianney.thon_rouge_presense using(cell_no)
inner join daily_sst_from_avhrr.fix_seasonal_event_characterics_90th B using(cell_no) 
  where A.threshold>29.5")
  head(event_thunnus)
  
  #'*for characterics event over tmax*
  event_thunnus2<-event_thunnus %>% mutate(nb_event=1) %>% 
    group_by(cell_no,year) %>% 
    summarise(mhw_day=round(mean(duration)),
              tot_intensity_cum=mean(intensity_cumulative),
              tot_intensity_mean=mean(intensity_mean),mean_nb_mhw=round(mean(nb_event))) %>%
    as.data.frame()
  head(event_thunnus2)
  event_thunnus2<-event_thunnus2 %>% select(cell_no,year,mhw_day) %>% arrange(year)
  max(event_thunnus2$mhw_day)
  color=as.vector(rainbow(196))
  
  #'* for both case execute until the end* #just change fill value
  load("data/MHW_raw.RData")
  
  geo_data<-st_read(liaisons_db_via, query ="select distinct A.geom_st as geometry, A.cell_no,B.eco_type,lati as lat,longi as lon
                  from geo.world_grid1x1_v3 A inner join geo.biogeo3 B using (cell_no)")
  geo_borders<-st_read(liaisons_db_via, query ="select distinct geom from geo.world_borders_ggplot")
  NE_box_2<- st_sfc(st_polygon(list(cbind(
    c(rep(180,1801),rep(-180,1801),180),
    c(rev(seq(-90,90,by=0.1)),seq(-90,90,by=0.1),90)))),crs=st_crs(geo_data))  
  event_thunnus2$mhw_day<-event_thunnus2$mhw_day
  thon_over_temp<- inner_join(geo_data,event_thunnus2, by="cell_no")
  
  plot <- ggplot() +
    geom_sf(data = filter(thon_over_temp,year>=2011),aes(fill=mhw_day),color=NA, size=0.01)+
    geom_sf(data = geo_borders)+
    coord_sf(xlim = c(-100,30),ylim = c(-40,40))+  
    facet_wrap(~year)+
    viridis::scale_fill_viridis(name="Number of mhw day\nwith tmax=29.5°C",
                                option = 'turbo', trans = 'log', breaks = c(10, 20, 40, 80))+
    theme(panel.grid.major.x = element_line(color = NA),panel.background = element_blank(),
          axis.text = element_blank(),axis.ticks = element_blank(), axis.title = element_blank(),
          plot.margin=unit(c(0,0,0,0), "cm"),
          # plot.title = element_text(size = 15, face = "bold", hjust = 0.5, vjust=-0.5),
          legend.title = element_text(size = 15, face = "bold", hjust = 0.5, vjust=0.5),
          legend.text = element_text(size=15))
  
  dbDisconnect(liaisons_db_via)

  return(list(plot = plot, thon_over_temp = thon_over_temp, geo_borders = geo_borders))
  
}

