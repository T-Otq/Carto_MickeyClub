library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)
library(kableExtra)
# library(shinythemes)
library(tidyverse)
library(RPostgreSQL)
library(rpostgis)
library(heatwaveR)
library(ggpubr)
library(sf)
library(rgdal)



drv <- dbDriver("PostgreSQL")
liaisons_db_via<- dbConnect(drv, dbname="vianney_db",host="halieut.agrocampus-ouest.fr",
                            port=5432,user="vianney",password="*******")

#Téléchargement des données depuis la base posgresql*
map_data<-dbGetQuery(liaisons_db_via,"select * from daily_sst_from_avhrr.fix_yearly_event_characterics_90th")
head(map_data)
test<-dplyr::filter(map_data,year%in%c(2003,2007,2011,2015))
map_data<-test
map_data<-map_data %>%  
  group_by(cell_no,year) %>% 
  summarise(mhw_day=round(sum(duration)),
            tot_intensity_cum=round(sum(intensity_cumulative)),
            tot_intensity_mean=round(sum(intensity_mean))) %>% 
  as.data.frame()
head(map_data)
#function for title of graph*
myplot_theme1 = function (plot, x.title = NULL, y.title = NULL, plot.title = NULL) {
  plot +   
    labs(title = paste(strwrap(plot.title, width = 50), collapse = "\n"),
         x = x.title, 
         y = y.title)
}
#function for style map*
style_pp<-function(ggp){
  ggp<-ggp+
    geom_sf(data=graticules_rob, linetype="dotted", color="black", size = 0.4) +
    # add graticules projected to Robinson
    geom_sf(data=geo_borders, colour=NA, fill="gray70") +
    # add Natural Earth box projected to Robinson
    geom_sf(data=box_rob,colour="black", fill=NA, size = 0.1) +
    # add label lat and lon
    geom_text(data = lbl.Y.prj, aes(x = X.prj2-1*10e5, y = Y.prj, label = lbl), color="grey20", size=1.5) + #0.9 0.8
    geom_text(data = lbl.X.prj, aes(x = X.prj, y = Y.prj-0.5*10e5, label = lbl), color="black", size=1.5)+ #0.7 0.6
    # theme
    theme(panel.grid.major.x = element_line(color = NA),panel.background = element_blank(),
          axis.text = element_blank(),axis.ticks = element_blank(), axis.title = element_blank(),
          plot.margin=unit(c(0,0,0,0), "cm"),
          plot.title = element_text(size = 15, face = "bold", hjust = 0.5, vjust=-0.5),
          legend.title = element_text(size = 15, face = "bold", hjust = 0.5, vjust=0.5),
          legend.text = element_text(size=15),
          legend.key = element_rect(size = 4))
  return(ggp)
}

load("data/MHW_raw.RData")
geo_data<-st_read(liaisons_db_via, query ="select distinct A.geom_st as geometry, A.cell_no,lati as lat,longi as lon
                  from geo.world_grid1x1_v3 A")
geo_borders<-st_read(liaisons_db_via, query ="select distinct geom from geo.world_borders_ggplot")
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

map_TYPE<- inner_join(geo_data,map_data, by="cell_no")
map_TYPE$year<-as.factor(map_TYPE$year)
max(map_TYPE$nb_event)
max(map_TYPE$mhw_day)

head(map_TYPE)

#----------------------------------------------------


#---------------------------
ui <- fluidPage(
  titlePanel("Exploration de données par eco_type"), #Specify here the title of the web page* 
  sidebarLayout(
    sidebarPanel(
      strong("Ma belle barre latérale"), #Specify here the title for the slide bar* 
      selectInput("select", label = h3("Choisir une annee"),
                  choices=unique(map_TYPE$year), multiple=F),
      
    )
    ,
    mainPanel(
      "Panneau principal: ici sont représentées les sorties désirées", #Specify here some explication* 
      plotOutput("mhw_day"),
      
    )
  )
)

map_TYPE$intensity<-as.factor(map_TYPE$tot_intensity_mean) #here is what happen behing the clic button function* 
server<- function(input, output) {
  output$mhw_day  = renderPlot({
    map_TYPE_year<- map_TYPE %>%   #filter the specific data need dor the map* 
      filter(year==input$select)
    style_pp(ggplot(data=map_TYPE_year) +  #execution of the map* 
               geom_sf(aes(fill=tot_intensity_mean),color=NA, size=0.01)+
               scale_fill_gradient2("Nb of MHW day",low="white",high="red4",midpoint=10,limits=c(0,20),oob=squish)+
               theme(legend.position = "bottom"))
    
    
    
  })
}
shinyApp(ui, server)

