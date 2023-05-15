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



drv <- dbDriver("PostgreSQL")
liaisons_db_via<- dbConnect(drv, dbname="vianney_db",host="halieut.agrocampus-ouest.fr",
                            port=5432,user="vianney",password="***")

# map_data<-dbGetQuery(liaisons_db_via,"select * from daily_sst_from_avhrr.fix_seasonal_baseline_nb_event")
map_data<-dbGetQuery(liaisons_db_via,"select * from daily_sst_from_avhrr.fix_yearly_event_characterics_90th")
head(map_data)
test<-dplyr::filter(map_data,year%in%c(2003,2007,2011,2015))
map_data<-test
map_data<-map_data %>%  
  group_by(cell_no,year) %>% 
  # mutate(nb_event=1) %>%
  summarise(mhw_day=round(sum(duration)),
            tot_intensity_cum=round(sum(intensity_cumulative)),
            tot_intensity_mean=round(sum(intensity_mean))) %>% 
            # mean_nb_mhw=round(sum(nb_event))) %>%
  as.data.frame()
head(map_data)
#function for title of graph
myplot_theme1 = function (plot, x.title = NULL, y.title = NULL, plot.title = NULL) {
  plot +   
    labs(title = paste(strwrap(plot.title, width = 50), collapse = "\n"),
         x = x.title, 
         y = y.title)
}
#function for style map
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
  #guides(fill=guide_colorbar(barwidth = 7.5, barheight =0.8))
  return(ggp)
}

source('data/background_map/create_map_rdata1x1.R')

map_TYPE<- inner_join(geo_data,map_data, by="cell_no")
map_TYPE$year<-as.factor(map_TYPE$year)
max(map_TYPE$nb_event)
max(map_TYPE$mhw_day)

head(map_TYPE)

#----------------------------------------------------


#---------------------------
ui <- fluidPage(
  titlePanel("Exploration de données par eco_type"),
  sidebarLayout(
    sidebarPanel(
      strong("Ma belle barre latérale"),
      selectInput("select", label = h3("Choisir une annee"),
                  choices=unique(map_TYPE$year), multiple=F),
     
      )
    ,
    mainPanel(
      "Panneau principal: ici sont représentées les sorties désirées",
      plotOutput("mhw_day"),

    )
  )
)

map_TYPE$intensity<-as.factor(map_TYPE$tot_intensity_mean)
server<- function(input, output) {
  output$mhw_day  = renderPlot({
    map_TYPE_year<- map_TYPE %>% 
      filter(year==input$select)
    style_pp(ggplot(data=map_TYPE_year) +
               geom_sf(aes(fill=tot_intensity_mean),color=NA, size=0.01)+
    scale_fill_gradient2("Nb of MHW day",low="white",high="red4",midpoint=10,limits=c(0,20),oob=squish)+
    theme(legend.position = "bottom"))
  
    
    
  })
  }
shinyApp(ui, server)

