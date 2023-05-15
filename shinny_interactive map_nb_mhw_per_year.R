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
                            port=5432,user="vianney",password="*********")

# map_data<-dbGetQuery(liaisons_db_via,"select * from daily_sst_from_avhrr.fix_seasonal_baseline_nb_event")
map_data<-dbGetQuery(liaisons_db_via,"select * from daily_sst_from_avhrr.fix_yearly_event_characterics_90th")
head(map_data)
# test<-dplyr::filter(map_data,year%in%c(2003,2007,2011,2015))
# map_data<-test
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
color<-as.vector(rainbow(21))
map_TYPE$intensity<-as.factor(map_TYPE$tot_intensity_mean)
server<- function(input, output) {
  output$mhw_day  = renderPlot({
    map_TYPE_year<- map_TYPE %>% 
      filter(year==input$select,tot_intensity_mean<=20)
    style_pp(ggplot(data=map_TYPE_year) +
               geom_sf(aes(fill=tot_intensity_mean),color=NA, size=0.01)+
               # scale_fill_manual(values = color)+
    scale_fill_gradient2("Nb of MHW day",low="white",high="red4",midpoint=10,limits=c(0,20),oob=squish)+
    theme(legend.position = "bottom"))
  
    
    
  })
  }
shinyApp(ui, server)


# style_pp(ggplot(data=filter(map_TYPE,year%in%c(1982,2013,2015,2020))) +
#            geom_sf(aes(fill=tot_intensity_mean),color=NA, size=0.01)+
#            facet_wrap(~year)+
#            # scale_fill_manual(values = color)+
#            scale_fill_gradient2("Nb of MHW day",low="green",mid="blue",high="red4",midpoint=10,limits=c(0,20),oob=squish)+
#            theme(legend.position = "bottom"))
# #--------------------------
# map_data1<-filter(map_data,year%in%c(2010:2020)) %>%  
#   group_by(cell_no) %>% 
#   mutate(nb_event=1) %>%
#   summarise(mhw_day=round(mean(duration)),
#             mean_nb_mhw=round(sum(nb_event)/10),
#             percent=round(mhw_day/365*100,1)) %>%
#   as.data.frame()
# head(map_data1)
# 
# map_TYPE1<- inner_join(geo_data,map_data1, by="cell_no")
# hist(map_TYPE1$mhw_day,breaks=500)
# hist(map_TYPE1$mean_nb_mhw,breaks=5000)
# hist(map_TYPE1$percent,breaks = 1500)
# 
# a<-style_pp(ggplot(data=map_TYPE1) +
#               geom_sf(aes(fill=mhw_day),color=NA, size=0.01)+
#               scale_fill_gradient2("Nb of MHW day",low="#ffeda0",mid="#feb24c",high="#bd0026",midpoint=20,limits=c(0,40),oob=squish))
# b<-style_pp(ggplot(data=map_TYPE1) +
#               geom_sf(aes(fill=mean_nb_mhw),color=NA, size=0.01)+
#               scale_fill_gradient2("Nb of MHW",low="#ffeda0",mid="#feb24c",high="#bd0026",midpoint=5,limits=c(0,10),oob=squish))
# c<-style_pp(ggplot(data=map_TYPE1) +
#               geom_sf(aes(fill=percent),color=NA, size=0.01)+
#               scale_fill_gradient2("percent of day on MHW",low="#ffeda0",mid="#feb24c",high="#bd0026",midpoint=7,limits=c(0,15),oob=squish))
# ggarrange(a,b,c)
# 
# 
# 
# map_data2<-filter(map_data,year%in%c(1982:1992)) %>%  
#   group_by(cell_no) %>% 
#   mutate(nb_event=1) %>%
#   summarise(mhw_day=round(mean(duration)),
#             mean_nb_mhw=round(sum(nb_event)/10),
#             percent=round(mhw_day/365*100,1)) %>%
#   as.data.frame()
# head(map_data2)
# 
# map_TYPE2<- inner_join(geo_data,map_data2, by="cell_no")
# hist(map_TYPE2$mhw_day)
# hist(map_TYPE2$mean_nb_mhw)
# hist(map_TYPE2$percent)
# d<-style_pp(ggplot(data=map_TYPE2) +
#               geom_sf(aes(fill=mhw_day),color=NA, size=0.01)+
#               scale_fill_gradient2("Nb of MHW day",low="#ffeda0",mid="#feb24c",high="#bd0026",midpoint=20,limits=c(0,40),oob=squish))
# e<-style_pp(ggplot(data=map_TYPE2) +
#               geom_sf(aes(fill=mean_nb_mhw),color=NA, size=0.01)+
#               scale_fill_gradient2("Nb of MHW",low="#ffeda0",mid="#feb24c",high="#bd0026",midpoint=5,limits=c(0,10),oob=squish))
# f<-style_pp(ggplot(data=map_TYPE2) +
#               geom_sf(aes(fill=percent),color=NA, size=0.01)+
#               scale_fill_gradient2("percent of day on MHW",low="#ffeda0",mid="#feb24c",high="#bd0026",midpoint=7,limits=c(0,15),oob=squish))
# #---------------
# output<-ggarrange(myplot_theme1(a, plot.title = "2010_2020"),
#                   myplot_theme1(b, plot.title = "2010_2020"),
#                   myplot_theme1(c, plot.title = "2010_2020"),
#                   myplot_theme1(d, plot.title = "1982_1992"),
#                   myplot_theme1(e, plot.title = "1982_1992"),
#                   myplot_theme1(f, plot.title = "1982_1992"))
# setwd("~/ownCloud/Thèse/Script/script_extraction_daily_sst/fix_baseline/test output_fix_baseline")
# ggsave("avg_mhw_evolution.jpeg", width = 50, height = 20, units = "cm")
