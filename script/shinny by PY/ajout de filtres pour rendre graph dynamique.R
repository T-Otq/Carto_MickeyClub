library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)
library(kableExtra)
library(shinythemes)


#### charger le jeu de données à utiliser
remotes::install_github("allisonhorst/palmerpenguins")
data(penguins,package = "palmerpenguins")
penguins <- penguins %>%
  rename(bill_l = bill_length_mm, bill_d = bill_depth_mm, flip_l = flipper_length_mm, bm = body_mass_g)
penguins %>%
  print(n=2)



########## Pour la partie pratique

#### Application basique
ui <- fluidPage(
  titlePanel("Exploration de données Pingouins"),
  sidebarLayout(
    sidebarPanel(
      strong("Ma belle barre latérale"),
      selectInput("select", label = h3("Select Species"), #permet une première selection suivant les espèces
                  choices = list("ADELIE" = "Adelie", "CHINSTRAP" = "Chinstrap", "GENTOO" = "Gentoo"),
                  selected = c("Adelie","Chinstrap") , multiple=T),
    sliderInput("slider",label =h3("Select Year"),
                min=2007,max=2009,value=c(2007,2009)),
    checkboxGroupInput("checkbox1", label = "Draw regression", 
                       choices=c("Linear reg."="linear","Loess"="loess")),
    checkboxInput("checkbox2", label = "Represent uncertainty", value = FALSE)
    )
    ,
    mainPanel(
      "Panneau principal: ici sont représentées les sorties désirées",
      plotlyOutput("pengPlot")
    )
  )
)

server<- function(input, output) {
  output$pengPlot = renderPlotly({
    penguins %>%
      filter(species%in%input$select) %>%
      filter(year>=input$slider[1]) %>%  ##slider pour rajouter un filter sur year
      filter(year<=input$slider[2]) %>%
      ggplot() + aes( x= bill_l, y=bill_d,col = species) + geom_point() +
      labs( x = 'Bill length in mm') +
      labs(y = 'Bill depth in mm') +
      labs(color = "Species")+
      theme_light()->toplot
    
    if("linear"%in%input$checkbox1){
      toplot<-toplot+geom_smooth(method = 'lm', se = input$checkbox2)
    }
    if("loess"%in%input$checkbox1){
      toplot<-toplot+geom_smooth(method = 'loess', se = input$checkbox2)
    }
    
    toplot
  })
}

shinyApp(ui, server)


