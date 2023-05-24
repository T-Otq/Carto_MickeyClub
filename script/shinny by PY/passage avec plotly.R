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
  titlePanel("Ma Donnée de Piafs"),
  sidebarLayout(
    sidebarPanel(
      "Voilà la barre latérale qui permettra de rentrer des informations/instructions"
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
    penguins %>%   ## partie graphique dans function renderplot pour renvoyer un graph sur internet
      ggplot() + aes( x= bill_l, y=bill_d, col = species) + geom_point() + 
      labs( x = 'Bill length in mm') +
      labs(y = 'Bill depth in mm') +
      labs(color = "Species")+
      theme_light()
  })
}

shinyApp(ui, server)

selectInput("NOM DE L'INPUT", label = "NOM/CONSIGNE POUR L'UTILISATEUR",
            choices=list("nomChoix1"="Choix1","NomChoix2"="Choix2",...,"NomChoixn"="Choixn"),
            selected = =[choix par défaut],multiple=[TRUE si plusieurs choix autorisés])
