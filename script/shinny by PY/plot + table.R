ui <- fluidPage(
  titlePanel("Exploration de données Pingouins"),
  sidebarLayout(
    sidebarPanel(
      strong("Ma belle barre latérale"),
      selectInput("select", label = h3("Choisir une espèce"),
                  choices = list("ADELIE" = "Adelie", "CHINSTRAP" = "Chinstrap", "GENTOO" ="Gentoo"),
                  selected = "Adelie", multiple=T),
      sliderInput("slider", label = h3("Choisir une période"), min = 2007, max = 2009, value = c(2007,2009)),
      checkboxGroupInput("checkbox1", label = "Draw regression", choices=c("Linear reg."="linear","Loess"="loess")),
      checkboxInput("checkbox2", label = "Represent uncertainty", value = FALSE)
    )
    ,
    mainPanel(
      "Panneau principal: ici sont représentées les sorties désirées",
      plotlyOutput("pengPlot"),
      dataTableOutput("table")
      
    )
  )
)
server<- function(input, output) {
  output$pengPlot = renderPlotly({
    penguins %>%
      filter(species%in%input$select) %>%
      filter(year>=input$slider[1]) %>%
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
output$table = renderDataTable({
  penguins %>%
    filter(species%in%input$select) %>%
    filter(year>=input$slider[1]) %>%
    filter(year<=input$slider[2])
})
}
shinyApp(ui, server)
