# Définition de l'interface utilisateur
ui <- fluidPage(
  
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  # Titre de la page
  titlePanel("Projet MigrenMer"),
  
  # Définition des onglets
  tabsetPanel(
    
    # Premier onglet
    tabPanel("Carte de distribution",
             
             # Ajout du texte
             fluidRow(
               column(
                 width = 12,
                 tags$b(tags$u("Options utilisateurs:")),
                 br(),
                 "- Sélectionner une espèce à afficher sur la carte.",
                 br(),
                 "- Choisir les types d'habitats à afficher et leur opacité.",
                 br(),
                 "- Choisir le type d'aires marines protégées (AMPs) à afficher.",
                 br(),
                 "- Afficher ou  masquer un tableau de données listant, parmis les AMPs sélectionnées, celles recouvrant le type d'habitats souhaités",
                 style = "padding-bottom: 20px;padding-top: 20px;"
               )
             ),
             
             # Créer une rangée avec deux colonnes
             fluidRow(
               
               # Colonne pour les inputs
               column(width = 2,
                      # Sélecteur d'espèce
                      selectInput("species", "Espèces",
                                  choices = c("Aalosa", "Afallax"), selected = "Aalosa"),
                      
                      # Sélecteur de valeurs à afficher
                      checkboxGroupInput("values", "Habitats:", 
                                         choices = c("Uncertain", "Core", "Unsuitable"), 
                                         selected = c("Core")),
                      
                      # Bouton pour régler l'opacité des rectangles
                      sliderInput("opacity", "Opacité des habitats",
                                  min = 0, max = 1, value = 0.5, step = 0.1),
                      
                      # Option pour afficher/masquer la couche MPA
                      checkboxGroupInput("categories", "AMPs",
                                         choices = category_options, selected = c("")),
                      
                      # Option pour afficher/masquer le tableau
                      checkboxInput("show_table", "Afficher le tableau", value = F)
               ),
               
               
               # Colonne pour la carte
               column(width =10,
                      # Carte
                      leafletOutput("map", width = "100%", height = "700px"))
             ),
             
             # Tableau
             conditionalPanel(
               condition = "input.show_table == true",
               DTOutput("table")
             ))
    
  ))


# Définition du serveur
server <- function(input, output) {
  
  
  # Filtrer les données en fonction des valeurs sélectionnées
  filtered_data <- reactive({
    data <- data %>% 
      mutate_at(vars(Aalosa, Afallax), as.character()) %>% # Convertir les colonnes de facteurs en colonnes de caractères
      filter(data[[input$species]] %in% input$values)
  })
  
  
  # Afficher la couche MPA si la case à cocher est cochée
  observeEvent(input$show_mpa, {
    if (input$show_mpa) {
      filtered_mpa <- MPA %>% filter(Category %in% input$categories)
      leafletProxy("map") %>% addPolygons(data = filtered_mpa, color = "blue", fillOpacity = 0.2, layerId = "mpa", group = "mpa")
    } else {
      leafletProxy("map") %>% removeShape(layerId = "mpa")
    }
  })
  
  # Création de la carte
  output$map <- renderLeaflet({    
    
    #give manual parameters and isolate the lookup
    isolate({
      if ("map_center" %in% names(input)) {
        mapparams <- list(center = input$map_center,
                          zoom = input$map_zoom)
      } else {
        mapparams <- list(center = list(lng=-2, lat=47),
                          zoom = 6) 
      }
    })
    
    # Filtrer les données de la couche MPA en fonction des catégories sélectionnées
    filtered_mpa <- MPA %>% filter(Category %in% input$categories)
    
    # Déterminer les couleurs pour les points
    colors <- ifelse(filtered_data()[[input$species]] == "Uncertain", "#ffffcc", 
                     ifelse(filtered_data()[[input$species]] == "Core", "#A50026", "#2171b5"))
    
    # Définir les couleurs pour chaque catégorie
    category_colors <- c("SAC" = "red", "SCI" = "blue", "PNM" = "green", "OSPAR" = "purple")
    
    # Changer la couleur de remplissage pour chaque catégorie d'MPA
    for (category in category_options) {
      filtered_mpa$fill_color[filtered_mpa$Category == category] <- category_colors[category]
    }
    
    # Créer la carte et définir le niveau de zoom initial
    m <- leaflet(filtered_data()) %>% 
      addProviderTiles("OpenStreetMap.DE") %>% 
      addMapPane("ames_mpa", zIndex = 430) %>% # shown below ames_data
      addMapPane("ames_data", zIndex = 440) %>% # shown above ames_mpa
      addPolygons(data = filtered_mpa, fillColor = ~fill_color, fillOpacity = 0.8, 
                  options = pathOptions(pane = "ames_mpa"),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE,
                                                      dashArray = NULL),
                  popup = ~as.character(sitename)) %>% 
      addRectangles(data = filtered_data(), lng1 = ~lon-0.025, lat1 = ~lat-0.025, lng2 = ~lon + 0.025, lat2 = ~lat +0.025,
                    color = colors, fillOpacity = input$opacity, 
                    options = pathOptions(pane = "ames_data")) %>% 
      addMiniMap(tiles = providers$CartoDB.Positron,
                 toggleDisplay = T) %>% 
      setView(lng = mapparams$center$lng, lat = mapparams$center$lat, zoom = mapparams$zoom) 
    
  })
  
  observeEvent(c(input$values, input$species, input$opacity, input$categories), {
    values <- reactiveValues(
      current_legend = NULL
    )
    option <- c("Core" %in% input$values, "Unsuitable" %in% input$values, "Uncertain" %in% input$values)
    option_count <- sum(option)
    switch(option_count,
           `1` = {
             if("Core" %in% input$values) {
               values$current_legend <- list(colors = "#A50026", labels = "Centraux")
             } else if( "Unsuitable" %in% input$values) {
               values$current_legend <- list(colors = "#2171b5", labels = "Inadaptés")
             } else {
               values$current_legend <- list(colors = "#ffffcc", labels = "Incertains")
             }
           },
           `2` = {
             if("Core" %in% input$values & "Unsuitable" %in% input$values) {
               values$current_legend <- list(colors = c("#A50026","#2171b5"), 
                                             labels = c("Centraux","Inadaptés"))
             } else if("Core" %in% input$values & "Uncertain" %in% input$values) {
               values$current_legend <- list(colors = c("#A50026","#ffffcc"), 
                                             labels = c("Centraux","Incertains"))
             } else if("Unsuitable" %in% input$values & "Uncertain" %in% input$values) {
               values$current_legend <- list(colors = c("#2171b5","#ffffcc"), 
                                             labels = c("Inadaptés","Incertains"))
             }
           },
           `3` = {
             values$current_legend <- list(colors = c("#A50026", "#2171b5","#ffffcc"), 
                                           labels = c("Centraux","Inadaptés","Incertains"))
           },
           stop("Sélectionnez une option")
    )
    
    if(!is.null(values$current_legend)){
      leafletProxy("map") %>% addLegend("bottomright",
                                        title = "Types d'habitats",
                                        colors = values$current_legend$colors,
                                        labels = values$current_legend$labels, 
                                        opacity = 1)
    } else {
      leafletProxy("map") %>% clearControls()
    }
  })
  
  # Mettre à jour la carte leaflet en fonction de la sélection dans la datatable
  observeEvent(input$table_rows_selected, {
    data <- data %>% 
      mutate_at(vars(Aalosa, Afallax), as.character()) %>% # Convertir les colonnes de facteurs en colonnes de caractères
      filter(data[[input$species]] %in% input$values)
    
    filtered_mpa <- MPA %>% filter(Category %in% input$categories) %>% 
      st_intersection(data) %>% 
      distinct(sitename, .keep_all = TRUE) 
    
    selected_row  <- input$table_rows_selected
    if (length(selected_row) > 0) {
      selectedPolygons <- filtered_mpa[selected_row,c("sitename","Category","geometry","long","lat")]
      print(selectedPolygons)
      leafletProxy("map")  %>% 
        setView(lng = selectedPolygons$long, lat = selectedPolygons$lat, zoom = 8)  
      # addPolygons(data = selectedPolygons, fillColor = "yellow") # Ajouter les polygones de l'amp sélectionnée à la carte et les styliser en jaune
      
    }
  })
  
  # Afficher le tableau
  output$table <- renderDataTable({
    
    data <- data %>% 
      mutate_at(vars(Aalosa, Afallax), as.character()) %>% # Convertir les colonnes de facteurs en colonnes de caractères
      filter(data[[input$species]] %in% input$values)
    
    filtered_mpa <- MPA %>% 
      filter(Category %in% input$categories)%>%
      st_intersection(data) %>% 
      select(sitename,Category,long,lat) %>% 
      distinct(sitename, .keep_all = TRUE) %>% 
      # calculer l'intersection avec les rectangles de filtered_data
      DT::datatable(
        .,
        options = list(
          pageLength = 10, 
          lengthMenu = c(5, 10, 20), 
          scrollX = TRUE
        ), # dom = '<"top"lf>rt<"bottom"ip>'), 
        caption = htmltools::tags$caption(
          style = "caption-side: top; font-size: 16px; font-weight: bold;margin-top: 2px;",
          "Aires Marines Protégées recouvrant les habitats sélectionnés",
          htmltools::tags$h2(style = "font-size: 14px; margin-top: 6px;", "Sélectionner pour Zoomer")
        ),
        selection = list(mode = "single", target = "row"))
    
    
    # Retourner le tableau
    return(filtered_mpa) 
  })
  
}

# Lancer l'application
shinyApp(ui = ui, server = server)