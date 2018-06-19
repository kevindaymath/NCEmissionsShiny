options(warn=-1)
# Packages <- c("shiny", "DT", "leaflet", "datasets", "devtools", "maptools", "rgdal", "rmapshaper", "rgeos","tidyr")
# lapply(Packages, library, character.only = TRUE)
load("data\\data.RData")

function(input, output, session) {
  # UI Reactives
  

  
  chosenVariable <- reactive({    # Map's variable
    input$variable
  })
  
  maxCo <- reactive({    # Map's variable
    round(max(completeCoNC[,chosenVariable()]), digits = -2)
  })
  
  variables <- reactive({    # Map's variable
    input$show_vars
  })
  
  chosenLevel <- reactive({
    input$Level
  })
  

  # ____________________________________________________________________________________
  # Texts
  output$County <- renderText({
    paste("<center><b>County Level")
  })
  output$Region <- renderText({
    paste("<center><b>Region Level")
  })


 
  
  
  # ____________________________________________________________________________________
  # Leaflets
  output$CountyMap <- renderLeaflet({
  
    bins <- quantile(completeCoNC[,chosenVariable()],prob = seq(0, 1, length = 6), type = 5, na.rm = TRUE)
    pal <- colorBin("YlOrRd", domain = completeCoNC[,chosenVariable()], bins = bins)
    labels <- sprintf("<strong>%s</strong><br/>%g tons", mapCo$NAME,
                      completeCoNC[,chosenVariable()]) %>% lapply(htmltools::HTML)
    labels2 <- sprintf("<strong>Region %s</strong><br/>", mapRe$NAME) %>% lapply(htmltools::HTML)
    m <- leaflet(completeCoNC) %>%
      setView(-79.8, 35.3, 6.5) %>%
      addPolygons(data = mapCo, fillColor = ~pal(completeCoNC[,chosenVariable()]), weight = 2, opacity = 1,
                  color = "white", dashArray = "3", fillOpacity = 0.7,group = "Counties",
                  highlight = highlightOptions(weight = 5, color = "#666", dashArray = "",
                                               fillOpacity = 0.7,bringToFront = TRUE),
                  label = labels, labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                                           padding = "3px 8px"),
                                                              textsize = "15px", direction = "auto")) %>%
      addPolygons(data = mapRe, weight = 1.5,fillOpacity = 0,group = "Region Overlay",color="red", label = labels2) %>%
      addLayersControl(baseGroups = c("Counties"), options = layersControlOptions(collapsed = F), overlayGroups = "Region Overlay")%>%
      hideGroup("Region Overlay") %>%
      addLegend(pal = pal, values = bins, opacity = 0.7, title = NULL, position = "bottomright")
  })


  output$RegionMap <- renderLeaflet({
    
    bins <- quantile(completeRegNC[,chosenVariable()],prob = seq(0, 1, length = 6), type = 5, na.rm = TRUE)
    pal <- colorBin("YlOrRd", domain = completeRegNC[,chosenVariable()], bins = bins)
    labels <- sprintf("<strong>%s</strong><br/> ", mapCo$NAME) %>% lapply(htmltools::HTML)
    labels2 <- sprintf("<strong>Region %s</strong><br/>%g tons", mapRe$NAME,completeRegNC[,chosenVariable()]) %>% lapply(htmltools::HTML)
    m <- leaflet(completeRegNC) %>%
      setView(-79.8, 35.3, 6.5) %>%
      addPolygons(data = mapRe, fillColor = ~pal(completeRegNC[,chosenVariable()]), weight = 2, opacity = 1,
                  color = "white", dashArray = "3", fillOpacity = 0.7,group = "Regions",
                  highlight = highlightOptions(weight = 5, color = "#666", dashArray = "",
                                               fillOpacity = 0.7,bringToFront = TRUE),
                  label = labels2, labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                                            padding = "3px 8px"),
                                                               textsize = "15px", direction = "auto")) %>%
      addPolygons(data = mapCo, weight = 1.5,fillOpacity = 0,group = "County Overlay",color="blue", label = labels) %>%
      addLayersControl(baseGroups = c("Regions"), options = layersControlOptions(collapsed = F), overlayGroups = "County Overlay")%>%
      hideGroup("County Overlay") %>%
      addLegend(pal = pal, values = bins, opacity = 0.7, title = NULL, position = "bottomright")
    m
  })
  
  
  output$tableCounty <- renderDataTable({
    a <- 0
    for (i in 1:length(variables()))
    {
      a <- c(a,which(categories$Category==variables()[i]))
    }
    a <- a+1
    if (chosenLevel() == 1) {
    datatable(completeCoNC[,a,drop=FALSE],rownames = FALSE, 
              options = list(order = list(list(0,'asc')),pageLength = -1,dom = 't',initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().column( 0 ).nodes()).css('border-right','3px solid #000');",
                "}")))
    }
    else {
      datatable(completeRegNC[,a,drop=FALSE],rownames = FALSE, 
                options = list(order = list(list(0,'asc')),pageLength = -1,dom = 't',initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().column( 0 ).nodes()).css('border-right','3px solid #000');",
                  "}")))
    }
  })
  
  
  }