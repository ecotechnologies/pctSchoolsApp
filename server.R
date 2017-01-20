library(shiny)
library(leaflet)
library(sp)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  weight = reactive({
    sqrt(rf@data[[input$scenario]]) * as.numeric(input$modeOfTravel == "Cycle")
  })
  
  rf = readRDS("rf_leeds_schools_all.Rds")
  z = readRDS("z.Rds")

  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>% addPolylines(data = rf, weight = "bicycle", opacity = 0)
  })
  observe({
    leafletProxy("map") %>%
      clearShapes() %>% 
      addPolylines(data = rf, weight = weight()) %>% 
      addPolygons(data = z)
  })
})
