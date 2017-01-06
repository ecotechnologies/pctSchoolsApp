library(shiny)
library(leaflet)
library(sp)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  rf = readRDS("rf_leeds_schools.Rds")

  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>% addPolylines(data = rf, weight = rf$bicycle / 5)
  })
})
