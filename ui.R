library(shiny)
library(leaflet)

scenarios <- c("2011 level (school survey data)" = "bicycle",
               "Government Target" = "govtarget_slc",
               "Go Dutch" = "dutch_slc")

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("pctSchoolsApp - prototype"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("modeOfTravel",
                  "Mode of travel:",
                  choices = c("Walk", "Cycle", "Car")),
      conditionalPanel(condition = "input.modeOfTravel == 'Cycle'",
                       selectInput("scenario", "Scenario:", scenarios))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("map", width="100%", height = 800)
    )
  )
))
