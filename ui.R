#     This is UI base that runs on every connected client
#
#     Copyright (C) 2016 Nikolai Berkoff, Ali Abbas and Robin Lovelace
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU Affero General Public License as
#     published by the Free Software Foundation, either version 3 of the
#     License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU Affero General Public License for more details.
#
#     You should have received a copy of the GNU Affero General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.

library(shiny)
library(leaflet)
library(shinyjs)

scenarios <- c("Census 2011 Cycling" = "olc",
               "Government Target" = "govtarget",
               "Go Dutch" = "dutch")

line_types <- c("None" = "none",
                "Route Network" = "route_network")

map_base_attrs <- c("Roadmap (Black & White)"   = "roadmap",
                    "Roadmap (OpenCycleMap)" = "opencyclemap",
                    "Satellite" = "satellite",
                    "Index of Deprivation" = "IMD",
                    "Hilliness" = "hilliness")

shinyUI(
  navbarPage(
    title = "Propensity to Cycle Tool",
    id="nav",
    tabPanel(
      "Map",
      div(
        class="outer",
        tags$head(
          includeScript("./assets/extra.js"),
          includeCSS("./assets/stylesheet.css"),
          includeHTML("./assets/favicon.html")
        ),
        br(),
        leafletOutput("map", width="100%", height="95%"),
        absolutePanel(
          id = "controls", class = "panel panel-default",
          fixed = TRUE,  top = 110,  right = 20, width = 180,
          height = "auto",  style = "opacity: 0.9",
          tags$div(title="Show/Hide Panel",
                   a(id = "toggle_panel", style="font-size: 80%", span(class="glyphicon glyphicon-circle-arrow-up", "Hide"))
          ),
          div(
            id = "input_panel",
            tags$div(title="Scenario",
                     selectInput("scenario", "Scenario:", scenarios, selectize = F)
            ),
            tags$div(title="Shows the cycling flow between the centres of zones",
                     selectInput("line_type", "Cycling Flows", line_types, selected = "route_network", selectize = F)
            ),
            tags$div(title="Shows the cycling flow between the centres of zones",
                     checkboxInput("show_zones", "Show Zones", value = T)
            ),
            conditionalPanel(
              condition = "input.line_type != 'none'",
              tags$div(title="Number of lines to show",
                       sliderInput("nos_lines", label = "Percent (%) of Network", 10, 50, step = 20, value = 30, ticks = F)
              )
            ),
            tags$div(title="Change base of the map",
                     selectInput("map_base", "Map Base:", map_base_attrs, selectize = F)
            )
          )
        ),
        conditionalPanel(
          condition = "input.map_base == 'IMD'",
          absolutePanel(
            cursor = "auto", id = "legend", class = "panel panel-default",
            bottom = 235, left = 5, height = 20, width = 225, draggable = TRUE,
            style = "opacity: 0.7",
            tags$div(title="Show/Hide map legend",
                     a(id = "toggle_map_legend", style="font-size: 80%", span(class="glyphicon glyphicon-circle-arrow-up", "Hide"))
            ),
            div(id = "map_legend",
                tags$div(title="Index of Multiple Deprivation",
                         plotOutput("imd_legend", width = "100%", height = 180)

                )
            )
          )
        )
      )
    )
  )
)
