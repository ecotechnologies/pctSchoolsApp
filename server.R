#     This is server base that the every client will connect to.
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

# # # # #
# Setup #
# # # # #

# Colours
zcols <- "RdYlBu" # for colourbrewer scale (see get_colour_ramp in pct-shiny-funs.R)
# expect pct-data as a sibling of pct-shiny
data_dir_root <- file.path('data')
# packages required
cran_pkgs <- c("shiny", "rgdal", "rgeos", "leaflet", "shinyjs", "dplyr", "readr")

lapply(cran_pkgs, library, character.only = T)

# Functions
source("pct-shiny-funs.R", local = T)

regions <- NULL
regions$Region <- c("west-yorkshire")

# # # # # # # #
# shinyServer #
# # # # # # # #
shinyServer(function(input, output, session){
  showing_all_trips <- function(){ F }

  # To set initialize to_plot
  observe({
    query <- parseQueryString(session$clientData$url_search)

    if (is.na(region$current)) {
      region$current <- if(isTRUE(query[['r']] %in% regions$Region)){
        query[['r']]
      } else if (exists("starting_region")) {
        starting_region
      } else {
        "west-yorkshire"
      }
      region$data_dir <- file.path(data_dir_root, region$current)
      region$all_trips <- dir.exists(file.path(data_dir_root, region$current , 'all-trips'))
    }

    session$sendCustomMessage("regionchange", region$current)
    region$data_dir
    region$repopulate_region

    to_plot$zones <<- readRDS(file.path(region$data_dir, "z.Rds"))
    to_plot$cents <<- readRDS(file.path(region$data_dir, "c.Rds"))

    to_plot$route_network <<- readRDS(file.path(region$data_dir, "rnet.Rds"))
    to_plot$route_network$id <<- 1:nrow(to_plot$route_network)

    region$repopulate_region <- T
  })

  region <- reactiveValues(current = NA, data_dir = NA, repopulate_region = F, all_trips = NA)
  lsoa <- reactiveValues(show = F)
  show_no_lines <- c("none", "lsoa_base_map")

  # For all plotting data
  to_plot <- NULL
  # For any other persistent values
  helper <- NULL

  helper$e_lat_lng <- ""

  # Select and sort lines within a bounding box - given by flows_bb()
  sort_lines <- function(lines, group_name, nos){
    if(group_name %in% show_no_lines) return(NULL)
    if(!line_data() %in% names(lines)) return(NULL)
    # If other than route network lines are selected, subset them by the bounding box
    if (group_name != "route_network"){
      poly <- flows_bb()
      if(is.null(poly)) return(NULL)
      poly <- spTransform(poly, CRS(proj4string(lines)))
      keep <- gContains(poly, lines,byid=TRUE )
      if(all(!keep)) return(NULL)
      lines_in_bb <- lines[drop(keep), ]
      # Sort by the absolute values
      lines_in_bb[ tail(order(abs(lines_in_bb[[line_data()]])), nos), ]
    }else{
      # For the route network, just sort them according to the percentage of display
      # Sort by the absolute values
      nos <- nos / 100 * nrow(lines)
      lines[ tail(order(abs(lines[[line_data()]])), nos), ]
    }
  }

  # Finds the Local Authority shown inside the map bounds
  find_region <- function(current_region){
    return(current_region) # force this to not change
    bb <- map_bb()
    if(is.null(bb)) return(NULL)
    regions_bb_intersects <- gIntersects(bb, regions, byid=T)
    # return NULL if centre is outside the shapefile
    if(all(drop(!regions_bb_intersects))) return(NULL)

    current_region_visible <- current_region %in% tolower(regions[drop(regions_bb_intersects), ]$Region)
    if(current_region_visible) return(NULL)

    regions_map_center_in <- gContains(regions, gCentroid(bb, byid=T), byid=T)
    if(all(drop(!regions_map_center_in))) return(NULL)
    tolower(regions[drop(regions_map_center_in), ]$Region[1])
  }

  # Initialize the leaflet map
  output$map = renderLeaflet(
    leaflet() %>%
      addTiles(., urlTemplate = map_tile_url(),
               attribution = '<a target="_blank" href="http://shiny.rstudio.com/">Shiny</a> |
               Routing <a target="_blank" href ="https://www.cyclestreets.net">CycleStreets</a> |
               Map &copy <a target="_blank" href ="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
               options=tileOptions(opacity = ifelse(input$map_base == "IMD", 0.3, 1),
                                   maxZoom = ifelse(input$map_base == "IMD", 14, 18), reuseTiles = T)) %>%
                                   {
                                     if (input$map_base == 'IMD'){
                                       addTiles(., urlTemplate = "http://tiles.oobrien.com/shine_urbanmask_dark/{z}/{x}/{y}.png",
                                                options=tileOptions(opacity = 0.3, maxZoom = 14, reuseTiles = T))
                                       addTiles(., urlTemplate = "http://tiles.oobrien.com/shine_labels_cdrc/{z}/{x}/{y}.png",
                                                options=tileOptions(opacity = 0.3, maxZoom = 14, reuseTiles = T))
                                     } else .
                                   } %>%
      addCircleMarkers(., data = to_plot$cents, radius = 0, group = "centres", opacity = 0.0) %>%
      mapOptions(zoomToLimits = "first")
  )

  observe({ # For highlighting the clicked line
    event <- input$map_shape_click
    if (is.null(event) || event$id == "highlighted")
      return()
    e_lat_lng <- paste0(event$lat,event$lng)

    # Fix bug when a line has been clicked then the click event is
    # re-emmited when the map is moved
    if( e_lat_lng == helper$e_lat_lng)
      return()
    helper$e_lat_lng <<- e_lat_lng

    isolate({
      id_group_name <- unlist(strsplit(event$id, "-"))
      id <- id_group_name[1]
      group_name <- id_group_name[2]

      if (event$group == "centres"){
        addPolygons(leafletProxy("map"), data = to_plot$zones[to_plot$z$geo_code == id,],
                    fill = F,
                    color = get_line_colour("centres") ,
                    opacity = 0.7,
                    layerId = "highlighted")
      } else if (event$group == "zones"){
        addPolygons(leafletProxy("map"), data = to_plot$zones[to_plot$z$geo_code == id,],
                    fill = FALSE,
                    color = "black",
                    opacity = 0.7 ,
                    layerId = "highlighted")
      } else {
        line <- to_plot[[group_name]][to_plot[[group_name]]$id == id,]
        if (!is.null(line))
          addPolylines(leafletProxy("map"), data = line, color = "white",
                       opacity = 0.4, layerId = "highlighted")
      }
    })
  })

  # Updates the Local Authority if the map is moved
  # over another region with data
  observe({
    new_region <- find_region(region$current)
    if(is.null(new_region)) return()

    new_data_dir <- file.path(data_dir_root, new_region)

    if(region$data_dir != new_data_dir && file.exists(new_data_dir) && !file.exists(file.path(new_data_dir, 'isolated'))){
      region$current <- new_region
      region$data_dir <- new_data_dir
      region$repopulate_region <- F
    }
  })

  # Plot if lines change
  observe({
    # Needed to force lines to be redrawn when scenario, zone or base map changes
    input$scenario
    input$map_base
    region$data_dir
    input$show_zones
    region$repopulate_region

    leafletProxy("map")  %>% clearGroup(., "route_network") %>%
      removeShape(., "highlighted")

    to_plot$ldata <<- sort_lines(to_plot[[input$line_type]], input$line_type, input$nos_lines)
    plot_lines(leafletProxy("map"), to_plot$ldata, input$line_type)
  })

  # This code displays centroids if zoom level is greater than 11 and lines are displayed
  observe({
    if(is.null(input$map_zoom) ) return()
    region$repopulate_region
    input$map_base
    if(input$map_zoom < 11 || input$line_type %in% show_no_lines)
      hideGroup(leafletProxy("map"), "centres")
    else
      showGroup(leafletProxy("map"), "centres")
  })


  # Displays zone popups when no lines are selected
  observe({
    region$repopulate_region
    input$map_base
    line_type <- isolate(input$line_type)
    show_zone_popup <- input$line_type %in% show_no_lines
    popup <- if(show_zone_popup) zone_popup(to_plot$zones, input$scenario, zone_attr(), showing_all_trips())
    leafletProxy("map")  %>% clearGroup(., c("zones", "centres")) %>%
      addPolygons(.,  data = to_plot$zones
                  , weight = 2
                  , fillOpacity = transp_rate()
                  , opacity = 0.2
                  , fillColor = get_colour_ramp(zcols, to_plot$zones[[zone_data()]]/to_plot$zones$all)
                  , color = "black"
                  , group = "zones"
                  , popup = popup
                  , options = pathOptions(clickable = show_zone_popup)
                  , layerId = paste0(to_plot$zones[['geo_code']], '-', "zones")) %>%
      addCircleMarkers(., data = to_plot$cents, radius = 1,
                       color = get_line_colour("centres"), group = "centres", opacity = 0.5,
                       popup = centroid_popup(to_plot$cents, input$scenario, zone_attr(), showing_all_trips())) %>%
      # Hide and Show line layers, so that they are displayed as the top layer in the map.
      # Leaflet's function bringToBack() or bringToFront() (see http://leafletjs.com/reference.html#path)
      # don't seem to exist for R
      # By default hide the centroids
      hideGroup(., "centres") %>%
      {
        if(!line_type %in% show_no_lines) {
          hideGroup(., line_type) %>% showGroup(., line_type)
        }
      }

    # Display centroids when zoom level is greater than 11 and lines are selected
    if (isTRUE(isolate(input$map_zoom) >= 11 && !line_type %in% show_no_lines))
      showGroup(leafletProxy("map"), "centres")
  })


  # Return the right directory name based on type of trips
  data_dir <- reactive({
    region$current
  })

  # Set transparency of zones to 0.5 when displayed, otherwise 0
  transp_rate <- reactive({
    if (input$show_zones) 0.5 else 0.0
  })

  # Identify suffix of lines variables
  line_attr <- reactive({
    if(input$scenario == 'olc') 'olc'
    else if (input$line_type != 'route_network') input$line_order
    else 'slc'
  })

  # Identify suffix of zones variables
  zone_attr <- reactive({
    if(input$scenario == 'olc') 'olc' else 'slc'
  })

  # Identify complete name of lines variable
  line_data <- reactive({
    data_filter(input$scenario, line_attr())
  })

  # Identify complete name of zones variable
  zone_data <- reactive({
    data_filter(input$scenario, zone_attr())
  })

  # Returns the map bounding box
  map_bb <- reactive({
    if (is.null(input$map_bounds)){ return (NULL)}
    lat <- c(input$map_bounds$west , input$map_bounds$east, input$map_bounds$east, input$map_bounds$west )
    lng <- c(input$map_bounds$north, input$map_bounds$north, input$map_bounds$south, input$map_bounds$south)
    c1 <- cbind(lat, lng)
    r1 <- rbind(c1, c1[1, ])
    bounds <- SpatialPolygons(list(Polygons(list(Polygon(r1)), 'bb')), proj4string=CRS("+init=epsg:4326 +proj=longlat"))
    proj4string(bounds)=CRS("+init=epsg:4326 +proj=longlat")
    bounds
  })

  # Updates the bounding box (bb) to the current map bb unless the map is frozen
  # Returns a bb
  flows_bb <- reactive({
    if(is.null(helper$bb)){
      helper$bb <<- map_bb()
    }
    helper$bb
  })

  # Adds polylines on the map, depending on types and number of lines
  plot_lines <- function(m, sorted_l, group_name){
    if(is.null(sorted_l)) return()

    min <- 1
    max <- 20

    line_opacity <- 0.8
    popup_fun_name <- paste0(group_name, "_popup")

    popop_fun <- get(popup_fun_name)
    addPolylines(m, data = sorted_l, color = get_line_colour(group_name)
                 # Plot widths proportional to attribute value
                 # Remove NAs from the weights
                 , weight = normalise(sorted_l[[line_data()]][!is.na(sorted_l[[line_data()]]) ], min = min, max = max)
                 , opacity = line_opacity
                 , group = group_name
                 , popup = popop_fun(sorted_l, input$scenario, showing_all_trips())
                 , layerId = paste0(sorted_l[['id']], '-', group_name))

  }
  # Updates map tile according to the selected map base
  map_tile_url <- reactive({
    lsoa$show <- input$line_type == "lsoa_base_map"
    switch(input$map_base,
           'roadmap' = "http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png",
           'satellite' = "http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
           'IMD' =  "http://tiles.oobrien.com/imd2015_eng/{z}/{x}/{y}.png",
           'opencyclemap' = "https://{s}.tile.thunderforest.com/cycle/{z}/{x}/{y}.png",
           'hilliness' = "http://server.arcgisonline.com/ArcGIS/rest/services/World_Shaded_Relief/MapServer/tile/{z}/{y}/{x}"
    )
  })

  # Set map attributes
  output$cite_html <- renderUI({
    HTML(paste(a("Code", href= "https://github.com/npct/pctSchoolsApp", target='_blank'),
               'released under a', a('GNU Affero GPL', href= "http://www.pct.bike/www/licence.html", target='_blank'),
               'and funded by the', a('DfT', href = "https://www.gov.uk/government/organisations/department-for-transport", target="_blank")
    ))
  })

  # Adds map legend
  observe({
    input$map_base
    leafletProxy("map") %>% clearControls(.)
    title <- ifelse(showing_all_trips(), "% trips cycled", "% cycling to school")
    if (input$show_zones) {
      leafletProxy("map") %>% addLegend("topleft", colors = get_colour_palette(zcols, 10),
                                        labels = c("0-1%",
                                                   "2-3%",
                                                   "4-6%",
                                                   "7-9%",
                                                   "10-14%",
                                                   "15-19%",
                                                   "20-24%",
                                                   "25-29%",
                                                   "30-39%",
                                                   "40%+"),
                                        title = title,
                                        opacity = 0.5
      )
    }
  })

  # Creates legend as a barplot for IMD map base
  output$imd_legend <- renderPlot({
    my_lab <- c("Most deprived decile", "2nd", "3rd", "4th", "5th",
                "6th", "7th", "8th", "9th", "Least deprived decile",
                "Data missing", "Data not available")

    my_lab <- rev(my_lab)

    my_colors <- c("#a50026","#d73027", "#f46d43","#fdae61","#fee08b",
                   "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850",
                   "#006837", "#aaaaaa", "#dddddd")

    my_colors <- rev(my_colors)

    # Set the labelling of Y-axis to bold
    par(font.lab = 2, mar=c(0.0,5.8,0.0,1.0))

    bp <- barplot(rep(1,12), beside = TRUE, col = my_colors,
                  ylab = "IMD From 2015\nIndex of Multiple Deprivation", horiz = T, axes = F)

    text(0, bp, my_lab, cex=0.8, pos=4, font=2, col = "black")
  })

  # Hide/show panels on user-demand
  shinyjs::onclick("toggle_panel", shinyjs::toggle(id = "input_panel", anim = FALSE))
  shinyjs::onclick("toggle_trip_menu", shinyjs::toggle(id = "trip_menu", anim = FALSE))
  shinyjs::onclick("toggle_map_legend", shinyjs::toggle(id = "map_legend", anim = FALSE))
})
