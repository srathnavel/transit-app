#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(tigris)
library(tidycensus)
library(mapview)
library(leaflet)
library(sp)

load("data/map_data.rda")
load("data/popup_data.rda")

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("How People Get to Work"),

    # Sidebar with an about section and options 
    sidebarLayout(
        sidebarPanel(
          h4("About this dashboard:"),
          p("Select a city and a mode of transport below to see how people who used that mode of transport to get to work between 2015 and 2019 were distributed around urban centers."),
          
          # select a city to determine transport dataset
          radioButtons("city", 
                       label = "Select a City", 
                       choices = c("Chicago",
                                   "Los Angeles", 
                                   "New York City"),
                       selected = "Chicago"),
          
          # select a mode of transport to specify a variable
          selectInput("var_fill", 
                      label = "Select a Mode of Transport",
                      choices = c("Car/Van/Truck",
                                  "Bus", 
                                  "Train",
                                  "Taxi",
                                  "Bicycle",
                                  "Walk",
                                  "Work from Home"),
                      selected = "Car/Van/Truck"),
          
          # select a type of map to select basemap for the visualization
          radioButtons("map_type", 
                       label = "Select a Base Map", 
                       choices = c("Cartographic Positron",
                                   "Open Street Map"),
                       selected = "Cartographic Positron"),
        ),

        # Show the map
        mainPanel(
          leafletOutput("map"),
          p(),
          p(strong("Data from:"), "IPUMS NHGIS, University of Minnesota at", a("www.nhgis.org",
                                                                              href = "https://data2.nhgis.org/main")),
        )
    )
)

# Define server logic required for a leaflet object
server <- function(input, output) {
  
    output$map <- renderLeaflet({
      # choose fill variable based on input$var_fill from ui.R
      fill_var <- switch(input$var_fill,
                         "None" = "none",
                         "Car/Van/Truck" = "drive_pct",
                         "Bus" = "bus_pct", 
                         "Train" = "rail_pct",
                         "Taxi" = "taxicab_pct",
                         "Bicycle" = "bicycle_pct",
                         "Walk" = "walk_pct",
                         "Work from Home" = "wfh_pct")
      
      # choose dataset based on input$city from ui.R
      transport <- switch(input$city,
                          "Chicago" = transport_chi,
                          "Los Angeles" = transport_la, 
                          "New York City" = transport_nyc)
      
      # choose dataset based on input$city from ui.R
      popup_city <- switch(input$city,
                           "Chicago" = popup_chi,
                           "Los Angeles" = popup_la, 
                           "New York City" = popup_nyc)
      
      # choose mapview based on input$map_type from ui.R
      basemap <- switch(input$map_type,
                        "Cartographic Positron" = "CartoDB.Positron",
                        "Open Street Map" = "OpenStreetMap")
      
      # plot map in mapview
      mapview(transport, 
              zcol = fill_var, 
              legend = TRUE,
              layer.name = "% of commuters",
              map.types = basemap,
              popup = popup_city)@map
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
