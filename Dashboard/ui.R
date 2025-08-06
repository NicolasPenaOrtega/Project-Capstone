# Load required libraries
library(leaflet)
library(shiny)



# Create a RShiny UI
shinyUI(
  fluidPage(
    
    titlePanel(column(width = 12,
                    uiOutput("change_title")
                    )
             ),
    

    fluidRow(
      column(width = 7,
             wellPanel(
               uiOutput("conditional_content_leafletOutput")
               #leafletOutput(outputId = "city_bike_map")
             )),

      column(width = 5,
             wellPanel(
               h4("Cities"),
               selectInput(inputId = "city_dropdown",
                           label = "Select a city",
                           choices = c("All", list_city),
                           selected = "All"),
               uiOutput("conditional_content")
             ))
    )
  
))
