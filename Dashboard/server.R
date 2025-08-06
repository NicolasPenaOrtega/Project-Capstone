# Install and import required libraries
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)
require(plotly)


# Create a RShiny server
shinyServer(function(input, output){
  
  
  output$change_title <- renderUI({

    if (input$city_dropdown == "All") {
      h1(paste("Bike Sharing demand forecast from ", min_date," to ", max_date))
    } else {
      h3("Bike - Sharing demand prediction app")
    }
    
    
  })
  
  
  output$conditional_content <- renderUI({
    # Accede al valor del dropdown usando input$city_dropdown
    if (input$city_dropdown != "All") {
      tagList(
        h5(paste("Select city from dropshow to show its bike predictions details between ", min_date, " to ", max_date)),
        plotOutput(outputId = "Temperature_Chart", height = 1200/3),
        plotOutput(outputId = "Prediction_Chart", height = 1200/3),
        plotOutput(outputId = "HumidityVSPrediction", height = 1200/3)
      )
    }
  })

  
  output$conditional_content_leafletOutput <- renderUI({
    if(input$city_dropdown != 'All') {
      leafletOutput(outputId = "city_bike_map", height = 1200)
    } else {
      leafletOutput(outputId = "city_bike_map", height = 400)
    }
  })
  
  observeEvent(input$city_dropdown, {
    
    if(input$city_dropdown != 'All') {
      #Render the city overview map
      selected_city_data <- cities_max_bike |>
        filter(CITY_ASCII == input$city_dropdown)
      
      output$city_bike_map <- renderLeaflet({
        # Complete this function to render a leaflet map
        leaflet(selected_city_data) |>
          addTiles() |>
          setView(
            lng = selected_city_data$LNG, 
            lat = selected_city_data$LAT, 
            zoom = 15) |>
          addCircleMarkers(
            lng = selected_city_data$LNG,
            lat = selected_city_data$LAT,
            popup = ~paste(selected_city_data$DETAILED_LABEL)
          )
      })
    }
    else {
      #Render the specific city map
      output$city_bike_map <- renderLeaflet({
        # Complete this function to render a leaflet map
        leaflet(cities_max_bike) |>
          addTiles() |>
          addCircleMarkers(
            lng = ~LNG, lat = ~LAT,
            radius = ~custom_radius,
            color = ~color_levels(BIKE_PREDICTION_LEVEL),
            fillOpacity = 0.5,
            popup = ~paste(LABEL,"<br>",
                           "<b> Demand : </b>", BIKE_PREDICTION_LEVEL, "<br>",
                           "<b> Number of bike requested: </b>", CITIES_MAX_BIKE)
          ) |>
          fitBounds(
            lng1 = -80, lat1 = 50,
            lng2 = 90, lat2 = 60
          ) |>
          addLegend(
            position = "bottomright",
            pal = color_levels,
            values = ~BIKE_PREDICTION_LEVEL,
            title = "Demand"
          )
      })
      
      # Render plots
      
      output$Temperature_Chart <- renderPlot({
        # Filter the data for the selected city and create a cumulative hours column
        selected_city_forecast <- city_weather_bike_df |>
          filter(CITY_ASCII == input$city_dropdown) |>
          mutate(CUM_HOURS = seq(from = 0, by = 3, length.out = n()))
        
        min_date_city <- min(selected_city_forecast$FORECASTDATETIME, na.rm = TRUE)
        max_date_city <- max(selected_city_forecast$FORECASTDATETIME, na.rm = TRUE)
        
        ggplot(selected_city_forecast, aes(x = CUM_HOURS, y = TEMPERATURE)) +
          geom_line(aes(color = I("yellow"))) +
          geom_point(aes(color = I("black"))) +
          geom_text(
            aes(label = TEMPERATURE),
            vjust = -1,  # Ajusta la posición vertical del texto para que no se superponga
            hjust = 0.5, # Ajusta la posición horizontal
            size = 3) +
          labs(
            x = "Time [3 hours ahead]",
            y = "Temperature [C]",
            title = "Temperature chart") +
          theme_bw()
      })

      output$Prediction_Chart <- renderPlot({
        selected_city_forecast <- city_weather_bike_df |>
          filter(CITY_ASCII == input$city_dropdown) |>
          mutate(CUM_HOURS = seq(from = 0, by = 3, length.out = n()))
        
        min_date_city <- min(selected_city_forecast$FORECASTDATETIME, na.rm = TRUE)
        max_date_city <- max(selected_city_forecast$FORECASTDATETIME, na.rm = TRUE)
        
        ggplot(selected_city_forecast, aes(x = CUM_HOURS, y = BIKE_PREDICTION)) +
          geom_line(aes(color = I("dodgerblue3")), linetype = 2) +
          geom_point(aes(color = I("black"))) +
          geom_text(
            aes(label = BIKE_PREDICTION),
            vjust = -1,  # Ajusta la posición vertical del texto para que no se superponga
            hjust = 0.5, # Ajusta la posición horizontal
            size = 3) +
          labs(
            x = "Time [3 hours ahead]",
            y = "Prediction bike count [C]",
            title = "Rented bike predictions chart") +
          theme_bw()
      })
      
      output$HumidityVSPrediction <- renderPlot({
        
        selected_city_forecast <- city_weather_bike_df |>
          filter(CITY_ASCII == input$city_dropdown)
        
        ggplot(selected_city_forecast, aes(x = HUMIDITY, y = BIKE_PREDICTION)) +
          geom_point(aes(color = I("black"))) +
          geom_smooth(formula = "y ~ poly(x,3)", color = I("darkred"), method = "lm") +
          labs(
            x = "Time [3 hours ahead]",
            y = "Temperature [C]",
            title = "Humidity vs Rented bike predictions") +
          theme_bw()
      })
      
    }
  })  
  
  

  # Observe drop-down event
  
  # Then render output plots with an id defined in ui.R
  
  # If All was selected from dropdown, then render a leaflet map with circle markers
  # and popup weather LABEL for all five cities
  
  # If just one specific city was selected, then render a leaflet map with one marker
  # on the map and a popup with DETAILED_LABEL displayed
  
})
