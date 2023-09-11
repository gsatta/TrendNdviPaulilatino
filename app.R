
library(shiny)

ui <- fluidPage(
  titlePanel(h1("NDVI trend analysis", allign = "center")),
  
  sidebarLayout(position = "right",
    sidebarPanel(h2("NDVI Trend analysis of the Wild Olive Trees decline in Paulilatino (OR), Sardinia")),
    mainPanel("main panel")
  )
)

# Define server logic ----
server <- function(leaflet(options = leafletOptions(maxZoom = 22)) %>%
                     addProviderTiles("Esri.WorldImagery", options = providerTileOptions(maxNativeZoom = 18)) %>%
                     addPolygons(data = lim_paul_wgs84, 
                                 fillOpacity = 0, 
                                 color = "black", 
                                 weight = 2,
                                 group = 'Lim. Amm. Paulilatino'  # Add a group name
                     ) %>% 
                     addPolygons(data = focolai_wgs84,
                                 fillOpacity = 0, 
                                 color = "red", 
                                 weight = 2,
                                 group = 'Outbreaks'
                     ) %>% 
                     addCircleMarkers(data = samples,
                                      lng = ~long,
                                      lat = ~lat,
                                      fillColor = ~color_factor_circle(samples$positivo),
                                      fillOpacity = 0.6,
                                      popup = ~paste("AREA:", location, "<br/>CAMPIONE:", id_sample, "<br/>SINTOMATICO:", sin_asin, "<br/>POSITIVITÀ:", positivo),
                                      radius = 3,
                                      group = 'samples',
                                      stroke = FALSE
                     ) %>%
                     addLegend(pal = color_factor_circle,
                               values = samples$positivo,
                               title = "Sample Positivity",
                               opacity = 0.6,
                               position = "bottomright"
                     ) %>% 
                     addPolygons(data = pixel_trend, 
                                 fillColor = ~color_factor(clss_tr),
                                 color="black",
                                 fillOpacity = 0.6,
                                 highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                     bringToFront = TRUE),
                                 popup = ~paste("COD:", COD, "<br/>coef:", coef, "<br/>classe:", clss_tr),
                                 group = 'Crowns',
                                 stroke = TRUE,
                                 weight = 1
                     ) %>%
                     addLegend(pal = color_factor,
                               values = pixel_trend$clss_tr,
                               title = "NDVI Trend Intensity",
                               opacity = 0.6,
                               position = "topright"
                     ) %>% 
                     addFullscreenControl() %>%
                     addLayersControl(
                       overlayGroups = c("Crowns", "samples", "Lim. Amm. Paulilatino", "Outbreaks"),  # Add the new group
                       options = layersControlOptions(collapsed = TRUE)
                     ), map) {
  
}



# Run the app ----
shinyApp(ui = ui, server = server)
