library(shiny)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(rgeos)
library(sf)
library(dplyr)

# Imposta l'opzione scipen su un valore elevato per eliminare la notazione esponenziale dei valori
options(scipen = 999)

# Carico il nuovo file con le classi
pixel_trend <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/NDVI_VALUES/pixels_trend3.shp")

# Carico il file della geolocalizzazione dei campioni
samples <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/sample_points.shp")

lim_paul_wgs84 <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/VETTORIALI/Limite_Amministrativo_Paulilatino_wgs84.shp")

focolai_wgs84 <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/FOCOLAI_wgs84.shp")

plots_wgs84 <- st_read("G:/Altri computer/Il_mio_computer/DOTTORATO/PROGETTI/OLIVASTRO_PAULILATINO/REGRESSIONE/VETTORIALI/BUFFER_ANALISI_NDVI_WGS84.shp")


# Define a custom color palette for classes
class_palette <- c("p-value > 0.05" = "#00ff00",  # Green
                   "0.05 > p-value > 0.01" = "yellow",  # Yellow-green
                   "0.01 > p-value > 0.001" = "orange",
                   "0.001 > p-value" = "red")

# Create a color factor with the custom palette and class labels
color_factor1 <- leaflet::colorFactor(palette = class_palette, levels = c("0", "1", "2", "3"))

color_factor2 <- leaflet::colorFactor(palette = class_palette, levels = c("Positive trends or trends not significantly different from the null slope", "Trends significantly negative, 0.05 > p-value > 0.01", "Trends significantly negative, 0.01 > p-value > 0.001", "Trends significantly negative, 0.001 > p-value"))

# Definisci l'ordine desiderato delle etichette delle classi
custom_order <- c("0", "1", "2", "3")

# Make sure 'Trnd_Ds' is a factor with the defined order
pixel_trend$Trnd_Cl  <- factor(pixel_trend$Trnd_Cl , levels = custom_order)

# Crea una funzione di colorazione per i cerchi
color_factor_circle <- colorFactor(
  palette = c("green", "red"),
  domain = c("+", "-")
)

ui <- fluidPage(
  titlePanel("NDVI trend analysis"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("plant_code", label="Select the plant code", choices = NULL),
      plotOutput("plot")  # Sposta questo qui per visualizzare il grafico sotto la selezione
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

server <- function(input, output, session) {
  updateSelectizeInput(session, "plant_code", choices = unique(pixel_trend$COD))
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(maxZoom = 22)) %>%
      addProviderTiles("Esri.WorldImagery", group = "Imagery", options = providerTileOptions()) %>%
      addPolygons(data = lim_paul_wgs84, 
                  fillOpacity = 0, 
                  color = "black", 
                  weight = 2,
                  group = 'Lim. Amm. Paulilatino'
      ) %>% 
      addPolygons(data = focolai_wgs84,
                  fillOpacity = 0, 
                  color = "red", 
                  weight = 2,
                  group = 'Outbreaks'
      ) %>% 
      addPolygons(data = plots_wgs84,
                  fillOpacity = 0, 
                  color = "yellow", 
                  weight = 2,
                  group = 'plots'
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
                  fillColor = ~ifelse(slope > 0, "#00ff00", color_factor1(Trnd_Cl)),
                  color = "black",
                  fillOpacity = 0.6,
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                  popup = ~paste("COD:", COD, "<br/>slope:", slope, "<br/>class:", Trnd_Cl, "<br/>Description:", Trnd_Ds),
                  group = 'Crowns',
                  stroke = TRUE,
                  weight = 1
      ) %>%
      addLegend(title="Trend: lm(NDVI ~ Month)",
                pal=color_factor2,
                values=pixel_trend$Trnd_Ds,
                opacity=0.6,
                position="topright"
      ) %>% 
      addFullscreenControl() %>%
      addLayersControl(
        overlayGroups=c("Crowns", "samples", "Lim. Amm. Paulilatino", "Outbreaks","plots"),
        options=layersControlOptions(collapsed=TRUE)
      )
  })
  
  observeEvent(input$plant_code,{
    selected_plant <- pixel_trend[pixel_trend$COD == input$plant_code,]
    
    if(nrow(selected_plant) > 0){
      centroid <- st_centroid(selected_plant$geometry)
      
      lng <- as.numeric(centroid[[1]][[1]][1])
      lat <- as.numeric(centroid[[1]][[1]][2])
      
      proxy <- leafletProxy("map")
      
      proxy %>% 
        clearGroup(group='selected_plant') %>%
        addPolygons(data=selected_plant,
                    fillColor=NA,
                    color="blue",
                    fillOpacity=0,
                    weight=3,
                    group='selected_plant',
                    stroke=TRUE) %>%
        setView(lng=lng, lat=lat, zoom=10)
    }
  })
  
  # Aggiungi questo al di fuori dell'observeEvent
  output$plot <- renderPlot({
    # Usa l'input reattivo per selezionare il grafico corretto
    results_list[[input$plant_code]]$plot
  })
}

shinyApp(ui=ui, server=server)

