library(shiny)
library(leaflet)
library(raster)
library(sf)
library(dplyr)
library(shinyjs)

# Load your raster data here
files_list <- list.files("~/Desktop/Git/Shiny-DistAnt/data", full.names = TRUE, recursive = TRUE, pattern = ".tif$")
raster_data <- lapply(files_list, raster)

# Define the complex polygon in WKT format
polygon_wkt <- 'POLYGON((180 -44.3, 173 -44.3, 173 -47.5, 170 -47.5, 157 -47.5, 157 -45.9, 150 -45.9, 150 -47.5, 143 -47.5, 143 -45.8, 140 -45.8, 140 -44.5, 137 -44.5, 137 -43, 135 -43, 135 -41.7, 131 -41.7, 131 -40.1, 115 -40.1, 92 -40.1, 92 -41.4, 78 -41.4, 78 -42.3, 69 -42.3, 69 -43.3, 47 -43.3, 47 -41.7, 30 -41.7, 12 -41.7, 12 -40.3, 10 -40.3, 10 -38.3, -5 -38.3, -5 -38.9, -9 -38.9, -9 -40.2, -13 -40.2, -13 -41.4, -21 -41.4, -21 -42.5, -39 -42.5, -39 -40.7, -49 -40.7, -49 -48.6, -54 -48.6, -54 -55.7, -62.7972582608082 -55.7, -64 -55.7, -64 -57.8, -71 -57.8, -71 -58.9, -80 -58.9, -80 -40, -125 -40, -167 -40, -167 -42.6, -171 -42.6, -171 -44.3, -180 -44.3, -180 -90, 0 -90, 180 -90, 180 -44.3))'
# Parse the WKT string into a polygon object
polygon <- st_as_sfc(polygon_wkt)

# Load the CSV file containing the species names
species_info <- read.csv("metadata.csv")

# Create a tibble to store the file names and taxon names and filter to keep only the rows corresponding to the raster files
taxon_lookup <- tibble(file = species_info$file, taxon = species_info$taxon) %>%
  filter(file %in% basename(files_list))

# Define UI
ui <- fluidPage(
  titlePanel("DistAnt - Ecological model outputs viewer"),
  sidebarLayout(
    sidebarPanel(
      # Input element for selecting species
      tags$h3("Choose your model output"),
      selectInput("species", "Select file", choices = basename(files_list)),
      hr(), # Add a horizontal line separator
      checkboxGroupInput("specific_areas", "Toggle specific areas",
                         choices = c("MEASO polygon","Antarctic Peninsula", "Wedell Sea")),
      checkboxGroupInput("ccamlr_areas", "Toggle CCAMLR areas",
                         choices = c("Convention area", "High seas area")),
      hr(), # Add a horizontal line separator
      # Title for custom bounding box creation section
      tags$h3("Create a custom bounding box"),
      # New sidebar panel for creating custom bounding box
      sliderInput("bbox_lat1", "Latitude 1 - Bottom", min = -90, max = 90, value = 0),
      sliderInput("bbox_lat2", "Latitude 2 - Top", min = -90, max = 90, value = 0),
      sliderInput("bbox_lng1", "Longitude 1 - Left", min = -180, max = 180, value = 0),
      sliderInput("bbox_lng2", "Longitude 2 - Right", min = -180, max = 180, value = 0),
      actionButton("add_bbox", "Add custom bounding box"),
      tags$div(id = "toggle_bbox_buttons", style = "display: none;",
               uiOutput("bbox_toggle_buttons")),
      downloadButton("download_cropped_raster", "Download custom area")
    ),
    mainPanel(
      # Leaflet map output
      leafletOutput("map"),
      # Display the name of the currently visualized species
      verbatimTextOutput("selected_species")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive value to store the name of the currently visualized species
  selected_species <- reactiveVal("")
  # Reactive values for storing custom bounding boxes and their visibility states
  rv <- reactiveValues(bboxes = list(), bbox_visibility = list())
  cropped_raster <- reactiveVal(NULL)
  
  observe({
    # Find the extent of the selected raster
    selected_raster <- raster_data[[which(basename(files_list) == input$species)]]
    extent_selected <- extent(selected_raster)
    # Update slider values based on extent
    updateSliderInput(session, "bbox_lat1", value = extent_selected@ymin, min = extent_selected@ymin, max = extent_selected@ymax)
    updateSliderInput(session, "bbox_lat2", value = extent_selected@ymax, min = extent_selected@ymin, max = extent_selected@ymax)
    updateSliderInput(session, "bbox_lng1", value = extent_selected@xmin, min = extent_selected@xmin, max = extent_selected@xmax)
    updateSliderInput(session, "bbox_lng2", value = extent_selected@xmax, min = extent_selected@xmin, max = extent_selected@xmax)
  })
  
  output$map <- renderLeaflet({
    # Create a Leaflet map
    map <- leaflet() %>%
      # Add tile layer (basic map) underneath the raster image
      addTiles() %>%
      # Add raster layer to the map based on the selected species
      addRasterImage(raster_data[[which(basename(files_list) == input$species)]],
                     colors = "viridis")
    
    # Add custom bounding box if it exists and is visible
    if (!is.null(rv$bbox) && rv$bbox_visibility) {
      map <- map %>% addRectangles(
        lng1 = rv$bbox$lng1, lat1 = rv$bbox$lat1,
        lng2 = rv$bbox$lng2, lat2 = rv$bbox$lat2,
        stroke = TRUE, fill = FALSE, color = "green"
      )
    }
    
    # Add bounding boxes for protected areas based on selected checkboxes
    if ("Antarctic Peninsula" %in% input$specific_areas) {
      map <- map %>% addRectangles(
        lng1 = -80, lat1 = -75,  # Lower left corner of bounding box 1
        lng2 = -50, lat2 = -60,  # Upper right corner of bounding box 1
        stroke = TRUE, fill = FALSE, color = "blue"
      )
    }
    
    if ("Wedell Sea" %in% input$specific_areas) {
      map <- map %>% addRectangles(
        lng1 = -70, lat1 = -79,  # Lower left corner of bounding box 2
        lng2 = -15, lat2 = -60,   # Upper right corner of bounding box 2
        stroke = TRUE, fill = FALSE, color = "blue"
      )
    }
    
    # Add custom bounding boxes
    for (i in seq_along(rv$bboxes)) {
      if (rv$bbox_visibility[[i]]) {
        map <- map %>% addRectangles(
          lng1 = rv$bboxes[[i]]$lng1, lat1 = rv$bboxes[[i]]$lat1,
          lng2 = rv$bboxes[[i]]$lng2, lat2 = rv$bboxes[[i]]$lat2,
          stroke = TRUE, fill = FALSE, color = "green"
        )
      }
    }
    
    # Add the complex polygon if the toggle is checked
    if ("MEASO polygon" %in% input$specific_areas) {
      map <- map %>% addPolygons(data = polygon, color = "grey", fill = FALSE)
    }
    
    # Add bounding boxes for CCAMLR areas based on selected checkboxes
    if ("Convention area" %in% input$ccamlr_areas) {
      map <- map %>% addRectangles(
        lng1 = 0, lat1 = -60,  # Lower left corner of bounding box 1
        lng2 = 180, lat2 = -45,  # Upper right corner of bounding box 1
        stroke = TRUE, fill = FALSE, color = "red"
      )
    }
    
    # Add bounding boxes for CCAMLR areas based on selected checkboxes
    if ("High seas area" %in% input$ccamlr_areas) {
      map <- map %>% addRectangles(
        lng1 = 180, lat1 = -60,  # Lower left corner of bounding box 1
        lng2 = -180, lat2 = -45,  # Upper right corner of bounding box 1
        stroke = TRUE, fill = FALSE, color = "red"
      )
    }
    
    # Add scale bar with latitude and longitude values
    map <- map %>% addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE))
    
    # Update the name of the currently visualized species
    selected_species(taxon_lookup$taxon[taxon_lookup$file == input$species])
    
    return(map)
  })
  
  output$selected_species <- renderText({
    paste("Currently visualized species:", selected_species())
  })
  
  # Event handler for adding custom bounding box
  observeEvent(input$add_bbox, {
    bbox <- list(
      lng1 = input$bbox_lng1, lat1 = input$bbox_lat1,
      lng2 = input$bbox_lng2, lat2 = input$bbox_lat2
    )
    rv$bbox <- bbox
    rv$bbox_visibility <- TRUE
  })
  
  # Function to crop raster to bounding box
  crop_raster_to_bbox <- function(raster_data, bbox) {
    extent_bbox <- extent(bbox$lng1, bbox$lng2, bbox$lat1, bbox$lat2)
    cropped_raster <- crop(raster_data, extent_bbox)
    return(cropped_raster)
  }
  
  # Observer for updating cropped raster when custom bounding box changes
  observeEvent(rv$bbox, {
    if (!is.null(rv$bbox)) {
      cropped_raster(crop_raster_to_bbox(raster_data[[which(basename(files_list) == input$species)]], rv$bbox))
    } else {
      cropped_raster(NULL)
    }
  })
  
  # Download handler for cropped raster
  output$download_cropped_raster <- downloadHandler(
    filename = function() {
      species_name <- selected_species()
      species_name <- gsub(" ", "_", species_name)  # Replace space with underscore
      file_name <- paste(species_name, "_custom_", Sys.Date(), ".tif", sep = "")
    },
    content = function(file) {
      if (!is.null(cropped_raster())) {
        writeRaster(cropped_raster(), filename = file, format = "GTiff")
      }
    }
  )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
