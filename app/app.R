library(shiny)
library(leaflet)
library(raster)
library(sf)

# Load your raster data here
files_list <- list.files("~/Desktop/Git/distant shiny/cuzin-roudy_et_al-2014", full.names = TRUE)
raster_data <- lapply(files_list, raster)

# Define the complex polygon in WKT format
polygon_wkt <- 'POLYGON((180 -44.3, 173 -44.3, 173 -47.5, 170 -47.5, 157 -47.5, 157 -45.9, 150 -45.9, 150 -47.5, 143 -47.5, 143 -45.8, 140 -45.8, 140 -44.5, 137 -44.5, 137 -43, 135 -43, 135 -41.7, 131 -41.7, 131 -40.1, 115 -40.1, 92 -40.1, 92 -41.4, 78 -41.4, 78 -42.3, 69 -42.3, 69 -43.3, 47 -43.3, 47 -41.7, 30 -41.7, 12 -41.7, 12 -40.3, 10 -40.3, 10 -38.3, -5 -38.3, -5 -38.9, -9 -38.9, -9 -40.2, -13 -40.2, -13 -41.4, -21 -41.4, -21 -42.5, -39 -42.5, -39 -40.7, -49 -40.7, -49 -48.6, -54 -48.6, -54 -55.7, -62.7972582608082 -55.7, -64 -55.7, -64 -57.8, -71 -57.8, -71 -58.9, -80 -58.9, -80 -40, -125 -40, -167 -40, -167 -42.6, -171 -42.6, -171 -44.3, -180 -44.3, -180 -90, 0 -90, 180 -90, 180 -44.3))'
# Parse the WKT string into a polygon object
polygon <- st_as_sfc(polygon_wkt)

# Define UI
ui <- fluidPage(
  titlePanel("DistAnt - Species distribution model outputs viewer"),
  sidebarLayout(
    sidebarPanel(
      # Input element for selecting species
      tags$h3("Choose your species"),
      selectInput("species", "Select species", choices = basename(files_list)),
      hr(), # Add a horizontal line separator
      checkboxGroupInput("specific_areas", "Toggle specific areas",
                         choices = c("Antarctic Peninsula", "Wedell Sea")),
      checkboxGroupInput("ccamlr_areas", "Toggle CCAMLR areas",
                         choices = c("Convention area", "High seas area")),
      checkboxInput("toggle_polygon", "Toggle MEASO polygon"),
      hr(), # Add a horizontal line separator
      # Title for custom bounding box creation section
      tags$h3("Create a custom bounding box"),
      # New sidebar panel for creating custom bounding box
      numericInput("bbox_lat1", "Latitude 1 (bottom)", value = 0),
      numericInput("bbox_lat2", "Latitude 2 (top)", value = 0),
      numericInput("bbox_lng1", "Longitude 1 (right)", value = 0),
      numericInput("bbox_lng2", "Longitude 2 (left)", value = 0),
      actionButton("add_bbox", "Add custom bounding box"),
      checkboxInput("toggle_custom_bbox", "Toggle custom bounding boxes"),
      conditionalPanel(
        condition = "input.toggle_custom_bbox",
        uiOutput("bbox_toggle_buttons")
      ),
      downloadButton("download_cropped_raster", "Download Cropped Raster")
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
  
  output$map <- renderLeaflet({
    # Create a Leaflet map
    map <- leaflet() %>%
      # Add tile layer (basic map) underneath the raster image
      addTiles() %>%
      # Add raster layer to the map based on the selected species
      addRasterImage(raster_data[[which(basename(files_list) == input$species)]],
                     colors = "viridis")
    
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
    if (input$toggle_polygon) {
      map <- map %>% addPolygons(data = polygon, color = "grey", fill = FALSE)
    }
    
    # Add bounding boxes for CCAMLR areas based on selected checkboxes
    if ("Convention area" %in% input$ccamlr_areas) {
      map <- map %>% addRectangles(
        lng1 = 20, lat1 = -60,  # Lower left corner of bounding box 1
        lng2 = 180, lat2 = -45,  # Upper right corner of bounding box 1
        stroke = TRUE, fill = FALSE, color = "red"
      )
    }
    
    # Add bounding boxes for CCAMLR areas based on selected checkboxes
    if ("High seas area" %in% input$ccamlr_areas) {
      map <- map %>% addRectangles(
        lng1 = 30, lat1 = -60,  # Lower left corner of bounding box 1
        lng2 = 160, lat2 = -45,  # Upper right corner of bounding box 1
        stroke = TRUE, fill = FALSE, color = "red"
      )
    }
    
    # Add scale bar with latitude and longitude values
    map <- map %>% addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE))
    
    # Update the name of the currently visualized species
    selected_species(basename(files_list)[which(basename(files_list) == input$species)])
    
    
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
    rv$bboxes <- c(rv$bboxes, list(bbox))
    rv$bbox_visibility <- c(rv$bbox_visibility, list(TRUE))
  })
  
  # Event handler for toggling custom bounding box visibility
  observeEvent(input$toggle_bbox, {
    rv$bbox_visibility[[input$toggle_bbox]] <- !rv$bbox_visibility[[input$toggle_bbox]]
  })
  
  # Event handler for cropping raster to custom bounding box
  observeEvent(input$download_cropped_raster, {
    if (!is.null(cropped_raster())) {
      downloadHandler(
        filename = function() {
          paste("cropped_raster_", Sys.Date(), ".tif", sep = "")
        },
        content = function(file) {
          writeRaster(cropped_raster(), filename = file, format = "GTiff")
        }
      )
    }
  })
  
  # Function to crop raster to bounding box
  crop_raster_to_bbox <- function(raster_data, bbox) {
    extent_bbox <- extent(bbox$lng1, bbox$lng2, bbox$lat1, bbox$lat2)
    cropped_raster <- crop(raster_data, extent_bbox)
    return(cropped_raster)
  }
  
  # Observer for updating cropped raster when custom bounding boxes change
  observeEvent(rv$bboxes, {
    if (!is.null(rv$bboxes) && length(rv$bboxes) > 0) {
      cropped_raster_list <- lapply(rv$bboxes, function(bbox) {
        crop_raster_to_bbox(raster_data[[which(basename(files_list) == input$species)]], bbox)
      })
      cropped_raster(raster::stack(cropped_raster_list))
    } else {
      cropped_raster(NULL)
    }
  })
  
  # Dynamically generate toggle buttons for custom bounding boxes
  output$bbox_toggle_buttons <- renderUI({
    toggle_buttons <- lapply(seq_along(rv$bboxes), function(i) {
      checkboxInput(paste0("toggle_bbox_", i), paste("Toggle Bounding Box", i), value = TRUE)
    })
    do.call(tagList, toggle_buttons)
  })
  
  # Event handler for toggling custom bounding boxes individually
  observeEvent(input$toggle_bbox, {
    bbox_index <- as.numeric(sub("toggle_bbox_", "", input$toggle_bbox))
    rv$bbox_visibility[[bbox_index]] <- !rv$bbox_visibility[[bbox_index]]
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
