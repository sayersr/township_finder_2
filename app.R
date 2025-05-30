library(shiny)
library(sf)
library(tidygeocoder)
library(dplyr)
library(leaflet)
library(httr)
library(jsonlite)

# Function to fetch data from ArcGIS REST service
fetch_feature_service <- function(service_url, where_clause = "1=1") {
  tryCatch({
    query_url <- paste0(service_url, "/query")
    response <- GET(query_url, query = list(
      where = where_clause,
      outFields = "*",
      f = "geojson",
      outSR = "4326"
    ))
    
    if (status_code(response) == 200) {
      geojson_text <- content(response, as = "text", encoding = "UTF-8")
      sf_data <- st_read(geojson_text, quiet = TRUE)
      return(sf_data)
    } else {
      return(NULL)
    }
  }, error = function(e) {
    return(NULL)
  })
}

ui <- fluidPage(
  titlePanel("New York Metro Township Finder"),
  
  # Add custom CSS for better styling
  tags$head(
    tags$style(HTML("
      body { font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; }
      .main-container { max-width: 1200px; margin: 0 auto; }
      .sidebar { background-color: #f8f9fa; padding: 20px; border-radius: 8px; }
      .results-panel { margin-top: 20px; }
      .btn-primary { 
        margin-top: 15px; 
        width: 100%; 
        background-color: #0066cc;
        border-color: #0066cc;
        font-weight: 500;
      }
      .btn-primary:hover {
        background-color: #0052a3;
        border-color: #0052a3;
      }
      .result-item { 
        font-size: 15px; 
        margin: 8px 0; 
        padding: 12px;
        background-color: #f8f9fa;
        border-left: 4px solid #0066cc;
        border-radius: 4px;
        min-height: 20px;
      }
      .result-item.placeholder {
        background-color: #f1f3f4;
        border-left-color: #6c757d;
        color: #6c757d;
        font-style: italic;
      }
      .status-text {
        font-size: 13px;
        color: #6c757d;
        font-style: italic;
      }
      .form-control {
        border-radius: 4px;
        border: 1px solid #ced4da;
      }
      .form-control:focus {
        border-color: #0066cc;
        box-shadow: 0 0 0 0.2rem rgba(0, 102, 204, 0.25);
      }
      .title-panel h1 {
        color: #0066cc;
        font-weight: 300;
        margin-bottom: 30px;
      }
      .info-text {
        background-color: #e7f3ff;
        border: 1px solid #bee5eb;
        border-radius: 4px;
        padding: 10px;
        margin-top: 20px;
        font-size: 13px;
        color: #055160;
      }
    ")),
    
    # JavaScript to update result styling
    tags$script(HTML("
      $(document).on('shiny:value', function(event) {
        if (event.name === 'matched_address') {
          var addressDiv = $('#address-result');
          var townshipDiv = $('#township-result');
          var countyDiv = $('#county-result');
          
          if (event.value && event.value.includes('📍')) {
            // Real result - remove placeholder styling
            addressDiv.removeClass('placeholder');
            townshipDiv.show();
            countyDiv.show();
          } else {
            // Placeholder or empty - add placeholder styling
            addressDiv.addClass('placeholder');
            if (!event.value || event.value === '') {
              townshipDiv.hide();
              countyDiv.hide();
            }
          }
        }
      });
    "))
  ),
  
  div(class = "main-container",
      sidebarLayout(
        sidebarPanel(
          class = "sidebar",
          width = 4,
          h4("Enter Address", style = "margin-top: 0; color: #0066cc;"),
          textInput("street", "Street Address", 
                    placeholder = "123 Main Street",
                    value = ""),
          textInput("city", "City (New York only)", 
                    placeholder = "Hempstead",
                    value = ""),
          actionButton("submit", "🔍 Find Township & County", 
                       class = "btn-primary"),
          br(),
          div(class = "status-text",
              textOutput("loading_status")
          ),
          
          div(class = "info-text",
              strong("Coverage Area:"), br(),
              "Long Island, NYC, and Westchester County", br(), br(),
              strong("Supported Counties:"), br(),
              "Nassau, Suffolk, Queens, Kings (Brooklyn), New York (Manhattan), Richmond (Staten Island), Bronx, Westchester"
          )
        ),
        
        mainPanel(
          width = 8,
          div(class = "results-panel",
              h4("Results", style = "color: #0066cc; margin-bottom: 20px;"),
              
              # Results display
              div(id = "results-container",
                  div(id = "address-result", class = "result-item placeholder", textOutput("matched_address")),
                  div(id = "township-result", class = "result-item", style = "display: none;", textOutput("township")),
                  div(id = "county-result", class = "result-item", style = "display: none;", textOutput("county"))
              ),
              
              br(),
              
              # Map
              leafletOutput("map", height = "500px"),
              
              # Footer info
              div(class = "info-text", style = "margin-top: 20px;",
                  p(strong("How it works:"), "Enter a street address and city, and this app will use the US Census Bureau's geocoding service to find the exact coordinates. It then determines which township/municipality and county the address falls within using official New York State boundary data."),
                  p(strong("Note:"), "Accuracy depends on the quality of the address entered and the geocoding service. For best results, use complete street addresses.")
              )
          )
        )
      )
  )
)

server <- function(input, output, session) {
  values <- reactiveValues(
    township_shapes = NULL,
    county_shapes = NULL,
    data_loaded = FALSE,
    data_loading_started = FALSE
  )
  
  # Status display
  output$loading_status <- renderText({
    if (!values$data_loading_started) {
      "Ready to load geographic data..."
    } else if (!values$data_loaded) {
      "Loading boundary data... This may take a moment."
    } else {
      township_count <- ifelse(is.null(values$township_shapes), 0, nrow(values$township_shapes))
      county_count <- ifelse(is.null(values$county_shapes), 0, nrow(values$county_shapes))
      paste("✓ Ready! Loaded", county_count, "counties and", township_count, "municipalities.")
    }
  })
  
  # Initialize outputs with helpful placeholder text
  output$matched_address <- renderText("Enter an address above to get started")
  output$township <- renderText("")
  output$county <- renderText("")
  
  # Load feature service data when app starts
  observeEvent(once = TRUE, ignoreNULL = FALSE, ignoreInit = FALSE, {
    TRUE
  }, {
    if (!values$data_loading_started) {
      values$data_loading_started <- TRUE
      
      # Service URLs - NY State GIS services
      township_service_url <- "https://services6.arcgis.com/EbVsqZ18sv1kVJ3k/arcgis/rest/services/NYS_Civil_Boundaries/FeatureServer/6"
      county_service_url <- "https://services6.arcgis.com/EbVsqZ18sv1kVJ3k/arcgis/rest/services/NYS_Civil_Boundaries/FeatureServer/2"
      
      # Target counties for municipal boundaries
      target_counties <- c("Suffolk", "Nassau", "Queens", "Kings", "New York", "Richmond", "Bronx", "Westchester")
      
      # Create WHERE clause for townships with LIKE operators for partial matches
      county_conditions <- paste0("COUNTY LIKE '%", target_counties, "%'")
      township_where <- paste(county_conditions, collapse = " OR ")
      
      # Fetch data
      township_data <- fetch_feature_service(township_service_url, township_where)
      county_data <- fetch_feature_service(county_service_url)  # All NY counties
      
      # Store data
      if (!is.null(county_data)) {
        values$county_shapes <- county_data
      }
      
      if (!is.null(township_data)) {
        values$township_shapes <- township_data
      }
      
      values$data_loaded <- TRUE
    }
  })
  
  # Handle address submission
  observeEvent(input$submit, {
    # Clean and validate inputs
    street_clean <- trimws(input$street)
    city_raw <- trimws(input$city)
    
    # Handle "City, State" format - strip everything after comma
    city_clean <- if (grepl(",", city_raw)) {
      trimws(strsplit(city_raw, ",")[[1]][1])
    } else {
      city_raw
    }
    
    if (street_clean == "" || city_clean == "") {
      showNotification("Please enter both street address and city.", type = "warning", duration = 3)
      return()
    }
    
    if (!values$data_loaded) {
      showNotification("Geographic data still loading. Please wait a moment.", type = "warning", duration = 3)
      return()
    }
    
    if (is.null(values$township_shapes) || is.null(values$county_shapes)) {
      showNotification("Geographic data not available. Please refresh the page and try again.", type = "error")
      return()
    }
    
    # Show loading notification
    loading_id <- showNotification("Geocoding address...", type = "message", duration = NULL)
    
    # Construct the full address using cleaned city
    full_address <- paste(street_clean, city_clean, "NY")
    
    # Geocode the address
    geo_result <- tryCatch({
      geo(full_address, method = "census", full_results = TRUE)
    }, error = function(e) {
      removeNotification(loading_id)
      showNotification("Geocoding failed. Please check your address and try again.", type = "error")
      return(NULL)
    })
    
    removeNotification(loading_id)
    
    if (is.null(geo_result) || nrow(geo_result) == 0) {
      output$matched_address <- renderText("❌ Address not found. Please verify the address and try again.")
      output$township <- renderText("")
      output$county <- renderText("")
      
      # Clear map
      output$map <- renderLeaflet({
        leaflet() %>% addTiles()
      })
      return()
    }
    
    # Extract coordinates
    lat <- geo_result$lat
    lon <- geo_result$long
    matched_address <- geo_result$matchedAddress
    
    # Create point geometry
    point <- st_sf(geometry = st_sfc(st_point(c(lon, lat)), crs = 4326))
    
    # Perform spatial joins
    township_result <- tryCatch({
      st_join(point, values$township_shapes, join = st_within)
    }, error = function(e) NULL)
    
    county_result <- tryCatch({
      st_join(point, values$county_shapes, join = st_within)
    }, error = function(e) NULL)
    
    # Extract municipality name
    township_name <- if (!is.null(township_result) && nrow(township_result) > 0) {
      name_cols <- c("NAME", "Name", "MUNI_NAME", "TOWN_NAME", "CITY_NAME")
      available_cols <- intersect(name_cols, names(township_result))
      if (length(available_cols) > 0 && !is.na(township_result[[available_cols[1]]][1])) {
        township_result[[available_cols[1]]][1]
      } else "Municipality data not available"
    } else "Not found in coverage area"
    
    # Extract county name
    county_name <- if (!is.null(county_result) && nrow(county_result) > 0) {
      name_cols <- c("NAME", "Name", "COUNTY_NAME")
      available_cols <- intersect(name_cols, names(county_result))
      if (length(available_cols) > 0 && !is.na(county_result[[available_cols[1]]][1])) {
        paste(county_result[[available_cols[1]]][1], "County")
      } else "County data not available"
    } else "Not found in New York State"
    
    # Render outputs
    output$matched_address <- renderText({
      paste("📍 Geocoded Address:", matched_address)
    })
    
    output$township <- renderText({
      paste("🏛️ Municipality/Township:", township_name)
    })
    
    output$county <- renderText({
      paste("🗺️ County:", county_name)
    })
    
    # Create map
    output$map <- renderLeaflet({
      # Base map
      map <- leaflet() %>%
        addTiles() %>%
        addMarkers(
          lng = lon, lat = lat, 
          popup = paste("<strong>", matched_address, "</strong><br/>",
                        "Municipality:", township_name, "<br/>",
                        "County:", county_name),
          options = markerOptions(riseOnHover = TRUE)
        ) %>%
        setView(lng = lon, lat = lat, zoom = 14)
      
      # Add county boundaries (light blue)
      if (!is.null(values$county_shapes)) {
        map <- map %>%
          addPolygons(
            data = values$county_shapes,
            fillColor = "transparent",
            color = "#0066cc",
            weight = 2,
            opacity = 0.7,
            group = "Counties",
            popup = ~paste("County:", NAME)
          )
      }
      
      # Add municipal boundaries (red)
      if (!is.null(values$township_shapes)) {
        map <- map %>%
          addPolygons(
            data = values$township_shapes,
            fillColor = "transparent", 
            color = "#cc3300",
            weight = 1.5,
            opacity = 0.6,
            group = "Municipalities",
            popup = ~paste("Municipality:", NAME, "<br/>County:", COUNTY)
          ) %>%
          addLayersControl(
            overlayGroups = c("Counties", "Municipalities"),
            options = layersControlOptions(collapsed = FALSE)
          )
      }
      
      map
    })
    
    # Success notification
    showNotification("✓ Address successfully geocoded!", type = "message", duration = 3)
  })
}

# Run the app
shinyApp(ui = ui, server = server)