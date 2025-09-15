# Example Shiny app created for the 2025 AWWA WQTC Workshop in Tacoma, WA (11/9/2025 - 11/13/2025)

# Load Packages ----------------------------------------------------------------
library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)


# Load Data --------------------------------------------------------------------
data <- read.csv("Data/wqp_data.csv", stringsAsFactors = FALSE)
sites <- read.csv("Data/streams_sites.csv", stringsAsFactors = FALSE)


# Data Preprocessing -----------------------------------------------------------
# Parse date column
data$date_time <- as.POSIXct(data$date_time, tz = "UTC")

# Merge site info
data <- data %>%
  left_join(sites %>% select(SITE_NAME, LAT, LON, AquaticLifeUse),
            by = "SITE_NAME")


# User Interface (i.e., Front-End Code) ----------------------------------------
ui <- fluidPage(
  titlePanel("Example Water Quality Dashboard"),
  br(),
  fluidRow(
    # Left: Plot + Stats
    column(6,
           fluidRow(
             column(6,
                    selectInput("site", "Select Site:", choices = unique(data$SITE_NAME))
             ),
             column(6,
                    selectInput("parameter", "Select Parameter:", choices = unique(data$parameter))
             )
           ),
           br(),
           plotOutput("timePlot", height = 500),
           hr(),
           h4("Summary Statistics"),
           tableOutput("summaryTable")
    ),
    
    # Right: Location Map
    column(6,
           leafletOutput("map", height = 800)
    )
  )
)


# Server (i.e., Back-End Code) -------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive values to store selected site (from dropdown or map click)
  selected_site <- reactiveVal(unique(data$SITE_NAME)[1])
  
  # Update selected site when dropdown changes
  observeEvent(input$site, {
    selected_site(input$site)
  })
  
  # Filtered dataset based on user selection
  filtered <- reactive({
    req(selected_site(), input$parameter)
    data %>%
      filter(SITE_NAME == selected_site(), parameter == input$parameter)
  })
  
  
  ## Location Map --------------------------------------------------------------
  output$map <- renderLeaflet({
    site_locs <- sites %>%
      select(SITE_NAME, LAT, LON, AquaticLifeUse) %>%
      distinct()
    
    last_samples <- data %>%
      group_by(SITE_NAME) %>%
      summarise(last_date = max(date_time, na.rm = TRUE), .groups = "drop")
    
    site_locs <- site_locs %>%
      left_join(last_samples, by = "SITE_NAME")
    
    leaflet(site_locs) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        ~LON, ~LAT,
        layerId = ~SITE_NAME,
        label = ~SITE_NAME,
        color = "blue",
        radius = 6, fillOpacity = 0.9, stroke = FALSE,
        popup = ~paste0(
          "<b>", SITE_NAME, "</b><br/>",
          "Aquatic Use: ", AquaticLifeUse, "<br/>",
          "Last Sample: ", format(last_date, "%Y-%m-%d")
        )
      )
  })
  
  # Handle map clicks without re-rendering (i.e., use leafletProxy)
  observeEvent(input$map_marker_click, {
    clicked <- input$map_marker_click$id
    if (!is.null(clicked)) {
      selected_site(clicked)
      updateSelectInput(session, "site", selected = clicked)
      
      # Update marker color dynamically
      leafletProxy("map") %>%
        clearGroup("selected") %>%
        addCircleMarkers(
          data = sites %>% filter(SITE_NAME == clicked),
          lng = ~LON, lat = ~LAT,
          color = "red", radius = 8,
          fillOpacity = 1, stroke = FALSE,
          group = "selected"
        )
    }
  })
  
  
  ## Time Series Plot ----------------------------------------------------------
  output$timePlot <- renderPlot({
    df <- filtered()
    if(nrow(df) == 0) return()
    
    ggplot(df, aes(x = date_time, y = value)) +
      geom_line(color = "blue") +
      geom_point(color = "darkblue") +
      labs(title = paste(input$parameter, "at", selected_site()),
           x = "Date", y = paste0(input$parameter, " (", unique(df$unit), ")")) +
      theme(
        plot.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14)
      )
  })
  
  
  ## Summary Statistics --------------------------------------------------------
  output$summaryTable <- renderTable({
    df <- filtered()
    if(nrow(df) == 0) return(data.frame(Message = "No data"))
    
    # Get metadata for the selected site
    site_info <- sites %>%
      filter(SITE_NAME == selected_site()) %>%
      slice(1)
    
    stats <- df %>%
      summarise(
        n = n(),
        mean = mean(value, na.rm = TRUE),
        min = min(value, na.rm = TRUE),
        max = max(value, na.rm = TRUE),
        start_date = min(date_time, na.rm = TRUE),
        end_date = max(date_time, na.rm = TRUE)
      )
    
    data.frame(
      Site = site_info$SITE_NAME,
      AquaticUse = site_info$AquaticLifeUse,
      Records = stats$n,
      Mean = round(stats$mean, 2),
      Min = round(stats$min, 2),
      Max = round(stats$max, 2)
    )
  })
}

shinyApp(ui, server)
