# Marburg Virus Disease: Global Surveillance Dashboard
# A modern health data science application

library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(tidyr)
library(plotly)
library(DT)
library(scales)

# Read and prepare data
marburg_data <- read.csv("marburg_virus_outbreaks_complete.csv", stringsAsFactors = FALSE)

# Data processing
marburg_data <- marburg_data %>%
  mutate(
    selection_label = paste0(Country, " (", `Year.Month`, ")"),
    # Extract numeric year for timeline
    year_numeric = as.numeric(sub("^[A-Za-z]*-?", "", `Year.Month`)),
    # Parse case fatality rate
    cfr_numeric = as.numeric(gsub("%", "", Case.Fatality.Rate)),
    # Parse deaths and infected
    deaths_numeric = as.numeric(gsub("[^0-9].*", "", Total.Deaths)),
    infected_numeric = as.numeric(gsub("[^0-9].*", "", Number.Infected)),
    # Create decade grouping
    decade = paste0(floor(year_numeric/10)*10, "s")
  )

# Country coordinates
country_coords <- data.frame(
  Country = c("Germany & Yugoslavia (Serbia)", "South Africa (Zimbabwe travel)", 
              "Kenya", "Russia (Koltsovo)", "Democratic Republic of Congo",
              "Angola", "Uganda", "USA (Colorado)", "Netherlands",
              "Guinea", "Ghana", "Equatorial Guinea", "Tanzania",
              "Rwanda", "Ethiopia"),
  lat = c(48.5, -29.0, 1.0, 55.0, -4.0, -12.5, 1.5, 39.5, 52.5,
          10.0, 7.5, 1.5, -6.5, -2.0, 9.0),
  lng = c(16.0, 24.0, 38.0, 83.0, 22.0, 18.5, 32.5, -105.5, 5.5,
          -11.0, -1.5, 10.5, 35.0, 30.0, 39.0),
  stringsAsFactors = FALSE
)

marburg_data <- left_join(marburg_data, country_coords, by = "Country")

# Modern UI with bslib theme
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#17A2B8",
    secondary = "#E74C3C",
    base_font = font_google("Inter"),
    heading_font = font_google("Poppins")
  ),
  
  # Custom CSS for better tab styling
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f8f9fa;
      }
      
      .app-header {
        background: linear-gradient(135deg, #17A2B8 0%, #138496 100%);
        color: white;
        padding: 20px 30px;
        margin: -15px -15px 0 -15px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
      }
      
      .app-title {
        font-size: 28px;
        font-weight: 600;
        margin: 0;
        display: flex;
        align-items: center;
        gap: 12px;
      }
      
      .tab-container {
        background-color: white;
        margin: 0 -15px;
        padding: 0;
        border-bottom: 2px solid #dee2e6;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
      
      .nav-pills {
        padding: 10px 30px;
        gap: 5px;
      }
      
      .nav-pills .nav-link {
        color: #495057;
        font-weight: 500;
        font-size: 15px;
        padding: 10px 24px;
        border-radius: 8px;
        transition: all 0.3s ease;
        border: 2px solid transparent;
      }
      
      .nav-pills .nav-link:hover {
        background-color: #e3f7fa;
        color: #17A2B8;
        border-color: #17A2B8;
        transform: translateY(-2px);
      }
      
      .nav-pills .nav-link.active {
        background-color: #17A2B8;
        color: white;
        font-weight: 600;
        box-shadow: 0 4px 12px rgba(23, 162, 184, 0.3);
      }
      
      .nav-pills .nav-link i {
        margin-right: 6px;
      }
      
      .main-content {
        padding-top: 25px;
      }
    "))
  ),
  
  # Header
  div(class = "app-header",
      h1(class = "app-title",
         icon("virus", style = "font-size: 32px;"),
         "Marburg Virus Disease Surveillance Dashboard"
      )
  ),
  
  # Tab Navigation
  div(class = "tab-container",
      navset_pill(
        id = "main_tabs",
        
        # Overview Tab
        nav_panel(
          title = tagList(icon("chart-line"), "Overview"),
          
          div(class = "main-content",
              
              layout_columns(
                col_widths = c(3, 3, 3, 3),
                
                value_box(
                  title = "Total Outbreaks",
                  value = textOutput("total_outbreaks"),
                  showcase = icon("disease"),
                  theme = "primary",
                  p("Since 1967", style = "font-size: 0.9rem; margin-top: 10px;")
                ),
                
                value_box(
                  title = "Total Cases",
                  value = textOutput("total_cases"),
                  showcase = icon("users"),
                  theme = "info"
                ),
                
                value_box(
                  title = "Total Deaths",
                  value = textOutput("total_deaths"),
                  showcase = icon("heart-broken"),
                  theme = "danger"
                ),
                
                value_box(
                  title = "Countries Affected",
                  value = textOutput("countries_affected"),
                  showcase = icon("globe"),
                  theme = "success"
                )
              ),
              
              br(),
              
              card(
                card_header(
                  class = "bg-dark",
                  "Outbreak Magnitude by Region"
                ),
                card_body(
                  plotlyOutput("outbreak_magnitude_chart", height = "300px")
                )
              )
          )
        ),
        
        # Geographic Analysis Tab
        nav_panel(
          title = tagList(icon("map"), "Geographic Analysis"),
          
          div(class = "main-content",
              
              layout_sidebar(
                sidebar = sidebar(
                  width = 350,
                  
                  selectInput(
                    "outbreak_selection",
                    "Select Outbreak:",
                    choices = c("All Outbreaks" = "all", 
                                setNames(marburg_data$selection_label, 
                                         marburg_data$selection_label)),
                    selected = "all"
                  ),
                  
                  hr(),
                  
                  uiOutput("outbreak_details_panel")
                ),
                
                card(
                  full_screen = TRUE,
                  card_header(
                    class = "bg-primary",
                    "Interactive Outbreak Map"
                  ),
                  card_body(
                    padding = 0,
                    leafletOutput("outbreak_map", height = "600px")
                  )
                )
              )
          )
        ),
        
        # Data Explorer Tab
        nav_panel(
          title = tagList(icon("table"), "Data Explorer"),
          
          div(class = "main-content",
              
              card(
                card_header(
                  class = "bg-success",
                  "Complete Outbreak Database"
                ),
                card_body(
                  DTOutput("data_table")
                )
              )
          )
        ),
        
        # About Tab
        nav_panel(
          title = tagList(icon("info-circle"), "About"),
          
          div(class = "main-content",
              
              card(
                card_header(
                  class = "bg-dark",
                  icon("info-circle"), " About This Dashboard"
                ),
                card_body(
                  h4("Marburg Virus Disease Surveillance Dashboard"),
                  p("This interactive dashboard provides comprehensive analysis of Marburg virus disease outbreaks from 1967 to 2025."),
                  tags$ul(
                    tags$li("Data sourced from CDC, WHO, and peer-reviewed publications"),
                    tags$li("Includes 19 documented outbreaks across 4 continents"),
                    tags$li("Real-time interactive visualizations and geospatial analysis"),
                    tags$li("Designed for public health professionals and researchers")
                  ),
                  hr(),
                  p(strong("Data Currency:"), " Updated through November 2025"),
                  p(strong("Developer:"), " Health Data Science Team")
                )
              )
          )
        )
      )
  )
)

# Server
server <- function(input, output, session) {
  
  # Summary Statistics (Overview Tab)
  output$total_outbreaks <- renderText({
    format(nrow(marburg_data), big.mark = ",")
  })
  
  output$total_cases <- renderText({
    total <- sum(marburg_data$infected_numeric, na.rm = TRUE)
    format(total, big.mark = ",")
  })
  
  output$total_deaths <- renderText({
    total <- sum(marburg_data$deaths_numeric, na.rm = TRUE)
    format(total, big.mark = ",")
  })
  
  output$countries_affected <- renderText({
    length(unique(marburg_data$Country))
  })
  
  # Outbreak Magnitude Chart
  output$outbreak_magnitude_chart <- renderPlotly({
    magnitude_data <- marburg_data %>%
      arrange(desc(infected_numeric)) %>%
      head(10)
    
    plot_ly(magnitude_data, 
            x = ~infected_numeric, 
            y = ~reorder(paste0(Country, " (", Year.Month, ")"), infected_numeric),
            type = 'bar',
            orientation = 'h',
            marker = list(color = '#3498DB'),
            text = ~paste0("Cases: ", Number.Infected, "<br>Deaths: ", Total.Deaths),
            hoverinfo = 'text') %>%
      layout(
        xaxis = list(title = "Number of Cases"),
        yaxis = list(title = ""),
        plot_bgcolor = '#ffffff',
        paper_bgcolor = '#ffffff',
        margin = list(l = 200)
      )
  })
  
  # Filtered data (no year filter now, shows all)
  filtered_data <- reactive({
    marburg_data
  })
  
  # Selected outbreak data
  selected_outbreak <- reactive({
    if(input$outbreak_selection == "all") {
      return(filtered_data())
    } else {
      marburg_data %>%
        filter(selection_label == input$outbreak_selection)
    }
  })
  
  is_all_outbreaks <- reactive({
    input$outbreak_selection == "all"
  })
  
  # Interactive Map
  output$outbreak_map <- renderLeaflet({
    outbreak <- selected_outbreak()
    
    if(is_all_outbreaks()) {
      # All outbreaks view
      map <- leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = 20, lat = 5, zoom = 3)
      
      for(i in 1:nrow(outbreak)) {
        # Determine marker color based on CFR
        cfr <- outbreak$cfr_numeric[i]
        marker_color <- if(is.na(cfr)) "#95A5A6"
        else if(cfr < 30) "#2ECC71"
        else if(cfr < 60) "#F39C12"
        else "#E74C3C"
        
        # Popup content
        popup_content <- paste0(
          "<div style='min-width: 200px; font-family: Inter, sans-serif;'>",
          "<h4 style='margin: 0 0 8px 0; color: #2C3E50; border-bottom: 2px solid ", marker_color, "; padding-bottom: 5px;'>",
          outbreak$Country[i], "</h4>",
          "<p style='margin: 4px 0; color: #7F8C8D;'><strong>Period:</strong> ", outbreak$`Year.Month`[i], "</p>",
          "<div style='background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%); padding: 12px; border-radius: 8px; margin: 10px 0;'>",
          "<table style='width: 100%; font-size: 13px;'>",
          "<tr><td style='color: #34495E;'><strong>Cases:</strong></td><td style='text-align: right; color: #E74C3C; font-weight: bold; font-size: 16px;'>", 
          outbreak$Number.Infected[i], "</td></tr>",
          "<tr><td style='color: #34495E;'><strong>Deaths:</strong></td><td style='text-align: right; color: #E74C3C; font-weight: bold; font-size: 16px;'>", 
          outbreak$Total.Deaths[i], "</td></tr>",
          "<tr><td style='color: #34495E;'><strong>CFR:</strong></td><td style='text-align: right; font-weight: bold; font-size: 16px; color: ", marker_color, ";'>", 
          outbreak$Case.Fatality.Rate[i], "</td></tr>",
          "</table></div>",
          "</div>"
        )
        
        # Label for hover
        label_content <- paste0(outbreak$Country[i], " (", outbreak$`Year.Month`[i], ")")
        
        map <- map %>%
          addCircleMarkers(
            lng = outbreak$lng[i],
            lat = outbreak$lat[i],
            radius = sqrt(outbreak$infected_numeric[i]) * 2,
            color = marker_color,
            fillColor = marker_color,
            fillOpacity = 0.7,
            stroke = TRUE,
            weight = 2,
            popup = popup_content,
            label = label_content,
            layerId = paste0("marker_", i)
          )
      }
      
      map
    } else {
      # Single outbreak view
      popup_content <- paste0(
        "<div style='min-width: 250px; font-family: Inter, sans-serif;'>",
        "<h3 style='margin: 0 0 10px 0; color: #2C3E50;'>", outbreak$Country, "</h3>",
        "<p style='color: #7F8C8D; margin: 5px 0;'><strong>Period:</strong> ", outbreak$`Year.Month`, "</p>",
        "<div style='background: #E8F5E9; padding: 15px; border-radius: 8px; margin: 15px 0; border-left: 4px solid #2ECC71;'>",
        "<h5 style='margin: 0 0 10px 0; color: #27AE60;'>Outbreak Statistics</h5>",
        "<div style='display: grid; grid-template-columns: 1fr 1fr; gap: 15px;'>",
        "<div style='text-align: center;'>",
        "<div style='color: #7F8C8D; font-size: 11px; text-transform: uppercase;'>Cases</div>",
        "<div style='color: #E74C3C; font-size: 24px; font-weight: bold;'>", outbreak$Number.Infected, "</div>",
        "</div>",
        "<div style='text-align: center;'>",
        "<div style='color: #7F8C8D; font-size: 11px; text-transform: uppercase;'>Deaths</div>",
        "<div style='color: #E74C3C; font-size: 24px; font-weight: bold;'>", outbreak$Total.Deaths, "</div>",
        "</div>",
        "</div>",
        "<div style='text-align: center; margin-top: 10px; padding-top: 10px; border-top: 1px solid #BDC3C7;'>",
        "<div style='color: #7F8C8D; font-size: 11px; text-transform: uppercase;'>Case Fatality Rate</div>",
        "<div style='color: #C0392B; font-size: 28px; font-weight: bold;'>", outbreak$Case.Fatality.Rate, "</div>",
        "</div>",
        "</div>",
        "</div>"
      )
      
      cfr <- outbreak$cfr_numeric
      marker_color <- if(is.na(cfr)) "#95A5A6"
      else if(cfr < 30) "#2ECC71"
      else if(cfr < 60) "#F39C12"
      else "#E74C3C"
      
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = outbreak$lng, lat = outbreak$lat, zoom = 6) %>%
        addCircleMarkers(
          lng = outbreak$lng,
          lat = outbreak$lat,
          radius = 25,
          color = marker_color,
          fillColor = marker_color,
          fillOpacity = 0.7,
          stroke = TRUE,
          weight = 3,
          popup = popup_content
        ) %>%
        addCircles(
          lng = outbreak$lng,
          lat = outbreak$lat,
          radius = 200000,
          color = marker_color,
          fillColor = marker_color,
          fillOpacity = 0.15,
          stroke = TRUE,
          weight = 2
        )
    }
  })
  
  # Outbreak Details Panel
  output$outbreak_details_panel <- renderUI({
    if(is_all_outbreaks()) {
      card(
        card_header(
          class = "bg-info",
          icon("info-circle"), " Information"
        ),
        card_body(
          p("Showing all outbreaks from 1967 to 2025"),
          p(strong("Total Outbreaks: "), nrow(filtered_data())),
          hr(),
          p("Click on any marker to view detailed statistics.", 
            style = "font-size: 0.9rem; color: #7F8C8D;")
        )
      )
    } else {
      outbreak <- selected_outbreak()
      card(
        card_header(
          class = "bg-primary",
          icon("virus"), " Outbreak Details"
        ),
        card_body(
          h5(outbreak$Country, class = "text-primary"),
          p(strong("Time Period:"), outbreak$`Year.Month`),
          p(strong("Duration:"), outbreak$Duration),
          hr(),
          h6("Key Information:", class = "text-secondary"),
          p(outbreak$Key.Details.and.Notes, style = "font-size: 0.9rem; line-height: 1.6;"),
          hr(),
          p(
            tags$a(
              href = outbreak$Source.Link,
              target = "_blank",
              icon("external-link-alt"), " View Source",
              class = "btn btn-sm btn-outline-primary"
            )
          )
        )
      )
    }
  })
  
  # Data Table
  output$data_table <- renderDT({
    display_data <- marburg_data %>%
      select(
        `Year` = Year.Month,
        Country,
        `Cases` = Number.Infected,
        `Deaths` = Total.Deaths,
        `CFR` = Case.Fatality.Rate,
        Duration
      )
    
    datatable(
      display_data,
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        scrollX = TRUE,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#2C3E50', 'color': '#fff'});",
          "}"
        )
      ),
      class = 'cell-border stripe hover',
      rownames = FALSE,
      extensions = 'Buttons'
    )
  })
  
  # Observer for marker clicks
  observeEvent(input$outbreak_map_marker_click, {
    click <- input$outbreak_map_marker_click
    if(!is.null(click) && is_all_outbreaks()) {
      marker_id <- click$id
      if(!is.null(marker_id) && grepl("^marker_", marker_id)) {
        marker_index <- as.numeric(gsub("marker_", "", marker_id))
        clicked_outbreak <- filtered_data()[marker_index, ]
        
        updateSelectInput(session, "outbreak_selection",
                          selected = clicked_outbreak$selection_label)
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)