##### Libraries #####
library(shiny)
library(cpaltemplates)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(tidyverse)
library(sf)
library(janitor)
library(rio)
library(DT)
library(classInt)

##### Set Options #####
#wd <- "C:/Users/Michael/CPAL Dropbox/Analytics/Dashboards + Tools/Internal Metrics Dashboard/internal-dashboard/"
wd <- ""

##### Import Data #####
geo_data <- st_read(paste0(wd, "data/Dallas CPAL Metrics.geojson"))
metricNames <- import(paste0(wd, "data/Dallas Metric Names.csv"))

# Create a mapping of field names to descriptions
fieldDescriptionMap <- setNames(metricNames$Description, metricNames$FieldName)

# List of fields in the 'Categorization' category
categorizationFields <- metricNames %>%
  filter(Category == "Categorization") %>%
  pull(FieldName)

metricFields <- metricNames %>%
  filter(Category != "Categorization")

##### App Title #####
title <- tags$a(
  div(
    tags$img(src = "images/CPAL_Logo_White.png",
             height = "30", style = "vertical-align: middle; margin-right: 40px;"), # Add margin to the right of the image
    strong("CPAL Census Reporter", style = "font-weight: bold; vertical-align: middle;"), # Ensure text is bold
    style = "display: flex; align-items: center;" # Use flexbox to align items centrally
  )
)

##### UI Set-Up #####
ui <- navbarPage(
  title = span(title, style = "display: inline-block;"), # Ensure title is inline to allow other elements next to it
  id = "nav",
  theme = cpaltemplates::cpal_shiny(),

  tabPanel("Map",
           div(class = "outer",
               tags$head(includeCSS(paste0(wd, "www/styles.css"))),
               leafletOutput("map", height = "80vh"),
               absolutePanel(id = "controls",
                             class = "panel panel-default",
                             fixed = TRUE,
                             draggable = TRUE,
                             top = 120,
                             left = "auto",
                             right = 20,
                             bottom = "auto",
                             width = 500,
                             height = "auto",

                             selectInput("selectedCategory", h4("Choose Category"),
                                         choices = unique(metricFields$Category),
                                         selected = "Poverty",
                                         width = "100%"
                             ),
                             selectInput("selectedMetric", h4("Choose Metric"),
                                         choices = NULL,
                                         width = "100%"),
                             selectInput("geoLevel", h4("Geography Level"),
                                         choices = unique(geo_data$GEOGRAPHY),
                                         selected = "County",
                                         width = "100%"
                             ),
                             selectInput("year", h4("Year"),
                                         choices = unique(geo_data$YEAR),
                                         selected = max(geo_data$YEAR),
                                         width = "100%"
                             ),
                             htmlOutput("selectedPolygonInfo"),
                             tableOutput("clickTable")
               )
           )
  )
)

##### Server Set-Up #####
server <- function(input, output, session) {

  # Debugging initialization
  print("Server initialized")

  observe({
    initial_category <- "Poverty"
    percentFields <- metricNames %>%
      filter(Category == initial_category, type == "Percent")
    choices <- setNames(percentFields$FieldName, percentFields$Description)
    updateSelectInput(session, "selectedMetric", choices = choices)
  })

  selectedGeoLevel <- reactiveVal()
  selectedName <- reactiveVal()
  selectedPolygon <- reactiveVal(NULL)

  observe({
    selectedGeoLevel(input$geoLevel)
    selectedName(input$neighborhoodName)
  })

  observeEvent(input$selectedCategory, {
    percentFields <- metricNames %>%
      filter(Category == input$selectedCategory, type == "Percent")
    choices <- setNames(percentFields$FieldName, percentFields$Description)
    updateSelectInput(session, "selectedMetric", choices = choices)
  })

  observeEvent(input$map_shape_click, {
    selectedPolygon(input$map_shape_click$id)
    selectedPolygonName <- geo_data$NAME[geo_data$GEOID == input$map_shape_click$id]
    if (length(selectedPolygonName) > 0) {
      selectedName(selectedPolygonName)
    }
  })

  observe({
    updateSelectInput(session, "geoLevel", selected = selectedGeoLevel())
  })

  observe({
    req(selectedName())
    updateSelectInput(session, "neighborhoodName", selected = selectedName())
  })

  observeEvent(input$geoLevel, {
    # Reset the selected polygon and clickTable when geoLevel is changed
    selectedPolygon(NULL)
    output$clickTable <- renderTable({
      NULL
    })
  })

  filtered_data <- reactive({
    data <- geo_data
    if (!is.null(selectedGeoLevel())) {
      data <- data %>% filter(GEOGRAPHY == selectedGeoLevel())
    }
    if (!is.null(input$year)) {
      data <- data %>% filter(YEAR == input$year)
    }
    data
  })

  pal <- reactive({
    req(input$selectedMetric, filtered_data())
    metricData <- filtered_data()[[input$selectedMetric]]
    if(all(is.na(metricData))) {
      return(function(x) "#808080")
    }
    breaks <- classIntervals(metricData, n = 5, style = "quantile")$brks
    colorBin("Purples", domain = metricData, bins = breaks, na.color = "#808080")
  })

  output$map <- renderLeaflet({
    req(filtered_data(), input$selectedMetric, pal())
    metricDescription <- metricNames$Description[metricNames$FieldName == input$selectedMetric]
    legendTitle <- if (length(metricDescription) > 0) metricDescription else input$selectedMetric
    leaflet() %>%
      addTiles(urlTemplate = cpal_mapbox_color, attribution = cpal_leaflet) %>%
      addPolygons(data = filtered_data(),
                  layerId = ~GEOID,
                  weight = 2,
                  color = "#3f3f3f",
                  fillColor = "#042D33",
                  fillOpacity = 0.5
      )
  })

  observe({
    req(input$selectedMetric, filtered_data(), pal())
    legendTitle <- metricNames$Description[metricNames$FieldName == input$selectedMetric]
    if (length(legendTitle) == 0) {
      legendTitle <- input$selectedMetric
    }
    metricData <- filtered_data()[[input$selectedMetric]]
    req(length(metricData) > 0)
    baseMetricName <- substr(input$selectedMetric, 1, nchar(input$selectedMetric) - 1)
    labels <- paste(
      "<strong>Name:</strong>", filtered_data()$NAME,
      "<br><strong>Year:</strong>", filtered_data()$YEAR,
      "<br><strong>Estimate:</strong>", scales::comma(filtered_data()[[paste0(baseMetricName, "E")]], big.mark = ","),
      "<br><strong>Margin of Error:</strong>", scales::comma(filtered_data()[[paste0(baseMetricName, "M")]], big.mark = ","),
      "<br><strong>Percent:</strong>", scales::percent(filtered_data()[[paste0(baseMetricName, "P")]], accuracy = 0.1)
    ) %>%
      lapply(htmltools::HTML)
    leafletProxy("map", data = filtered_data()) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        layerId = ~GEOID,
        fillColor = ~pal()(metricData),
        fillOpacity = 0.7,
        weight = 2,
        color = "#3f3f3f",
        label = ~labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        ),
        highlightOptions = highlightOptions(weight = 3,
                                            color = "#E7ECEE",
                                            fillOpacity = 0.5,
                                            bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal(),
                values = ~metricData,
                title = legendTitle,
                position = "bottomleft",
                labFormat = labelFormat(
                  suffix = "%",
                  between = "% - ",
                  transform = function(x) 100 * x),
                opacity = 1.0)
  })

  observeEvent(input$map_shape_click, {
    req(input$map_shape_click$id)
    selectedPolygon(input$map_shape_click$id)
    req(selectedPolygon(), pal(), input$selectedMetric)
    metricData <- filtered_data()[[input$selectedMetric]]
    legendTitle <- metricNames$Description[metricNames$FieldName == input$selectedMetric]
    if (length(legendTitle) == 0) {
      legendTitle <- input$selectedMetric
    }
    baseMetricName <- substr(input$selectedMetric, 1, nchar(input$selectedMetric) - 1)
    labels <- paste(
      "<strong>Name:</strong>", filtered_data()$NAME,
      "<br><strong>Year:</strong>", filtered_data()$YEAR,
      "<br><strong>Estimate:</strong>", scales::comma(filtered_data()[[paste0(baseMetricName, "E")]], big.mark = ","),
      "<br><strong>Margin of Error:</strong>", scales::comma(filtered_data()[[paste0(baseMetricName, "M")]], big.mark = ","),
      "<br><strong>Percent:</strong>", scales::percent(filtered_data()[[paste0(baseMetricName, "P")]], accuracy = 0.1)
    ) %>%
      lapply(htmltools::HTML)

    leafletProxy("map", data = filtered_data()) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(layerId = ~GEOID,
                  fillColor = ~ifelse(GEOID == selectedPolygon(), "#E7ECEE", pal()(metricData)),
                  fillOpacity = ~ifelse(GEOID == selectedPolygon(), 0.5, 0.7),
                  weight = 2,
                  color = "#3f3f3f",
                  highlightOptions = highlightOptions(weight = 3,
                                                      color = "#E7ECEE",
                                                      fillOpacity = 0.7,
                                                      bringToFront = TRUE),
                  label = ~labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto"
                  )
      ) %>%
      addLegend(pal = pal(),
                values = ~metricData,
                title = legendTitle,
                position = "topleft",
                labFormat = labelFormat(
                  suffix = "%",
                  between = "% - ",
                  transform = function(x) 100 * x),
                opacity = 1.0)

    output$clickTable <- renderTable({
      req(selectedPolygon())
      selectedData <- filtered_data() %>%
        st_drop_geometry() %>%
        filter(GEOID == selectedPolygon()) %>%
        select(-YEAR, -GEOGRAPHY, -SURVEY, -NAME)
      longData <- selectedData %>%
        pivot_longer(cols = -GEOID, names_to = "field", values_to = "value") %>%
        mutate(
          field_type = case_when(
            str_ends(field, "E") ~ "Estimate",
            str_ends(field, "M") ~ "MoE",
            str_ends(field, "P") ~ "Percent",
            TRUE ~ NA_character_
          ),
          field_name = str_sub(field, 1, str_length(field) - 1)
        ) %>%
        pivot_wider(
          id_cols = c(GEOID, field_name),
          names_from = field_type,
          values_from = value,
          values_fill = list(value = NA),
          values_fn = list(value = function(x) str_c(x, collapse = ", "))
        ) %>%
        mutate(Estimate = scales::number(as.numeric(Estimate), big.mark = ",", accuracy = 1),
               MoE = scales::number(as.numeric(MoE), big.mark = ",", accuracy = 1),
               Percent = scales::percent(as.numeric(Percent), accuracy = 0.1)) %>%
        select(-GEOID) %>%
        rename(acsNames = field_name) %>%
        left_join(., metricNames) %>%
        arrange(Order) %>%
        filter(Category == input$selectedCategory) %>%
        select(Description, Estimate, MoE, Percent) %>%
        distinct()
      longData
    })

    # Output GEOID and NAME of the selected polygon
    output$selectedPolygonInfo <- renderUI({
      if (is.null(selectedPolygon())) {
        return(NULL)
      }
      name <- selectedName()
      geo <- selectedGeoLevel()
      HTML(paste0("<strong>", unique(name), "</strong> in <strong> ", unique(geo), "</strong>"))
    })
  })
}

# Run the application
shinyApp(ui, server)
