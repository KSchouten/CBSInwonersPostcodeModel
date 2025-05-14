#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(tidyverse)
library(shiny)
library(sf)

# Define UI for application that draws a histogram
ui <- fluidPage(

  waiter::useWaiter(),
  waiter::waiter_on_busy(color = NULL, html = waiter::spin_3circles()),
  # Application title
  titlePanel("Inwonersvoorspelling per postcode"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("year",
                  "Voorspeljaar (2022 en 2023 zijn out of sample)",
                  min = 2018,
                  max = 2023,
                  value = 2023),
      plotly::plotlyOutput("lines")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      leaflet::leafletOutput("map", height = "900px")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  is_local<-Sys.getenv('SHINY_PORT')==""
  config <- yaml::read_yaml("config.yaml")
  if (is_local){
    con <- DBI::dbConnect(odbc::odbc(),
                          Driver = "ODBC Driver 18 for SQL Server",
                          server = "tcp:istarion.database.windows.net,1433",
                          database = "cbsdata",
                          uid = config$DB_UID,
                          pwd = config$DB_PWD,
                          timeout = 30)
  } else {
    con <- DBI::dbConnect(odbc::odbc(),
                          Driver = "SQLServer",
                          server = "istarion.database.windows.net",
                          port = 1433,
                          database = "cbsdata",
                          uid = config$DB_UID,
                          pwd = config$DB_PWD,
                          timeout = 30)
  }

  mapdata <- reactive({
    readRDS("data/pc4_geometry.Rds")
  })
  popdata <- reactive({
    DBI::dbGetQuery(con, "SELECT jaar, postcode, aantalInwoners FROM pc4_data")
  })
  predictions <- reactive({
    DBI::dbGetQuery(con, "SELECT * FROM predictions")
  })
  diffs <- reactive({
    predictions() %>% mutate(jaar = jaar+2) %>%
      select(jaar, postcode, .pred) %>%
      inner_join(popdata(), by = c("jaar", "postcode")) %>%
      mutate(diff = round(aantalInwoners - .pred)) %>%
      pull(diff)
  })
  pal <- reactive({
    leaflet::colorNumeric(
      palette = "viridis",
      domain = diffs()
    )
  })

  output$map <- leaflet::renderLeaflet({
    print("Creating map...")

    leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addLegend(pal = pal(), values = diffs(), opacity = 1.0, group = "regions") %>%
      leaflet::flyTo(lng = 5.328584, lat = 52.14517, zoom = 8)
  })

  leafletproxy <- leaflet::leafletProxy("map")
  observe({
    req(input$year, is.numeric(input$year))
    print("Updating map...")
    predictiondata <- predictions() %>% filter(jaar == input$year-2) %>% select(postcode, .pred)

    leafletdata <- popdata() %>%
      filter(jaar == input$year) %>%
      left_join(predictiondata, by = c("postcode")) %>%
      mutate(diff = round(aantalInwoners - .pred)) %>%
      left_join(mapdata())

    class(leafletdata) <- c("sf", class(leafletdata))
    class(leafletdata$geometry) <- c("sfc_MULTIPOLYGON","sfc")
    sf::st_geometry(leafletdata) <- "geometry"

    leafletproxy %>%
      leaflet::addPolygons(data = leafletdata,
                           stroke = TRUE, weight = 1, color = "black", smoothFactor = 0.3, fillOpacity = 0.8,
                           layerId = ~postcode,
                           fillColor = pal()(leafletdata$diff),
                           label = ~map(str_c("Postcode: ",postcode,
                                              "\n<br>Inwoners: ", aantalInwoners,
                                              "\n<br>Voorspelling: ", round(.pred),
                                              "\n<br>Verschil: ", diff), htmltools::HTML),
                           group = "regions")
  }) %>% bindEvent(input$year)

  output$lines <- plotly::renderPlotly({
    pred_data <- predictions() %>% filter(jaar == input$year -2 & postcode == input$map_shape_click$id) %>%
      with({
        tibble(jaar = c(jaar-1, jaar, jaar+2),
               aantalInwoners = c(aantalInwoners.vorig_jaar, aantalInwoners.dit_jaar, aantalInwoners),
               aantalGeboorten = c(aantalGeboorten.vorig_jaar, aantalGeboorten.dit_jaar, NA),
               aantalWoningen = c(aantalWoningen.vorig_jaar, aantalWoningen.dit_jaar, NA),
               voorspelling = c(NA, NA, .pred))
      })
    plot <- ggplot() +
      geom_line(data = pred_data, aes(x = jaar, y = aantalGeboorten), color = "grey", linetype = "dashed") +
      geom_line(data = pred_data, aes(x = jaar, y = aantalWoningen), color = "grey", linetype = "dotted") +
      geom_line(data = pred_data, aes(x = jaar, y = aantalInwoners)) +
      geom_errorbar(data = pred_data %>% filter(jaar == input$year),
                    aes(x = jaar, ymin = aantalInwoners, ymax = voorspelling), color = "red") +
      ggplot2::ylim(c(0, max(pred_data$aantalInwoners, pred_data$voorspelling, na.rm = TRUE)))
    plotly::ggplotly(plot)
  }) %>% bindEvent(input$map_shape_click)

}

# Run the application
shinyApp(ui = ui, server = server)
