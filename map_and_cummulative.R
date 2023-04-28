library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(leaflet)
library(jsonlite)
library(shinythemes)
library(shinyjs)

ui <- fluidPage(
  theme = shinytheme("cyborg"),
  titlePanel("COVID-19 Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select a country:",
                  c("Australia", "Canada", "China", "France", "Germany", "India",
                    "Italy", "Japan", "Mexico", "Spain", "United Kingdom", "United States")),
      
      dateRangeInput("dates", "Select date range:", 
                     start = "2020-01-22", end = Sys.Date())
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map")),
        tabPanel("Cases", plotOutput("Cases_plot"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Generate map of confirmed cases
  output$map <- renderLeaflet({
    # Define reactive data for selected country and date range
    data <- reactive({
      country <- tolower(input$country)
      url <- paste0("https://api.covid19api.com/country/", country, "/status/confirmed",
                    "?from=", format(input$dates[1]), "&to=", format(input$dates[2]))
      response <- tryCatch(
        {
          fromJSON(url, flatten = TRUE)
        },
        error = function(e) {
          NULL
        }
      )
      if (is.null(response)) {
        return(NULL)
      } else {
        data <- response %>%
          mutate(date = as.Date(Date, format = "%Y-%m-%d"), Lon = as.numeric(Lon),
                 Lat = as.numeric(Lat)) %>%
          select(-c(Date))
        return(data)
      }
    })
    
    # Create leaflet map
    if (!is.null(data())) {
      leaflet(data()) %>%
        addTiles() %>%
        addMarkers(
          lng = ~Lon,
          lat = ~Lat,
          popup = paste0("<strong>Province:</strong> ", data()$Province, "<br>",
                         "<strong>Cases:</strong> ", data()$Cases),
          options = popupOptions(closeButton = FALSE)
        )
    }
  })
  
  
  # Define reactive data for selected country and date range
  data2 <- reactive({
    country <- tolower(input$country)
    url <- paste0("https://api.covid19api.com/country/", country, "/status/confirmed",
                  "?from=", format(input$dates[1]), "&to=", format(input$dates[2]))
    data2 <- fromJSON(url)
    data2 <- data2 %>%
      mutate(date = as.Date(data2$Date, format = "%Y-%m-%d"))
    return(data2)
  })
  
  # Generate plot of confirmed Cases
  output$Cases_plot <- renderPlot({
    ggplot(data2(), aes(x = date, y = Cases)) +
      geom_line(color = "lightblue", alpha = 0.5) +
      labs(title = "Confirmed Cases", x = "Date", y = "Cases") +
      theme(plot.background = element_rect(fill = alpha("white", 1)),
            panel.border = element_rect(color = "black", fill = NA, size = 1))
  })
  

  
  
}

shinyApp(ui = ui, server = server)