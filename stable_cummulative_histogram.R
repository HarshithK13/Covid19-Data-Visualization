library(shiny)
library(ggplot2)
library(plotly)
library(readr)
library(lubridate)

data <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")
countries <- unique(data$Country)

ui <- fluidPage(
  titlePanel("COVID-19 Cases Histogram"),
  sidebarLayout(
    sidebarPanel(
      selectInput("countryInput", "Country:", choices = countries),
      dateInput("fromDateInput", "From Date:"),
      dateInput("toDateInput", "To Date:"),
      actionButton("generateButton", "Generate Histogram")
    ),
    mainPanel(
      plotlyOutput("histogramPlot")
    )
  )
)

server <- function(input, output) {
  filteredData <- reactive({
    country <- input$countryInput
    fromDate <- input$fromDateInput
    toDate <- input$toDateInput
    
    # Filter data based on country and date range
    filtered <- data[data$Country == country & as.Date(data$Date_reported) >= as.Date(fromDate) & as.Date(data$Date_reported) <= as.Date(toDate), ]
    
    return(filtered)
  })
  
  output$histogramPlot <- renderPlotly({
    data <- filteredData()
    
    if (nrow(data) > 0) {
      # Convert Date_reported to a date object
      data$Date_reported <- ymd(data$Date_reported)
      
      # Calculate the cumulative sum of cases
      data$Total_cases <- cumsum(data$New_cases)
      
      # Create the histogram using ggplot2
      p <- ggplot(data, aes(x = Date_reported, y = Total_cases)) +
        geom_bar(stat = "identity", color = "black", fill = "skyblue") +
        xlab("Date") +
        ylab("Number of Cases") +
        ggtitle(paste("COVID-19 Cases Histogram -", input$countryInput)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      # Convert ggplot to plotly object
      ggplotly(p, tooltip = c("x", "y")) 
    } else {
      # Show a message if no data is available for the selected country and date range
      message <- paste("No data available for", input$countryInput, "from", input$fromDateInput, "to", input$toDateInput)
      plot.new()
      text(0.5, 0.5, message, col = "red", cex = 1.5, font = 2)
    }
  })
}

shinyApp(ui = ui, server = server)
