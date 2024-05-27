library(shiny)
library(httr)
library(jsonlite)
library(ggplot2)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Real-Time Stock Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("stockSymbol", "Select Stock:", choices = list("Microsoft" = "MSFT", "Apple" = "AAPL", "NVIDIA" = "NVDA", "Alphabet (Google)" = "GOOG", "Saudi Aramco" = "2222", "Amazon" = "AMZN", "Meta Platforms (Facebook)" = "META", "Berkshire Hathaway" = "BRK-B", "Eli Lilly" = "LLY", "Taiwan Semiconductor Manufacturing Company" = "TSM", "Broadcom" = "AVGO", "Novo Nordisk" = "NVO", "Visa" = "V", "JPMorgan Chase" = "JPM", "Walmart" = "WMT", "Exxon Mobil" = "XOM", "United Health" = "UNH", "Tesla" = "TSLA", "Mastercard" = "MA", "LVMH Moët Hennessy Louis Vuitton" = "MC", "Tencent" = "TCEHY", "Procter & Gamble" = "PG", "Samsung" = "005930", "Johnson & Johnson" = "JNJ", "ASML" = "ASML", "Home Depot" = "HD", "Merck" = "MRK", "Costco" = "COST", "Oracle" = "ORCL", "Toyota" = "TM"), selected = "MSFT"),
      selectInput("timePeriod", "Select Time Period", 
                  choices = list("One Day" = "1D","Five Day" = "5D","One Month" = "1M","Six Months" = "6M","Year-to-date" = "YTD","One Year" = "1Y", "YTD","Five Year" = "5Y", "Max" = "MAX"), selected = "6M"),
      actionButton("getData", "Get Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Price Data", plotOutput("stockPlot"), tableOutput("criticalValuesP")),
        tabPanel("Volume Data", plotOutput("stockChangePlot"), tableOutput("criticalValuesV"))
      )
    )
  )
)

server <- function(input, output) {
  stockData <- eventReactive(input$getData, {
    url <- "https://real-time-finance-data.p.rapidapi.com/stock-time-series"
    response <- GET(url, query = list(
      symbol = input$stockSymbol,  
      period = input$timePeriod, 
      language = "en"
    ), add_headers(
      `X-RapidAPI-Key` = "bacf733d14mshf0cabdf15ee10afp1dc180jsnae1c9ed3b770",
      `X-RapidAPI-Host` = "real-time-finance-data.p.rapidapi.com"
    ))
    if (response$status_code == 200) {
      data <- content(response, "parsed")
      symbol_value <- data$data$symbol
      time_series <- data$data$time_series
      formatTime <- function(time) {
        format(as.POSIXct(time, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S")
      }
      
      time_series_list <- lapply(names(time_series), function(x) {
        ts_data <- time_series[[x]]
        data.frame(
          company = symbol_value,
          time = as.POSIXct(x, origin = "1970-01-01", tz = "UTC"),
          formatted_time = formatTime(x), 
          price = ifelse(!is.null(ts_data$price), ts_data$price, NA),
          change = ifelse(!is.null(ts_data$change), ts_data$change, NA),
          change_percent = ifelse(!is.null(ts_data$change_percent), ts_data$change_percent, NA),
          volume = ifelse(!is.null(ts_data$volume), ts_data$volume, NA),
          stringsAsFactors = FALSE
        )
      })
      
      do.call(rbind, time_series_list)
    } else {
      stop("Failed to fetch data, status code:", response$status_code)
    }
  })
  
  output$stockPlot <- renderPlot({
    req(stockData())
    data <- stockData()%>%
      filter(!is.na(price))
    ggplot(data, aes(x = time, y = price)) +
      geom_line() +
      geom_point(data = data[which.max(data$price), ], aes(x = time, y = price), 
                 colour = "red", size = 3) +
      geom_text(data = data[which.max(data$price), ], aes(x = time, y = price, label = paste("Max:", price)),
                vjust = -1, hjust = 0.5, color = "red") +
      geom_point(data = data[which.min(data$price), ], aes(x = time, y = price),
                 colour = "blue", size = 3) + 
      geom_text(data = data[which.min(data$price), ], aes(x = time, y = price, label = paste("Min:", price)),
                vjust = 1, hjust = 0.5, color = "blue") +
 geom_hline(aes(yintercept = mean(price), color = "darkgreen"), linetype = "dashed") + 
      geom_hline(aes(yintercept = median(price), color = "purple"), linetype = "dashed") +
      scale_color_manual(name = "Reference Lines",
                         values = c("darkgreen", "purple"),
                         labels = c("Mean", "Median")) +
      labs(title = paste(data$company[1], "Closing Prices over Time"),
           x = "Time", y = "Closing Price (US$)") +
      theme_minimal()
  })
  
  output$criticalValuesP <- renderTable({
    req(stockData())
    data <- stockData()
    pivot_wider(as_tibble(list("Critical Values" = c("Mean", "Median", "Max", "Min"), 
                   "Stock Price" = c(round(mean(data$price, na.rm = TRUE),2), round(median(data$price, na.rm = TRUE), 2), round(data$price[which.max(data$price)], 2), round(data$price[which.min(data$price)], 2)))), names_from = "Critical Values", values_from = "Stock Price")
    
  })
  
  output$stockChangePlot <- renderPlot({
    req(stockData())
    data <- stockData() %>%
      filter(!is.na(volume))
    ggplot(data, aes(x = time, y = volume)) +
      geom_line() +
      geom_point(data = data[which.max(data$volume), ], aes(x = time, y = volume), 
                 colour = "red", size = 3) +
      geom_text(data = data[which.max(data$volume), ], aes(x = time, y = volume + 1, label = paste("Max:", volume)),
                vjust = -1, hjust = 0.5, color = "red") +
      geom_point(data = data[which.min(data$volume), ], aes(x = time, y = volume),
                 colour = "blue", size = 3) + 
      geom_text(data = data[which.min(data$volume), ], aes(x = time, y = volume, label = paste("Min:", volume)),
                vjust = 1, hjust = 0.5, color = "blue") +
      geom_hline(aes(yintercept = mean(volume), color = "darkgreen"), linetype = "dashed") + 
      geom_hline(aes(yintercept = median(volume), color = "purple"), linetype = "dashed") +
         scale_color_manual(name = "Reference Lines",
                         values = c("darkgreen", "purple"),
                         labels = c("Mean", "Median")) +
      labs(title = paste(data$company[1], "Exchange Volume over Time"),
           x = "Time", y = "Exchange Volume (Shares)") +
      theme_minimal()
  })
  
  output$criticalValuesV <- renderTable({
    req(stockData())
    data <- stockData()
    pivot_wider(data = as_tibble(list("Critical Values" = c("Mean", "Median", "Max", "Min"), 
                   "Volume" = c(round(mean(data$volume, na.rm = TRUE),2), round(median(data$volume, na.rm = TRUE), 2), round(data$volume[which.max(data$volume)], 2), round(data$volume[which.min(data$volume)], 2)))), names_from = "Critical Values", values_from = "Volume")
    
  })
}

shinyApp(ui = ui, server = server)
