
library(shiny)
library(tidyverse)
library(plotly)
library(arrow)
library(rsconnect)


ncma_data <- open_dataset("ncma_data/parquet/channel_daily_kpis_v1")

channel_data <- ncma_data |>
  collect() |>
  mutate(
    event_dt = as.Date(event_dt),
    engagement_rate = engaged_sessions/ sessions
  )

ui <- fluidPage(
  titlePanel("NCMA SGV Website Performance Shinny App"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = min(channel_data$event_dt, na.rm = TRUE),
        end = max(channel_data$event_dt, na.rm = TRUE)
      ),
      
      selectInput(
        "device",
        "Select Device Category",
        choices = c("ALL", sort(unique(channel_data$device_category))),
        selected = "ALL"
      )
    ),
    mainPanel(
      plotlyOutput("sessions_time"),
      plotlyOutput("device_perfromance")
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    data <- channel_data |>
      filter(
        event_dt >= input$date_range[1],
        event_dt <= input$date_range[2]
      )
    if(input$device != "ALL") {
      data <- data |>
        filter(device_category == input$device)
    }
    data
  })
  output$sessions_time <- renderPlotly({
    sessions_time <- filtered_data() |>
      group_by(event_dt) |>
      summarise(
        sessions = sum(sessions, na.rm = TRUE),
        .groups = "drop"
      )
    p <- ggplot(sessions_time,
                aes(x = event_dt,
                    y = sessions)) +
      geom_line() + 
      theme_minimal() +
      labs(
        title = "Sessions Over Time",
        x = "Date",
        y = "Sessions"
      )
    ggplotly(p)
  })
  output$device_performance <- renderPlotly({
    device_performance <- filtered_data() |>
      group_by(device_category) |>
      summarise(
        sessions = sum(sessions, na.rm = TRUE),
        engaged_sessions = sum(engaged_sessions, na.rm = TRUE),
        engagement_rate = engaged_sessions/ sessions, 
        .groups = "drop"
      )
    p <- ggplot(device_performance, 
                aes(x = device_category,
                    y = engagement_rate,
                    fill = device_category)) +
      geom_col() + 
      theme_minimal() +
      labs(
        title = "Device Perfromance by Engagement Rate",
        x = "Device Category",
        y = "Engagement Rate"
      )
    ggplotly(p)
  })
}

shinyApp(ui, server)