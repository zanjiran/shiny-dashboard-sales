library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)

# Sample data
set.seed(123)
consultants <- c("Dr. Sara", "Dr. Reza", "Dr. Narges")
data <- data.frame(
  date = sample(seq.Date(as.Date("2025-01-01"), as.Date("2025-04-30"), by = "day"), 300, replace = TRUE),
  consultant = sample(consultants, 300, replace = TRUE),
  sessions = sample(1:3, 300, replace = TRUE),
  income = sample(seq(200, 500, by = 10), 300, replace = TRUE) * 1000
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Psychology Clinic Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      dateRangeInput("date_range", "Select Date Range:", start = min(data$date), end = max(data$date)),
      selectInput("consultant_filter", "Select Consultant:", choices = c("All", unique(data$consultant)))
    )
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("total_income"),
      valueBoxOutput("total_sessions"),
      valueBoxOutput("unique_visits")
    ),
    fluidRow(
      box(title = "Monthly Income Trend", width = 6, status = "primary", solidHeader = TRUE,
          plotOutput("income_plot")),
      box(title = "Sessions by Consultant", width = 6, status = "warning", solidHeader = TRUE,
          plotOutput("consultant_pie"))
    ),
    fluidRow(
      box(title = "Session Table", width = 12, status = "success", solidHeader = TRUE,
          DTOutput("session_table"))
    )
  )
)

# Server
server <- function(input, output) {
  filtered_data <- reactive({
    df <- data %>% filter(date >= input$date_range[1] & date <= input$date_range[2])
    if (input$consultant_filter != "All") {
      df <- df %>% filter(consultant == input$consultant_filter)
    }
    df
  })
  
  output$total_income <- renderValueBox({
    valueBox(
      paste0("$", format(sum(filtered_data()$income), big.mark = ",")),
      "Total Income",
      icon = icon("wallet"),
      color = "green"
    )
  })
  
  output$total_sessions <- renderValueBox({
    valueBox(
      sum(filtered_data()$sessions),
      "Total Sessions",
      icon = icon("calendar-check"),
      color = "blue"
    )
  })
  
  output$unique_visits <- renderValueBox({
    valueBox(
      nrow(filtered_data()),
      "Total Visits",
      icon = icon("users"),
      color = "purple"
    )
  })
  
  output$income_plot <- renderPlot({
    filtered_data() %>%
      mutate(month = format(date, "%Y-%m")) %>%
      group_by(month) %>%
      summarise(total_income = sum(income)) %>%
      ggplot(aes(x = month, y = total_income)) +
      geom_line(group = 1, color = "steelblue", size = 1.2) +
      geom_point(color = "darkblue", size = 3) +
      theme_minimal() +
      labs(x = "Month", y = "Income", title = "Monthly Income Trend") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$consultant_pie <- renderPlot({
    df <- filtered_data() %>%
      group_by(consultant) %>%
      summarise(total_sessions = sum(sessions))
    ggplot(df, aes(x = "", y = total_sessions, fill = consultant)) +
      geom_col(width = 1) +
      coord_polar("y") +
      theme_void() +
      labs(title = "Sessions by Consultant") +
      scale_fill_brewer(palette = "Pastel1")
  })
  
  output$session_table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10), rownames = FALSE)
  })
}

shinyApp(ui, server)
