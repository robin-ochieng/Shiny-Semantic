library(shiny)
library(shiny.semantic)
library(ggplot2)
library(dplyr)

# Sample data
data <- data.frame(
  date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day"),
  category = sample(c("A", "B", "C"), 366, replace = TRUE),
  value = rnorm(366, 100, 10)
)

ui <- semanticPage(
  title = "Complex Shiny App",
  div(
    class = "ui grid",
    div(class = "six wide column",
        calendar("start_date", type = "date", value = "2020-01-01", placeholder = "Start Date", min = "2020-01-01", max = "2020-12-31"),
        calendar("end_date", type = "date", value = "2020-12-31", placeholder = "End Date", min = "2020-01-01", max = "2020-12-31")
    ),
    div(class = "four wide column",
        selectInput("category", "Select Category", choices = unique(data$category))
    ),
    div(class = "six wide column",
        actionButton("update", "Update View")
    )
  ),
  plotOutput("plot"),
  dataTableOutput("table")
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    data %>%
      filter(date >= as.Date(input$start_date) & date <= as.Date(input$end_date),
             category == input$category)
  })
  
  output$plot <- renderPlot({
    req(input$update)  # Require a button press before plotting
    ggplot(filtered_data(), aes(x = date, y = value, color = category)) +
      geom_line() +
      labs(title = "Value Over Time", x = "Date", y = "Value")
  })
  
  output$table <- renderDataTable({
    req(input$update)  # Require a button press before displaying the table
    filtered_data()
  })
  
  observeEvent(input$update, {
    update_calendar(session, "start_date", value = "2020-01-01")
    update_calendar(session, "end_date", value = "2020-12-31")
  })
}

shinyApp(ui = ui, server = server)
