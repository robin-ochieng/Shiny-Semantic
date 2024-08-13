library(shiny)
library(shiny.semantic)
library(ggplot2)
library(DT)

# Sample data
data <- data.frame(
  Category = rep(c("A", "B", "C"), each = 10),
  Value = rnorm(30),
  Date = seq(as.Date("2023-01-01"), by = "1 month", length.out = 30)
)

# UI
ui <- semanticPage(
  title = "Professional Dashboard",
  div(class = "ui raised very padded text container segment",
      h2("Interactive Dashboard", class = "ui header"),
      div(class = "ui form",
          date_input("date_range", "Select Date Range:",
                     value = c("2023-01-01", "2023-03-01")),
          selectInput("category", "Choose Category:", choices = unique(data$Category))
      ),
      plotOutput("plot"),
      dataTableOutput("table")
  )
)

server <- function(input, output, session) {
  observe({
    print(head(data))  # Check the initial data
  })
  
  reactive_data <- reactive({
    req(input$date_range)  # Ensure date_range input is available
    req(input$category)    # Ensure category input is available
    filtered_data <- data %>%
      filter(Date >= as.Date(input$date_range[1]) & Date <= as.Date(input$date_range[2]),
             Category == input$category)
    print(head(filtered_data))  # Debug filtered data
    return(filtered_data)
  })
  
  output$plot <- renderPlot({
    req(reactive_data())  # Ensure reactive data is available before plotting
    ggplot(reactive_data(), aes(x = Date, y = Value, color = Category)) +
      geom_line() +
      theme_minimal() +
      labs(title = "Category Trends", x = "Date", y = "Value")
  })
  
  output$table <- renderDataTable({
    reactive_data()  # Use the reactive data for the table
  }, options = list(pageLength = 5, dom = 't'))
}

# Run the application 
shinyApp(ui, server)
