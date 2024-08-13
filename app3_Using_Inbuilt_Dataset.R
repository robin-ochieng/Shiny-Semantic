library(shiny)
library(shiny.semantic)
library(ggplot2)
library(DT)
library(dplyr)

# UI
ui <- semanticPage(
  title = "Professional Dashboard",
  div(class = "ui raised very padded text container segment",
      h2("Car Performance Dashboard", class = "ui header"),
      div(class = "ui form",
          selectInput("cyl", "Select Cylinder Count:", choices = unique(mtcars$cyl)),
          selectInput("gear", "Select Gear Count:", choices = unique(mtcars$gear))
      ),
      plotOutput("mpg_plot"),
      dataTableOutput("car_table")
  )
)
server <- function(input, output, session) {
  
  # Check columns again within the server function
  observe({
    print(names(mtcars))  # Should list 'gear' among other columns
  })
  
  # Reactive data based on inputs
  reactive_data <- reactive({
    req(input$cyl, input$gear)  # Ensure input is present
    tryCatch({
      filtered_data <- mtcars %>%
        dplyr::filter(cyl == input$cyl, gear == input$gear)  # Use explicit namespace
      print(head(filtered_data))  # Print filtered data for debugging
      return(filtered_data)
    }, error = function(e) {
      print(e)  # Print error if any during filtering
      return(data.frame())  # Return an empty dataframe on error
    })
  })
  
  output$mpg_plot <- renderPlot({
    data <- req(reactive_data())  # Ensure data is available
    ggplot(data, aes(x = wt, y = mpg)) +
      geom_point() +
      theme_minimal() +
      labs(title = "Miles Per Gallon vs Weight", x = "Weight", y = "Miles per Gallon")
  })
  
  output$car_table <- renderDataTable({
    req(reactive_data())  # Make sure data is ready
    reactive_data()
  }, options = list(pageLength = 5, dom = 't'))
}


# Run the application 
shinyApp(ui, server)
