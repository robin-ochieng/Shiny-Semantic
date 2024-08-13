library(shiny)
library(shiny.semantic)
library(ggplot2)
library(DT)

ui <- semanticPage(
  title = "Semantic Dashboard",
  tabset(
    tabPanel("Dashboard", icon = "chart line",
             fluidRow(
               column(4,
                      selectInput("cyl", "Choose Cylinder:", choices = unique(mtcars$cyl), selected = 6),
                      selectInput("gear", "Choose Gear:", choices = unique(mtcars$gear), selected = 4)
               ),
               column(8,
                      segment(plotOutput("plot"))
               )
             ),
             fluidRow(
               dataTableOutput("data_table")
             )
    ),
    tabPanel("Summary", icon = "table",
             segment(
               h1("Car Summary", style = "color: #1e70bf; text-align: center;"),
               dataTableOutput("summary_table")
             )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    mtcars %>%
      filter(cyl == input$cyl, gear == input$gear)
  })
  
  output$plot <- renderPlot({
    ggplot(filtered_data(), aes(x = wt, y = mpg)) +
      geom_point(aes(color = as.factor(cyl)), size = 4) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Miles Per Gallon vs Weight",
           subtitle = paste("Filtered by", input$cyl, "cylinders and", input$gear, "gears"),
           x = "Weight (1000 lbs)", y = "Miles per Gallon") +
      theme_minimal()
  })
  
  output$data_table <- renderDataTable({
    filtered_data()
  }, options = list(pageLength = 5))
  
  output$summary_table <- renderDataTable({
    summary <- mtcars %>%
      group_by(cyl, gear) %>%
      summarise(AvgMPG = mean(mpg, na.rm = TRUE), MinMPG = min(mpg, na.rm = TRUE), MaxMPG = max(mpg, na.rm = TRUE))
    summary
  }, options = list(pageLength = 5))
}

shinyApp(ui, server)
