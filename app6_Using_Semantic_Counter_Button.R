library(shiny)
library(shiny.semantic)

ui <- semanticPage(
  tags$head(
    tags$style(HTML("
      .positive.button {
        transition: background-color 0.3s ease;
      }
      .positive.button:hover {
        background-color: #21ba45 !important;
      }
    "))
  ),
  counter_button("counter", "How many dogs?",
                 icon = icon("dog"),
                 size = "massive", color = "green"),
  h1("Dog count:"),
  h2(textOutput("dogs")),
  actionButton("reset", "Reset Counter", class = "ui red button")
)

server <- function(input, output, session) {
  # Reactive value to store the count
  counter_value <- reactiveVal(0)
  
  # Update the count based on counter button
  observeEvent(input$counter, {
    counter_value(counter_value() + 1)
  })
  
  # Reset counter
  observeEvent(input$reset, {
    counter_value(0)
  })
  
  # Display the count
  output$dogs <- renderText({
    paste(counter_value())
  })
  
  # Conditional styling based on the count
  observe({
    if (counter_value() >= 10) {
      show_modal(
        modalDialog(
          title = "Wow!",
          "That's a lot of dogs!",
          easyClose = TRUE,
          footer = NULL
        )
      )
    }
  })
}

shinyApp(ui, server)
