library(shiny.semantic)
ui <- semanticPage(
  div(class = "ui raised segment",
      div(
        a(class="ui green ribbon label", "Link"),
        p("Lorem ipsum, lorem ipsum, lorem ipsum"),
        actionButton("button", "Click")
      )
  )
)
server <- function(input, output, session) {
}

shinyApp(ui, server)