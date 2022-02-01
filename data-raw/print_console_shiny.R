library(shiny)

withConsoleRedirect <- function(containerId, expr) {
  # Change type="output" to type="message" to catch stderr
  # (messages, warnings, and errors) instead of stdout.
  txt <- capture.output(results <- expr, type = "output")
  if (length(txt) > 0) {
    insertUI(paste0("#", containerId), where = "beforeEnd",
             ui = paste0(txt, "\n", collapse = "")
    )
  }
  results
}

# Example usage

ui <- fluidPage(
  pre(id = "console")
)

server <- function(input, output, session) {
  observe({
    invalidateLater(1000)
    
    withConsoleRedirect("console", {
      str(cars)
    })
  })
}

shinyApp(ui, server)
