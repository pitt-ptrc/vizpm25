#' panel_download UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_panel_download_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        radioButtons(ns("download_choice"),
                     "Choose download",
                     choices = c("Selected data", "All data")),
        downloadButton(ns("downloadData"), "Download")
      ),
      mainPanel(
        verbatimTextOutput(ns("summary")),
        dataTableOutput(ns("datatable"))
      )
    )
  )
}
    
#' panel_download Server Functions
#'
#' @noRd 
#' @importFrom dplyr glimpse
mod_panel_download_server <- function(id, dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$summary <- renderPrint({
      glimpse(dataset())
    })
    
    output$downloadData <- downloadHandler(
      filename <- function() {
        paste0("geo_ts_", Sys.Date(), ".csv")
      },
      content <- function(file) {
        write.csv(dataset(), file, row.names = FALSE)
      }
    )
  })
}
    
## To be copied in the UI
# mod_panel_download_ui("panel_download_ui_1")
    
## To be copied in the server
# mod_panel_download_server("panel_download_ui_1")
