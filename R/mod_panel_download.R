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
        fileInput(
          inputId = "filedata",
          label = "Upload data. Choose csv file",
          accept = c(".csv")
        ),
        verbatimTextOutput(ns("view_data")),
        actionButton(
          ns("download"),
          label = "Download"
        )
      ),
      mainPanel()
    )
  )
}
    
#' panel_download Server Functions
#'
#' @noRd 
mod_panel_download_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_panel_download_ui("panel_download_ui_1")
    
## To be copied in the server
# mod_panel_download_server("panel_download_ui_1")
