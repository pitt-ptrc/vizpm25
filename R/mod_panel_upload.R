#' panel_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_panel_upload_ui <- function(id){
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
        textInput(ns("access_code"), "Access Code"),
        actionButton(
          ns("calculate"),
          label = "Geolocate"
        )
      ),
      mainPanel()
    )
  )
}
    
#' panel_upload Server Functions
#'
#' @noRd 
mod_panel_upload_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_panel_upload_ui("panel_upload_ui_1")
    
## To be copied in the server
# mod_panel_upload_server("panel_upload_ui_1")
