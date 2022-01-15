#' panel_visualize UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_panel_visualize_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        verbatimTextOutput(ns("filename")),
        sliderInput(
          ns("slider"),
          "Time",
          min = as.Date("2018-01-01"),
          max = as.Date("2021-12-01"),
          value = c(
            as.Date("2018-12-01"),
            as.Date("2018-06-01")
          ),
          timeFormat = "%b %Y"
        ),
        verbatimTextOutput(ns("view_data")),
        actionButton(
          ns("calculate"),
          label = "Fetch PM2.5 Data"
        )
      ),
      mainPanel()
    )
  )
}
    
#' panel_visualize Server Functions
#'
#' @noRd 
mod_panel_visualize_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_panel_visualize_ui("panel_visualize_ui_1")
    
## To be copied in the server
# mod_panel_visualize_server("panel_visualize_ui_1")
