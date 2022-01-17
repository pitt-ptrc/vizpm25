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
          inputId = ns("filedata"),
          label = "Upload data. Choose csv file",
          accept = c(".csv")
        ),
        verbatimTextOutput(ns("warning")),
        textInput(ns("access_code"), "Access Code"),
        actionButton(
          ns("geolocate"),
          label = "Geolocate"
        )
      ),
      mainPanel(
        verbatimTextOutput(ns("view_data")),
        fluidRow(
          textOutput(ns("data_validation")),
          verbatimTextOutput(ns("geo_update"))
        )
      )
    )
  )
}
    
#' panel_upload Server Functions
#'
#' @noRd 
#' @importFrom dplyr mutate n
#' @importFrom magrittr %>%
mod_panel_upload_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    dataset <- reactive({
      req(input$filedata)
      read.csv(input$filedata$datapath)
    })
    
    output$view_data <- renderPrint(head(dataset()))
    
    # x = [-79.95, -80.00], y = [40.35, 40.50]
    dataset_geo <- eventReactive(input$geolocate, {
      if (input$access_code == "geo"){
        dataset() %>% 
          mutate(x = runif(n(), min = -80.00, max = -79.95)) %>% 
          mutate(y = runif(n(), min = 40.35, max = 40.50)) %>% 
          return()
      } else {
        return(NULL)
      }
    })
    
    output$geo_update <- renderPrint(summary(dataset_geo()))
    
    return(dataset_geo)
  })
}
    
## To be copied in the UI
# mod_panel_upload_ui("panel_upload_ui_1")
    
## To be copied in the server
# mod_panel_upload_server("panel_upload_ui_1")
