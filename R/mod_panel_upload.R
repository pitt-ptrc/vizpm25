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
        fileInput(ns("filedata"), "Choose your file in csv",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        actionButton(ns("v_file"),"Validate File"),
        uiOutput(ns("valid_message")),
        hr(),
        verbatimTextOutput(ns("console"))
        # verbatimTextOutput("view_valid_data"),
        # fileInput(
        #   inputId = ns("filedata"),
        #   label = "Upload data. Choose csv file",
        #   accept = c(".csv")
        # ),
        # textInput(ns("access_code"), "Access Code"),
        # actionButton(
        #   ns("geolocate"),
        #   label = "Geolocate"
        # )
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
#' @importFrom tidygeocoder geocode
mod_panel_upload_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    dataset <- reactive({
      req(input$filedata)
      read.csv(input$filedata$datapath)
    })
    
    output$view_data <- renderPrint(head(dataset()))
    
    observeEvent(input$v_file,{
      if(is.null(input$filedata$datapath)){
        output$valid_message <- renderUI(
          tags$div(
            tags$br(),
            "Please upload a csv file.", 
            tags$br()
          )
        ) 
      } else {
        # dataset <- input$filedata$datapath %>% 
        #   read.csv()
        
        if (!is.element("address", colnames(dataset()))) {
          output$valid_message <- renderUI(
            tags$div(
              tags$br(),
              "'address' must be a column name.", 
              tags$br()
            )
          )
        } else if (length(unique(dataset()$address)) != length(dataset()$address)) {
          output$valid_message <- renderUI(
            tags$div(
              tags$br(),
              "Addresses must be unique.", 
              tags$br()
            )
          )
        } else {
          output$valid_message <- renderUI(
            tags$div(
              tags$br(),
              textInput(session$ns("access_code"), "Access Code"),
              actionButton(session$ns("geo_file"),"Geolocate File")
            )
          )
        }
      }
    })
    
    # x = [-79.95, -80.00], y = [40.35, 40.50]
    dataset_geo <- eventReactive(input$geo_file, {
      if (input$access_code == "geo"){
        
        dataset() %>% 
          geocode(address, method = 'census', lat = y , long = x) %>%
          return()
        
        # withConsoleRedirect(output$console, {
        #   dataset() %>% 
        #     geocode(address, method = 'census', lat = y , long = x) %>% 
        #     return()
        # })
        
        # dataset() %>% 
        #   geocode(address, method = 'census', lat = y , long = x) %>% 
        #   return()
        
        # set.seed(1)
        # dataset() %>% 
        #   mutate(x = runif(n(), min = -80.00, max = -79.95)) %>% 
        #   mutate(y = runif(n(), min = 40.35, max = 40.50)) %>% 
        #   return()
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
