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
        copy$upload_intro,
        tableOutput(ns("table_io")),
        hr(),
        radioButtons(
          ns("fetch_type"), 
          "Data to fetch",
          inline = TRUE,
          choices = c(
            'PM2.5' = "pm25",
            "ADI" = "adi",
            "Both " = "both"
          )
        ),
        hr(),
        fileInput(ns("filedata"), "Choose your file in csv",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        # actionButton(ns("check_file"),"Check File"),
        uiOutput(ns("check_message")),
        hr(),
        uiOutput(ns("valid_message"))
      ),
      mainPanel(
        "1. Top of Dataset",
        verbatimTextOutput(ns("view_data")),
        "2. Result of Validation",
        fluidRow(
          verbatimTextOutput(ns("data_validation")),
          uiOutput(ns("usps_check_message"))
        ),
        "3. Geocoding",
        verbatimTextOutput(ns("geo_update"))
      )
    )
  )
}
    
#' panel_upload Server Functions
#'
#' @noRd 
#' @importFrom dplyr mutate n rename matches pull bind_cols
#' @importFrom magrittr %>%
#' @importFrom tidygeocoder geocode
#' @importFrom purrr map
#' @importFrom tidyr unite
#' @importFrom data.table rbindlist
#' @importFrom addressr validateAddress
#' @importFrom tibble tibble
#' @importFrom zipcodeR geocode_zip

mod_panel_upload_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$table_io <- renderTable(
      copy$table_io
    )
    
    dataset <- reactive({
      req(input$filedata)
      read.csv(input$filedata$datapath)
    })
    
    
    
    output$view_data <- renderPrint(head(dataset()))
    
    observeEvent(dataset(), {
      
      output$check_message <- renderText(validate_format(dataset()))
      
      if (is.null(validate_format(dataset()))) {
        output$check_message <- renderUI(tags$div(tags$br(),
                                                  actionButton(
                                                    session$ns("validate_file"), "Validate File"
                                                  )))
      }
    })
    
    observeEvent(input$validate_file, {
      output$valid_message <- renderUI(
        tags$div(
          tags$br(),
          textInput(session$ns("access_code"), "Access Code"),
          radioButtons(
            session$ns("service"),
            "Service",
            choices = c(
              "US Census" = "census",
              "Google" = "google",
              "US Census w/ Google fallback" = "fallback"
            ),
            selected = "census"
          ), 
          actionButton(session$ns("geo_file"),"Geolocate File")
        )
      )
    })
    
    dataset_valid <- eventReactive(input$validate_file, {
      dataset()$address %>%
        validate_address()
    })
    
    output$data_validation <- renderPrint(dataset_valid())
    
    dataset_geo <- eventReactive(input$geo_file, {
      
      if (input$access_code == "geo"){
        
        if (
          is.element("lat", colnames(dataset())) & 
          is.element("lng", colnames(dataset()))
        ) {
          
          dataset_valid()  %>% 
            select(-id) %>% 
            dplyr::bind_cols(dataset() %>% rename(address_raw = address)) %>% 
            return()
          
        } else {
          
          if(input$service != "fallback") {
            withProgress(
              message = paste("Geocoding", nrow(dataset_valid()), "address(es) with", input$service ),
              detail = "Usually less than a minute.",
              {
                
                dataset_valid()  %>%
                  select(-id) %>%
                  dplyr::bind_cols(dataset() %>% rename(address_raw = address)) %>%
                  geocode(address, method = input$service, lat = "lat" , long = "lng") %>%
                  return()
              })
          } else {
            withProgress(
              message = paste("Geocoding", nrow(dataset_valid()), "address(es) with", input$service ),
              detail = "Usually less than a minute.",
              {
                
                dataset_cen <- dataset_valid()  %>%
                  select(-id) %>%
                  dplyr::bind_cols(dataset() %>% rename(address_raw = address)) %>%
                  geocode(address, method = "census", lat = "lat" , long = "lng")
                
                dataset_cen %>% 
                  filter(is.na(lat)) %>% 
                  select(-lat, -lng) %>% 
                  geocode(address, method = "google", lat = "lat" , long = "lng") %>% 
                  bind_rows(
                    dataset_cen %>% 
                      filter(!is.na(lat))
                  ) %>% 
                  return()
              })
          }
        }
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
