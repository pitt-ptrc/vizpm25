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
        hr(),
        fileInput(ns("filedata"), "Choose your file in csv",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        actionButton(ns("check_file"),"Check File"),
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
#' @importFrom reticulate import virtualenv_create use_virtualenv 
#' @importFrom purrr map
#' @importFrom tidyr unite
#' @importFrom data.table rbindlist
#' @importFrom addressr validateAddress
#' @importFrom tibble tibble
#' @importFrom zipcodeR geocode_zip

mod_panel_upload_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # python address
    virtualenv_create("testenv3", packages = c("usaddress"))
    use_virtualenv("testenv3", required = TRUE)
    usaddress <- import("usaddress")
    
    # end python
    
    dataset <- reactive({
      req(input$filedata)
      read.csv(input$filedata$datapath)
    })
    
    output$view_data <- renderPrint(head(dataset()))
    
    observeEvent(input$check_file,{
      if(is.null(input$filedata$datapath)){
        output$check_message <- renderUI(
          tags$div(
            tags$br(),
            "Please upload a csv file.", 
            tags$br()
          )
        ) 
      } else {
        # dataset <- input$filedata$datapath %>% 
        #   read.csv()
        
        if (!is.element("id", colnames(dataset()))) {
          output$check_message <- renderUI(
            tags$div(
              tags$br(),
              "'id' must be a column name.", 
              tags$br()
            )
          )
        } else if (!is.element("address", colnames(dataset()))) {
          output$check_message <- renderUI(
            tags$div(
              tags$br(),
              "'address' must be a column name.", 
              tags$br()
            )
          )
          
        } else if (
          as.logical(
            sum(
              !is.element(colnames(dataset()), c("id", "address", "lat", "lng"))
            )
          )
        ) {
          output$check_message <- renderUI(
            tags$div(
              tags$br(),
              "Only 'id', 'address', 'lat', 'lng' are permitted columns.", 
              tags$br()
            )
          )
        } else if (length(unique(dataset()$address)) != length(dataset()$address)) {
          output$check_message <- renderUI(
            tags$div(
              tags$br(),
              "Addresses must be unique.", 
              tags$br()
            )
          )
        } else {
          
          output$check_message <- renderUI(
            tags$div(
              tags$br(),
              actionButton(session$ns("validate_file"),"Validate File")
            )
          )
        }
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
      dataset() %>%
        pull(address) %>% 
        map(usaddress$tag) %>% 
        map(1) %>% 
        rbindlist(fill = TRUE) %>% 
        unite("Address1", matches("Address|Street"), sep = " ", na.rm = TRUE) %>% 
        rename(City = PlaceName, State = StateName, Zip5 = ZipCode) %>% 
        mutate(Zip4 = NA, Address2 = NA) %>% 
        validateAddress(userid = "558UNIVE2177", address = .) %>% 
        tibble() %>% 
        unite("address_main", Address2, City, State, sep = ", ") %>% 
        mutate(zip5 = Zip5) %>% 
        unite("zip9", Zip5, Zip4, sep = "", remove = FALSE) %>% 
        mutate(zip = zip9 %>% as.integer()) %>% 
        unite("address_zip", Zip5, Zip4, sep = "-") %>% 
        unite("address", address_main, address_zip, sep = " ")
    })
    
    output$data_validation <- renderPrint(dataset_valid())
    
    dataset_geo <- eventReactive(input$geo_file, {
      
      # load(system.file("data", package = "zipcodeR", "zip_code_db.rda"))
      
      if (input$access_code == "geo"){
        
        # zip_code_db <- zipcodeR::zip_code_db
        
        # dataset_geozip <- dataset_valid()$zip5 %>% 
        #   geocode_zip() %>% 
        #   rename(zip5 = zipcode, zip_lat = lat, zip_lng = lng)
        
        if (
          is.element("lat", colnames(dataset())) & 
          is.element("lng", colnames(dataset()))
        ) {
          
          dataset_valid()  %>% 
            # left_join(dataset_geozip) %>% 
            select(-id) %>% 
            dplyr::bind_cols(dataset() %>% rename(address_raw = address)) %>% 
            # geocode(address, method = 'census', lat = "lat" , long = "lng") %>%
            return()
          
        } else {
          
          if(input$service != "fallback") {
            withProgress(
              message = paste("Geocoding", nrow(dataset_valid()), "address(es) with", input$service ),
              detail = "Usually less than a minute.",
              {
                
                dataset_valid()  %>%
                  # left_join(dataset_geozip) %>%
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
                  # left_join(dataset_geozip) %>%
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
