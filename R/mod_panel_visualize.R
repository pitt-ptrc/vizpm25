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
        # selectInput(ns("column"), "Choose column", character(0)),
        verbatimTextOutput(ns("summary")),
        verbatimTextOutput(ns("filename")),
        sliderInput(
          ns("slider"),
          "Time",
          min = as.Date("2018-01-01"),
          max = as.Date("2021-12-01"),
          value = c(
            as.Date("2018-12-01"),
            as.Date("2020-06-01")
          ),
          timeFormat = "%b %Y"
        ),
        verbatimTextOutput(ns("view_data")),
        actionButton(
          ns("calculate"),
          label = "Fetch PM2.5 Data"
        )
      ),
      mainPanel(
        tabsetPanel(
          # tabPanel("Map", leafletOutput(ns("map_plot"))),
          tabPanel("Map"),
          tabPanel("Plot", plotOutput(ns("plot_path"))),
          tabPanel("Table", dataTableOutput(ns("table")))
        )
      )
    )
  )
}
    
#' panel_visualize Server Functions
#'
#' @noRd 
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_path
#' @importFrom dplyr left_join select

data("test_restaurant")

mod_panel_visualize_server <- function(id, dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$summary <- renderPrint({
      summary(dataset())
    })
    
    dataset_ts <- eventReactive(input$calculate, {
      dataset() %>% 
        left_join(test_restaurant) %>% 
        return()
    })
    
    output$plot_path <- renderPlot({
      dataset_ts() %>% 
        ggplot(aes(inspect_dt, cs, group = id)) +
        geom_path()
    })
    
    output$table <- renderDataTable({
      dataset_ts() %>% 
        select(facility_name,
               encounter,
               description_new,
               inspect_dt,
               chain,
               liquor,
               v_level)
    })
  })
}
    
## To be copied in the UI
# mod_panel_visualize_ui("panel_visualize_ui_1")
    
## To be copied in the server
# mod_panel_visualize_server("panel_visualize_ui_1")
