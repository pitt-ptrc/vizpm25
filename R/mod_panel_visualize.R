#' panel_visualize UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom plotly plotlyOutput
#' @importFrom leaflet leafletOutput
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
          max = as.Date("2018-03-01"),
          value = c(
            as.Date("2018-01-01"),
          as.Date("2018-02-01")
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
          tabPanel(
            "Map",
            leafletOutput(ns("map_plot"))
          ),
          tabPanel(
            "Plot",
            plotlyOutput(ns("plot_path")),
            downloadButton(ns('download_plot'),'Download Static Plot')
          ),
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
#' @import ggplot2
#' @import leaflet
#' @importFrom dplyr left_join select
#' @importFrom tidyr pivot_longer
#' @importFrom tibble as_tibble rowid_to_column
#' @importFrom terra vect extract subset
#' @importFrom stringr str_replace
#' @importFrom plotly renderPlotly

data("test_restaurant")

mod_panel_visualize_server <- function(id, dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$summary <- renderPrint({
      summary(dataset())
    })
    
    # output$map_plot <- renderLeaflet({
    #   leaflet() %>%
    #     addProviderTiles("Stamen.Watercolor") %>%
    #     setView(lng = -79.9959,
    #             lat = 40.4406,
    #             zoom = 10)
    # })
    # 
    # observe({
    #   leafletProxy(session$ns("map_plot"), data = dataset()) %>%
    #     clearMarkers() %>%
    #     clearControls() %>% 
    #     addCircleMarkers(
    #       lng = ~x,
    #       lat = ~y,
    #       label = ~facility_name
    #       # color = ~pal(status)
    #     ) %>% 
    #     # addLegend("bottomright", pal = pal, values = ~status,
    #     #           title = "Status",
    #     #           opacity = 1
    #     # ) %>% 
    #     identity()
    # })
    
    dataset_ts <- eventReactive(input$calculate, {
      dataset() %>% 
        left_join(test_restaurant) %>% 
        return()
    })
    
    dataset_pm25 <- eventReactive(input$calculate, {
      
      req(dataset())
      
      dataset_prep <- 
        dataset() %>% 
        rowid_to_column() %>% 
        # filter(!is.na(x)) %>% 
        # sf::st_as_sf(coords = c("x","y"))
        terra::vect(geom = c("x", "y"))
      
      correct_dates <-
        stringr::str_replace(input$slider, pattern = "\\d\\d$", replacement = "01")
      
      sel_dates <- as.character(seq(
        from = as.Date(correct_dates[1]),
        to = as.Date(correct_dates[2]),
        by = "month"
      ))
      
      rt_sub <- terra::subset(rt_bcog, sel_dates)
      
      ex_test <- terra::extract(rt_sub, dataset_prep) %>% 
        tibble::as_tibble() %>%
        # mutate(ID = as.character(ID)) %>% 
        # pivot_longer(!ID, names_to = "date", values_to = "pm25") %>% 
        # mutate(date = as.Date(date)) %>%
        identity()
      
      
      
      dplyr::left_join(dataset_prep %>% as_tibble(), ex_test, by = c("rowid" = "ID"))
    })
    
    # output$plot_path <- renderPlotly({
    #   dataset_ts() %>% 
    #     ggplot(aes(inspect_dt, cs, group = facility_name, color = facility_name)) +
    #     geom_path() +
    #     xlab("Inspection Date") +
    #     ylab("Cumulative Violation Score") +
    #     # scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    #     NULL
    # })
    
    output$plot_path <- renderPlotly({
      dataset_pm25() %>% 
        pivot_longer(cols = starts_with("2018"), names_to = "date", values_to = "pm25") %>%
        mutate(date = as.Date(date)) %>% 
        ggplot(aes(date, pm25, group = facility_name)) +
        geom_path()
    })
    
    data <- reactiveValues()
    
    data$static_plot <- renderPlot({
      dataset_ts() %>% 
        ggplot(aes(inspect_dt, cs, group = facility_name, color = facility_name)) +
        geom_path() +
        xlab("Inspection Date") +
        ylab("Cumulative Violation Score")
    })
    
    output$download_plot <- downloadHandler(
      filename = function() {
        paste("static_plot", '.png', sep = '')
      },
      content = function(file) {
        ggsave(file, plot = data$plot)
      }
    )
    
    output$table <- renderDataTable({
      # dataset_ts() %>% 
      #   select(facility_name,
      #          encounter,
      #          description_new,
      #          inspect_dt,
      #          chain,
      #          liquor,
      #          v_level)
      
      dataset_pm25()
    })
    
    return(dataset_pm25)
  })
}
    
## To be copied in the UI
# mod_panel_visualize_ui("panel_visualize_ui_1")
    
## To be copied in the server
# mod_panel_visualize_server("panel_visualize_ui_1")
