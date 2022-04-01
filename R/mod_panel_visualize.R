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
        wellPanel(
          selectInput(
            ns("pm_select"),
            "PM2.5 Type",
            choices = 
              list(
                "sulfate (SO4)",
                "nitrate (NO3)",
                "ammonium (NH4)",
                "organic matter (OM)",
                "black carbon (BC)",
                "mineral dust (DUST)",
                "sea-salt (SS)"
              )
          ),
          sliderInput(
            ns("pm_slider"),
            "Time Period",
            min = as.Date("2018-01-01"),
            max = as.Date("2018-03-01"),
            value = c(
              as.Date("2018-01-01"),
              as.Date("2018-02-01")
            ),
            timeFormat = "%b %Y"
          )
        ),
        wellPanel(
          splitLayout(
            checkboxGroupInput(
              ns("adi_type"),
              "ADI Type",
              choices = 
                list(
                  "State",
                  "National"
                )
            ),
            checkboxGroupInput(
              ns("adi_year"),
              "ADI Year",
              choices = 
                list(
                  "2015",
                  "2019"
                )
            )
          )
        ),
        actionButton(
          ns("fetch_feat"),
          label = "Fetch data"
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
          tabPanel("Annotations", dataTableOutput(ns("feat_table")))
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
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom tibble as_tibble rowid_to_column
#' @importFrom terra vect extract subset
#' @importFrom stringr str_replace
#' @importFrom plotly renderPlotly
#' @importFrom DBI dbGetQuery

mod_panel_visualize_server <- function(id, dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    dataset_feat <- eventReactive(input$fetch_feat, {
      
      req(dataset())
      
      dataset_prep <- 
        dataset() %>% 
        rowid_to_column() %>% 
        # filter(!is.na(x)) %>% 
        # sf::st_as_sf(coords = c("x","y"))
        mutate(x = lng) %>% 
        mutate(y = lat) %>% 
        terra::vect(geom = c("x", "y"))
      
      correct_dates <-
        stringr::str_replace(input$pm_slider, pattern = "\\d\\d$", replacement = "01")
      
      sel_dates <- as.character(seq(
        from = as.Date(correct_dates[1]),
        to = as.Date(correct_dates[2]),
        by = "month"
      ))
      
      rt_sub <- terra::subset(rt_bcog, sel_dates)
      
      ex_test <- terra::extract(rt_sub, dataset_prep) %>% 
        tibble::as_tibble() %>%
        identity()
      
      dataset_poll <- dplyr::left_join(dataset_prep %>% as_tibble(), ex_test, by = c("rowid" = "ID"))
      
      ### ADI
      
      zips <- dataset() %>% 
        pull(zip)
      
      adis <- tbl(con, "adi_rank")
      
      zip_adi <- adis %>% 
        filter(zip %in% zips) %>% 
        collect()
      
      dataset_poll %>% 
        left_join(zip_adi)
      
        
      
    })
    
    output$map_plot <- renderLeaflet({
      
      binpal <- colorBin("RdYlGn", dataset_feat()$adi_natrank, 5, pretty = FALSE)
      
      leaflet(data = dataset_feat()) %>% 
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(
          lng = ~lng,
          lat = ~lat,
          label = ~id,
          # radius = ~sqrt(adi_natrank)
          color = ~binpal(adi_natrank),
        ) %>%
        addLegend("bottomright", pal = binpal, values = ~adi_natrank,
                  title = "ADI National Rank",
                  opacity = 1
        )
    })
    
    output$plot_path <- renderPlotly({
      dataset_feat() %>% 
        pivot_longer(cols = starts_with("2018"), names_to = "date", values_to = "pm25") %>%
        mutate(date = as.Date(date)) %>% 
        ggplot(aes(date, pm25, group = id)) +
        geom_path()
    })
    
    static_plot_input <- function(){
      dataset_feat() %>% 
        pivot_longer(cols = starts_with("2018"), names_to = "date", values_to = "pm25") %>%
        mutate(date = as.Date(date)) %>% 
        ggplot(aes(date, pm25, group = id)) +
        geom_path()
    }
    
    output$download_plot <- downloadHandler(
      filename = function() {
        paste("static_plot", '.png', sep = '')
      },
      content = function(file) {
        ggsave(file, plot = static_plot_input(), device = "png")
      }
    )
    
    output$feat_table <- renderDataTable({
      dataset_feat()
    })
    
    return(dataset_feat)
  })
}
    
## To be copied in the UI
# mod_panel_visualize_ui("panel_visualize_ui_1")
    
## To be copied in the server
# mod_panel_visualize_server("panel_visualize_ui_1")
