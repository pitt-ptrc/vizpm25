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
          checkboxGroupInput(
            ns("select_type"),
            "Dataset Type",
            choices = c(
              
            )
          ),
          checkboxGroupInput(
            ns("mat_select"),
            "Material Type",
            choices = 
              c(
                "Particulate Matter (PM25)" = "PM25",
                "Black Carbon (BC)" = "BC",
                "Ammonium (NH4)" = "NH4",
                "Nitrate (NIT)" = "NIT",
                "Sulfate (SO4)" = "SO4",
                "Soil (SOIL)" = "SOIL"
                # "organic matter (OM)" = "OM",
                # "mineral dust (DUST)" = "DUS",
                # "sea-salt (SS)" = "SS"
              ),
            selected = "PM25"
          ),
          sliderInput(
            ns("mat_slider"),
            "Time Period",
            min = as.Date("2000-01-01"),
            max = as.Date("2017-12-01"),
            value = c(
              as.Date("2001-01-01"),
              as.Date("2002-01-01")
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
#' @import purrr
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
        stringr::str_replace(input$mat_slider, pattern = "\\d\\d$", replacement = "01")
      
      sel_dates <- as.character(seq(
        from = as.Date(correct_dates[1]),
        to = as.Date(correct_dates[2]),
        by = "month"
      ))
      
      mat_rast_list_sel <- mat_rast_list[input$mat_select] %>% 
        map(~ terra::subset(.x, sel_dates))
      
      geo_extr <- mat_rast_list_sel %>% 
        map(~ terra::extract(.x, dataset_prep)) %>% 
        map(~ tibble(.x) %>% pivot_longer(-ID) %>% 
              mutate(date = name %>% as.Date())) %>% 
        imap(~ mutate(.x, material = .y)) %>% 
        bind_rows()
      
      # rt_sub <- terra::subset(rt_bcog, sel_dates)
      # 
      # ex_test <- terra::extract(rt_sub, dataset_prep) %>% 
      #   tibble::as_tibble() %>%
      #   identity()
      
      dataset_poll <- dplyr::left_join(dataset_prep %>% as_tibble(), geo_extr, by = c("rowid" = "ID"))
      
      ### ADI
      
      zips <- dataset() %>% 
        pull(zip)
      
      # adis <- tbl(con, "adi_rank")
      
      
      
      zip_adi <- adis %>% 
        filter(zip %in% zips) %>% 
        collect() %>% 
        mutate(adi_staterank = as.integer(adi_staterank)) %>% 
        mutate(adi_natrank = as.integer(adi_natrank))
      
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
        ggplot(aes(date, value, group = id)) +
        geom_path() +
        facet_wrap(vars(material))
    })
    
    static_plot_input <- function(){
      dataset_feat() %>% 
        ggplot(aes(date, value, group = id)) +
        geom_path() +
        facet_wrap(vars(material))
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
