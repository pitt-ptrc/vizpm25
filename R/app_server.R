#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  mod_panel_upload_server("panel_upload_ui_1")
  mod_panel_visualize_server("panel_visualize_ui_1")
  mod_panel_download_server("panel_download_ui_1")
}
