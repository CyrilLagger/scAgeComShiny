#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import data.table
#' @noRd
app_server <- function(input, output, session ) {
  # Your application server logic
  mod_introduction_server("introduction_ui_1")
  mod_tsa_server("tsa_ui_1")
  mod_tca_server("tca_ui_1")
  mod_glossary_server("glossary_ui_1")
}
