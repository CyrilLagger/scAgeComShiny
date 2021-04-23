#' glossary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_glossary_ui <- function(id){
  ns <- NS(id)
  tagList(
    htmlOutput(ns("HELP"))
  )
}
    
#' glossary Server Functions
#'
#' @noRd 
mod_glossary_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$HELP <- renderUI({
      fluidPage(
        fluidRow(
          column(
            width = 10,
            offset = 1,
            scAgeCom_data$shiny_bsplus_glossary,
            style = "padding:10px"
          )
        )
      )
    })
 
  })
}
    
## To be copied in the UI
# mod_glossary_ui("glossary_ui_1")
    
## To be copied in the server
# mod_glossary_server("glossary_ui_1")
