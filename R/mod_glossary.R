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
  fluidPage(
    fluidRow(
      column(
        width = 10,
        offset = 1,
        uiOutput(ns("GLOSSARY_ACCORDION")),
        style = "padding:10px"
      )
    )
  )
}
    
#' glossary Server Functions
#'
#' @noRd 
mod_glossary_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$GLOSSARY_ACCORDION <- renderUI({
      bsplus::bs_accordion(
        id = "HELP_GLOSSARY"
      ) %>%
        bsplus::bs_append(
          title = "LRI: Ligand-Receptor Interaction",
          content = paste(
            "TODO"
          )
        ) %>%
        bsplus::bs_append(
          title = "CCI: Cell-Cell Interaction",
          content = paste(
            "TODO"
          )
        ) %>%
        bsplus::bs_append(
          title = "ORA Score",
          content = paste(
            "TODO"
          )
        ) %>%
        bsplus::bs_append(
          title = "TODO",
          content = paste(
            "TODO"
          )
        )
    })
  })
}