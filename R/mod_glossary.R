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
          title = "LRI: ligand-receptor interaction",
          content = paste(
            "TODO"
          )
        ) %>%
        bsplus::bs_append(
          title = "CCI: cell-cell interaction",
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
          title = "ORA Score",
          content = paste(
            "TODO"
          )
        ) %>%
        bsplus::bs_append(
          title = "TMS FACS",
          content = paste(
            "Refers to Tabula Muris Senis scRNA-seq datasets obtained by",
            "cell sorting in microtiter well plates followed by Smart-seq2",
            "library preparation and full-length sequencing."
          )
        ) %>%
        bsplus::bs_append(
          title = "TMS Droplet / Calico Droplet",
          content = paste(
            "Refers to Tabula Muris Senis and Calico scRNA-seq datasets",
            "obtained by cell capture by microfluidic droplets as per the",
            "10x Genomics protocol followed by 3' end counting."
          )
        )
    })
  })
}