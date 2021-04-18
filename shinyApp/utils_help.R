
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

