server_scAgeCom <- function(
  input,
  output,
  session
) {
  output$MAIN_TITLE <- renderUI({
    scAgeCom_data$shiny_html_content$main_title
  })
  source("utils_introduction.R", local = TRUE)
  source("utils_tissue_specific.R", local = TRUE)
  source("utils_combined_analysis.R", local = TRUE)
  source("utils_help.R", local = TRUE)
}

