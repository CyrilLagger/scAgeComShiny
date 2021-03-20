
source("tab_description.R")
source("tab_tissue_specific.R")
source("tab_combined_analysis.R")

ui_scAgeCom <- fluidPage(
  titlePanel(
    title = htmlOutput("MAIN_TITLE")
  ),
  tags$head(
    tags$style(
      HTML(
        "hr {border-top: 1px solid #000000;}"
      ))
  ),
  navbarPage(
    title = "scAgeCom",
    tab_description,
    tab_tissue_specific,
    tab_combined_analysis
  )
)

