
source("tab_description.R")
source("tab_tissue_specific.R")
source("tab_combined_analysis.R")
source("tab_help.R")

ui_scAgeCom <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel(
    title = textOutput("MAIN_TITLE", inline = TRUE),
    windowTitle = "scAgeCom"
  ),
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}")),
    tags$script(src = "https://kit.fontawesome.com/8deb5c53bb.js",
                crossorigin = "anonymous"),
    tags$script(src = "scAgeCom.js"),
    tags$link(rel = "stylesheet",
              type = "text/css",
              href = "scAgeCom.css")
  ),
  navbarPage(
    title = "scAgeCom",
    id = "navbarID",
    tab_description,
    tab_tissue_specific,
    tab_combined_analysis,
    tab_help
  )
)

