
ui_scAgeCom <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel(
    title = textOutput("MAIN_TITLE", inline = TRUE),
    windowTitle = "scAgeCom"
  ),
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}")),
    tags$style(
      type = 'text/css','.navbar-brand{display:none;}'
    ),
    tags$script(
      src = "https://kit.fontawesome.com/8deb5c53bb.js",
      crossorigin = "anonymous"
    ),
    tags$script(
      src = "scAgeCom.js"
    ),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "scAgeCom.css"
    )
  ),
  navbarPage(
    title = NULL,
    id = "navbarID",
    tabPanel(
      title = "scAgeCom",
      uiOutput("INTRO_PAGE_VIEW"),
      value = "INTRO_navbar"
    ),
    tabPanel(
      title = "Tissue Specific Results",
      uiOutput("TSA_TOP_VIEW"),
      uiOutput("TSA_PANEL_VIEW"),
      value = "TSA_navbar"
    ),
    tabPanel(
      title = "Results Across Tissues",
      uiOutput("TCA_TOP_VIEW"),
      uiOutput("TCA_PANEL_VIEW"),
      value = "TCA_navbar"
    ),
    tabPanel(
      title = "Glossary",
      htmlOutput("HELP"),
      value = "HELP_navbar"
    )
  )
)

