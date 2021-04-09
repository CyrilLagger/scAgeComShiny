
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
      title = "Tissue-Specific Analysis",
      uiOutput("TSA_TOP_VIEW"),
      uiOutput("TSA_PANEL_VIEW"),
      value = "TSA_navbar"
    ),
    tabPanel(
      title = "Global Analysis",
      uiOutput("TCA_TOP_VIEW"),
      uiOutput("TCA_PANEL_VIEW"),
      value = "TCA_navbar"
    ),
    # tabPanel(
    #   title = "Ligand-Receptor Interaction Database",
    #   sidebarLayout(
    #     sidebarPanel(
    #       width = 3,
    #       selectInput(
    #         inputId = "INTRO_LRI_DETAILS_CHOICE",
    #         label = "Choose information to display",
    #         choices = c(
    #           "LRI Table",
    #           "Upset Plot by Source",
    #           "References"
    #         )
    #       ),
    #       conditionalPanel(
    #         condition = "input.INTRO_LRI_DETAILS_CHOICE != 'References'",
    #         selectInput(
    #           inputId = "INTRO_LRI_SPECIES_CHOICE",
    #           label = "Choose a Species",
    #           choices = c(
    #             "Mouse",
    #             "Human"
    #           )
    #         )
    #       ),
    #       conditionalPanel(
    #         condition = "input.INTRO_LRI_DETAILS_CHOICE == 'LRI Table'",
    #         uiOutput("INTRO_LRI_DATABASE_CHOICE")
    #       )
    #     ),
    #     mainPanel(
    #       fluidRow(
    #         column(
    #           width = 12,
    #           uiOutput("INTRO_LRI_DETAILS"),
    #           style = "padding:50px"
    #         )
    #       )
    #     )
    #   ),
    #   value = "INTRO_LRI_DATABASE_TAB"
    # ),
    tabPanel(
      title = "Help",
      htmlOutput("HELP"),
      value = "HELP_navbar"
    )
  )
)

