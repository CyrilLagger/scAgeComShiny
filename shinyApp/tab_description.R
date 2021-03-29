tab_description <- tabPanel(
  title = "Introduction",
  fluidRow(
    column(
      width = 6,
      titlePanel(htmlOutput("INTRO_TITLE")),
      offset = 3
    ),
  ),
  tabsetPanel(
    type = "tabs",
    tabPanel(
      title = "Overview",
      fluidRow(
        column(
          width = 12,
          htmlOutput("INTRO_OVERVIEW"),
          style = "padding:50px"
        )
      ),
      value = "INTRO_OVERVIEW_TAB"
    ),
    tabPanel(
      title = "Methodology",
      fluidRow(
        column(
          width = 12,
          htmlOutput("INTRO_METHOD"),
          style = "padding:50px"
        )
      ),
      value = "INTRO_METHOD_TAB"
    ),
    tabPanel(
      title = "Single-cell Data",
      fluidRow(
        column(
          width = 12,
          htmlOutput("INTRO_SCRNA_DATA"),
          style = "padding:50px"
        )
      ),
      value = "INTRO_SCRNA_DATA_TAB"
    ),
    tabPanel(
      title = "Ligand-Receptor Interaction Database",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(
            inputId = "INTRO_LRI_DETAILS_CHOICE",
            label = "Choose information to display",
            choices = c(
              "LRI Table",
              "Upset Plot by Source",
              "References"
            )
          ),
          conditionalPanel(
            condition = "input.INTRO_LRI_DETAILS_CHOICE != 'References'",
            selectInput(
              inputId = "INTRO_LRI_SPECIES_CHOICE",
              label = "Choose a Species",
              choices = c(
                "Mouse",
                "Human"
              )
            )
          ),
          conditionalPanel(
            condition = "input.INTRO_LRI_DETAILS_CHOICE == 'LRI Table'",
            uiOutput("INTRO_LRI_DATABASE_CHOICE")
          )
        ),
        mainPanel(
          fluidRow(
            column(
              width = 12,
              uiOutput("INTRO_LRI_DETAILS"),
              style = "padding:50px"
            )
          )
        )
      ),
      value = "INTRO_LRI_DATABASE_TAB"
    ),
    id = "active_INTRO_panel"
  )
)
