tab_tissue_specific <- tabPanel(
  title = "Tissue-Specific Analysis",
  fluidRow(
    column(
      width = 6,
      titlePanel(htmlOutput("TSA_TITLE")),
      offset = 3
      ),
    column(
      width = 6,
      DT::dataTableOutput("TSA_OVERVIEW_TABLE"),
      style = "padding-bottom: 50px",
      offset = 3
      )
  ),
  tabsetPanel(
    type = "tabs",
    tabPanel(
      title = "Cell-Cell Interactions",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          uiOutput("TSA_CCI_DATASET_CHOICE"),
          hr(),
          downloadButton("TSA_DOWNLOAD_TABLE", "Download Table"),
          hr(),
          uiOutput("TSA_EMITTER_CHOICE"),
          uiOutput("TSA_RECEIVER_CHOICE"),
          selectizeInput(
            inputId = "TSA_LRI_CHOICE",
            label = "Filter by Ligand-Receptor Interactions",
            choices = NULL,
            multiple = TRUE
            )
        ),
        mainPanel(
          uiOutput("TSA_CCI_DETAILS")
        )
      ),
      value = "TSA_INTERACTION_ANALYSIS"
    ),
    tabPanel(
      title = "Over-Representation Analysis",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          uiOutput("TSA_ORA_DATASET_CHOICE"),
          hr(),
          uiOutput("TSA_ORA_CATEGORY_CHOICE"),
          selectInput(
            inputId = "TSA_ORA_TYPE_CHOICE",
            label = "Age Regulation",
            choices = list("Up", "Down", "Flat")
          ),
          conditionalPanel(
            condition = "input.TSA_ORA_CATEGORY_CHOICE == 'GO Terms'",
            hr(),
            selectInput(
              inputId = "TSA_ORA_GO_ASPECT_CHOICE",
              label = "GO Aspect",
              choices = c("Biological Process", "Molecular Function", "Cellular Component")
            )
          )
        ),
        mainPanel(
          uiOutput("TSA_ORA_DETAILS")
        )
      ),
      value = "TSA_ORA"
    ),
    id = "active_TSA_panel"
  ),
  value = "TSA_navbar"
)
