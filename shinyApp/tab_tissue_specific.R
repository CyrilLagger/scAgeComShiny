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
          selectInput(
            inputId = "TSA_CCI_DETAILS_CHOICE",
            label = "Choose between Table or Plots",
            choices = c(
              "CCI Table",
              "Volcano Plot",
              "Score Plot",
              "LRI-FC Plot"
            )
          ),
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
            ),
          sliderInput(
            inputId = "TSA_SLIDER_PVALUE",
            label = "Filter by Adj. p-value",
            min = 0,
            max = 1,
            value = 1
          ),
          uiOutput("TSA_SLIDER_LOG2FC")
        ),
        mainPanel(
          fluidRow(
            column(
              width = 12,
              uiOutput("TSA_CCI_DETAILS"),
              style = "padding:50px"
            ),
            column(
              width = 12,
              uiOutput("TSA_CCI_TEXTOUTPUT")
            )
          )
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
          selectInput(
            inputId = "TSA_ORA_DETAILS_CHOICE",
            label = "Choose between Table or Plot",
            choices = c("ORA Table", "ORA Score Plot", "ORA Network")
          ),
          conditionalPanel(
            condition = "input.TSA_ORA_DETAILS_CHOICE != 'ORA Network'",
            uiOutput("TSA_ORA_CATEGORY_CHOICE"),
            selectInput(
              inputId = "TSA_ORA_TYPE_CHOICE",
              label = "Age Regulation",
              choices = list("Up", "Down", "Flat")
            )
          )
        ),
        mainPanel(
          fluidRow(
            column(
              width = 12,
              uiOutput("TSA_ORA_DETAILS"),
              style = "padding:50px"
            ),
            column(
              width = 12,
              htmlOutput("TSA_ORA_INTRO"),
              style = "padding:50px"
            )
          )
        )
      ),
      value = "TSA_ORA"
    ),
    id = "active_TSA_panel"
  )
)
