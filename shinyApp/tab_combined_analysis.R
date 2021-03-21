tab_combined_analysis <- tabPanel(
  title = "Global Analysis",
  fluidRow(
    column(width = 6, titlePanel(htmlOutput("TCA_TITLE")), offset = 3),
    #column(width = 6, DT::dataTableOutput("TCA_OVERVIEW_TABLE"), style = "padding-bottom: 50px", offset = 3)
  ),
  tabsetPanel(
    type = "tabs",
    tabPanel(
      title = "Summary Table",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(
            inputId = "TCA_GLOBAL_TABLE_CHOICE",
            label = "Category",
            choices = c("LRI", "GO Terms", "KEGG Pathways", "Cell-Type Families")
          ),
          selectInput(
            inputId = "TCA_GLOBAL_ORA_REGULATION_CHOICE",
            label = "ORA Regulation",
            choices = c("UP", "DOWN", "FLAT")
          )
        ),
        mainPanel(
          fluidRow(
            column(width = 12, uiOutput("TCA_GLOBAL_DETAILS"), style = "padding:50px")
          )
        )
      ),
      value = "TCA_SUMMARY_TABLE"
    ),
    tabPanel(
      title = "Keyword summary",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          # uiOutput("TCA_KEYWORD_CATEGORY_CHOICE"),
          # uiOutput("TCA_KEYWORD_VALUE_CHOICE")
          selectInput(
            inputId = "TCA_KEYWORD_CATEGORY_CHOICE",
            label = "Category",
            choices = c("LRI", "GO Terms", "KEGG Pathways", "Cell-Type Families")
          ),
          selectizeInput(
            inputId = "TCA_KEYWORD_VALUE_CHOICE",
            label = "Choose a term of interest",
            choices = NULL,
            multiple = FALSE
          )
        ),
        mainPanel(
          fluidRow(
            column(width = 12, plotOutput("TCA_KEYWORD_SUMMARY",  height = "600px"), style = "padding:50px")
          )
        )
      ),
      value = "TCA_KEYWORD_SUMMARY"
    ),
    id = "active_TCA_panel"
  )
)
