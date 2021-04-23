#' tsa UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tsa_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(
          width = 6,
          uiOutput(ns("TSA_TISSUE_CHOICE")),
          offset = 3
        )
      ),
      fluidRow(
        column(
          width = 6,
          align = "center",
          DT::dataTableOutput(ns("TSA_OVERVIEW_TABLE")),
          offset = 3
        )
      ),
      fluidRow(
        column(
          width = 6,
          uiOutput(ns("TSA_DATASET_CHOICE")),
          offset = 3
        )
      )
    ),
    uiOutput(ns("TSA_PANEL_VIEW"))
  )
}

#' tsa Server Functions
#'
#' @noRd 
mod_tsa_server <- function(id){
  moduleServer(
    id,
    function(
      input,
      output,
      session
    ) {
      ns <- session$ns

      output$TSA_TISSUE_CHOICE <- renderUI({
        #print("coucou2")
        choices <- scAgeComShiny::scAgeCom_data$ALL_TISSUES
        tags$table(
          style = "margin-top: 10px; margin-left: auto; margin-right:auto;",
          tags$tbody(
            tags$tr(
              tags$td(
                style = "vertical-align: top; padding-right: 10px;",
                tags$h2(
                  style = "margin-top: 4px; font-size: 20px;",
                  "Please choose a tissue: "
                )
              ),
              tags$td(
                selectizeInput(
                  inputId = ns("TSA_TISSUE_CHOICE"),
                  label = NULL,
                  choices = choices,
                  width = "200px",
                  options = list(
                    placeholder = 'Please select an option',
                    onInitialize = I('function() { this.setValue(""); }')
                  )
                )
              )
            )
          )
        )
      })
      
      output$TSA_OVERVIEW_TABLE <- DT::renderDT({
        #print("coucou3a")
        req(input$TSA_TISSUE_CHOICE)
        #print("coucou3b")
        Tissue <- NULL
        dt <- scAgeComShiny::scAgeCom_data$TISSUE_COUNTS_SUMMARY[
          Tissue == input$TSA_TISSUE_CHOICE
        ]
        display_tissue_counts(
          tissue_counts_summary = dt
        )
      })
      
      output$TSA_DATASET_CHOICE <- renderUI({
        #print("coucou4a")
        req(input$TSA_TISSUE_CHOICE)
        #print("coucou4b")
        dt <- scAgeComShiny::scAgeCom_data$TISSUE_COUNTS_SUMMARY[
          Tissue == input$TSA_TISSUE_CHOICE
        ]
        tags$table(
          style = "margin-top: 25px; margin-left: auto; margin-right:auto;",
          tags$tbody(
            tags$tr(
              tags$td(
                style = "vertical-align: top; padding-right: 10px;",
                tags$h2(
                  style = "margin-top: 4px; font-size: 20px;",
                  "Please select a dataset: "
                )
              ),
              tags$td(
                selectizeInput(
                  inputId = ns("TSA_DATASET_CHOICE"),
                  label = NULL,
                  width = "200px",
                  choices = sort(unique(dt$Dataset)),
                  options = list(
                    placeholder = 'Please select an option',
                    onInitialize = I('function() { this.setValue(""); }')
                  )
                )
              )
            )
          )
        )
      })
      
      rv_tsa <- reactiveValues(
        tissue_choice = NULL,
        dataset_choice = NULL
      )
      
      observeEvent(
        input$TSA_TISSUE_CHOICE,
        {
          #print("coucouA1a")
          req(input$TSA_TISSUE_CHOICE)
          rv_tsa$dataset_choice <- NULL
          #print("coucouA1b")
          rv_tsa$tissue_choice <- input$TSA_TISSUE_CHOICE
        }
      )
      
      observeEvent(
        input$TSA_DATASET_CHOICE,
        {
          #print("coucouB1a")
          req(
            input$TSA_TISSUE_CHOICE,
            input$TSA_DATASET_CHOICE
            )
          #print("coucouB1b")
          rv_tsa$dataset_choice <- input$TSA_DATASET_CHOICE
        }
      )
      
      mod_tsa_cci_server(
        "tsa_cci_ui_1",
        rv_tsa = rv_tsa
      )
      
      mod_tsa_ora_server(
        "tsa_ora_ui_1",
        rv_tsa = rv_tsa
      )
      
      output$TSA_PANEL_VIEW <- renderUI({
        #print("coucou5a")
        req(
          input$TSA_TISSUE_CHOICE,
          input$TSA_DATASET_CHOICE
        )
        #print("coucou5b")
        tabsetPanel(
          type = "tabs",
          mod_tsa_cci_ui(ns("tsa_cci_ui_1")),
          mod_tsa_ora_ui(ns("tsa_ora_ui_1")),
          id = "active_TSA_panel",
          selected = "TSA_INTERACTION_ANALYSIS"
        )
      })
      
    })
}

display_tissue_counts <- function(
  tissue_counts_summary
) {
  dt <- tissue_counts_summary[, -c(8)]
  DT::datatable(
    data = dt,
    class = "display compact",
    options =list(
      pageLength = 10,
      dom = "t"
    ),
    rownames = FALSE,
    callback = htmlwidgets::JS(
      "var tips = [
        'Selected Tissue',
        'Dataset on which the interaction analysis has been performed',
        'Number of cell-types in the tissue of interest',
        'Number of cell-cell interactions detected in the tissue of interest',
        'Number of stable CCIs with age',
        'Number of down-regulated CCIs with age',
        'Number of up-regulated CCIs with age'
        ],
        header = table.columns().header();
        for (var i = 0; i < tips.length; i++) {
        $(header[i]).attr('title', tips[i]);
        }"
    )
  ) %>%
    DT::formatStyle(
      colnames(dt)[3:7],
      `text-align` = 'center'
    )
}

