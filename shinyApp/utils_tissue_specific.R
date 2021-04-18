
output$TSA_TOP_VIEW <- renderUI({
  fluidPage(
    fluidRow(
      column(
        width = 6,
        #align = "center",
        uiOutput("TSA_TISSUE_CHOICE"),
        offset = 3
      )
    ),
    fluidRow(
      column(
        width = 6,
        align = "center",
        DT::dataTableOutput("TSA_OVERVIEW_TABLE"),
        offset = 3
      )
    ),
    fluidRow(
      column(
        width = 6,
        #align = "center",
        uiOutput("TSA_DATASET_CHOICE"),
        offset = 3
      )
    )
  )
})

output$TSA_TISSUE_CHOICE <- renderUI({
  choices <- scAgeCom_data$ALL_TISSUES
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
            inputId = "TSA_TISSUE_CHOICE",
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

# output$TSA_TISSUE_CHOICE <- renderUI({
#   choices <- scAgeCom_data$ALL_TISSUES
#   tags$h2(
#     style = "text-align: center; margin-top: 10px;",
#     div(
#       style = "display: inline-block; font-size: 20px;",
#       "Please choose a tissue: "
#     ),
#     div(
#       style = "display: inline-block; text-align: left",
#       selectizeInput(
#         inputId = "TSA_TISSUE_CHOICE",
#         label = NULL,
#         choices = choices,
#         width = "200px",
#         options = list(
#           placeholder = 'Please select an option',
#           onInitialize = I('function() { this.setValue(""); }')
#         )
#       )
#     )
#   )
# })

output$TSA_OVERVIEW_TABLE <- DT::renderDT({
  req(input$TSA_TISSUE_CHOICE)
  dt <- scAgeCom_data$TISSUE_COUNTS_SUMMARY[Tissue == input$TSA_TISSUE_CHOICE]
  scAgeCom_data$build_tissue_counts_display(
    tissue_counts_summary = dt
  )
})

output$TSA_DATASET_CHOICE <- renderUI({
  req(input$TSA_TISSUE_CHOICE)
  dt <- scAgeCom_data$TISSUE_COUNTS_SUMMARY[
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
            inputId = "TSA_DATASET_CHOICE",
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

# output$TSA_DATASET_CHOICE <- renderUI({
#   req(input$TSA_TISSUE_CHOICE)
#   dt <- scAgeCom_data$TISSUE_COUNTS_SUMMARY[
#     Tissue == input$TSA_TISSUE_CHOICE
#   ]
#   tags$h2(
#     style = "text-align: center; margin-top: 20px;",
#     div(
#       style = "display: inline-block; font-size: 20px;",
#       "Please select a dataset: "
#     ),
#     div(
#       style = "display: inline-block; text-align: left",
#       selectizeInput(
#         inputId = "TSA_DATASET_CHOICE",
#         label = NULL,
#         width = "200px",
#         choices = sort(unique(dt$Dataset)),
#         options = list(
#           placeholder = 'Please select an option',
#           onInitialize = I('function() { this.setValue(""); }')
#         )
#       )
#     )
#   )
# })

output$TSA_PANEL_VIEW <- renderUI({
  req(
    input$TSA_DATASET_CHOICE,
    input$TSA_TISSUE_CHOICE
  )
  tabsetPanel(
    type = "tabs",
    tabPanel(
      title = "Table of Interactions",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          downloadButton(
            "TSA_DOWNLOAD_TABLE",
            "Download Full Table"
          ),
          hr(),
          h4("Filtering Options"),
          uiOutput("TSA_EMITTER_CHOICE"),
          uiOutput("TSA_RECEIVER_CHOICE"),
          selectizeInput(
            inputId = "TSA_LRI_CHOICE",
            label = "Ligand-Receptor Interactions",
            choices = NULL,
            multiple = TRUE
          ),
          selectizeInput(
            inputId = "TSA_GENE_CHOICE",
            label = "Single Genes",
            choices = NULL,
            multiple = TRUE
          ),
          selectizeInput(
            inputId = "TSA_GO_CHOICE",
            label = "GO Terms",
            choices = NULL,
            multiple = TRUE
          ),
          selectizeInput(
            inputId = "TSA_KEGG_CHOICE",
            label = "KEGG Pathways",
            choices = NULL,
            multiple = TRUE
          ),
          actionButton(
            inputId = "TSA_FILTER_BUTTON",
            label = "Filter"
          ),
          actionButton(
            inputId = "TSA_RESET_BUTTON",
            label = "Undo Filtering"
          )
        ),
        mainPanel(
          width = 10,
          uiOutput("TSA_CCI_TITLE"),
          uiOutput("TSA_CCI_DETAILS")
        )
      ),
      value = "TSA_INTERACTION_ANALYSIS"
    ),
    tabPanel(
      title = "Over-Representation Analysis",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          uiOutput("TSA_ORA_CATEGORY_CHOICE"),
          conditionalPanel(
            condition = "input.TSA_ORA_CATEGORY_CHOICE != 'By Cell Types'",
            hr(),
            uiOutput("TSA_ORA_TYPE_CHOICE")
          ),
          conditionalPanel(
            condition = "input.TSA_ORA_CATEGORY_CHOICE == 'By GO/KEGG'",
            hr(),
            uiOutput("TSA_ORA_GO_ASPECT_CHOICE")
          )
        ),
        mainPanel(
          width = 10,
          uiOutput("TSA_ORA_TITLE"),
          uiOutput("TSA_ORA_DETAILS")
        )
      ),
      value = "TSA_ORA"
    ),
    id = "active_TSA_panel"
  )
})

output$TSA_EMITTER_CHOICE <- renderUI({
  req(
    input$TSA_DATASET_CHOICE,
    input$TSA_TISSUE_CHOICE
  )
  choices <- sort(scAgeCom_data$ALL_CELLTYPES[
    Dataset == input$TSA_DATASET_CHOICE &
      Tissue == input$TSA_TISSUE_CHOICE
  ][["CELLTYPE"]])
  pickerInput(
    inputId = "TSA_EMITTER_CHOICE",
    label = "Emitter Cell Types",
    choices = choices,
    selected = choices,
    options = list(`actions-box` = TRUE),
    multiple = TRUE
  )
})

output$TSA_RECEIVER_CHOICE <- renderUI({
  req(
    input$TSA_DATASET_CHOICE,
    input$TSA_TISSUE_CHOICE
  )
  choices <- sort(scAgeCom_data$ALL_CELLTYPES[
    Dataset == input$TSA_DATASET_CHOICE &
      Tissue == input$TSA_TISSUE_CHOICE
  ][["CELLTYPE"]])
  pickerInput(
    inputId = "TSA_RECEIVER_CHOICE",
    label = "Receiver Cell Types",
    choices = choices,
    selected = choices,
    options = list(`actions-box` = TRUE),
    multiple = TRUE
  )
})

observe({
  req(
    input$TSA_DATASET_CHOICE,
    input$TSA_TISSUE_CHOICE
  )
  ALL_LRI_LABEL = 'All LRIs'
  choices <-
    c(
      ALL_LRI_LABEL,
      sort(
        scAgeCom_data$ALL_LRIs[
          Dataset == input$TSA_DATASET_CHOICE &
            Tissue == input$TSA_TISSUE_CHOICE
        ][["LRI"]]
      )
    )
  updateSelectizeInput(
    session = session,
    "TSA_LRI_CHOICE",
    choices = choices,
    selected = ALL_LRI_LABEL,
    options = list(
      allowEmptyOption = TRUE,
      placeholder = 'Type LRIs',
      maxOptions = length(choices)
    ),
    server = TRUE
  )
})

observe({
  req(
    input$TSA_DATASET_CHOICE,
    input$TSA_TISSUE_CHOICE
  )
  ALL_GENE_LABEL = 'All Genes'
  choices <-
    c(
      ALL_GENE_LABEL,
      sort(
        scAgeCom_data$ALL_GENES[
          Dataset == input$TSA_DATASET_CHOICE &
            Tissue == input$TSA_TISSUE_CHOICE
        ][["GENE"]]
      )
    )
  updateSelectizeInput(
    session = session,
    "TSA_GENE_CHOICE",
    choices = choices,
    selected = ALL_GENE_LABEL,
    options = list(
      allowEmptyOption = TRUE,
      placeholder = 'Type Genes',
      maxOptions = length(choices)
    ),
    server = TRUE
  )
})

observe({
  req(
    input$TSA_DATASET_CHOICE,
    input$TSA_TISSUE_CHOICE
  )
  ALL_GO_LABEL = 'All GO Terms'
  choices <-
    c(
      ALL_GO_LABEL,
      sort(
        scAgeCom_data$ALL_GO_TERMS[
          Dataset == input$TSA_DATASET_CHOICE &
            Tissue == input$TSA_TISSUE_CHOICE
        ][["GO_NAMES"]]
      )
    )
  updateSelectizeInput(
    session = session,
    "TSA_GO_CHOICE",
    choices = choices,
    selected = ALL_GO_LABEL,
    options = list(
      allowEmptyOption = TRUE,
      placeholder = 'Type GO Terms',
      maxOptions = length(choices)
    ),
    server = TRUE
  )
})

observe({
  req(
    input$TSA_DATASET_CHOICE,
    input$TSA_TISSUE_CHOICE
  )
  ALL_KEGG_LABEL = 'All KEGG Pathways'
  choices <-
    c(
      ALL_KEGG_LABEL,
      sort(
        scAgeCom_data$ALL_KEGG_PWS[
          Dataset == input$TSA_DATASET_CHOICE &
            Tissue == input$TSA_TISSUE_CHOICE
        ][["KEGG_NAMES"]]
      )
    )
  updateSelectizeInput(
    session = session,
    "TSA_KEGG_CHOICE",
    choices = choices,
    selected = ALL_KEGG_LABEL,
    options = list(
      allowEmptyOption = TRUE,
      placeholder = 'Type KEGG Pathways',
      maxOptions = length(choices)
    ),
    server = TRUE
  )
})

filter_values <- reactiveValues(
  do_filtering = FALSE,
  emitter_choice = NULL,
  receiver_choice = NULL,
  LRI_choice = NULL,
  GENE_choice = NULL,
  GO_choice = NULL,
  KEGG_choice = NULL
)

observeEvent(
  input$TSA_FILTER_BUTTON,
  {
    filter_values$do_filtering <- TRUE
    filter_values$emitter_choice <- input$TSA_EMITTER_CHOICE
    filter_values$receiver_choice <- input$TSA_RECEIVER_CHOICE
    filter_values$LRI_choice <- input$TSA_LRI_CHOICE
    filter_values$GENE_choice <- input$TSA_GENE_CHOICE
    filter_values$GO_choice <- input$TSA_GO_CHOICE
    filter_values$KEGG_choice <- input$TSA_KEGG_CHOICE
  }
)

observeEvent(
  input$TSA_RESET_BUTTON,
  {
    filter_values$do_filtering <- FALSE
    filter_values$emitter_choice <- NULL
    filter_values$receiver_choice <- NULL
    filter_values$LRI_choice <- NULL
    filter_values$GENE_choice <- NULL
    filter_values$GO_choice <- NULL
    filter_values$KEGG_choice <- NULL
    choices <- sort(scAgeCom_data$ALL_CELLTYPES[
      Dataset == input$TSA_DATASET_CHOICE &
        Tissue == input$TSA_TISSUE_CHOICE
    ][["CELLTYPE"]])
    updatePickerInput(
      session = session,
      inputId = "TSA_EMITTER_CHOICE",
      choices = choices,
      selected = choices
    )
    updatePickerInput(
      session = session,
      inputId = "TSA_RECEIVER_CHOICE",
      choices = choices,
      selected = choices
    )
    ALL_LRI_LABEL = 'All LRIs'
    choices_lri <-
      c(
        ALL_LRI_LABEL,
        sort(
          scAgeCom_data$ALL_LRIs[
            Dataset == input$TSA_DATASET_CHOICE &
              Tissue == input$TSA_TISSUE_CHOICE
          ][["LRI"]]
        )
      )
    updateSelectizeInput(
      session = session,
      "TSA_LRI_CHOICE",
      choices = choices_lri,
      selected = ALL_LRI_LABEL,
      options = list(
        allowEmptyOption = TRUE,
        placeholder = 'Type LRIs',
        maxOptions = length(choices)
      ),
      server = TRUE
    )
    ALL_GENE_LABEL = 'All Genes'
    choices <-
      c(
        ALL_GENE_LABEL,
        sort(
          scAgeCom_data$ALL_GENES[
            Dataset == input$TSA_DATASET_CHOICE &
              Tissue == input$TSA_TISSUE_CHOICE
          ][["GENE"]]
        )
      )
    updateSelectizeInput(
      session = session,
      "TSA_GENE_CHOICE",
      choices = choices,
      selected = ALL_GENE_LABEL,
      options = list(
        allowEmptyOption = TRUE,
        placeholder = 'Type Genes',
        maxOptions = length(choices)
      ),
      server = TRUE
    )
    ALL_GO_LABEL = 'All GO Terms'
    choices <-
      c(
        ALL_GO_LABEL,
        sort(
          scAgeCom_data$ALL_GO_TERMS[
            Dataset == input$TSA_DATASET_CHOICE &
              Tissue == input$TSA_TISSUE_CHOICE
          ][["GO_NAMES"]]
        )
      )
    updateSelectizeInput(
      session = session,
      "TSA_GO_CHOICE",
      choices = choices,
      selected = ALL_GO_LABEL,
      options = list(
        allowEmptyOption = TRUE,
        placeholder = 'Type GO Terms',
        maxOptions = length(choices)
      ),
      server = TRUE
    )
    ALL_KEGG_LABEL = 'All KEGG Pathways'
    choices <-
      c(
        ALL_KEGG_LABEL,
        sort(
          scAgeCom_data$ALL_KEGG_PWS[
            Dataset == input$TSA_DATASET_CHOICE &
              Tissue == input$TSA_TISSUE_CHOICE
          ][["KEGG_NAMES"]]
        )
      )
    updateSelectizeInput(
      session = session,
      "TSA_KEGG_CHOICE",
      choices = choices,
      selected = ALL_KEGG_LABEL,
      options = list(
        allowEmptyOption = TRUE,
        placeholder = 'Type KEGG Pathways',
        maxOptions = length(choices)
      ),
      server = TRUE
    )
  }
)

output$TSA_CCI_TITLE <- renderUI({
  req(
    input$TSA_DATASET_CHOICE,
    input$TSA_TISSUE_CHOICE
  )
  fluidPage(
    fluidRow(
      column(
        width = 12,
        titlePanel(
          tags$p(
            div(
              style = paste(
                "width: 80%;",
                "margin:auto;",
                "font-size: 20px;",
                "text-align: center;"
              ),
              "Plots and Table for the ",
              span(
                style = "font-weight: bold",
                input$TSA_TISSUE_CHOICE
              ),
              " from ",
              span(
                style = "font-weight: bold",
                input$TSA_DATASET_CHOICE
              )
            )
          )
        )
      )
    )
  )
})

CCI_table <- reactive({
  req(
    input$TSA_DATASET_CHOICE,
    input$TSA_TISSUE_CHOICE
  )
  if (filter_values$do_filtering) {
    CCI_table <- scAgeCom_data$subset_CCI_table(
      CCI_table = scAgeCom_data$CCI_table,
      dataset_choice = input$TSA_DATASET_CHOICE,
      tissue_choice = input$TSA_TISSUE_CHOICE,
      emitter_choice = filter_values$emitter_choice,
      receiver_choice = filter_values$receiver_choice,
      LRI_choice = filter_values$LRI_choice,
      GENE_choice = filter_values$GENE_choice,
      GO_choice = filter_values$GO_choice,
      KEGG_choice = filter_values$KEGG_choice,
      filter = TRUE
    )
  } else {
    CCI_table <- scAgeCom_data$subset_CCI_table(
      CCI_table = scAgeCom_data$CCI_table,
      dataset_choice = input$TSA_DATASET_CHOICE,
      tissue_choice = input$TSA_TISSUE_CHOICE,
      filter = FALSE
    )
  }
  CCI_table
})

CCI_display <- reactive({
  scAgeCom_data$build_CCI_display(
    CCI_table = CCI_table()
  )
})

output$TSA_CCI_DT <- renderDT({CCI_display()$CCI_DT})

output$TSA_PLOTLY_VOLCANO <- renderPlotly({CCI_display()$CCI_VOLCANO_PLOT})

output$TSA_PLOTLY_SCORE <- renderPlotly({CCI_display()$CCI_SCORE_PLOT})

output$TSA_PLOTLY_LRFC <- renderPlotly({CCI_display()$CCI_LRFC_PLOT})

output$TSA_CCI_DETAILS <- renderUI({
  fluidPage(
    fluidRow(
      column(
        style = "padding: 10px;",
        width = 6,
        plotlyOutput(
          outputId = "TSA_PLOTLY_VOLCANO",
          height = "520px"
        )
      ),
      column(
        style = "padding: 10px;",
        width = 6,
        plotlyOutput(
          outputId = "TSA_PLOTLY_SCORE",
          height = "520px"
        )
      )
    ),
    fluidRow(
      column(
        style = "padding: 10px;",
        width = 6,
        offset = 3,
        plotlyOutput(
          outputId = "TSA_PLOTLY_LRFC",
          height = "520px"
        )
      )
    ),
    fluidRow(
      column(
        width = 10,
        offset = 1,
        DTOutput(
          outputId = "TSA_CCI_DT"
        )
      )
    )
  )
})

output$TSA_DOWNLOAD_TABLE <- downloadHandler(
  filename = function() {
    paste0(
      "cci_table_",
      tolower(gsub(" ", "_", input$TSA_DATASET_CHOICE, fixed = TRUE)),
      "_",
      tolower(gsub(" ", "_", input$TSA_TISSUE_CHOICE, fixed = TRUE)),
      ".csv"
    )
  },
  content = function(file) {
    fwrite(CCI_table()[, 1:12], file)
  }
)

output$TSA_ORA_TITLE <- renderUI({
  req(
    input$TSA_DATASET_CHOICE,
    input$TSA_TISSUE_CHOICE
  )
  fluidPage(
    fluidRow(
      column(
        width = 12,
        titlePanel(
          tags$p(
            div(
              style = paste(
                "width: 80%;",
                "margin:auto;",
                "font-size: 20px;",
                "text-align: center;"
              ),
              "Over-representation Results for the ",
              span(
                style = "font-weight: bold",
                input$TSA_TISSUE_CHOICE
              ),
              " from ",
              span(
                style = "font-weight: bold",
                input$TSA_DATASET_CHOICE
              )
            )
          )
        )
      )
    )
  )
})

output$TSA_ORA_CATEGORY_CHOICE <- renderUI({
  choices <- scAgeCom_data$ALL_ORA_CATEGORIES_SPECIFIC
  pickerInput(
    inputId = "TSA_ORA_CATEGORY_CHOICE",
    label = "Category",
    choices = choices,
    options = list(`actions-box` = TRUE),
    multiple = FALSE
  )
})

output$TSA_ORA_TYPE_CHOICE <- renderUI({
  choices <- scAgeCom_data$ALL_ORA_TYPES
  selectInput(
    inputId = "TSA_ORA_TYPE_CHOICE",
    label = "Age Regulation",
    choices = choices
  )
})

output$TSA_ORA_GO_ASPECT_CHOICE <- renderUI({
  choices <- scAgeCom_data$ALL_ORA_GO_ASPECTS
  selectInput(
    inputId = "TSA_ORA_GO_ASPECT_CHOICE",
    label = "GO Aspect",
    choices = choices
  )
})

output$TSA_ORA_DETAILS <-  renderUI({
  req(input$TSA_ORA_CATEGORY_CHOICE)
  if (input$TSA_ORA_CATEGORY_CHOICE == "By Cell Types") {
    fluidPage(
      fluidRow(
        column(
          width = 12,
          visNetworkOutput("TSA_ORA_NETWORK_PLOT", height = "800px")
        )
      )
    )
  } else if (input$TSA_ORA_CATEGORY_CHOICE == "By Genes") {
    fluidPage(
      fluidRow(
        column(
          width = 4,
          plotOutput("TSA_ORA_PLOT_LRI", height = "500px")
        ),
        column(
          width = 4,
          plotOutput("TSA_ORA_PLOT_LIGAND", height = "500px")
        ),
        column(
          width = 4,
          plotOutput("TSA_ORA_PLOT_RECEPTOR", height = "500px")
        )
      ),
      fluidRow(
        column(
          style = "padding: 10px;",
          width = 8,
          offset = 2,
          dataTableOutput("TSA_ORA_TABLE_LRI")
        )
      ),
      fluidRow(
        column(
          style = "padding: 10px;",
          width = 8,
          offset = 2,
          dataTableOutput("TSA_ORA_TABLE_LIGAND")
        )
      ),
      fluidRow(
        column(
          style = "padding: 10px;",
          width = 8,
          offset = 2,
          dataTableOutput("TSA_ORA_TABLE_RECEPTOR")
        )
      )
    )
  } else if (input$TSA_ORA_CATEGORY_CHOICE == "By GO/KEGG") {
    fluidPage(
      fluidRow(
        column(
          width = 6,
          plotOutput("TSA_ORA_PLOT_GO", height = "600px")
        ),
        column(
          width = 6,
          plotOutput("TSA_ORA_PLOT_KEGG", height = "600px")
        )
      ),
      fluidRow(
        column(
          style = "padding: 10px;",
          width = 8,
          offset = 2,
          dataTableOutput("TSA_ORA_TABLE_GO")
        )
      ),
      fluidRow(
        column(
          style = "padding: 10px;",
          width = 8,
          offset = 2,
          dataTableOutput("TSA_ORA_TABLE_KEGG")
        )
      )
    )
  }
})

output$TSA_ORA_TABLE_LRI <- DT::renderDataTable({
  req(
    input$TSA_DATASET_CHOICE,
    input$TSA_TISSUE_CHOICE,
    input$TSA_ORA_CATEGORY_CHOICE,
    input$TSA_ORA_TYPE_CHOICE
  )
  dt <- scAgeCom_data$subset_ORA_table(
    ORA_table = scAgeCom_data$ORA_table,
    dataset_choice = input$TSA_DATASET_CHOICE,
    tissue_choice = input$TSA_TISSUE_CHOICE
  )
  scAgeCom_data$build_ORA_display(
    ORA_table = dt,
    category_choice = "LRIs",
    go_aspect_choice = NULL,
    type_choice = input$TSA_ORA_TYPE_CHOICE
  )
})

output$TSA_ORA_TABLE_LIGAND <- DT::renderDataTable({
  req(
    input$TSA_DATASET_CHOICE,
    input$TSA_TISSUE_CHOICE,
    input$TSA_ORA_CATEGORY_CHOICE,
    input$TSA_ORA_TYPE_CHOICE
  )
  dt <- scAgeCom_data$subset_ORA_table(
    ORA_table = scAgeCom_data$ORA_table,
    dataset_choice = input$TSA_DATASET_CHOICE,
    tissue_choice = input$TSA_TISSUE_CHOICE
  )
  scAgeCom_data$build_ORA_display(
    ORA_table = dt,
    category_choice = "Ligand Gene(s)",
    go_aspect_choice = NULL,
    type_choice = input$TSA_ORA_TYPE_CHOICE
  )
})

output$TSA_ORA_TABLE_RECEPTOR <- DT::renderDataTable({
  req(
    input$TSA_DATASET_CHOICE,
    input$TSA_TISSUE_CHOICE,
    input$TSA_ORA_CATEGORY_CHOICE,
    input$TSA_ORA_TYPE_CHOICE
  )
  dt <- scAgeCom_data$subset_ORA_table(
    ORA_table = scAgeCom_data$ORA_table,
    dataset_choice = input$TSA_DATASET_CHOICE,
    tissue_choice = input$TSA_TISSUE_CHOICE
  )
  scAgeCom_data$build_ORA_display(
    ORA_table = dt,
    category_choice = "Receptor Gene(s)",
    go_aspect_choice = NULL,
    type_choice = input$TSA_ORA_TYPE_CHOICE
  )
})

output$TSA_ORA_TABLE_GO <- DT::renderDataTable({
  req(
    input$TSA_DATASET_CHOICE,
    input$TSA_TISSUE_CHOICE,
    input$TSA_ORA_CATEGORY_CHOICE,
    input$TSA_ORA_TYPE_CHOICE,
    input$TSA_ORA_GO_ASPECT_CHOICE
  )
  dt <- scAgeCom_data$subset_ORA_table(
    ORA_table = scAgeCom_data$ORA_table,
    dataset_choice = input$TSA_DATASET_CHOICE,
    tissue_choice = input$TSA_TISSUE_CHOICE
  )
  scAgeCom_data$build_ORA_display(
    ORA_table = dt,
    category_choice = "GO Terms",
    go_aspect_choice = input$TSA_ORA_GO_ASPECT_CHOICE,
    type_choice = input$TSA_ORA_TYPE_CHOICE
  )
})

output$TSA_ORA_TABLE_KEGG <- DT::renderDataTable({
  req(
    input$TSA_DATASET_CHOICE,
    input$TSA_TISSUE_CHOICE,
    input$TSA_ORA_CATEGORY_CHOICE,
    input$TSA_ORA_TYPE_CHOICE
  )
  dt <- scAgeCom_data$subset_ORA_table(
    ORA_table = scAgeCom_data$ORA_table,
    dataset_choice = input$TSA_DATASET_CHOICE,
    tissue_choice = input$TSA_TISSUE_CHOICE
  )
  scAgeCom_data$build_ORA_display(
    ORA_table = dt,
    category_choice = "KEGG Pathways",
    go_aspect_choice = NULL,
    type_choice = input$TSA_ORA_TYPE_CHOICE
  )
})

output$TSA_ORA_TABLE_CELLFAMILY <- DT::renderDataTable({
  req(
    input$TSA_DATASET_CHOICE,
    input$TSA_TISSUE_CHOICE,
    input$TSA_ORA_CATEGORY_CHOICE,
    input$TSA_ORA_TYPE_CHOICE
  )
  dt <- scAgeCom_data$subset_ORA_table(
    ORA_table = scAgeCom_data$ORA_table,
    dataset_choice = input$TSA_DATASET_CHOICE,
    tissue_choice = input$TSA_TISSUE_CHOICE
  )
  scAgeCom_data$build_ORA_display(
    ORA_table = dt,
    category_choice = "Cell Families",
    go_aspect_choice = NULL,
    type_choice = input$TSA_ORA_TYPE_CHOICE
  )
})

output$TSA_ORA_PLOT_LRI <- renderPlot({
  req(
    input$TSA_DATASET_CHOICE,
    input$TSA_TISSUE_CHOICE,
    input$TSA_ORA_CATEGORY_CHOICE,
    input$TSA_ORA_TYPE_CHOICE
  )
  scAgeCom_data$build_ORA_plot(
    ORA_table = scAgeCom_data$ORA_table,
    tissue_choice = input$TSA_TISSUE_CHOICE,
    dataset_choice = input$TSA_DATASET_CHOICE,
    category_choice = "LRI",
    type_choice = input$TSA_ORA_TYPE_CHOICE
  )
})

output$TSA_ORA_PLOT_LIGAND <- renderPlot({
  req(
    input$TSA_DATASET_CHOICE,
    input$TSA_TISSUE_CHOICE,
    input$TSA_ORA_CATEGORY_CHOICE,
    input$TSA_ORA_TYPE_CHOICE
  )
  scAgeCom_data$build_ORA_plot(
    ORA_table = scAgeCom_data$ORA_table,
    tissue_choice = input$TSA_TISSUE_CHOICE,
    dataset_choice = input$TSA_DATASET_CHOICE,
    category_choice = "LIGAND_COMPLEX",
    type_choice = input$TSA_ORA_TYPE_CHOICE
  )
})

output$TSA_ORA_PLOT_RECEPTOR <- renderPlot({
  req(
    input$TSA_DATASET_CHOICE,
    input$TSA_TISSUE_CHOICE,
    input$TSA_ORA_CATEGORY_CHOICE,
    input$TSA_ORA_TYPE_CHOICE
  )
  scAgeCom_data$build_ORA_plot(
    ORA_table = scAgeCom_data$ORA_table,
    tissue_choice = input$TSA_TISSUE_CHOICE,
    dataset_choice = input$TSA_DATASET_CHOICE,
    category_choice = "RECEPTOR_COMPLEX",
    type_choice = input$TSA_ORA_TYPE_CHOICE
  )
})

output$TSA_ORA_PLOT_GO <- renderPlot({
  req(
    input$TSA_DATASET_CHOICE,
    input$TSA_TISSUE_CHOICE,
    input$TSA_ORA_CATEGORY_CHOICE,
    input$TSA_ORA_TYPE_CHOICE,
    input$TSA_ORA_GO_ASPECT_CHOICE
  )
  go_aspect <- ifelse(
    input$TSA_ORA_GO_ASPECT_CHOICE == "Biological Process",
    "biological_process",
    ifelse(
      input$TSA_ORA_GO_ASPECT_CHOICE == "Molecular Function",
      "molecular_function",
      "cellular_component"
    )
  )
  scAgeCom_data$build_ORA_plot(
    ORA_table = scAgeCom_data$ORA_table,
    tissue_choice = input$TSA_TISSUE_CHOICE,
    dataset_choice = input$TSA_DATASET_CHOICE,
    category_choice = "GO_TERMS",
    type_choice = input$TSA_ORA_TYPE_CHOICE,
    go_aspect_choice = go_aspect
  )
})

output$TSA_ORA_PLOT_KEGG <- renderPlot({
  req(
    input$TSA_DATASET_CHOICE,
    input$TSA_TISSUE_CHOICE,
    input$TSA_ORA_CATEGORY_CHOICE,
    input$TSA_ORA_TYPE_CHOICE
  )
  scAgeCom_data$build_ORA_plot(
    ORA_table = scAgeCom_data$ORA_table,
    tissue_choice = input$TSA_TISSUE_CHOICE,
    dataset_choice = input$TSA_DATASET_CHOICE,
    category_choice = "KEGG_PWS",
    type_choice = input$TSA_ORA_TYPE_CHOICE
  )
})

output$TSA_ORA_PLOT_CELLFAMILY <- renderPlot({
  req(
    input$TSA_DATASET_CHOICE,
    input$TSA_TISSUE_CHOICE,
    input$TSA_ORA_CATEGORY_CHOICE,
    input$TSA_ORA_TYPE_CHOICE
  )
  scAgeCom_data$build_ORA_plot(
    ORA_table = scAgeCom_data$ORA_table,
    tissue_choice = input$TSA_TISSUE_CHOICE,
    dataset_choice = input$TSA_DATASET_CHOICE,
    category_choice = "ER_CELLFAMILIES",
    type_choice = input$TSA_ORA_TYPE_CHOICE
  )
})

output$TSA_ORA_NETWORK_PLOT <- renderVisNetwork({
  req(
    input$TSA_DATASET_CHOICE,
    input$TSA_TISSUE_CHOICE
  )
  scAgeCom_data$build_ORA_visnetwork(
    CCI_table = scAgeCom_data$CCI_table,
    ORA_table = scAgeCom_data$ORA_table,
    tissue_choice = input$TSA_TISSUE_CHOICE,
    dataset_choice = input$TSA_DATASET_CHOICE,
    abbr_celltype = scAgeCom_data$ABBR_CELLTYPE
  )
})
