
output$TSA_TOP_VIEW <- renderUI({
  fluidRow(
    column(
      width = 6,
      uiOutput("TSA_TISSUE_CHOICE"),
      offset = 3
    ),
    column(
      width = 6,
      DT::dataTableOutput("TSA_OVERVIEW_TABLE"),
      style = "padding-bottom: 50px",
      offset = 3
    ),
    column(
      width = 6,
      uiOutput("TSA_DATASET_CHOICE"),
      offset = 3
    )
  )
})

output$TSA_TISSUE_CHOICE <- renderUI({
  choices <- scAgeCom_data$ALL_TISSUES
  titlePanel(
    tags$p(
      div(
        style = "display: inline-block;",
        "Please choose a tissue: "
      ),
      div(
        style = "display: inline-block;margin-top: 25px;",
        selectizeInput(
          inputId = "TSA_TISSUE_CHOICE",
          label = NULL,
          choices = choices,
          options = list(
            placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      )
    )
  )
})

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
  titlePanel(
    tags$p(
      div(
        style = "display: inline-block;",
        "Please choose a dataset: "
      ),
      div(
        style = "display: inline-block;margin-top: 25px;",
        selectizeInput(
          inputId = "TSA_DATASET_CHOICE",
          label = NULL,
          choices = sort(unique(dt$Dataset)),
          options = list(
            placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
      )
    )
  )
})

output$TSA_PANEL_VIEW <- renderUI({
  req(
    input$TSA_DATASET_CHOICE,
    input$TSA_TISSUE_CHOICE
  )
  tabsetPanel(
    type = "tabs",
    tabPanel(
      title = "Cell-Cell Interactions",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          downloadButton(
            "TSA_DOWNLOAD_TABLE",
            "Download Full CCI Table"
          ),
          hr(),
          h3("Filtering Options:"),
          uiOutput("TSA_EMITTER_CHOICE"),
          uiOutput("TSA_RECEIVER_CHOICE"),
          selectizeInput(
            inputId = "TSA_LRI_CHOICE",
            label = "Ligand-Receptor Interactions",
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
          width = 3,
          uiOutput("TSA_ORA_CATEGORY_CHOICE"),
          uiOutput("TSA_ORA_TYPE_CHOICE"),
          conditionalPanel(
            condition = "input.TSA_ORA_CATEGORY_CHOICE == 'GO Terms'",
            hr(),
            uiOutput("TSA_ORA_GO_ASPECT_CHOICE")
          )
        ),
        mainPanel(
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

filter_values <- reactiveValues(
  do_filtering = FALSE,
  emitter_choice = NULL,
  receiver_choice = NULL,
  LRI_choice = NULL
)

observeEvent(
  input$TSA_FILTER_BUTTON,
  {
    filter_values$do_filtering <- TRUE
    filter_values$emitter_choice <- input$TSA_EMITTER_CHOICE
    filter_values$receiver_choice <- input$TSA_RECEIVER_CHOICE
    filter_values$LRI_choice <- input$TSA_LRI_CHOICE
  }
)

observeEvent(
  input$TSA_RESET_BUTTON,
  {
    filter_values$do_filtering <- FALSE
    filter_values$emitter_choice <- NULL
    filter_values$receiver_choice <- NULL
    filter_values$LRI_choice <- NULL
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
  }
)

output$TSA_CCI_TITLE <- renderUI({
  req(
    input$TSA_DATASET_CHOICE,
    input$TSA_TISSUE_CHOICE
  )
  fluidRow(
    column(
      width = 12,
      titlePanel(
        tags$p(
          div(
            style = "display: inline-block;",
            paste0(
              "Results for the ",
              input$TSA_TISSUE_CHOICE,
              " from ",
              input$TSA_DATASET_CHOICE
            )
          )
        )
      ),
      style = "padding:50px"
    )
  )
})

CCI_table <- reactive({
  req(
    input$TSA_DATASET_CHOICE,
    input$TSA_TISSUE_CHOICE
  )
  celltype_choice <- sort(scAgeCom_data$ALL_CELLTYPES[
    Dataset == input$TSA_DATASET_CHOICE &
      Tissue == input$TSA_TISSUE_CHOICE
  ][["CELLTYPE"]])
  if (filter_values$do_filtering) {
    CCI_table <- scAgeCom_data$subset_cci_table(
      CCI_table = scAgeCom_data$CCI_table,
      dataset_choice = input$TSA_DATASET_CHOICE,
      tissue_choice = input$TSA_TISSUE_CHOICE,
      emitter_choice = filter_values$emitter_choice,
      receiver_choice = filter_values$receiver_choice,
      LRI_choice = filter_values$LRI_choice,
      filter = TRUE
    )
  } else {
    CCI_table <- scAgeCom_data$subset_cci_table(
      CCI_table = scAgeCom_data$CCI_table,
      dataset_choice = input$TSA_DATASET_CHOICE,
      tissue_choice = input$TSA_TISSUE_CHOICE,
      filter = FALSE
    )
  }
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
  fluidRow(
    column(
      width = 12,
      DTOutput(
        outputId = "TSA_CCI_DT"
      ),
      style = "padding:50px"
    ),
    column(
      width = 12,
      plotlyOutput(
        outputId = "TSA_PLOTLY_VOLCANO",
        height = "600px"
      ),
      style = "padding:50px"
    ),
    column(
      width = 12,
      plotlyOutput(
        outputId = "TSA_PLOTLY_SCORE",
        height = "600px"
      ),
      style = "padding:50px"
    ),
    column(
      width = 12,
      plotlyOutput(
        outputId = "TSA_PLOTLY_LRFC",
        height = "600px"
      ),
      style = "padding:50px"
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

output$TSA_ORA_CATEGORY_CHOICE <- renderUI({
  choices <- scAgeCom_data$ALL_ORA_CATEGORIES
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
  if (input$TSA_ORA_CATEGORY_CHOICE == "Cell Types") {
    fluidRow(
      column(
        width = 12,
        visNetworkOutput("TSA_ORA_NETWORK_PLOT", height = "800px"),
        style = "padding:50px"
      ),
      column(
        width = 12,
        dataTableOutput("TSA_ORA_TABLE"),
        style = "padding:50px"
      ),
      column(
        width = 12,
        #plotOutput("TSA_ORA_PLOT", height = "800px"),
        style = "padding:50px"
      )
    )
  } else {
    fluidRow(
      column(
        width = 12,
        dataTableOutput("TSA_ORA_TABLE"),
        style = "padding:50px"
      ),
      column(
        width = 12,
        #plotOutput("TSA_ORA_PLOT", height = "800px"),
        style = "padding:50px"
      )
    )
  }
})

output$TSA_ORA_TABLE <- DT::renderDataTable({
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
  if (input$TSA_ORA_CATEGORY_CHOICE == "GO Terms") {
    req(input$TSA_ORA_GO_ASPECT_CHOICE)
    DT <- scAgeCom_data$build_ORA_display(
      ORA_table = dt,
      category_choice = input$TSA_ORA_CATEGORY_CHOICE,
      go_aspect_choice = input$TSA_ORA_GO_ASPECT_CHOICE,
      type_choice = input$TSA_ORA_TYPE_CHOICE
    )
  } else {
    DT <- scAgeCom_data$build_ORA_display(
      ORA_table = dt,
      category_choice = input$TSA_ORA_CATEGORY_CHOICE,
      go_aspect_choice = NULL,
      type_choice = input$TSA_ORA_TYPE_CHOICE
    )
  }
  DT
})

output$TSA_ORA_NETWORK_PLOT <- renderVisNetwork({
  req(
    input$TSA_DATASET_CHOICE,
    input$TSA_TISSUE_CHOICE
  )
  #abbr <- scAgeCom_data$ABBR_CELLTYPE[[input$TSA_DATASET_CHOICE]]
  #abbr <- unique(abbr[ORIGINAL_CELLTYPE %in% obj@cci_table_detected[ID == input$TSA_TISSUE_CHOICE][["EMITTER_CELLTYPE"]]])
  scAgeCom_data$build_ORA_visnetwork(
    CCI_table = scAgeCom_data$CCI_table,
    ORA_table = scAgeCom_data$ORA_table,
    tissue_choice = input$TSA_TISSUE_CHOICE,
    dataset_choice = input$TSA_DATASET_CHOICE
  )
})

# output$TSA_ORA_PLOT <- renderPlot({
#   req(input$TSA_DATASET_CHOICE, input$TSA_TISSUE_CHOICE, input$TSA_ORA_CATEGORY_CHOICE, input$TSA_ORA_TYPE_CHOICE)
#   replacement <- data.table(
#     VALUE_OLD = c("KEGG_PWS", "GO_TERMS", "LRI", "ER_CELLTYPES", "ER_CELLFAMILIES"),
#     VALUE_NEW = c("KEGG Pathways", "GO Terms", "LRIs", "Cell Types", "Cell Families")
#   )
#   replacement <- replacement[VALUE_NEW == input$TSA_ORA_CATEGORY_CHOICE ][["VALUE_OLD"]]
#   if (input$TSA_ORA_TYPE_CHOICE == "Up") {
#     reg <- "UP"
#   } else if (input$TSA_ORA_TYPE_CHOICE == "Down") {
#     reg <- "DOWN"
#   } else if ( input$TSA_ORA_TYPE_CHOICE == "Flat") {
#     reg <- "FLAT"
#   }
#   obj <- scAgeCom_data$DATASETS_COMBINED[[input$TSA_DATASET_CHOICE]]
#   req(obj)
#   p <- PlotORA(
#     object = obj,
#     subID = input$TSA_TISSUE_CHOICE,
#     category = replacement,
#     regulation = reg,
#     max_terms_show = 20,
#     OR_threshold = 1,
#     p_value_threshold = 0.05,
#     stringent = FALSE
#   )
#   if (is.character(p)) {
#     return(p)
#   }
#   p <- p + ggtitle(
#     paste0(
#       "Top-20 ",
#       input$TSA_ORA_CATEGORY_CHOICE,
#       " over-represented among ",
#       input$TSA_ORA_TYPE_CHOICE,
#       "-regulated CCIs"
#     )
#   ) +
#     ylab("") +
#     theme(text=element_text(size=20)) +
#     theme(plot.title = element_text(hjust = 0.5, size = 16))
#   return(p)
# })
#


