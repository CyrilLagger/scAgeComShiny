#' tsa_cci UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tsa_cci_ui <- function(id){
  ns <- NS(id)
  tabPanel(
    title = "Table of Interactions",
    sidebarLayout(
      sidebarPanel(
        width = 2,
        downloadButton(
          ns("TSA_DOWNLOAD_TABLE"),
          "Download Full Table"
        ),
        hr(
          style = "border-top: 1px solid #000000;"
        ),
        h4("Filtering Options"),
        uiOutput(ns("TSA_EMITTER_CHOICE")),
        uiOutput(ns("TSA_RECEIVER_CHOICE")),
        selectizeInput(
          inputId = ns("TSA_LRI_CHOICE"),
          label = "Ligand-Receptor Interactions",
          choices = NULL,
          multiple = TRUE
        ),
        selectizeInput(
          inputId = ns("TSA_GENE_CHOICE"),
          label = "Individual Genes",
          choices = NULL,
          multiple = TRUE
        ),
        selectizeInput(
          inputId = ns("TSA_GO_CHOICE"),
          label = "GO Terms",
          choices = NULL,
          multiple = TRUE
        ),
        selectizeInput(
          inputId = ns("TSA_KEGG_CHOICE"),
          label = "KEGG Pathways",
          choices = NULL,
          multiple = TRUE
        ),
        actionButton(
          inputId = ns("TSA_FILTER_BUTTON"),
          label = "Filter"
        ),
        actionButton(
          inputId = ns("TSA_RESET_BUTTON"),
          label = "Undo Filtering"
        )
      ),
      mainPanel(
        width = 10,
        uiOutput(ns("TSA_CCI_TITLE")),
        uiOutput(ns("TSA_CCI_DETAILS"))
      )
    ),
    value = "TSA_INTERACTION_ANALYSIS"
  )
}

#' tsa_cci Server Functions
#'
#' @noRd 
mod_tsa_cci_server <- function(
  id,
  rv_tsa
) {
  moduleServer(
    id,
    function(
      input,
      output,
      session
    ) {
      ns <- session$ns
      
      output$TSA_DOWNLOAD_TABLE <- downloadHandler(
        filename = function() {
          paste0(
            "cci_table_",
            tolower(gsub(" ", "_", rv_tsa$dataset_choice, fixed = TRUE)),
            "_",
            tolower(gsub(" ", "_", rv_tsa$tissue_choice, fixed = TRUE)),
            ".csv"
          )
        },
        content = function(file) {
          CCI_table_downl <- subset_CCI_table(
            CCI_table = scAgeCom_data$CCI_table,
            dataset_choice = rv_tsa$dataset_choice,
            tissue_choice = rv_tsa$tissue_choice,
            filter = FALSE
          )
          fwrite(CCI_table_downl[, 1:12], file)
        }
      )
      
      output$TSA_EMITTER_CHOICE <- renderUI({
        #print("coucou6a")
        req(
          rv_tsa$tissue_choice,
          rv_tsa$dataset_choice
        )
        #print("coucou6b")
        #print(rv_tsa$dataset_choice)
        #print(rv_tsa$tissue_choice)
        choices_cts <- sort(scAgeCom_data$ALL_CELLTYPES[
          Dataset == rv_tsa$dataset_choice &
            Tissue == rv_tsa$tissue_choice
        ][["CELLTYPE"]])
        shinyWidgets::pickerInput(
          inputId = ns("TSA_EMITTER_CHOICE"),
          label = "Emitter Cell Types",
          choices = choices_cts,
          selected = choices_cts,
          options = list(`actions-box` = TRUE),
          multiple = TRUE
        )
      })
      
      output$TSA_RECEIVER_CHOICE <- renderUI({
        #print("coucou7a")
        req(
          rv_tsa$tissue_choice,
          rv_tsa$dataset_choice
        )
        #print("coucou7b")
        choices <- sort(scAgeCom_data$ALL_CELLTYPES[
          Dataset == rv_tsa$dataset_choice &
            Tissue == rv_tsa$tissue_choice
        ][["CELLTYPE"]])
        shinyWidgets::pickerInput(
          inputId = ns("TSA_RECEIVER_CHOICE"),
          label = "Receiver Cell Types",
          choices = choices,
          selected = choices,
          options = list(`actions-box` = TRUE),
          multiple = TRUE
        )
      })
      
      output$TSA_CCI_TITLE <- renderUI({
        #print("coucou8a")
        req(
          rv_tsa$dataset_choice,
          rv_tsa$tissue_choice
        )
        #print("coucou8b")
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
                      rv_tsa$tissue_choice
                    ),
                    " from ",
                    span(
                      style = "font-weight: bold",
                      rv_tsa$dataset_choice
                    )
                  )
                )
              )
            )
          )
        )
      })
      
      output$TSA_CCI_DETAILS <- renderUI({
        #print("coucou9a")
        req(
          rv_tsa$tissue_choice,
          rv_tsa$dataset_choice
        )
        #print("coucou9b")
        fluidPage(
          fluidRow(
            column(
              style = "padding: 10px;",
              width = 6,
              plotly::plotlyOutput(
                outputId = ns("TSA_PLOTLY_VOLCANO"),
                height = "520px"
              )
            ),
            column(
              style = "padding: 10px;",
              width = 6,
              plotly::plotlyOutput(
                outputId = ns("TSA_PLOTLY_SCORE"),
                height = "520px"
              )
            )
          ),
          fluidRow(
            column(
              style = "padding: 10px;",
              width = 6,
              offset = 3,
              plotly::plotlyOutput(
                outputId = ns("TSA_PLOTLY_LRFC"),
                height = "520px"
              )
            )
          ),
          fluidRow(
            column(
              width = 10,
              offset = 1,
              DT::DTOutput(
                outputId = ns("TSA_CCI_DT")
              )
            )
          )
        )
      })
      
      output$TSA_PLOTLY_VOLCANO <- plotly::renderPlotly({
        plot_volcano_CCI(CCI_table())
      })
      
      output$TSA_PLOTLY_SCORE <- plotly::renderPlotly({
        plot_scores_CCI(CCI_table())
      })
      
      output$TSA_PLOTLY_LRFC <- plotly::renderPlotly({
        plot_lrfc_CCI(CCI_table())
      })
      
      output$TSA_CCI_DT <- DT::renderDT({
        display_CCI_table(CCI_table())
      })
      
      CCI_table <- reactive({
        #print("coucouR1a")
        req(
          rv_tsa$dataset_choice,
          rv_tsa$tissue_choice
        )
        #print("coucouR1b")
        if (filter_values$do_filtering) {
          CCI_table <- subset_CCI_table(
            CCI_table = scAgeCom_data$CCI_table,
            dataset_choice = rv_tsa$dataset_choice,
            tissue_choice = rv_tsa$tissue_choice,
            emitter_choice = filter_values$emitter_choice,
            receiver_choice = filter_values$receiver_choice,
            LRI_choice = filter_values$LRI_choice,
            GENE_choice = filter_values$GENE_choice,
            GO_choice = filter_values$GO_choice,
            KEGG_choice = filter_values$KEGG_choice,
            GO_REF = scAgeCom_data$REFERENCE_GO_TERMS,
            KEGG_REF = scAgeCom_data$REFERENCE_KEGG_PWS,
            filter = TRUE
          )
        } else {
          CCI_table <- subset_CCI_table(
            CCI_table = scAgeCom_data$CCI_table,
            dataset_choice = rv_tsa$dataset_choice,
            tissue_choice = rv_tsa$tissue_choice,
            filter = FALSE
          )
        }
        CCI_table
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
        rv_tsa$dataset_choice,
        {
          #print("coucoux4a")
          req(
            rv_tsa$dataset_choice
          )
          #freezeReactiveValue(input, "TSA_DATASET_CHOICE")
          # updateTabsetPanel(
          #   session,
          #   "active_TSA_panel",
          #   selected = "Table of Interactions"
          # )
          #print("coucoux4b")
          #ora_reactives$category_choice <- NULL
          ALL_LRI_LABEL = 'All LRIs'
          choices_LRI <-
            c(
              ALL_LRI_LABEL,
              sort(
                scAgeCom_data$ALL_LRIs[
                  Dataset == rv_tsa$dataset_choice &
                    Tissue == rv_tsa$tissue_choice
                ][["LRI"]]
              )
            )
          updateSelectizeInput(
            session = session,
            "TSA_LRI_CHOICE",
            choices = choices_LRI,
            selected = ALL_LRI_LABEL,
            options = list(
              allowEmptyOption = TRUE,
              placeholder = 'Type LRIs',
              maxOptions = length(choices_LRI)
            ),
            server = TRUE
          )
          ALL_GENE_LABEL = 'All Genes'
          choices_genes <-
            c(
              ALL_GENE_LABEL,
              sort(
                scAgeCom_data$ALL_GENES[
                  Dataset == rv_tsa$dataset_choice &
                    Tissue == rv_tsa$tissue_choice
                ][["GENE"]]
              )
            )
          updateSelectizeInput(
            session = session,
            "TSA_GENE_CHOICE",
            choices = choices_genes,
            selected = ALL_GENE_LABEL,
            options = list(
              allowEmptyOption = TRUE,
              placeholder = 'Type Genes',
              maxOptions = length(choices_genes)
            ),
            server = TRUE
          )
          ALL_GO_LABEL = 'All GO Terms'
          choices_go <-
            c(
              ALL_GO_LABEL,
              sort(
                scAgeCom_data$ALL_GO_TERMS[
                  Dataset == rv_tsa$dataset_choice &
                    Tissue == rv_tsa$tissue_choice
                ][["GO_NAMES"]]
              )
            )
          updateSelectizeInput(
            session = session,
            "TSA_GO_CHOICE",
            choices = choices_go,
            selected = ALL_GO_LABEL,
            options = list(
              allowEmptyOption = TRUE,
              placeholder = 'Type GO Terms',
              maxOptions = length(choices_go)
            ),
            server = TRUE
          )
          ALL_KEGG_LABEL = 'All KEGG Pathways'
          choices_kegg <-
            c(
              ALL_KEGG_LABEL,
              sort(
                scAgeCom_data$ALL_KEGG_PWS[
                  Dataset == rv_tsa$dataset_choice &
                    Tissue == rv_tsa$tissue_choice
                ][["KEGG_NAMES"]]
              )
            )
          updateSelectizeInput(
            session = session,
            "TSA_KEGG_CHOICE",
            choices = choices_kegg,
            selected = ALL_KEGG_LABEL,
            options = list(
              allowEmptyOption = TRUE,
              placeholder = 'Type KEGG Pathways',
              maxOptions = length(choices_kegg)
            ),
            server = TRUE
          )
          #print("coucoux4c")
          #print(input$TSA_ORA_CATEGORY_CHOICE)
        }
      )
      
      observeEvent(
        input$TSA_FILTER_BUTTON,
        {
          #print("coucoux8")
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
          #print("coucoux9")
          filter_values$do_filtering <- FALSE
          filter_values$emitter_choice <- NULL
          filter_values$receiver_choice <- NULL
          filter_values$LRI_choice <- NULL
          filter_values$GENE_choice <- NULL
          filter_values$GO_choice <- NULL
          filter_values$KEGG_choice <- NULL
          choices_cts <- sort(scAgeCom_data$ALL_CELLTYPES[
            Dataset == rv_tsa$dataset_choice &
              Tissue == rv_tsa$tissue_choice
          ][["CELLTYPE"]])
          shinyWidgets::updatePickerInput(
            session = session,
            inputId = "TSA_EMITTER_CHOICE",
            choices = choices_cts,
            selected = choices_cts
          )
          shinyWidgets::updatePickerInput(
            session = session,
            inputId = "TSA_RECEIVER_CHOICE",
            choices = choices_cts,
            selected = choices_cts
          )
          ALL_LRI_LABEL = 'All LRIs'
          choices_lri <-
            c(
              ALL_LRI_LABEL,
              sort(
                scAgeCom_data$ALL_LRIs[
                  Dataset == rv_tsa$dataset_choice &
                    Tissue == rv_tsa$tissue_choice
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
              maxOptions = length(choices_lri)
            ),
            server = TRUE
          )
          ALL_GENE_LABEL = 'All Genes'
          choices_gene <-
            c(
              ALL_GENE_LABEL,
              sort(
                scAgeCom_data$ALL_GENES[
                  Dataset == rv_tsa$dataset_choice &
                    Tissue == rv_tsa$tissue_choice
                ][["GENE"]]
              )
            )
          updateSelectizeInput(
            session = session,
            "TSA_GENE_CHOICE",
            choices = choices_gene,
            selected = ALL_GENE_LABEL,
            options = list(
              allowEmptyOption = TRUE,
              placeholder = 'Type Genes',
              maxOptions = length(choices_gene)
            ),
            server = TRUE
          )
          ALL_GO_LABEL = 'All GO Terms'
          choices_go <-
            c(
              ALL_GO_LABEL,
              sort(
                scAgeCom_data$ALL_GO_TERMS[
                  Dataset == rv_tsa$dataset_choice &
                    Tissue == rv_tsa$tissue_choice
                ][["GO_NAMES"]]
              )
            )
          updateSelectizeInput(
            session = session,
            "TSA_GO_CHOICE",
            choices = choices_go,
            selected = ALL_GO_LABEL,
            options = list(
              allowEmptyOption = TRUE,
              placeholder = 'Type GO Terms',
              maxOptions = length(choices_go)
            ),
            server = TRUE
          )
          ALL_KEGG_LABEL = 'All KEGG Pathways'
          choices_kegg <-
            c(
              ALL_KEGG_LABEL,
              sort(
                scAgeCom_data$ALL_KEGG_PWS[
                  Dataset == rv_tsa$dataset_choice &
                    Tissue == rv_tsa$tissue_choice
                ][["KEGG_NAMES"]]
              )
            )
          updateSelectizeInput(
            session = session,
            "TSA_KEGG_CHOICE",
            choices = choices_kegg,
            selected = ALL_KEGG_LABEL,
            options = list(
              allowEmptyOption = TRUE,
              placeholder = 'Type KEGG Pathways',
              maxOptions = length(choices_kegg)
            ),
            server = TRUE
          )
        }
      )
      
    })
}

subset_CCI_table <- function(
  CCI_table,
  dataset_choice,
  tissue_choice,
  emitter_choice = NULL,
  receiver_choice = NULL,
  LRI_choice = NULL,
  GENE_choice = NULL,
  GO_choice = NULL,
  KEGG_choice = NULL,
  GO_REF = NULL,
  KEGG_REF = NULL,
  filter
) {
  dt <- CCI_table[
    Dataset == dataset_choice &
      Tissue == tissue_choice
  ]
  if (filter) {
    dt <- dt[
      `Emitter Cell Type` %in% emitter_choice &
        `Receiver Cell Type` %in% receiver_choice
    ]
    if (!("All Genes" %in% GENE_choice)) {
      dt <- dt[
        (LIGAND_1 %in% GENE_choice | LIGAND_2 %in% GENE_choice |
           RECEPTOR_1 %in% GENE_choice | RECEPTOR_2 %in% GENE_choice |
           RECEPTOR_3 %in% GENE_choice)
      ]
    }
    if (!('All LRIs' %in% LRI_choice)) {
      dt <- dt[
        `Ligand-Receptor Interaction` %in% LRI_choice
      ]
    }
    if (!('All GO Terms' %in% GO_choice)) {
      LRI_GO <- unique(GO_REF[GO_NAME %in% GO_choice]$LRI)
      dt <- dt[
        `Ligand-Receptor Interaction` %in% LRI_GO
      ]
      # choice_go <- paste0(paste0(";", GO_choice, ";"), collapse = "|")
      # dt <- dt[
      #   grepl(choice_go, GO_NAMES, fixed = FALSE)
      # ]
    }
    if (!('All KEGG Pathways' %in% KEGG_choice)) {
      LRI_KEGG <- unique(KEGG_REF[KEGG_NAME %in% KEGG_choice]$LRI)
      dt <- dt[
        `Ligand-Receptor Interaction` %in% LRI_KEGG
      ]
      # choice_kegg <- paste0(paste0(";", KEGG_choice, ";"), collapse = "|")
      # dt <- dt[
      #   grepl(choice_kegg, KEGG_NAMES, fixed = FALSE)
      # ]
    }
  }
  dt
}

display_CCI_table <- function(
  CCI_table
) {
  dt <- CCI_table[
    ,
    3:12
  ]
  CCI_DT <- DT::datatable(
    data = dt[, -c(9, 10)],
    class = "display compact",
    options =list(
      pageLength = 10,
      dom = '<"top"f>rt<"bottom"lip><"clear">'
    ),
    caption = tags$caption(
      style = paste0(
        'caption-side: top; text-align: center; ',
        'color:black; font-size:120% ;'
      ),
      "Table of Cell-Cell Interactions"
    ),
    rownames = rownames,
    extensions = c("Buttons")
  ) %>% DT::formatStyle(
      colnames(dt[, -c(9, 10)])[4:8],
      `text-align` = 'center'
    )
}

plot_volcano_CCI <- function(
  CCI_table
) {
  dt <- CCI_table[
    ,
    3:12
  ]
  vline <- function(x = 0, color = "black") {
    list(
      type = "line", 
      y0 = 0, 
      y1 = 1, 
      yref = "paper",
      x0 = x, 
      x1 = x, 
      line = list(color = color)
    )
  }
  hline <- function(y = 0, color = "black") {
    list(
      type = "line", 
      x0 = 0, 
      x1 = 1, 
      xref = "paper",
      y0 = y, 
      y1 = y, 
      line = list(color = color)
    )
  }
  dt$`Age Regulation` <- factor(
    dt$`Age Regulation`,
    levels = c("UP", "DOWN", "FLAT", "NSC")
  )
  m <- list(
    l = 10,
    r = 10,
    b = 10,
    t = 30,
    pad = 10
  )
  plotly::plot_ly(
    data = dt,
    type = "scatter",
    mode = "markers",
    x = ~`Log2 FC`,
    y = ~-log10(`Adj. p-value` + 1E-4),
    text = ~paste(
      "LRI: ",
      `Ligand-Receptor Interaction`, 
      '<br>Emitter:',
      `Emitter Cell Type`,
      '<br>Receiver:',
      `Receiver Cell Type`
    ),
    color = ~`Age Regulation`,
    colors = setNames(
      c("red", "blue", "green", "gray"),
      c("UP", "DOWN", "FLAT", "NSC")
    )
  ) %>% plotly::layout(
    title = list(
      text = "Interactive Aging Volcano Plot",
      font = list(size = 16),
      xanchor = "left",
      x = 0.0
    ),
    xaxis = list(
      title = list(
        text = "Log2(FC)",
        font = list(size = 14)
      )
    ),
    yaxis = list(
      title = list(
        text = "-Log10(Adj. p-value)",
        font = list(size = 14)
      )
    ),
    shapes = list(
      vline(log2(1.5)),
      vline(-log2(1.5)),
      hline(-log10(0.05))
    ),
    legend = list(
      orientation = "h",
      xanchor = "center",
      x = 0.5,
      y = 1.02
    ),
    margin = m
  ) %>% plotly::toWebGL()
}

plot_scores_CCI <- function(
  CCI_table
) {
  dt <- CCI_table[
    ,
    3:12
  ]
  dt$`Age Regulation` <- factor(
    dt$`Age Regulation`,
    levels = c("UP", "DOWN", "FLAT", "NSC")
  )
  min_score <-  10^(floor(
    log10(
      min(
        min(dt[`Young CCI Score` > 0]$`Young CCI Score`),
        min(dt[`Old CCI Score` > 0]$`Old CCI Score`)
      )
    )
  ))
  m <- list(
    l = 10,
    r = 10,
    b = 10,
    t = 30,
    pad = 10
  )
  plotly::plot_ly(
    data = dt,
    type = "scatter",
    mode = "markers",
    x = ~log10(`Young CCI Score` + min_score),
    y = ~log10(`Old CCI Score` + min_score),
    text = ~paste(
      "LRI: ",
      `Ligand-Receptor Interaction`, 
      '<br>Emitter:',
      `Emitter Cell Type`,
      '<br>Receiver:',
      `Receiver Cell Type`
    ),
    color = ~`Age Regulation`,
    colors = setNames(
      c("red", "blue", "green", "gray"),
      c("UP", "DOWN", "FLAT", "NSC")
    )
  ) %>% plotly::layout(
    title = list(
      text = "Interactive Score Plot",
      font = list(size = 16),
      xanchor = "left",
      x = 0.0
    ),
    xaxis = list(
      title = list(
        text = "Log10(Young CCI Score)",
        font = list(size = 14)
      )
    ),
    yaxis = list(
      title = list(
        text = "Log10(Old CCI Score)",
        font = list(size = 14)
      )
    ),
    legend = list(
      orientation = "h",
      xanchor = "center",
      x = 0.5,
      y = 1.02
    ),
    margin = m
  ) %>% plotly::toWebGL()
}

plot_lrfc_CCI <- function(
  CCI_table
) {
  dt <- CCI_table[
    ,
    3:12
  ]
  dt$`Age Regulation` <- factor(
    dt$`Age Regulation`,
    levels = c("UP", "DOWN", "FLAT", "NSC")
  )
  m <- list(
    l = 10,
    r = 10,
    b = 10,
    t = 30,
    pad = 10
  )
  plotly::plot_ly(
    data = dt,
    type = "scatter",
    mode = "markers",
    x = ~`Ligand Log2 FC`,
    y = ~`Receptor Log2 FC`,
    text = ~paste(
      "LRI: ",
      `Ligand-Receptor Interaction`, 
      '<br>Emitter:',
      `Emitter Cell Type`,
      '<br>Receiver:',
      `Receiver Cell Type`
    ),
    color = ~`Age Regulation`,
    colors = setNames(
      c("red", "blue", "green", "gray"),
      c("UP", "DOWN", "FLAT", "NSC")
    )
  )  %>% plotly::layout(
    title = list(
      text = "Interactive 'Ligand-FC vs Receptor-FC' Plot",
      font = list(size = 16),
      xanchor = "left",
      x = 0.0
    ),
    xaxis = list(
      title = list(
        text = "Ligand Log2(FC)",
        font = list(size = 14)
      )
    ),
    yaxis = list(
      title = list(
        text = "Receptor Log2(FC)",
        font = list(size = 14)
      )
    ),
    legend = list(
      orientation = "h",
      xanchor = "center",
      x = 0.5,
      y = 1.02
    ),
    margin = m
  ) %>% plotly::toWebGL()
}

