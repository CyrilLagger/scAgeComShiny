#' tsa_ora UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tsa_ora_ui <- function(id){
  ns <- NS(id)
  tabPanel(
    title = "Over-representation Analysis",
    sidebarLayout(
      sidebarPanel(
        width = 2,
        uiOutput(ns("TSA_ORA_CATEGORY_CHOICE")),
        hr(
          style = "border-top: 1px solid #000000;"
        ),
        uiOutput(ns("TSA_ORA_TYPE_CHOICE"))#,
        # conditionalPanel(
        #   condition = "input.TSA_ORA_CATEGORY_CHOICE != 'By Cell Types'",
        #   ns = ns,
        #   hr(
        #     style = "border-top: 1px solid #000000;"
        #   ),
        #   uiOutput(ns("TSA_ORA_TYPE_CHOICE"))
        # ),
        # conditionalPanel(
        #   condition = "input.TSA_ORA_CATEGORY_CHOICE == 'By GO/KEGG'",
        #   ns = ns,
        #   hr(
        #     style = "border-top: 1px solid #000000;"
        #   ),
        #   uiOutput(ns("TSA_ORA_GO_ASPECT_CHOICE"))
        # )
      ),
      mainPanel(
        width = 10,
        uiOutput(ns("TSA_ORA_TITLE")),
        uiOutput(ns("TSA_ORA_DETAILS"))
      )
    ),
    value = "TSA_ORA"
  )
}

#' tsa_ora Server Functions
#'
#' @noRd 
mod_tsa_ora_server <- function(
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
      
      output$TSA_ORA_CATEGORY_CHOICE <- renderUI({
        choices <- scAgeComShiny::scAgeCom_data$ALL_ORA_CATEGORIES_SPECIFIC
        selectInput(
          inputId = ns("TSA_ORA_CATEGORY_CHOICE"),
          label = "Category",
          choices = choices
        )
      })
      
      output$TSA_ORA_TYPE_CHOICE <- renderUI({
        choices <- scAgeComShiny::scAgeCom_data$ALL_ORA_TYPES
        selectInput(
          inputId = ns("TSA_ORA_TYPE_CHOICE"),
          label = "Age Regulation",
          choices = choices
        )
      })
      
      # output$TSA_ORA_GO_ASPECT_CHOICE <- renderUI({
      #   req(
      #     rv_tsa$tissue_choice,
      #     rv_tsa$dataset_choice
      #   )
      #   choices <- scAgeComShiny::scAgeCom_data$ALL_ORA_GO_ASPECTS
      #   selectInput(
      #     inputId = ns("TSA_ORA_GO_ASPECT_CHOICE"),
      #     label = "GO Aspect",
      #     choices = choices
      #   )
      # })
      
      output$TSA_ORA_TITLE <- renderUI({
        req(
          rv_tsa$dataset_choice#,
          #rv_tsa$tissue_choice
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
                      "text-align: center;",
                      "margin-bottom:50px"
                    ),
                    "Over-representation Results for the ",
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
      
      output$TSA_ORA_DETAILS <-  renderUI({
        req(
          #rv_tsa$tissue_choice,
          rv_tsa$dataset_choice,
          input$TSA_ORA_CATEGORY_CHOICE
        )
        if (input$TSA_ORA_CATEGORY_CHOICE == "By Cell Types") {
          fluidPage(
            fluidRow(
              column(
                width = 9,
                offset = 1,
                style = "margin-bottom: 50px;",
                visNetwork::visNetworkOutput(
                  ns("TSA_ORA_NETWORK_PLOT"),
                  height = "700px"
                )
              )
            ),
            fluidRow(
              column(
                style = "padding: 10px;",
                width = 8,
                offset = 2,
                DT::DTOutput(ns("TSA_ORA_TABLE_ERI"))
              )
            ),
            fluidRow(
              column(
                style = "padding: 10px;",
                width = 8,
                offset = 2,
                DT::DTOutput(ns("TSA_ORA_TABLE_EMITTER"))
              )
            ),
            fluidRow(
              column(
                style = "padding: 10px;",
                width = 8,
                offset = 2,
                DT::DTOutput(ns("TSA_ORA_TABLE_RECEIVER"))
              )
            )
          )
        } else if (input$TSA_ORA_CATEGORY_CHOICE == "By Genes") {
          fluidPage(
            fluidRow(
              style = "margin-bottom:50px;",
              column(
                width = 4,
                plotOutput(
                  ns("TSA_ORA_PLOT_LRI"),
                  height = "500px"
                )
              ),
              column(
                width = 4,
                plotOutput(
                  ns("TSA_ORA_PLOT_LIGAND"),
                  height = "500px"
                )
              ),
              column(
                width = 4,
                plotOutput(
                  ns("TSA_ORA_PLOT_RECEPTOR"),
                  height = "500px"
                )
              )
            ),
            fluidRow(
              column(
                style = "padding: 10px;",
                width = 8,
                offset = 2,
                DT::DTOutput(ns("TSA_ORA_TABLE_LRI"))
              )
            ),
            fluidRow(
              column(
                style = "padding: 10px;",
                width = 8,
                offset = 2,
                DT::DTOutput(ns("TSA_ORA_TABLE_LIGAND"))
              )
            ),
            fluidRow(
              column(
                style = "padding: 10px;",
                width = 8,
                offset = 2,
                DT::DTOutput(ns("TSA_ORA_TABLE_RECEPTOR"))
              )
            )
          )
        } else if (input$TSA_ORA_CATEGORY_CHOICE == "By GO/KEGG") {
          fluidPage(
            fluidRow(
              style = "margin-bottom:50px;",
              column(
                width = 6,
                plotly::plotlyOutput(
                  ns("TSA_ORA_TREEMAP_GO_BP"),
                  height = "700px"
                )
              ),
              column(
                width = 6,
                plotly::plotlyOutput(
                  ns("TSA_ORA_TREEMAP_GO_MF"),
                  height = "700px"
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                plotly::plotlyOutput(
                  ns("TSA_ORA_TREEMAP_GO_CC"),
                  height = "700px"
                )
              ),
              column(
                width = 6,
                plotOutput(
                  ns("TSA_ORA_PLOT_KEGG"),
                  height = "700px")
              )
            ),
            fluidRow(
              column(
                style = "padding: 10px;",
                width = 8,
                offset = 2,
                DT::DTOutput(
                  ns("TSA_ORA_TABLE_GO")
                )
              )
            ),
            fluidRow(
              column(
                style = "padding: 10px;",
                width = 8,
                offset = 2,
                DT::DTOutput(ns("TSA_ORA_TABLE_KEGG"))
              )
            )
          )
        }
      })
      
      output$TSA_ORA_NETWORK_PLOT <- visNetwork::renderVisNetwork({
        #print("hello_visnetwork_not_do")
        req(
          #rv_tsa$tissue_choice,
          rv_tsa$dataset_choice
        )
        #print(input$TSA_ORA_CATEGORY_CHOICE)
        #print("hello_visnetwork_do")
        plot_ORA_visnetwork(
          CCI_table = scAgeComShiny::scAgeCom_data$CCI_table,
          ORA_table = scAgeComShiny::scAgeCom_data$ORA_table,
          tissue_choice = rv_tsa$tissue_choice,
          dataset_choice = rv_tsa$dataset_choice,
          abbr_celltype = scAgeComShiny::scAgeCom_data$ABBR_CELLTYPE
        )
      })
      
      output$TSA_ORA_TABLE_ERI <- DT::renderDT({
        req(
          rv_tsa$dataset_choice,
          #rv_tsa$tissue_choice,
          input$TSA_ORA_CATEGORY_CHOICE,
          input$TSA_ORA_TYPE_CHOICE
        )
        dt <- subset_ORA_table(
          ORA_table = scAgeComShiny::scAgeCom_data$ORA_table,
          dataset_choice = rv_tsa$dataset_choice,
          tissue_choice = rv_tsa$tissue_choice
        )
        display_ORA_table(
          ORA_table = dt,
          category_choice = "Emitter-Receiver Cell Type pair",
          #go_aspect_choice = NULL,
          type_choice = input$TSA_ORA_TYPE_CHOICE
        )
      })
      
      output$TSA_ORA_TABLE_EMITTER <- DT::renderDT({
        req(
          rv_tsa$dataset_choice,
          #rv_tsa$tissue_choice,
          input$TSA_ORA_CATEGORY_CHOICE,
          input$TSA_ORA_TYPE_CHOICE
        )
        dt <- subset_ORA_table(
          ORA_table = scAgeComShiny::scAgeCom_data$ORA_table,
          dataset_choice = rv_tsa$dataset_choice,
          tissue_choice = rv_tsa$tissue_choice
        )
        display_ORA_table(
          ORA_table = dt,
          category_choice = "Emitter Cell Type",
          #go_aspect_choice = NULL,
          type_choice = input$TSA_ORA_TYPE_CHOICE
        )
      })
      
      output$TSA_ORA_TABLE_RECEIVER <- DT::renderDT({
        req(
          rv_tsa$dataset_choice,
          #rv_tsa$tissue_choice,
          input$TSA_ORA_CATEGORY_CHOICE,
          input$TSA_ORA_TYPE_CHOICE
        )
        dt <- subset_ORA_table(
          ORA_table = scAgeComShiny::scAgeCom_data$ORA_table,
          dataset_choice = rv_tsa$dataset_choice,
          tissue_choice = rv_tsa$tissue_choice
        )
        display_ORA_table(
          ORA_table = dt,
          category_choice = "Receiver Cell Type",
          #go_aspect_choice = NULL,
          type_choice = input$TSA_ORA_TYPE_CHOICE
        )
      })
      
      output$TSA_ORA_PLOT_LRI <- renderPlot({
        req(
          rv_tsa$dataset_choice,
          #rv_tsa$tissue_choice,
          input$TSA_ORA_CATEGORY_CHOICE,
          input$TSA_ORA_TYPE_CHOICE
        )
        plot_ORA_score(
          ORA_table = scAgeComShiny::scAgeCom_data$ORA_table,
          tissue_choice = rv_tsa$tissue_choice,
          dataset_choice = rv_tsa$dataset_choice,
          category_choice = "LRI",
          type_choice = input$TSA_ORA_TYPE_CHOICE
        )
      })
      
      output$TSA_ORA_PLOT_LIGAND <- renderPlot({
        req(
          rv_tsa$dataset_choice,
          #rv_tsa$tissue_choice,
          input$TSA_ORA_CATEGORY_CHOICE,
          input$TSA_ORA_TYPE_CHOICE
        )
        plot_ORA_score(
          ORA_table = scAgeComShiny::scAgeCom_data$ORA_table,
          tissue_choice = rv_tsa$tissue_choice,
          dataset_choice = rv_tsa$dataset_choice,
          category_choice = "LIGAND_COMPLEX",
          type_choice = input$TSA_ORA_TYPE_CHOICE
        )
      })
      
      output$TSA_ORA_PLOT_RECEPTOR <- renderPlot({
        req(
          rv_tsa$dataset_choice,
          #rv_tsa$tissue_choice,
          input$TSA_ORA_CATEGORY_CHOICE,
          input$TSA_ORA_TYPE_CHOICE
        )
        plot_ORA_score(
          ORA_table = scAgeComShiny::scAgeCom_data$ORA_table,
          tissue_choice = rv_tsa$tissue_choice,
          dataset_choice = rv_tsa$dataset_choice,
          category_choice = "RECEPTOR_COMPLEX",
          type_choice = input$TSA_ORA_TYPE_CHOICE
        )
      })
      
      output$TSA_ORA_TABLE_LRI <- DT::renderDT({
        req(
          rv_tsa$dataset_choice,
          #rv_tsa$tissue_choice,
          input$TSA_ORA_CATEGORY_CHOICE,
          input$TSA_ORA_TYPE_CHOICE
        )
        dt <- subset_ORA_table(
          ORA_table = scAgeComShiny::scAgeCom_data$ORA_table,
          dataset_choice = rv_tsa$dataset_choice,
          tissue_choice = rv_tsa$tissue_choice
        )
        display_ORA_table(
          ORA_table = dt,
          category_choice = "Ligand-Receptor Interaction",
          #go_aspect_choice = NULL,
          type_choice = input$TSA_ORA_TYPE_CHOICE
        )
      })
      
      output$TSA_ORA_TABLE_LIGAND <- DT::renderDT({
        req(
          rv_tsa$dataset_choice,
          #rv_tsa$tissue_choice,
          input$TSA_ORA_CATEGORY_CHOICE,
          input$TSA_ORA_TYPE_CHOICE
        )
        dt <- subset_ORA_table(
          ORA_table = scAgeComShiny::scAgeCom_data$ORA_table,
          dataset_choice = rv_tsa$dataset_choice,
          tissue_choice = rv_tsa$tissue_choice
        )
        display_ORA_table(
          ORA_table = dt,
          category_choice = "Ligand",
          #go_aspect_choice = NULL,
          type_choice = input$TSA_ORA_TYPE_CHOICE
        )
      })
      
      output$TSA_ORA_TABLE_RECEPTOR <- DT::renderDT({
        req(
          rv_tsa$dataset_choice,
          #rv_tsa$tissue_choice,
          input$TSA_ORA_CATEGORY_CHOICE,
          input$TSA_ORA_TYPE_CHOICE
        )
        dt <- subset_ORA_table(
          ORA_table = scAgeComShiny::scAgeCom_data$ORA_table,
          dataset_choice = rv_tsa$dataset_choice,
          tissue_choice = rv_tsa$tissue_choice
        )
        display_ORA_table(
          ORA_table = dt,
          category_choice = "Receptor",
          #go_aspect_choice = NULL,
          type_choice = input$TSA_ORA_TYPE_CHOICE
        )
      })
      
      output$TSA_ORA_TREEMAP_GO_BP <- plotly::renderPlotly({
        req(
          rv_tsa$dataset_choice,
          #rv_tsa$tissue_choice,
          input$TSA_ORA_CATEGORY_CHOICE,
          input$TSA_ORA_TYPE_CHOICE#,
          #input$TSA_ORA_GO_ASPECT_CHOICE
        )
        # go_aspect <- ifelse(
        #   input$TSA_ORA_GO_ASPECT_CHOICE == "Biological Process",
        #   "biological_process",
        #   ifelse(
        #     input$TSA_ORA_GO_ASPECT_CHOICE == "Molecular Function",
        #     "molecular_function",
        #     "cellular_component"
        #   )
        # )
        plot_ORA_GO_treemap(
          GO_REDUCED_table = scAgeComShiny::scAgeCom_data$GO_REDUCED_table,
          tissue_choice = rv_tsa$tissue_choice,
          dataset_choice = rv_tsa$dataset_choice,
          type_choice = input$TSA_ORA_TYPE_CHOICE,
          go_aspect_choice = "biological_process",
          title_text = paste0(
            "GO Biological Processes - ",
            input$TSA_ORA_TYPE_CHOICE
          )
        )
      })
      
      output$TSA_ORA_TREEMAP_GO_MF <- plotly::renderPlotly({
        req(
          rv_tsa$dataset_choice,
          #rv_tsa$tissue_choice,
          input$TSA_ORA_CATEGORY_CHOICE,
          input$TSA_ORA_TYPE_CHOICE#,
          #input$TSA_ORA_GO_ASPECT_CHOICE
        )
        # go_aspect <- ifelse(
        #   input$TSA_ORA_GO_ASPECT_CHOICE == "Biological Process",
        #   "biological_process",
        #   ifelse(
        #     input$TSA_ORA_GO_ASPECT_CHOICE == "Molecular Function",
        #     "molecular_function",
        #     "cellular_component"
        #   )
        # )
        plot_ORA_GO_treemap(
          GO_REDUCED_table = scAgeComShiny::scAgeCom_data$GO_REDUCED_table,
          tissue_choice = rv_tsa$tissue_choice,
          dataset_choice = rv_tsa$dataset_choice,
          type_choice = input$TSA_ORA_TYPE_CHOICE,
          go_aspect_choice = "molecular_function",
          title_text = paste0(
            "GO Molecular Functions - ",
            input$TSA_ORA_TYPE_CHOICE
          )
        )
      })
      
      output$TSA_ORA_TREEMAP_GO_CC <- plotly::renderPlotly({
        req(
          rv_tsa$dataset_choice,
          #rv_tsa$tissue_choice,
          input$TSA_ORA_CATEGORY_CHOICE,
          input$TSA_ORA_TYPE_CHOICE#,
          #input$TSA_ORA_GO_ASPECT_CHOICE
        )
        # go_aspect <- ifelse(
        #   input$TSA_ORA_GO_ASPECT_CHOICE == "Biological Process",
        #   "biological_process",
        #   ifelse(
        #     input$TSA_ORA_GO_ASPECT_CHOICE == "Molecular Function",
        #     "molecular_function",
        #     "cellular_component"
        #   )
        # )
        plot_ORA_GO_treemap(
          GO_REDUCED_table = scAgeComShiny::scAgeCom_data$GO_REDUCED_table,
          tissue_choice = rv_tsa$tissue_choice,
          dataset_choice = rv_tsa$dataset_choice,
          type_choice = input$TSA_ORA_TYPE_CHOICE,
          go_aspect_choice = "cellular_component",
          title_text = paste0(
            "GO Cellular Components - ",
            input$TSA_ORA_TYPE_CHOICE
          )
        )
      })
      
      # output$TSA_ORA_PLOT_GO <- renderPlot({
      #   req(
      #     rv_tsa$dataset_choice,
      #     rv_tsa$tissue_choice,
      #     input$TSA_ORA_CATEGORY_CHOICE,
      #     input$TSA_ORA_TYPE_CHOICE,
      #     input$TSA_ORA_GO_ASPECT_CHOICE
      #   )
      #   go_aspect <- ifelse(
      #     input$TSA_ORA_GO_ASPECT_CHOICE == "Biological Process",
      #     "biological_process",
      #     ifelse(
      #       input$TSA_ORA_GO_ASPECT_CHOICE == "Molecular Function",
      #       "molecular_function",
      #       "cellular_component"
      #     )
      #   )
      #   plot_ORA_score(
      #     ORA_table = scAgeComShiny::scAgeCom_data$ORA_table,
      #     tissue_choice = rv_tsa$tissue_choice,
      #     dataset_choice = rv_tsa$dataset_choice,
      #     category_choice = "GO_TERMS",
      #     type_choice = input$TSA_ORA_TYPE_CHOICE,
      #     go_aspect_choice = go_aspect
      #   )
      # })
      
      output$TSA_ORA_PLOT_KEGG <- renderPlot({
        req(
          rv_tsa$dataset_choice,
          #rv_tsa$tissue_choice,
          input$TSA_ORA_CATEGORY_CHOICE,
          input$TSA_ORA_TYPE_CHOICE
        )
        plot_ORA_score(
          ORA_table = scAgeComShiny::scAgeCom_data$ORA_table,
          tissue_choice = rv_tsa$tissue_choice,
          dataset_choice = rv_tsa$dataset_choice,
          category_choice = "KEGG_PWS",
          type_choice = input$TSA_ORA_TYPE_CHOICE
        )
      })
      
      output$TSA_ORA_TABLE_GO <- DT::renderDT({
        req(
          rv_tsa$dataset_choice,
          #rv_tsa$tissue_choice,
          input$TSA_ORA_CATEGORY_CHOICE,
          input$TSA_ORA_TYPE_CHOICE#,
          #input$TSA_ORA_GO_ASPECT_CHOICE
        )
        dt <- subset_ORA_table(
          ORA_table = scAgeComShiny::scAgeCom_data$ORA_table,
          dataset_choice = rv_tsa$dataset_choice,
          tissue_choice = rv_tsa$tissue_choice
        )
        display_ORA_table(
          ORA_table = dt,
          category_choice = "GO Term",
          #go_aspect_choice = input$TSA_ORA_GO_ASPECT_CHOICE,
          type_choice = input$TSA_ORA_TYPE_CHOICE
        )
      })
      
      output$TSA_ORA_TABLE_KEGG <- DT::renderDT({
        #print("helloKa")
        req(
          rv_tsa$dataset_choice,
          #rv_tsa$tissue_choice,
          input$TSA_ORA_CATEGORY_CHOICE,
          input$TSA_ORA_TYPE_CHOICE
        )
        #print("helloKb")
        dt <- subset_ORA_table(
          ORA_table = scAgeComShiny::scAgeCom_data$ORA_table,
          dataset_choice = rv_tsa$dataset_choice,
          tissue_choice = rv_tsa$tissue_choice
        )
        #print(dt)
        display_ORA_table(
          ORA_table = dt,
          category_choice = "KEGG Pathway",
          #go_aspect_choice = NULL,
          type_choice = input$TSA_ORA_TYPE_CHOICE
        )
      })
      
    })
}

plot_ORA_visnetwork <- function(
  CCI_table,
  ORA_table,
  tissue_choice,
  dataset_choice,
  abbr_celltype
) {
  Dataset <- Tissue <- ORA_CATEGORY <- ORIGINAL_CELLTYPE <-
    EMITTER_CELLTYPE <- RECEIVER_CELLTYPE <- i.ABBR_CELLTYPE <- NULL
  CCI_dt <- copy(CCI_table)
  data.table::setnames(
    CCI_dt,
    old = c("Emitter Cell Type", "Receiver Cell Type", "Age Regulation"),
    new = c("EMITTER_CELLTYPE", "RECEIVER_CELLTYPE", "REGULATION")
  )
  ora_table_ER <- ORA_table[
    Dataset == dataset_choice &
      Tissue == tissue_choice &
      ORA_CATEGORY == "ER_CELLTYPES"
  ]
  ora_table_EMITTER <- ORA_table[
    Dataset == dataset_choice &
      Tissue == tissue_choice &
      ORA_CATEGORY == "EMITTER_CELLTYPE"
  ]
  ora_table_RECEIVER <- ORA_table[
    Dataset == dataset_choice &
      Tissue == tissue_choice &
      ORA_CATEGORY == "RECEIVER_CELLTYPE"
  ]
  cci_table_detected <- CCI_dt[
    Dataset == dataset_choice &
      Tissue == tissue_choice
  ]
  actual_celltypes <- union(
    cci_table_detected[["EMITTER_CELLTYPE"]],
    cci_table_detected[["RECEIVER_CELLTYPE"]]
  )
  abbreviation_table <- abbr_celltype[[dataset_choice]][
    ORIGINAL_CELLTYPE %in% actual_celltypes
  ]
  if (!identical(
    sort(actual_celltypes),
    sort(abbreviation_table[["ORIGINAL_CELLTYPE"]])
  )) {
    stop(
      paste0(
        "No abbreviation will be used:",
        " `abbreviation table` must contain",
        " a column with the original cell-types")
    )
  } else if (sum(duplicated(abbreviation_table)) > 0) {
    stop(
      paste0(
        "No abbreviation will be used:",
        " `abbreviation table` must not contain duplicated rows"))
  } else {
    cci_table_detected[
      ,
      "EMITTER_CELLTYPE_ORIGINAL" := EMITTER_CELLTYPE
    ]
    cci_table_detected[
      ,
      "RECEIVER_CELLTYPE_ORIGINAL" := RECEIVER_CELLTYPE
    ]
    cci_table_detected[
      abbreviation_table,
      on = "EMITTER_CELLTYPE==ORIGINAL_CELLTYPE",
      "EMITTER_CELLTYPE" := i.ABBR_CELLTYPE]
    cci_table_detected[
      abbreviation_table,
      on = "RECEIVER_CELLTYPE==ORIGINAL_CELLTYPE",
      "RECEIVER_CELLTYPE" := i.ABBR_CELLTYPE]
    ora_table_ER[
      abbreviation_table,
      on = "EMITTER_CELLTYPE==ORIGINAL_CELLTYPE",
      "EMITTER_CELLTYPE" := i.ABBR_CELLTYPE]
    ora_table_ER[
      abbreviation_table,
      on = "RECEIVER_CELLTYPE==ORIGINAL_CELLTYPE",
      "RECEIVER_CELLTYPE" := i.ABBR_CELLTYPE]
    ora_table_EMITTER[
      abbreviation_table,
      on = "VALUE==ORIGINAL_CELLTYPE",
      "VALUE" := i.ABBR_CELLTYPE]
    ora_table_RECEIVER[
      abbreviation_table,
      on = "VALUE==ORIGINAL_CELLTYPE",
      "VALUE" := i.ABBR_CELLTYPE]
  }
  scDiffCom:::interactive_from_igraph(
    cci_table_detected = cci_table_detected,
    conds = c("YOUNG", "OLD"),
    ora_table_ER = ora_table_ER,
    ora_table_EMITTER = ora_table_EMITTER,
    ora_table_RECEIVER = ora_table_RECEIVER,
    ora_table_LR = ORA_table[
      Dataset == dataset_choice &
        Tissue == tissue_choice &
        ORA_CATEGORY == "LRI"
    ],
    network_type = "ORA_network",
    layout_type = "bipartite",
    object_name = tissue_choice
  )
}

plot_ORA_score <- function(
  ORA_table,
  tissue_choice,
  dataset_choice,
  category_choice,
  type_choice,
  go_aspect_choice = "biological_process"
) {
  Dataset <- Tissue <- ORA_CATEGORY <- NULL
  dt <- ORA_table[
    Dataset == dataset_choice &
      Tissue == tissue_choice &
      ORA_CATEGORY == category_choice
  ]
  data.table::setnames(
    dt,
    "GO Level",
    "LEVEL"
  )
  p <- scDiffCom:::plot_ora(
    ora_dt = dt,
    category = category_choice,
    regulation = type_choice,
    max_terms_show = 20,
    GO_aspect = go_aspect_choice,
    OR_threshold = 1,
    bh_p_value_threshold = 0.05
  )
  if (category_choice %in% c("GO_TERMS", "KEGG_PWS")) {
    p <- p +
      ggplot2::scale_y_discrete(
        label = function(x) {
          y <- gsub("\n", " ", x)
          y <- ifelse(
            nchar(y) <= 50,
            y,
            paste(substr(y, 1, 46), "...")
          )
          stringr::str_wrap(y, 30)
        }
      )
  }
  p
}

plot_ORA_GO_treemap <- function(
  GO_REDUCED_table,
  tissue_choice,
  dataset_choice,
  type_choice,
  go_aspect_choice,
  title_text,
  domain = NULL
) {
  Dataset <- Tissue <- ASPECT <- REGULATION <- 
    new_parent <- term <- parentTerm <- ids <- 
    parents <- score <- text <- NULL
  ex_data <- GO_REDUCED_table[
    Dataset == dataset_choice &
      Tissue == tissue_choice &
      ASPECT == go_aspect_choice &
      REGULATION == type_choice
  ][, c("score", "term", "parentTerm")]
  if (nrow(ex_data) == 0) return(NULL)
  ex_data[, new_parent := ifelse(
    term %in% parentTerm,
    "",
    parentTerm
  )]
  new_data <- data.table(
    labels = c(ex_data$term, ex_data[new_parent == ""]$term),
    parents = c(ex_data$parentTerm, rep("", length(ex_data[new_parent == ""]$term)))
  )
  new_data[
    ,
    ids := sapply(
      1:nrow(.SD),
      function(i) {
        if (labels[[i]] == parents[[i]]) {
          res <- paste(labels[[i]], parents[[i]], sep = " - ")
        } else {
          res <- labels[[i]]
        }
        res
      }
    )
  ]
  new_data[
    ,
    score := sapply(
      1:nrow(.SD),
      function(i) {
        if (parents[[i]] == "") {
          res <- sum(ex_data[parentTerm == labels[[i]]]$score)
        } else {
          res <- ex_data[term == labels[[i]]]$score
        }
        res
      }
    )
  ]
  new_data[
    ,
    text := gsub(" ", "\n", labels)
  ]
  m <- list(
    l = 5,
    r = 5,
    b = 5,
    t = 30,
    pad = 0
  )
  plotly::plot_ly(
    new_data,
    type = "treemap",
    opacity = 1,
    ids = ~ids,
    parents = ~parents,
    values = ~score,
    labels = ~labels,
    text = ~text,
    textposition = "middle center",
    branchvalues = "total",
    hoverinfo = "label+value",
    marker = list(
      line = list(color = "black")
    ),
    textinfo = "text",
    domain = domain
  ) %>% plotly::layout(
    title = list(
      text = title_text,
      font = list(size = 16),
      xanchor = "left",
      x = 0.0
    ),
    #uniformtext = list(
    #  minsize = 14,
    #  mode = "hide"
    #),
    margin = m
  )
}

subset_ORA_table <- function(
  ORA_table,
  dataset_choice,
  tissue_choice
) {
  Dataset <- Tissue <- ORA_CATEGORY <- i.category_new <- ASPECT <-
    i.new_aspect <- NULL
  dt <- ORA_table[
    Dataset == dataset_choice &
      Tissue == tissue_choice
  ]
  category_keep <- c(
    "LRI",
    "LIGAND_COMPLEX",
    "RECEPTOR_COMPLEX",
    "ER_CELLTYPES",
    "EMITTER_CELLTYPE",
    "RECEIVER_CELLTYPE",
    "GO_TERMS",
    "KEGG_PWS"#,
    #"ER_CELLFAMILIES",
    #"EMITTER_CELLFAMILY",
    #"RECEIVER_CELLFAMILY"
  )
  dt <- dt[ORA_CATEGORY %in% category_keep]
  dt[data.table(
    category_old = category_keep,
    category_new = c(
      "Ligand-Receptor Interaction",
      "Ligand",
      "Receptor",
      "Emitter-Receiver Cell Type pair",
      "Emitter Cell Type",
      "Receiver Cell Type",
      "GO Term",
      "KEGG Pathway"#,
      #"ER Cell Families",
      #"Emitter Cell Families",
      #"Receiver Cell Families"
    )
  ),
  on = c("ORA_CATEGORY==category_old"),
  ORA_CATEGORY := i.category_new
  ]
  dt[
    data.table(
      old_aspect = c(
        "biological_process",
        "cellular_component",
        "molecular_function"
      ),
      new_aspect = c(
        "Biological Process",
        "Cellular Component",
        "Molecular Function"
      )
    ),
    on = "ASPECT==old_aspect",
    ASPECT := i.new_aspect
  ]
  dt
}

display_ORA_table <- function(
  ORA_table,
  category_choice,
  #go_aspect_choice,
  type_choice
) {
  ORA_CATEGORY <- ASPECT <- OR_UP <- BH_P_VALUE_UP <-
    OR_DOWN <- BH_P_VALUE_DOWN <- OR_FLAT <- BH_P_VALUE_FLAT <-
    `GO Level` <- `ORA Score` <- NULL
  #print("C1")
  dt <- ORA_table[ORA_CATEGORY == category_choice]
  if (category_choice == "GO Term") {
    #dt <- dt[ASPECT == go_aspect_choice]
    level_str <- c("GO Level", "ASPECT")
    filter <- "top"
    category_label <- "GO Terms"
    # if (go_aspect_choice == "Biological Process") {
    #   category_label <- paste0("GO ", go_aspect_choice, "es")
    # } else {
    #   category_label <- paste0("GO ", go_aspect_choice, "s")
    # }
  } else {
    level_str <- NULL
    filter <- "top"
    category_label <- paste0(category_choice, "s")
  }
  if(type_choice == "UP") {
    cols_to_keep <- c(
      "VALUE",
      "ORA_SCORE_UP",
      "OR_UP",
      "BH_P_VALUE_UP",
      level_str
    )
    dt <- dt[
      OR_UP >= 1 & BH_P_VALUE_UP <= 0.05,
      cols_to_keep,
      with = FALSE
    ]
  } else if(type_choice == "DOWN") {
    cols_to_keep <- c(
      "VALUE",
      "ORA_SCORE_DOWN",
      "OR_DOWN",
      "BH_P_VALUE_DOWN",
      level_str
    )
    dt <- dt[
      `OR_DOWN` >= 1 & BH_P_VALUE_DOWN <= 0.05,
      cols_to_keep,
      with = FALSE
    ]
  } else if(type_choice == "FLAT") {
    cols_to_keep <- c(
      "VALUE",
      "ORA_SCORE_FLAT",
      "OR_FLAT",
      "BH_P_VALUE_FLAT",
      level_str
    )
    dt <- dt[
      `OR_FLAT` >= 1 & BH_P_VALUE_FLAT <= 0.05,
      cols_to_keep,
      with = FALSE]
  } 
  if (category_choice == "GO Term") {
    dt[, `GO Level` := as.factor(`GO Level`)]
    dt[, ASPECT := as.factor(ASPECT)]
  }
  #print("C2")
  data.table::setnames(
    dt,
    old = colnames(dt),
    new = c(
      category_choice,
      "ORA Score",
      "Odds Ratio",
      "Adj. p-value",
      level_str
    )
  )
  data.table::setorder(dt, -`ORA Score`)
  #print("C3")
  #print(dt)
  DT::datatable(
    data = dt,
    options = list(
      columnDefs = list(
        list(width = '300px', targets = c(0))
      ),
      dom = '<"top"f>rt<"bottom"lip><"clear">'
    ),
    caption = tags$caption(
      style = paste0(
        "caption-side: top; ",
        "text-align: center; ",
        "color: black; ",
        "font-size: 120%;"
      ),
      paste0(
        category_label,
        " over-represented among ",
        type_choice,
        "-regulated cell-cell interactions"
      )
    ),
    rownames = FALSE,
    filter = filter,
    class = "display compact"
  ) %>% DT::formatStyle(
    colnames(dt)[-1],
    `text-align` = 'center'
  )
}
