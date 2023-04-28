#' tca UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tca_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        style = "text-align:center;",
        width = 6,
        titlePanel(htmlOutput(ns("TCA_TITLE"))),
        offset = 3
      ),
    ),
    # fluidRow(
    #   column(
    #     style = "text-align:center;",
    #     width = 6,
    #     titlePanel(htmlOutput(ns("TCA_NOTE"))),
    #     offset = 3
    #   ),
    # ),
    tabsetPanel(
      type = "tabs",
      tabPanel(
        title = "Tissue-count Tables",
        sidebarLayout(
          sidebarPanel(
            width = 2,
            uiOutput(ns("TCA_GLOBAL_TABLE_CHOICE")),
            uiOutput(ns("TCA_GLOBAL_ORA_REGULATION_CHOICE")),
            conditionalPanel(
              condition = "input.TCA_GLOBAL_TABLE_CHOICE == 'By GO/KEGG'",
              ns = ns,
              hr(
                style = "border-top: 1px solid #000000;"
              ),
              uiOutput(ns("TCA_ORA_GO_ASPECT_CHOICE"))
            )
          ),
          mainPanel(
            width = 10,
            uiOutput(ns("TCA_GLOBAL_DETAILS"))
          )
        ),
        value = "TCA_SUMMARY_TABLE"
      ),
      tabPanel(
        title = "Keyword over-representation across datasets",
        sidebarLayout(
          sidebarPanel(
            width = 2,
            uiOutput(ns("TCA_KEYWORD_CATEGORY_CHOICE")),
            uiOutput(ns("TCA_KEYWORD_VALUE_CHOICE"))
          ),
          mainPanel(
            width = 10,
            fluidRow(
              column(
                width = 10,
                offset = 1,
                plotly::plotlyOutput(
                  ns("TCA_KEYWORD_SUMMARY_PLOT"),
                  height = "600px"
                )
              )
            )
          )
        ),
        value = "TCA_KEYWORD_SUMMARY"
      ),
      # tabPanel(
      #   title = "Cell type families",
      #   sidebarLayout(
      #     sidebarPanel(
      #       width = 2,
      #       selectInput(
      #         inputId = "TCA_ERI_FAMILY_REGULATION_CHOICE",
      #         label = "Regulation",
      #         choices = c("UP", "DOWN", "FLAT")
      #       ),
      #       sliderInput(
      #         inputId = "TCA_ERI_FAMILY_NUM_TISS_THRESHOLD",
      #         label = "Minimum number of tissues",
      #         min = 0, max = 15, value = 0
      #       )
      #     ),
      #     mainPanel(
      #       width = 10,
      #       fluidRow(
      #         column(
      #           width = 12,
      #           visNetwork::visNetworkOutput(
      #             "TCA_ERI_FAMILY_NETWORK",
      #             height='600px'
      #           ),
      #           style = "padding:10px"
      #         )
      #       )
      #     )
      #   )
      # ),
      id = "active_TCA_panel"
    )
  )
}

#' tca Server Functions
#'
#' @noRd 
mod_tca_server <- function(id){
  moduleServer(
    id,
    function(
      input, 
      output,
      session
    ){
      ns <- session$ns
      
      output$TCA_TITLE <- renderUI({
          tags$div(
            style = paste(
              #"display: inline-block;",
              #"text-align: center;",
              "font-size: 26px"
              ),
            "Over-represented signals shared accross Tissues and Datasets"
          )
      })
      
      # output$TCA_NOTE <- renderUI({
      #   tags$div(
      #       style = paste(
      #         "font-size: 20px"
      #       ),
      #       "Note: we recommend reading relevant sections of the Glossary",
      #       "before interpreting these results."
      #   )
      # })
      
      output$TCA_GLOBAL_TABLE_CHOICE <- renderUI({
        selectInput(
          inputId = ns("TCA_GLOBAL_TABLE_CHOICE"),
          label = "Category",
          choices = scAgeComShiny::scAgeCom_data$ALL_ORA_CATEGORIES_GLOBAL
        )
      })
      
      output$TCA_GLOBAL_ORA_REGULATION_CHOICE <- renderUI({
        selectInput(
          inputId = ns("TCA_GLOBAL_ORA_REGULATION_CHOICE"),
          label = "Age Regulation",
          choices = scAgeComShiny::scAgeCom_data$ALL_ORA_TYPES
        )
      })
      
      output$TCA_ORA_GO_ASPECT_CHOICE <- renderUI({
        choices <- scAgeComShiny::scAgeCom_data$ALL_ORA_GO_ASPECTS
        selectInput(
          inputId = ns("TCA_ORA_GO_ASPECT_CHOICE"),
          label = "GO Aspect",
          choices = choices
        )
      })
      
      output$TCA_GLOBAL_DETAILS <- renderUI({
        req(input$TCA_GLOBAL_TABLE_CHOICE)
        if (input$TCA_GLOBAL_TABLE_CHOICE == "By Genes") {
          fluidPage(
            fluidRow(
              column(
                width = 12,
                #offset = 2,
                DT::DTOutput(ns("TCA_GLOBAL_TABLE_LRI")),
                style = "padding:10px"
              )
            ),
            fluidRow(
              column(
                width = 12,
                #offset = 2,
                DT::DTOutput(ns("TCA_GLOBAL_TABLE_LIGAND")),
                style = "padding:10px"
              )
            ),
            fluidRow(
              column(
                width = 12,
                #offset = 2,
                DT::DTOutput(ns("TCA_GLOBAL_TABLE_RECEPTOR")),
                style = "padding:10px"
              )
            )
          )
        } else if (input$TCA_GLOBAL_TABLE_CHOICE == "By GO/KEGG") {
          fluidPage(
            fluidRow(
              column(
                width = 12,
                #offset = 2,
                DT::DTOutput(ns("TCA_GLOBAL_TABLE_GO")),
                style = "padding:10px"
              )
            ),
            fluidRow(
              column(
                width = 12,
                #offset = 2,
                DT::DTOutput(ns("TCA_GLOBAL_TABLE_KEGG")),
                style = "padding:10px"
              )
            )
          )
        } else {
          fluidPage(
            fluidRow(
              column(
                width = 12,
                #offset = 2,
                DT::DTOutput(ns("TCA_GLOBAL_TABLE_ER_CELLFAMILY")),
                style = "padding:10px"
              )
            ),
            fluidRow(
              column(
                width = 12,
                #offset = 2,
                DT::DTOutput(ns("TCA_GLOBAL_TABLE_EMITTER_CELLFAMILY")),
                style = "padding:10px"
              )
            ),
            fluidRow(
              column(
                width = 12,
                #offset = 2,
                DT::DTOutput(ns("TCA_GLOBAL_TABLE_RECEIVER_CELLFAMILY")),
                style = "padding:10px"
              )
            )
          )
        }
      })
      
      output$TCA_GLOBAL_TABLE_LRI <- DT::renderDT({
        req(
          input$TCA_GLOBAL_TABLE_CHOICE,
          input$TCA_GLOBAL_ORA_REGULATION_CHOICE
        )
        display_KEYWORD_counts(
          ora_keyword_counts = scAgeComShiny::scAgeCom_data$ORA_KEYWORD_COUNTS,
          category = "Ligand-Receptor Interaction",
          regulation = input$TCA_GLOBAL_ORA_REGULATION_CHOICE
        )
      })
      
      output$TCA_GLOBAL_TABLE_LIGAND <- DT::renderDT({
        req(
          input$TCA_GLOBAL_TABLE_CHOICE,
          input$TCA_GLOBAL_ORA_REGULATION_CHOICE
        )
        display_KEYWORD_counts(
          ora_keyword_counts = scAgeComShiny::scAgeCom_data$ORA_KEYWORD_COUNTS,
          category = "Ligand",
          regulation = input$TCA_GLOBAL_ORA_REGULATION_CHOICE
        )
      })
      
      output$TCA_GLOBAL_TABLE_RECEPTOR <- DT::renderDT({
        req(
          input$TCA_GLOBAL_TABLE_CHOICE,
          input$TCA_GLOBAL_ORA_REGULATION_CHOICE
        )
        display_KEYWORD_counts(
          ora_keyword_counts = scAgeComShiny::scAgeCom_data$ORA_KEYWORD_COUNTS,
          category = "Receptor",
          regulation = input$TCA_GLOBAL_ORA_REGULATION_CHOICE
        )
      })
      
      output$TCA_GLOBAL_TABLE_GO <- DT::renderDT({
        req(
          input$TCA_GLOBAL_TABLE_CHOICE,
          input$TCA_GLOBAL_ORA_REGULATION_CHOICE,
          input$TCA_ORA_GO_ASPECT_CHOICE
        )
        display_KEYWORD_counts(
          ora_keyword_counts = scAgeComShiny::scAgeCom_data$ORA_KEYWORD_COUNTS,
          category = "GO Term",
          regulation = input$TCA_GLOBAL_ORA_REGULATION_CHOICE,
          go_aspect = input$TCA_ORA_GO_ASPECT_CHOICE
        )
      })
      
      output$TCA_GLOBAL_TABLE_KEGG <- DT::renderDT({
        req(
          input$TCA_GLOBAL_TABLE_CHOICE,
          input$TCA_GLOBAL_ORA_REGULATION_CHOICE
        )
        display_KEYWORD_counts(
          ora_keyword_counts = scAgeComShiny::scAgeCom_data$ORA_KEYWORD_COUNTS,
          category = "KEGG Pathway",
          regulation = input$TCA_GLOBAL_ORA_REGULATION_CHOICE
        )
      })
      
      output$TCA_GLOBAL_TABLE_ER_CELLFAMILY <- DT::renderDT({
        req(
          input$TCA_GLOBAL_TABLE_CHOICE,
          input$TCA_GLOBAL_ORA_REGULATION_CHOICE
        )
        display_KEYWORD_counts(
          ora_keyword_counts = scAgeComShiny::scAgeCom_data$ORA_KEYWORD_COUNTS,
          category = "Emitter-Receiver Cell Type Family",
          regulation = input$TCA_GLOBAL_ORA_REGULATION_CHOICE
        )
      })
      
      output$TCA_GLOBAL_TABLE_EMITTER_CELLFAMILY <- DT::renderDT({
        req(
          input$TCA_GLOBAL_TABLE_CHOICE,
          input$TCA_GLOBAL_ORA_REGULATION_CHOICE
        )
        display_KEYWORD_counts(
          ora_keyword_counts = scAgeComShiny::scAgeCom_data$ORA_KEYWORD_COUNTS,
          category = "Emitter Cell Type Family",
          regulation = input$TCA_GLOBAL_ORA_REGULATION_CHOICE
        )
      })
      
      output$TCA_GLOBAL_TABLE_RECEIVER_CELLFAMILY <- DT::renderDT({
        req(
          input$TCA_GLOBAL_TABLE_CHOICE,
          input$TCA_GLOBAL_ORA_REGULATION_CHOICE
        )
        display_KEYWORD_counts(
          ora_keyword_counts = scAgeComShiny::scAgeCom_data$ORA_KEYWORD_COUNTS,
          category = "Receiver Cell Type Family",
          regulation = input$TCA_GLOBAL_ORA_REGULATION_CHOICE
        )
      })
      
      output$TCA_KEYWORD_CATEGORY_CHOICE <- renderUI({
        selectInput(
          inputId = ns("TCA_KEYWORD_CATEGORY_CHOICE"),
          label = "Category",
          choices = scAgeComShiny::scAgeCom_data$ALL_ORA_CATEGORIES_KEYWORD
        )
      })
      
      output$TCA_KEYWORD_VALUE_CHOICE <- renderUI({
        selectizeInput(
          inputId = ns("TCA_KEYWORD_VALUE_CHOICE"),
          label = "Choose a term of interest",
          choices = NULL,
          multiple = FALSE
        )
      })
      
      output$TCA_KEYWORD_SUMMARY_PLOT <- plotly::renderPlotly({
        req(
          input$TCA_KEYWORD_CATEGORY_CHOICE,
          input$TCA_KEYWORD_VALUE_CHOICE
        )
        plot_KEYWORD_summary(
          ora_keyword_summary = scAgeComShiny::scAgeCom_data$ORA_KEYWORD_SUMMARY,
          ora_keyword_template = scAgeComShiny::scAgeCom_data$ORA_KEYWORD_TEMPLATE,
          category = input$TCA_KEYWORD_CATEGORY_CHOICE,
          keyword = input$TCA_KEYWORD_VALUE_CHOICE
        )
      })
      
      # output$TCA_ERI_FAMILY_NETWORK <- visNetwork::renderVisNetwork({
      #   req(
      #     input$TCA_ERI_FAMILY_REGULATION_CHOICE,
      #     input$TCA_ERI_FAMILY_NUM_TISS_THRESHOLD
      #   )
      #   REGULATION_CHOICE = input$TCA_ERI_FAMILY_REGULATION_CHOICE
      #   NUM_TISSUE_THRESHOLD = input$TCA_ERI_FAMILY_NUM_TISS_THRESHOLD
      #   graph_config = scDiffCom:::setup_graph_config()
      #   edge_color = ifelse(
      #     REGULATION_CHOICE == 'UP',
      #     graph_config$EDGE_COLORING$ORA_COLOR_UP,
      #     ifelse(
      #       REGULATION_CHOICE == 'DOWN',
      #       graph_config$EDGE_COLORING$ORA_COLOR_DOWN,
      #       graph_config$EDGE_COLORING$ORA_COLOR_FLAT
      #     )
      #   )
      #   edges = copy(scAgeComShiny::scAgeCom_data$ORA_KEYWORD_COUNTS)[
      #     ORA_CATEGORY == 'ER Cell Families' &
      #       ORA_REGULATION == REGULATION_CHOICE &
      #       as.integer(gsub("/23", "", `Overall (Union)`)) >= NUM_TISSUE_THRESHOLD,
      #     c('VALUE', 'Overall (Union)')
      #   ]
      #   edges[, "from" := sapply(strsplit(edges[, VALUE], "_"), function(v) v[1])]
      #   edges[, "to" := sapply(strsplit(edges[, VALUE], "_"), function(v) v[2])]
      #   edges[, "value" := list(as.integer(gsub("/23", "", `Overall (Union)`)))]
      #   edges[, "label" := value]
      #   edges[, "color" := edge_color]
      #   nodes_set = union(edges[, from], edges[, to])
      #   nodes = data.table::data.table(id = nodes_set, label = nodes_set)
      #   nodes[
      #     ,
      #     c("color.background", "color.border",
      #       "color.highlight.background", "color.highlight.border",
      #       "color.hover.background", "color.hover.border",
      #       "shadow"
      #     ) := list(
      #       graph_config$NODE_COLORING$BACKGROUND,
      #       graph_config$NODE_COLORING$BORDER,
      #       graph_config$NODE_COLORING$HIGHLIGHT$BACKGROUND,
      #       graph_config$NODE_COLORING$HIGHLIGHT$BORDER,
      #       graph_config$NODE_COLORING$HOVER$BACKGROUND,
      #       graph_config$NODE_COLORING$HOVER$BORDER,
      #       TRUE
      #     )
      #   ]
      #   visNetwork::visNetwork(
      #     nodes = nodes,
      #     edges = edges,
      #     # width = 100,
      #     # height = 100,
      #     main = "Cell families",
      #     # submain = sprintf("%s", object_name),
      #     # footer = sprintf("Network type: %s", layout_type),
      #     # background = config$VISNETWORK$BACKGROUND
      #   ) %>% visNetwork::visNodes(
      #     shape = "dot",
      #     physics = FALSE,
      #     font = list(size = 18, align = "left")
      #   ) %>% visNetwork::visEdges(
      #     shadow = TRUE,
      #     arrows = "middle",
      #     smooth = list(enabled = TRUE, roundness = 0.75)
      #   )
      # })
      
      observeEvent(
        input$TCA_KEYWORD_CATEGORY_CHOICE,
        {
          req(input$TCA_KEYWORD_CATEGORY_CHOICE)
          ORA_CATEGORY <- NULL
          freezeReactiveValue(input, "TCA_KEYWORD_VALUE_CHOICE")
          updateSelectInput(
            session = session,
            'TCA_KEYWORD_CATEGORY_CHOICE',
            selected = input$TCA_KEYWORD_CATEGORY_CHOICE
          )
          choices <- sort(unique(scAgeComShiny::scAgeCom_data$ORA_KEYWORD_COUNTS[
            ORA_CATEGORY == input$TCA_KEYWORD_CATEGORY_CHOICE
          ]$VALUE))
          #print(head(choices))
          updateSelectizeInput(
            session = session,
            "TCA_KEYWORD_VALUE_CHOICE",
            choices = choices,
            options = list(
              maxOptions = length(choices)
            ),
            server = TRUE
          )
        },
        ignoreNULL = FALSE,
        ignoreInit = FALSE
      )
      
      # Separate handling of JS triggered event
      observeEvent(
        input$TCA_KEYWORD_VALUE_CHOICE_JS_TRIGGERED,
        {
          updateSelectizeInput(
            session = session,
            "TCA_KEYWORD_VALUE_CHOICE",
            selected = input$TCA_KEYWORD_VALUE_CHOICE_JS_TRIGGERED,
          )
        }
      )

      ## TCA CELL FAMILIES Tab ##
      # get_TCA_eri_family_network <- function(
      #   input
      # ) {
      #   visNetwork::renderVisNetwork({
      #     req(input$TCA_ERI_FAMILY_REGULATION_CHOICE, input$TCA_ERI_FAMILY_NUM_TISS_THRESHOLD)
      #     REGULATION_CHOICE = input$TCA_ERI_FAMILY_REGULATION_CHOICE
      #     NUM_TISSUE_THRESHOLD = input$TCA_ERI_FAMILY_NUM_TISS_THRESHOLD
      #
      #     graph_config = scDiffCom:::setup_graph_config()
      #     edge_color = ifelse(
      #       REGULATION_CHOICE == 'UP',
      #       graph_config$EDGE_COLORING$ORA_COLOR_UP,
      #       graph_config$EDGE_COLORING$ORA_COLOR_DOWN
      #     )
      #
      #     ORA_CATEGORY <- REGULATION <- VALUE <- from <- to <- NULL
      #     edges = scAgeComShiny::scAgeCom_data$ORA_KEYWORD_COUNTS[
      #       ORA_CATEGORY == 'ERI Family' &
      #         REGULATION == REGULATION_CHOICE &
      #         `Overall (Union)` >= NUM_TISSUE_THRESHOLD,
      #       c('VALUE', 'Overall (Union)')
      #     ]
      #     edges[, "from" := sapply(strsplit(edges[, VALUE], "_"), function(v) v[1])]
      #     edges[, "to" := sapply(strsplit(edges[, VALUE], "_"), function(v) v[2])]
      #     edges[, "value" := list(`Overall (Union)`)]
      #     edges[, "label" := value]
      #     edges[, "color" := edge_color]
      #
      #     nodes_set = union(edges[, from], edges[, to])
      #     nodes = data.table::data.table(id = nodes_set, label = nodes_set)
      #     nodes[
      #       ,
      #       c("color.background", "color.border",
      #         "color.highlight.background", "color.highlight.border",
      #         "color.hover.background", "color.hover.border",
      #         "shadow"
      #       ) := list(
      #         graph_config$NODE_COLORING$BACKGROUND,
      #         graph_config$NODE_COLORING$BORDER,
      #         graph_config$NODE_COLORING$HIGHLIGHT$BACKGROUND,
      #         graph_config$NODE_COLORING$HIGHLIGHT$BORDER,
      #         graph_config$NODE_COLORING$HOVER$BACKGROUND,
      #         graph_config$NODE_COLORING$HOVER$BORDER,
      #         TRUE
      #       )
      #     ]
      #
      #     visNetwork::visNetwork(
      #       nodes = nodes,
      #       edges = edges,
      #       # width = 100,
      #       # height = 100,
      #       main = "Cell families",
      #       # submain = sprintf("%s", object_name),
      #       # footer = sprintf("Network type: %s", layout_type),
      #       # background = config$VISNETWORK$BACKGROUND
      #     ) %>% visNetwork::visNodes(
      #       shape = "dot",
      #       physics = FALSE,
      #       font = list(size = 18, align = "left")
      #     ) %>% visNetwork::visEdges(
      #       shadow = TRUE,
      #       arrows = "middle",
      #       smooth = list(enabled = TRUE, roundness = 0.75)
      #     )
      #   })
      # }
      
    })
}

display_KEYWORD_counts <- function(
  ora_keyword_counts,
  category,
  regulation,
  go_aspect = NULL
) {
  ORA_CATEGORY <- ORA_REGULATION <- ASPECT <- NULL
  dt <- ora_keyword_counts[
    ORA_CATEGORY == category &
      ORA_REGULATION == regulation
  ]
  data.table::setnames(dt, old = "VALUE", new = category)
  if (category == "GO Term") {
    temp_aspect <- ifelse(
      go_aspect == "Biological Process",
      "biological_process",
      ifelse(
        go_aspect == "Molecular Function",
        "molecular_function",
        "cellular_component"
      )
    )
    dt <- dt[ASPECT == temp_aspect]
    dt <- dt[, -c(1,2,10)]
    if (go_aspect == "Biological Process") {
      category_label <- paste0("GO ", go_aspect, "es")
    } else {
      category_label <- paste0("GO ", go_aspect, "s")
    }
  } else {
    dt <- dt[, -c(1,2,10,11)]
    if(grepl("Family", category)){
      category_label <- sub("Family", "Families", category)
    } else {
      category_label <- paste0(category, "s")
    }
  }
  DT <- DT::datatable(
    data = dt,
    filter = list(
      position ="top",
      clear = FALSE,
      plain = FALSE
    ),
    class = "display compact",
    options =list(
      pageLength = 10,
      dom = '<"top"f>rt<"bottom"lip><"clear">',
      columnDefs = list(
        list(width = '300px', targets = c(1))
      )
    ),
    caption = tags$caption(
      style = paste0(
        "caption-side: top; ",
        "text-align: center; ",
        "color: black; ",
        "font-size: 120%;"
      ),
      paste0(
        "Number of tissues in which ",
        category_label,
        " are over-represented among ",
        regulation,
        "-regulated cell-cell interactions"
      )
    )
  ) %>% DT::formatStyle(
    colnames(dt)[-1],
    `text-align` = 'center'
  )
  if (category == "GO Term") {
    DT <- DT %>% DT::formatStyle(c(7), `border-right` = "solid 2px")
  }
  DT
}

plot_KEYWORD_summary <- function(
  ora_keyword_summary,
  ora_keyword_template,
  category,
  keyword
) {
  ORA_CATEGORY <- VALUE <- Regulation <- i.ORA_REGULATION <-
    Dataset <- Tissue <- NULL
  dt <- copy(ora_keyword_summary)[
    ORA_CATEGORY == category
  ]
  if (!(keyword %in% dt$VALUE)) {
    stop("`keyword` not found")
  }
  dt <- dt[
    VALUE == keyword
  ]
  dt <- copy(ora_keyword_template)[
    dt,
    on = c("Tissue", "Dataset"),
    Regulation := i.ORA_REGULATION
  ]
  #dt$Dataset <- gsub(" ", "\n", dt$Dataset)
  dt[is.na(dt)] <- "Not Detected"
  p <- ggplot2::ggplot(dt) +
    ggplot2::geom_tile(
      ggplot2::aes(
        Dataset,
        Tissue,
        fill = Regulation,
        width = 0.9,
        height = 0.9
      ),
      colour = "black"
    ) +
    ggplot2::scale_fill_manual(
      name = NULL,
      values = c(
        "No Data" = "transparent",
        "Not Over-represented" = "white",
        "Not Detected" = "gray",
        "UP" = "red",
        "DOWN" = "blue",
        "FLAT" = "black",
        "UP:DOWN" = "yellow"
      )
    ) +
    ggplot2::ggtitle(
      stringr::str_trunc(
        paste0(
          "Over-representation of ",
          keyword
        ),
        70, 
        "right"
      )
    ) +
    ggplot2::scale_x_discrete(
      limits = c(
        "TMS FACS (male)",
        "TMS FACS (female)" ,
        "TMS Droplet (male)",
        "TMS Droplet (female)",
        "Calico Droplet (male)"
      ),
      labels = c(
        "TMS\nFACS\n(male)",
        "TMS\nFACS\n(female)",
        "TMS\nDroplet\n(male)",
        "TMS\nDroplet\n(female)",
        "Calico\nDroplet\n(male)"
      ),
      guide = ggplot2::guide_axis(n.dodge = 2)
    ) +
    ggplot2::scale_y_discrete(
      limits = sort(
        unique(dt$Tissue),
        decreasing = TRUE
      )
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    #theme(plot.title = element_text(hjust = 0.5)) +
    ggplot2::theme(text=ggplot2::element_text(size = 10)) +
    ggplot2::theme(axis.text=ggplot2::element_text(size = 10))# +
    #ggplot2::theme(legend.position = c(0.8, 0.8))
  plotly::ggplotly(
    p,
    source = "TCA_PLOT_KEYWORD_SUMMARY",
    tooltip = c("Dataset", "Tissue", "Regulation")
  ) #%>% plotly::layout(
  #  legend = list(
  #    title = list(text = "")
  #  ) 
  # )
}
