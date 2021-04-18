output$TCA_TOP_VIEW <- renderUI({
  fluidRow(
    column(
      width = 6,
      titlePanel(htmlOutput("TCA_TITLE")),
      offset = 3
    ),
  )
})

output$TCA_TITLE <- renderUI({
  tags$p(
    div(
      style = "display: inline-block; text-align: center; font-size: 24px",
      "Over-represented Signals shared accross Tissues and Datasets"
    )
  )
})

output$TCA_PANEL_VIEW <- renderUI({
  tabsetPanel(
    type = "tabs",
    tabPanel(
      title = "Table of Counts",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          uiOutput("TCA_GLOBAL_TABLE_CHOICE"),
          uiOutput("TCA_GLOBAL_ORA_REGULATION_CHOICE"),
          conditionalPanel(
            condition = "input.TCA_GLOBAL_TABLE_CHOICE == 'By GO/KEGG'",
            hr(),
            uiOutput("TCA_ORA_GO_ASPECT_CHOICE")
          )
        ),
        mainPanel(
          width = 10,
          uiOutput("TCA_GLOBAL_DETAILS")
        )
      ),
      value = "TCA_SUMMARY_TABLE"
    ),
    tabPanel(
      title = "Tissue Summary by Keywords",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          uiOutput("TCA_KEYWORD_CATEGORY_CHOICE"),
          uiOutput("TCA_KEYWORD_VALUE_CHOICE")
        ),
        mainPanel(
          width = 10,
          fluidRow(
            column(
              width = 12,
              plotly::plotlyOutput(
                "TCA_KEYWORD_SUMMARY_PLOT",
                height = "800px"
              ),
              style = "padding:10px"
            )
          )
        )
      ),
      value = "TCA_KEYWORD_SUMMARY"
    ),
    tabPanel(
      title = 'Celltype families',
      sidebarLayout(
        sidebarPanel(
          width = 2,
          selectInput(
            inputId = "TCA_ERI_FAMILY_REGULATION_CHOICE",
            label = "Regulation",
            choices = c("UP", "DOWN", "FLAT")
          ),
          sliderInput(
            inputId = "TCA_ERI_FAMILY_NUM_TISS_THRESHOLD",
            label = "Minimum number of tissues",
            min = 0, max = 15, value = 0
          )
        ),
        mainPanel(
          width = 10,
          fluidRow(
            column(
              width = 12,
              visNetwork::visNetworkOutput(
                "TCA_ERI_FAMILY_NETWORK",
                height='600px'
              ),
              style = "padding:10px"
            )
          )
        )
      )
    ),
    id = "active_TCA_panel"
  )
})

output$TCA_GLOBAL_TABLE_CHOICE <- renderUI({
  selectInput(
    inputId = "TCA_GLOBAL_TABLE_CHOICE",
    label = "Category",
    choices = scAgeCom_data$ALL_ORA_CATEGORIES_GLOBAL
  )
})

output$TCA_GLOBAL_ORA_REGULATION_CHOICE <- renderUI({
  selectInput(
    inputId = "TCA_GLOBAL_ORA_REGULATION_CHOICE",
    label = "Age Regulation",
    choices = scAgeCom_data$ALL_ORA_TYPES
  )
})

output$TCA_ORA_GO_ASPECT_CHOICE <- renderUI({
  choices <- scAgeCom_data$ALL_ORA_GO_ASPECTS
  selectInput(
    inputId = "TCA_ORA_GO_ASPECT_CHOICE",
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
          DT::dataTableOutput("TCA_GLOBAL_TABLE_LRI")
        )
      ),
      fluidRow(
        column(
          width = 12,
          #offset = 2,
          DT::dataTableOutput("TCA_GLOBAL_TABLE_LIGAND")
        )
      ),
      fluidRow(
        column(
          width = 12,
          #offset = 2,
          DT::dataTableOutput("TCA_GLOBAL_TABLE_RECEPTOR")
        )
      )
    )
  } else if (input$TCA_GLOBAL_TABLE_CHOICE == "By GO/KEGG") {
    fluidPage(
      fluidRow(
        column(
          width = 12,
          #offset = 2,
          DT::dataTableOutput("TCA_GLOBAL_TABLE_GO"),
          style = "padding:10px"
        )
      ),
      fluidRow(
        column(
          width = 12,
          #offset = 2,
          DT::dataTableOutput("TCA_GLOBAL_TABLE_KEGG"),
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
          DT::dataTableOutput("TCA_GLOBAL_TABLE_ER_CELLFAMILY"),
          style = "padding:10px"
        )
      ),
      fluidRow(
        column(
          width = 12,
          #offset = 2,
          DT::dataTableOutput("TCA_GLOBAL_TABLE_EMITTER_CELLFAMILY"),
          style = "padding:10px"
        )
      ),
      fluidRow(
        column(
          width = 12,
          #offset = 2,
          DT::dataTableOutput("TCA_GLOBAL_TABLE_RECEIVER_CELLFAMILY"),
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
  scAgeCom_data$build_KEYWORD_COUNTS_display(
    ora_keyword_counts = scAgeCom_data$ORA_KEYWORD_COUNTS,
    category = "LRIs",
    regulation = input$TCA_GLOBAL_ORA_REGULATION_CHOICE
  )
})

output$TCA_GLOBAL_TABLE_LIGAND <- DT::renderDT({
  req(
    input$TCA_GLOBAL_TABLE_CHOICE,
    input$TCA_GLOBAL_ORA_REGULATION_CHOICE
  )
  scAgeCom_data$build_KEYWORD_COUNTS_display(
    ora_keyword_counts = scAgeCom_data$ORA_KEYWORD_COUNTS,
    category = "Ligand Gene(s)",
    regulation = input$TCA_GLOBAL_ORA_REGULATION_CHOICE
  )
})

output$TCA_GLOBAL_TABLE_RECEPTOR <- DT::renderDT({
  req(
    input$TCA_GLOBAL_TABLE_CHOICE,
    input$TCA_GLOBAL_ORA_REGULATION_CHOICE
  )
  scAgeCom_data$build_KEYWORD_COUNTS_display(
    ora_keyword_counts = scAgeCom_data$ORA_KEYWORD_COUNTS,
    category = "Receptor Gene(s)",
    regulation = input$TCA_GLOBAL_ORA_REGULATION_CHOICE
  )
})

output$TCA_GLOBAL_TABLE_GO <- DT::renderDT({
  req(
    input$TCA_GLOBAL_TABLE_CHOICE,
    input$TCA_GLOBAL_ORA_REGULATION_CHOICE,
    input$TCA_ORA_GO_ASPECT_CHOICE
  )
  scAgeCom_data$build_KEYWORD_COUNTS_display(
    ora_keyword_counts = scAgeCom_data$ORA_KEYWORD_COUNTS,
    category = "GO Terms",
    regulation = input$TCA_GLOBAL_ORA_REGULATION_CHOICE,
    go_aspect = input$TCA_ORA_GO_ASPECT_CHOICE
  )
})

output$TCA_GLOBAL_TABLE_KEGG <- DT::renderDT({
  req(
    input$TCA_GLOBAL_TABLE_CHOICE,
    input$TCA_GLOBAL_ORA_REGULATION_CHOICE
  )
  scAgeCom_data$build_KEYWORD_COUNTS_display(
    ora_keyword_counts = scAgeCom_data$ORA_KEYWORD_COUNTS,
    category = "KEGG Pathways",
    regulation = input$TCA_GLOBAL_ORA_REGULATION_CHOICE
  )
})

output$TCA_GLOBAL_TABLE_ER_CELLFAMILY <- DT::renderDT({
  req(
    input$TCA_GLOBAL_TABLE_CHOICE,
    input$TCA_GLOBAL_ORA_REGULATION_CHOICE
  )
  scAgeCom_data$build_KEYWORD_COUNTS_display(
    ora_keyword_counts = scAgeCom_data$ORA_KEYWORD_COUNTS,
    category = "ER Cell Families",
    regulation = input$TCA_GLOBAL_ORA_REGULATION_CHOICE
  )
})

output$TCA_GLOBAL_TABLE_EMITTER_CELLFAMILY <- DT::renderDT({
  req(
    input$TCA_GLOBAL_TABLE_CHOICE,
    input$TCA_GLOBAL_ORA_REGULATION_CHOICE
  )
  scAgeCom_data$build_KEYWORD_COUNTS_display(
    ora_keyword_counts = scAgeCom_data$ORA_KEYWORD_COUNTS,
    category = "Emitter Cell Families",
    regulation = input$TCA_GLOBAL_ORA_REGULATION_CHOICE
  )
})

output$TCA_GLOBAL_TABLE_RECEIVER_CELLFAMILY <- DT::renderDT({
  req(
    input$TCA_GLOBAL_TABLE_CHOICE,
    input$TCA_GLOBAL_ORA_REGULATION_CHOICE
  )
  scAgeCom_data$build_KEYWORD_COUNTS_display(
    ora_keyword_counts = scAgeCom_data$ORA_KEYWORD_COUNTS,
    category = "Receiver Cell Families",
    regulation = input$TCA_GLOBAL_ORA_REGULATION_CHOICE
  )
})

output$TCA_KEYWORD_CATEGORY_CHOICE <- renderUI({
  selectInput(
    inputId = "TCA_KEYWORD_CATEGORY_CHOICE",
    label = "Category",
    choices = scAgeCom_data$ALL_ORA_CATEGORIES_KEYWORD
  )
})

output$TCA_KEYWORD_VALUE_CHOICE <- renderUI({
  selectizeInput(
    inputId = "TCA_KEYWORD_VALUE_CHOICE",
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
  scAgeCom_data$plot_KEYWORD_SUMMARY(
    ora_keyword_summary = scAgeCom_data$ORA_KEYWORD_SUMMARY,
    ora_keyword_template = scAgeCom_data$ORA_KEYWORD_TEMPLATE,
    category = input$TCA_KEYWORD_CATEGORY_CHOICE,
    keyword = input$TCA_KEYWORD_VALUE_CHOICE
  )
})

output$TCA_ERI_FAMILY_NETWORK <- visNetwork::renderVisNetwork({
  req(
    input$TCA_ERI_FAMILY_REGULATION_CHOICE,
    input$TCA_ERI_FAMILY_NUM_TISS_THRESHOLD
  )
  REGULATION_CHOICE = input$TCA_ERI_FAMILY_REGULATION_CHOICE
  NUM_TISSUE_THRESHOLD = input$TCA_ERI_FAMILY_NUM_TISS_THRESHOLD
  graph_config = scDiffCom:::setup_graph_config()
  edge_color = ifelse(
    REGULATION_CHOICE == 'UP',
    graph_config$EDGE_COLORING$ORA_COLOR_UP,
    ifelse(
      REGULATION_CHOICE == 'DOWN',
      graph_config$EDGE_COLORING$ORA_COLOR_DOWN,
      graph_config$EDGE_COLORING$ORA_COLOR_FLAT
    )
  )
  edges = copy(scAgeCom_data$ORA_KEYWORD_COUNTS)[
    ORA_CATEGORY == 'ER Cell Families' &
      ORA_REGULATION == REGULATION_CHOICE &
      as.integer(gsub("/23", "", `Overall (Union)`)) >= NUM_TISSUE_THRESHOLD,
    c('VALUE', 'Overall (Union)')
  ]
  edges[, "from" := sapply(strsplit(edges[, VALUE], "_"), function(v) v[1])]
  edges[, "to" := sapply(strsplit(edges[, VALUE], "_"), function(v) v[2])]
  edges[, "value" := list(as.integer(gsub("/23", "", `Overall (Union)`)))]
  edges[, "label" := value]
  edges[, "color" := edge_color]
  nodes_set = union(edges[, from], edges[, to])
  nodes = data.table::data.table(id = nodes_set, label = nodes_set)
  nodes[
    ,
    c("color.background", "color.border",
      "color.highlight.background", "color.highlight.border",
      "color.hover.background", "color.hover.border",
      "shadow"
    ) := list(
      graph_config$NODE_COLORING$BACKGROUND,
      graph_config$NODE_COLORING$BORDER,
      graph_config$NODE_COLORING$HIGHLIGHT$BACKGROUND,
      graph_config$NODE_COLORING$HIGHLIGHT$BORDER,
      graph_config$NODE_COLORING$HOVER$BACKGROUND,
      graph_config$NODE_COLORING$HOVER$BORDER,
      TRUE
    )
  ]
  visNetwork::visNetwork(
    nodes = nodes,
    edges = edges,
    # width = 100,
    # height = 100,
    main = "Cell families",
    # submain = sprintf("%s", object_name),
    # footer = sprintf("Network type: %s", layout_type),
    # background = config$VISNETWORK$BACKGROUND
  ) %>% visNetwork::visNodes(
    shape = "dot",
    physics = FALSE,
    font = list(size = 18, align = "left")
  ) %>% visNetwork::visEdges(
    shadow = TRUE,
    arrows = "middle",
    smooth = list(enabled = TRUE, roundness = 0.75)
  )
})

observeEvent(
  input$TCA_KEYWORD_CATEGORY_CHOICE,
  {
    req(input$TCA_KEYWORD_CATEGORY_CHOICE)
    freezeReactiveValue(input, "TCA_KEYWORD_VALUE_CHOICE")
    updateSelectInput(
      session = session,
      'TCA_KEYWORD_CATEGORY_CHOICE',
      selected = input$TCA_KEYWORD_CATEGORY_CHOICE
    )
    choices <- sort(unique(scAgeCom_data$ORA_KEYWORD_COUNTS[
      ORA_CATEGORY == input$TCA_KEYWORD_CATEGORY_CHOICE
    ]$VALUE))
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
#     edges = scAgeCom_data$ORA_KEYWORD_COUNTS[
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
