
## TCA overall ####

get_TCA_title <- function(
  input
) {
  renderUI({
      tags$p(
        div(style="display: inline-block;", "Choose a title for here: ")
      )
    })
}

## TCA GlOBAL mainpanel ####

get_TCA_global_details <- function(
  input
) {
  renderUI({
    DT::dataTableOutput("TCA_GLOBAL_TABLE")
  })
}

get_TCA_global_table <- function(
  input
) {
  DT::renderDT({
    req(input$TCA_GLOBAL_TABLE_CHOICE, input$TCA_GLOBAL_ORA_REGULATION_CHOICE)
    dt <- scAgeCom_data$ORA_KEYWORD_COUNTS[
      TYPE == input$TCA_GLOBAL_TABLE_CHOICE &
        REGULATION == input$TCA_GLOBAL_ORA_REGULATION_CHOICE
    ][order(-`Overall (union)`)]
    cols_to_keep <-  c(
      "VALUE",
      "Overall (union)",
      "TMS FACS (male)", "TMS FACS (female)",
      "TMS Droplet (male)", "TMS Droplet (female)",
      "Calico2019"
    )
    scAgeCom_data$show_DT(
      dt,
      cols_to_show = cols_to_keep,
      cols_to_keep[-1],
      table_title = paste0(
        "Summary over-representation for ",
        input$TCA_GLOBAL_ORA_REGULATION_CHOICE,
        " by tissue and dataset.")
    )
  })
}

## TCA KEYWORD mainpanel ####

get_TCA_keyword_summary <- function(
  input
) {
  plotly::renderPlotly({
    req(
      input$TCA_KEYWORD_CATEGORY_CHOICE,
      input$TCA_KEYWORD_VALUE_CHOICE
      )
    #print(input$TCA_KEYWORD_CATEGORY_CHOICE)
    #print(input$TCA_KEYWORD_VALUE_CHOICE)
    scAgeCom_data$plot_keyword_tissue_vs_dataset(
      scAgeCom_data$ORA_KEYWORD_SUMMARY_UNIQUE,
      scAgeCom_data$ORA_KEYWORD_TEMPLATE,
      input$TCA_KEYWORD_CATEGORY_CHOICE,
      input$TCA_KEYWORD_VALUE_CHOICE
    )
  })
}

## TCA CELL FAMILIES Tab ##
get_TCA_eri_family_network <- function(
  input
) {
  visNetwork::renderVisNetwork({
    req(input$TCA_ERI_FAMILY_REGULATION_CHOICE, input$TCA_ERI_FAMILY_NUM_TISS_THRESHOLD)
    REGULATION_CHOICE = input$TCA_ERI_FAMILY_REGULATION_CHOICE
    NUM_TISSUE_THRESHOLD = input$TCA_ERI_FAMILY_NUM_TISS_THRESHOLD
    
    graph_config = scDiffCom:::setup_graph_config()
    edge_color = ifelse(
      REGULATION_CHOICE == 'UP',
      graph_config$EDGE_COLORING$ORA_COLOR_UP, 
      graph_config$EDGE_COLORING$ORA_COLOR_DOWN
    )
    
    TYPE <- REGULATION <- VALUE <- from <- to <- NULL
    edges = scAgeCom_data$ORA_KEYWORD_COUNTS[
      TYPE == 'ERI Family' &
        REGULATION == REGULATION_CHOICE &
        `Overall (union)` >= NUM_TISSUE_THRESHOLD,
      c('VALUE', 'Overall (union)')
    ]
    edges[, "from" := sapply(strsplit(edges[, VALUE], "_"), function(v) v[1])]
    edges[, "to" := sapply(strsplit(edges[, VALUE], "_"), function(v) v[2])]
    edges[, "value" := list(`Overall (union)`)]
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
}
