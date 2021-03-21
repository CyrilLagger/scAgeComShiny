
## TSA overall ####

choose_tsa_tissue <- function(
  input
) {
  renderUI({
    choices <- scAgeCom_data$ALL_TISSUES
    pickerInput(
      inputId = "TSA_TISSUE_CHOICE",
      choices = choices,
      options = list(`actions-box` = TRUE),
      multiple = FALSE,
      inline = FALSE
    )
  })
}

get_tsa_title <- function(
  input
) {
  renderUI(
    {
      tags$p(
        div(
          style = "display: inline-block;",
          "Please choose a tissue: "
        ),
        div(
          style = "display: inline-block;margin-top: 25px;",
          uiOutput("TSA_TISSUE_CHOICE", inline = TRUE)
        )
      )
    }
  )
}

get_tsa_overview_table <- function(
  input
) {
  DT::renderDT({
    req(input$TSA_TISSUE_CHOICE)
    dt <- scAgeCom_data$TISSUE_COUNTS_SUMMARY[Tissue == input$TSA_TISSUE_CHOICE]
    scAgeCom_data$show_DT(
      data = dt,
      cols_to_show = colnames(dt)[-c(1,8)],
      cols_numeric = NULL,
      table_title = NULL,
      options = list(dom = "t"),
      callback = htmlwidgets::JS(
        "var tips = [
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
      }"),
      rownames = FALSE
    )
  })
}

## TSA CCIs sidebar ####

choose_TSA_dataset <- function(
  input,
  choice
) {
  renderUI({
    req(input$TSA_TISSUE_CHOICE)
    dt <- scAgeCom_data$TISSUE_COUNTS_SUMMARY[Tissue == input$TSA_TISSUE_CHOICE ]
    if (choice == "CCI") {
      iid <- "TSA_CCI_DATASET_CHOICE"
    }
    if (choice == "ORA") {
      iid <- "TSA_ORA_DATASET_CHOICE"
    }
    pickerInput(
      label = "Choose a dataset",
      inputId = iid,
      choices = sort(unique(dt$Dataset)),
      options = list(
        `actions-box` = TRUE
      ),
      multiple = FALSE,
      inline = FALSE
    )
  })
}

choose_TSA_emitter <- function(
  input
) {
  renderUI({
    req(input$TSA_CCI_DATASET_CHOICE, input$TSA_TISSUE_CHOICE)
    choices <- sort(
      unique(
        scAgeCom_data$DATASETS_COMBINED[[
          input$TSA_CCI_DATASET_CHOICE
        ]]@cci_table_detected[
          ID == input$TSA_TISSUE_CHOICE
        ][["EMITTER_CELLTYPE"]]
      )
    )
    pickerInput(
      inputId = "TSA_EMITTER_CHOICE",
      label = "Filter by Emitter Cell Types",
      choices = choices,
      selected = choices,
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    )
  })
}

choose_TSA_receiver <- function(
  input
) {
  renderUI({
    req(input$TSA_CCI_DATASET_CHOICE, input$TSA_TISSUE_CHOICE)
    choices <- sort(
      unique(
        scAgeCom_data$DATASETS_COMBINED[[
          input$TSA_CCI_DATASET_CHOICE
        ]]@cci_table_detected[
          ID == input$TSA_TISSUE_CHOICE
        ][["RECEIVER_CELLTYPE"]]
      )
    )
    pickerInput(
      inputId = "TSA_RECEIVER_CHOICE",
      label = "Filter by Receiver Cell Types",
      choices = choices,
      selected = choices,
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    )
  })
}

get_TSA_slider_log2fc <- function(
  input
) {
  renderUI({
    req(input$TSA_CCI_DATASET_CHOICE, input$TSA_TISSUE_CHOICE)
    max_val <- ceiling(
      max(
        scAgeCom_data$DATASETS_COMBINED[[
          input$TSA_CCI_DATASET_CHOICE
        ]]@cci_table_detected[
          ID == input$TSA_TISSUE_CHOICE
        ][["LOG2FC_ID"]]
      )
    )
    sliderInput(
      inputId = "TSA_SLIDER_LOG2FC",
      label = "Filter by LOG2FC",
      min = 0,
      max = max_val,
      value = 0,
      step = 0.01
    )
  })
}

download_TSA_table <- function(
  input
) {
  dt <- reactive(
    {
      dt <-  scAgeCom_data$DATASETS_COMBINED[[
        input$TSA_CCI_DATASET_CHOICE
      ]]@cci_table_detected[
        ID == input$TSA_TISSUE_CHOICE
      ]
      setorder(
        dt,
        -LOG2FC_ID,
        `BH_P_VALUE_DE`
      )
      cols_to_keep <- c("LRI", "Emitter Cell Type", "Receiver Cell Type", "LOG2FC", "Adj. p-value",
                        "Age-Regulation", "Score Young", "Score Old")
      setnames(
        dt,
        old = c("LRI", "EMITTER_CELLTYPE", "RECEIVER_CELLTYPE", "LOG2FC_ID", "BH_P_VALUE_DE",
                "REGULATION", "CCI_SCORE_YOUNG", "CCI_SCORE_OLD"),
        new = cols_to_keep
      )
      dt[, cols_to_keep, with = FALSE]
    }
  )
  downloadHandler(
    filename = function() {
      paste0(
        "cci_table_",
        tolower(gsub(" ", "_", input$TSA_CCI_DATASET_CHOICE, fixed = TRUE)),
        "_",
        tolower(gsub(" ", "_", input$TSA_TISSUE_CHOICE, fixed = TRUE)),
        ".csv"
      )
    },
    content = function(file) {
      fwrite(dt(), file)
    }
  )
}

## TSA CCIs mainpanel ####

get_TSA_cci_details <- function(
  input
) {
  renderUI({
    if (input$TSA_CCI_DETAILS_CHOICE == "CCI Table") {
      DT::dataTableOutput("TSA_INTERACTION_TABLE")
    } else if (input$TSA_CCI_DETAILS_CHOICE == "Volcano Plot") {
      plotOutput("TSA_VOLCANO_PLOT", brush = "TSA_VOLCANO_brush", height = "600px")
    } else if (input$TSA_CCI_DETAILS_CHOICE == "Score Plot") {
      plotOutput("TSA_SCORES_PLOT", brush = "TSA_SCORES_brush", height = "600px")
    } else if (input$TSA_CCI_DETAILS_CHOICE == "LRI-FC Plot") {
      plotOutput("TSA_LRIFC_PLOT", brush = "TSA_LRIFC_brush", height = "600px")
    }
  })
}

get_TSA_cci_text <- function(
  input
) {
  renderUI({
    if (input$TSA_CCI_DETAILS_CHOICE == "CCI Table") {
      NULL
    } else if (input$TSA_CCI_DETAILS_CHOICE == "Volcano Plot") {
      verbatimTextOutput("TSA_VOLCANO_TEXTOUTPUT")
    } else if (input$TSA_CCI_DETAILS_CHOICE == "Score Plot") {
      verbatimTextOutput("TSA_SCORES_TEXTOUTPUT")
    } else if (input$TSA_CCI_DETAILS_CHOICE == "LRI-FC Plot") {
      verbatimTextOutput("TSA_LRIFC_TEXTOUTPUT")
    }
  })
}

get_TSA_interaction_table <- function(
  input
) {
  DT::renderDT({
    req(
      input$TSA_CCI_DATASET_CHOICE,
      input$TSA_TISSUE_CHOICE,
      input$TSA_EMITTER_CHOICE,
      input$TSA_RECEIVER_CHOICE,
      input$TSA_LRI_CHOICE,
      input$TSA_SLIDER_PVALUE,
      input$TSA_SLIDER_LOG2FC
    )
    dt <-  scAgeCom_data$DATASETS_COMBINED[[
      input$TSA_CCI_DATASET_CHOICE
    ]]@cci_table_detected
    dt <- scAgeCom_data$subset_cci_table(
      dt,
      input$TSA_TISSUE_CHOICE,
      input$TSA_EMITTER_CHOICE,
      input$TSA_RECEIVER_CHOICE,
      input$TSA_LRI_CHOICE,
      input$TSA_SLIDER_PVALUE,
      input$TSA_SLIDER_LOG2FC,
      TRUE
    )
    dt <- scAgeCom_data$fix_cci_table_names(
      dt,
      scAgeCom_data$cols_old_cci_table,
      scAgeCom_data$cols_to_show_cci_table
    )
    scAgeCom_data$show_DT(
      dt,
      scAgeCom_data$cols_to_show_cci_table,
      scAgeCom_data$cols_numeric_cci_table,
      "Cell-Cell Interation Table"
    )
  })
}

plot_TSA_VOLCANO <- function(
  input
) {
  renderPlot({
    req(
      input$TSA_CCI_DATASET_CHOICE,
      input$TSA_TISSUE_CHOICE,
      input$TSA_EMITTER_CHOICE,
      input$TSA_RECEIVER_CHOICE,
      input$TSA_LRI_CHOICE,
      input$TSA_SLIDER_PVALUE,
      input$TSA_SLIDER_LOG2FC
    )
    dt <-  scAgeCom_data$DATASETS_COMBINED[[
      input$TSA_CCI_DATASET_CHOICE
    ]]@cci_table_detected
    xlims <- c(min(dt[["LOG2FC_ID"]]), max(dt[["LOG2FC_ID"]]))
    ylims <- c(0, 4)
    dt <- scAgeCom_data$subset_cci_table(
      dt,
      input$TSA_TISSUE_CHOICE,
      input$TSA_EMITTER_CHOICE,
      input$TSA_RECEIVER_CHOICE,
      input$TSA_LRI_CHOICE,
      input$TSA_SLIDER_PVALUE,
      input$TSA_SLIDER_LOG2FC,
      FALSE
    )
    dt <- scAgeCom_data$fix_cci_table_names(
      dt,
      scAgeCom_data$cols_old_cci_table,
      scAgeCom_data$cols_to_show_cci_table
    )
    dt[, minus_log10_pval := -log10(`Adj. p-value` + 1E-4)]
    scAgeCom_data$show_volcano(
      data = dt,
      xlims = xlims,
      ylims = ylims
    )
  })
}

get_TSA_VOLCANO_text <- function(
  input
) {
  renderPrint({
    req(
      input$TSA_CCI_DATASET_CHOICE,
      input$TSA_TISSUE_CHOICE,
      input$TSA_EMITTER_CHOICE,
      input$TSA_RECEIVER_CHOICE,
      input$TSA_LRI_CHOICE,
      input$TSA_SLIDER_PVALUE,
      input$TSA_SLIDER_LOG2FC
    )
    dt <-  scAgeCom_data$DATASETS_COMBINED[[
      input$TSA_CCI_DATASET_CHOICE
    ]]@cci_table_detected
    dt <- scAgeCom_data$subset_cci_table(
      dt,
      input$TSA_TISSUE_CHOICE,
      input$TSA_EMITTER_CHOICE,
      input$TSA_RECEIVER_CHOICE,
      input$TSA_LRI_CHOICE,
      input$TSA_SLIDER_PVALUE,
      input$TSA_SLIDER_LOG2FC,
      FALSE
    )
    dt <- scAgeCom_data$fix_cci_table_names(
      dt,
      scAgeCom_data$cols_old_cci_table,
      scAgeCom_data$cols_to_show_cci_table
    )
    dt[, minus_log10_pval := -log10(`Adj. p-value` + 1E-4)]
    brushedPoints(
      dt,
      input$TSA_VOLCANO_brush,
      xvar = "LOG2FC",
      yvar = "minus_log10_pval"
    )
  })
}

plot_TSA_SCORES <- function(
  input
) {
  renderPlot({
    req(
      input$TSA_CCI_DATASET_CHOICE,
      input$TSA_TISSUE_CHOICE,
      input$TSA_EMITTER_CHOICE,
      input$TSA_RECEIVER_CHOICE,
      input$TSA_LRI_CHOICE,
      input$TSA_SLIDER_PVALUE,
      input$TSA_SLIDER_LOG2FC
    )
    dt <-  scAgeCom_data$DATASETS_COMBINED[[
      input$TSA_CCI_DATASET_CHOICE
    ]]@cci_table_detected
    min_young <- min(dt[CCI_SCORE_YOUNG > 0][["CCI_SCORE_YOUNG"]])
    min_old <- min(dt[CCI_SCORE_OLD > 0][["CCI_SCORE_OLD"]])
    dt[, CCI_SCORE_YOUNG := ifelse(CCI_SCORE_YOUNG == 0, min_young, CCI_SCORE_YOUNG)]
    dt[, CCI_SCORE_OLD := ifelse(CCI_SCORE_OLD == 0, min_old, CCI_SCORE_OLD)]
    xlims <- c(min(dt[["CCI_SCORE_YOUNG"]]), max(dt[["CCI_SCORE_YOUNG"]]))
    ylims <- c(min(dt[["CCI_SCORE_OLD"]]), max(dt[["CCI_SCORE_OLD"]]))
    dt <- scAgeCom_data$subset_cci_table(
      dt,
      input$TSA_TISSUE_CHOICE,
      input$TSA_EMITTER_CHOICE,
      input$TSA_RECEIVER_CHOICE,
      input$TSA_LRI_CHOICE,
      input$TSA_SLIDER_PVALUE,
      input$TSA_SLIDER_LOG2FC,
      FALSE
    )
    dt <- scAgeCom_data$fix_cci_table_names(
      dt,
      scAgeCom_data$cols_old_cci_table,
      scAgeCom_data$cols_to_show_cci_table
    )
    scAgeCom_data$show_scores(
      data = dt,
      xlims = xlims,
      ylims = ylims
    )
  })
}

get_TSA_SCORES_text <- function(
  input
) {
  renderPrint({
    req(
      input$TSA_CCI_DATASET_CHOICE,
      input$TSA_TISSUE_CHOICE,
      input$TSA_EMITTER_CHOICE,
      input$TSA_RECEIVER_CHOICE,
      input$TSA_LRI_CHOICE,
      input$TSA_SLIDER_PVALUE,
      input$TSA_SLIDER_LOG2FC
    )
    dt <-  scAgeCom_data$DATASETS_COMBINED[[
      input$TSA_CCI_DATASET_CHOICE
    ]]@cci_table_detected
    min_young <- min(dt[CCI_SCORE_YOUNG > 0][["CCI_SCORE_YOUNG"]])
    min_old <- min(dt[CCI_SCORE_OLD > 0][["CCI_SCORE_OLD"]])
    dt[, CCI_SCORE_YOUNG := ifelse(CCI_SCORE_YOUNG == 0, min_young, CCI_SCORE_YOUNG)]
    dt[, CCI_SCORE_OLD := ifelse(CCI_SCORE_OLD == 0, min_old, CCI_SCORE_OLD)]
    dt <- scAgeCom_data$subset_cci_table(
      dt,
      input$TSA_TISSUE_CHOICE,
      input$TSA_EMITTER_CHOICE,
      input$TSA_RECEIVER_CHOICE,
      input$TSA_LRI_CHOICE,
      input$TSA_SLIDER_PVALUE,
      input$TSA_SLIDER_LOG2FC,
      FALSE
    )
    dt <- scAgeCom_data$fix_cci_table_names(
      dt,
      scAgeCom_data$cols_old_cci_table,
      scAgeCom_data$cols_to_show_cci_table
    )
    brushedPoints(dt, input$TSA_SCORES_brush)
  })
}

plot_TSA_LRIFC <- function(
  input
) {
  renderPlot({
    req(
      input$TSA_CCI_DATASET_CHOICE,
      input$TSA_TISSUE_CHOICE,
      input$TSA_EMITTER_CHOICE,
      input$TSA_RECEIVER_CHOICE,
      input$TSA_LRI_CHOICE,
      input$TSA_SLIDER_PVALUE,
      input$TSA_SLIDER_LOG2FC
    )
    dt <-  scAgeCom_data$DATASETS_COMBINED[[
      input$TSA_CCI_DATASET_CHOICE
    ]]@cci_table_detected
    dt[, LOG2FC_L := log2(
      pmin(L1_EXPRESSION_OLD, L2_EXPRESSION_OLD, na.rm = TRUE)
      /
        pmin(L1_EXPRESSION_YOUNG, L2_EXPRESSION_YOUNG, na.rm = TRUE)
    ) ]
    dt[, LOG2FC_R := log2(
      pmin(R1_EXPRESSION_OLD, R2_EXPRESSION_OLD, R3_EXPRESSION_OLD, na.rm = TRUE)
      /
        pmin(R1_EXPRESSION_YOUNG, R2_EXPRESSION_YOUNG, R3_EXPRESSION_YOUNG, na.rm = TRUE)
    )]
    max_L <- max(dt[is.finite(LOG2FC_L)][["LOG2FC_L"]])
    min_L <- min(dt[is.finite(LOG2FC_L)][["LOG2FC_L"]])
    max_L <- max(max_L, -min_L)
    min_L <- min(-max_L, min_L)
    dt[, LOG2FC_L := ifelse(is.infinite(LOG2FC_L) & LOG2FC_L > 0, max_L,
                            ifelse(is.infinite(LOG2FC_L) & LOG2FC_L < 0, min_L, LOG2FC_L))]
    max_R <- ceiling(max(dt[is.finite(LOG2FC_R)][["LOG2FC_R"]]))
    min_R <- floor(min(dt[is.finite(LOG2FC_R)][["LOG2FC_R"]]))
    max_R <- max(max_R, -min_R)
    min_R <- min(-max_R, min_R)
    dt[, LOG2FC_R := ifelse(is.infinite(LOG2FC_R) & LOG2FC_R > 0, max_R,
                            ifelse(is.infinite(LOG2FC_R) & LOG2FC_R < 0, min_R, LOG2FC_R))]
    xlims <- c(min_L, max_L)
    ylims <- c(min_R, max_R)
    dt <- scAgeCom_data$subset_cci_table(
      dt,
      input$TSA_TISSUE_CHOICE,
      input$TSA_EMITTER_CHOICE,
      input$TSA_RECEIVER_CHOICE,
      input$TSA_LRI_CHOICE,
      input$TSA_SLIDER_PVALUE,
      input$TSA_SLIDER_LOG2FC,
      FALSE
    )
    dt <- scAgeCom_data$fix_cci_table_names(
      dt,
      c(scAgeCom_data$cols_old_cci_table, "LOG2FC_L", "LOG2FC_R"),
      c(scAgeCom_data$cols_to_show_cci_table, "LOG2FC_L", "LOG2FC_R")
    )
    scAgeCom_data$show_LRIFC(
      data = dt,
      xlims = xlims,
      ylims = ylims
    )
  })
}

get_TSA_LRIFC_text <- function(
  input
) {
  renderPrint({
    req(
      input$TSA_CCI_DATASET_CHOICE,
      input$TSA_TISSUE_CHOICE,
      input$TSA_EMITTER_CHOICE,
      input$TSA_RECEIVER_CHOICE,
      input$TSA_LRI_CHOICE,
      input$TSA_SLIDER_PVALUE,
      input$TSA_SLIDER_LOG2FC
    )
    dt <-  scAgeCom_data$DATASETS_COMBINED[[
      input$TSA_CCI_DATASET_CHOICE
    ]]@cci_table_detected
    dt[, LOG2FC_L := log2(
      pmin(L1_EXPRESSION_OLD, L2_EXPRESSION_OLD, na.rm = TRUE)
      /
        pmin(L1_EXPRESSION_YOUNG, L2_EXPRESSION_YOUNG, na.rm = TRUE)
    ) ]
    dt[, LOG2FC_R := log2(
      pmin(R1_EXPRESSION_OLD, R2_EXPRESSION_OLD, R3_EXPRESSION_OLD, na.rm = TRUE)
      /
        pmin(R1_EXPRESSION_YOUNG, R2_EXPRESSION_YOUNG, R3_EXPRESSION_YOUNG, na.rm = TRUE)
    )]
    max_L <- max(dt[is.finite(LOG2FC_L)][["LOG2FC_L"]])
    min_L <- min(dt[is.finite(LOG2FC_L)][["LOG2FC_L"]])
    max_L <- max(max_L, -min_L)
    min_L <- min(-max_L, min_L)
    dt[, LOG2FC_L := ifelse(is.infinite(LOG2FC_L) & LOG2FC_L > 0, max_L,
                            ifelse(is.infinite(LOG2FC_L) & LOG2FC_L < 0, min_L, LOG2FC_L))]
    max_R <- ceiling(max(dt[is.finite(LOG2FC_R)][["LOG2FC_R"]]))
    min_R <- floor(min(dt[is.finite(LOG2FC_R)][["LOG2FC_R"]]))
    max_R <- max(max_R, -min_R)
    min_R <- min(-max_R, min_R)
    dt[, LOG2FC_R := ifelse(is.infinite(LOG2FC_R) & LOG2FC_R > 0, max_R,
                            ifelse(is.infinite(LOG2FC_R) & LOG2FC_R < 0, min_R, LOG2FC_R))]
    dt <- scAgeCom_data$subset_cci_table(
      dt,
      input$TSA_TISSUE_CHOICE,
      input$TSA_EMITTER_CHOICE,
      input$TSA_RECEIVER_CHOICE,
      input$TSA_LRI_CHOICE,
      input$TSA_SLIDER_PVALUE,
      input$TSA_SLIDER_LOG2FC,
      FALSE
    )
    dt <- scAgeCom_data$fix_cci_table_names(
      dt,
      c(scAgeCom_data$cols_old_cci_table, "LOG2FC_L", "LOG2FC_R"),
      c(scAgeCom_data$cols_to_show_cci_table, "LOG2FC_L", "LOG2FC_R")
    )
    brushedPoints(dt, input$TSA_LRIFC_brush)
  })
}

## TSA ORA sidebar ####

choose_TSA_ORA_category <- function(
  input
) {
  renderUI({
    #choices <- names(DATASETS_COMBINED[[input$TSA_ORA_DATASET_CHOICE]]@ora_table)
    choices <- c("KEGG Pathways", "GO Terms", "LRIs", "Cell Types", "Cell Families")
    pickerInput(
      inputId = "TSA_ORA_CATEGORY_CHOICE",
      label = "Category",
      choices = choices,
      options = list(`actions-box` = TRUE),
      multiple = FALSE
    )
  })
}

## TSA ORA mainpanel ####

get_TSA_ora_details <- function(
  input
) {
  renderUI({
    if (input$TSA_ORA_DETAILS_CHOICE == "ORA Table") {
      DT::dataTableOutput("TSA_ORA_TABLE")
    } else if (input$TSA_ORA_DETAILS_CHOICE == "ORA Score Plot") {
      plotOutput("TSA_ORA_PLOT", height = "800px")
    } else if (input$TSA_ORA_DETAILS_CHOICE == "ORA Network") {
      visNetworkOutput("TSA_ORA_NETWORK_PLOT", height = "800px")
    }
  })
}

get_TSA_ORA_table <- function(
  input
) {
  DT::renderDataTable({
    req(input$TSA_ORA_DATASET_CHOICE, input$TSA_TISSUE_CHOICE, input$TSA_ORA_CATEGORY_CHOICE, input$TSA_ORA_TYPE_CHOICE)
    replacement <- data.table(
      VALUE_OLD = c("KEGG_PWS", "GO_TERMS", "LRI", "ER_CELLTYPES", "ER_CELLFAMILIES"),
      VALUE_NEW = c("KEGG Pathways", "GO Terms", "LRIs", "Cell Types", "Cell Families")
    )
    replacement <- replacement[VALUE_NEW == input$TSA_ORA_CATEGORY_CHOICE ][["VALUE_OLD"]]
    dt <- scAgeCom_data$DATASETS_COMBINED[[input$TSA_ORA_DATASET_CHOICE]]@ora_table[[replacement]][
      ID == input$TSA_TISSUE_CHOICE
    ]
    req(dt)
    if(input$TSA_ORA_TYPE_CHOICE == "Up") {
      dt <- dt[`OR_UP` >= 1 & BH_P_VALUE_UP <= 0.05, c("VALUE", "ORA_SCORE_UP", "OR_UP", "BH_P_VALUE_UP")]
    } else if(input$TSA_ORA_TYPE_CHOICE == "Down") {
      dt <- dt[`OR_DOWN` >= 1 & BH_P_VALUE_DOWN <= 0.05, c("VALUE","ORA_SCORE_DOWN", "OR_DOWN", "BH_P_VALUE_DOWN")]
    } else if(input$TSA_ORA_TYPE_CHOICE == "Flat") {
      dt <- dt[`OR_FLAT` >= 1 & BH_P_VALUE_FLAT <= 0.05, c("VALUE","ORA_SCORE_FLAT", "OR_FLAT", "BH_P_VALUE_FLAT")]
    }
    setnames(
      dt,
      old = colnames(dt),
      new = c(input$TSA_ORA_CATEGORY_CHOICE, "ORA Score", "Odds Ratio", "Adj. p-value")
    )
    setorder(dt, -`ORA Score`)
    cols_numeric <- c("ORA Score", "Odds Ratio", "Adj. p-value")
    scAgeCom_data$show_DT(
      dt,
      cols_to_show = colnames(dt),
      cols_numeric = cols_numeric,
      table_title = paste0(
        input$TSA_ORA_CATEGORY_CHOICE,
        " over-represented among ",
        input$TSA_ORA_TYPE_CHOICE,
        "-regulated CCIs"
      )
    )
  })
}

plot_TSA_ORA <- function(
  input
) {
  renderPlot({
    req(input$TSA_ORA_DATASET_CHOICE, input$TSA_TISSUE_CHOICE, input$TSA_ORA_CATEGORY_CHOICE, input$TSA_ORA_TYPE_CHOICE)
    replacement <- data.table(
      VALUE_OLD = c("KEGG_PWS", "GO_TERMS", "LRI", "ER_CELLTYPES", "ER_CELLFAMILIES"),
      VALUE_NEW = c("KEGG Pathways", "GO Terms", "LRIs", "Cell Types", "Cell Families")
    )
    replacement <- replacement[VALUE_NEW == input$TSA_ORA_CATEGORY_CHOICE ][["VALUE_OLD"]]
    if (input$TSA_ORA_TYPE_CHOICE == "Up") {
      reg <- "UP"
    } else if (input$TSA_ORA_TYPE_CHOICE == "Down") {
      reg <- "DOWN"
    } else if ( input$TSA_ORA_TYPE_CHOICE == "Flat") {
      reg <- "FLAT"
    }
    obj <- scAgeCom_data$DATASETS_COMBINED[[input$TSA_ORA_DATASET_CHOICE]]
    req(obj)
    p <- PlotORA(
      object = obj,
      subID = input$TSA_TISSUE_CHOICE,
      category = replacement,
      regulation = reg,
      max_terms_show = 20,
      OR_threshold = 1,
      p_value_threshold = 0.05,
      stringent = FALSE
    )
    if (is.character(p)) {
      return(p)
    }
    p <- p + ggtitle(
      paste0(
        "Top-20 ",
        input$TSA_ORA_CATEGORY_CHOICE,
        " over-represented among ",
        input$TSA_ORA_TYPE_CHOICE,
        "-regulated CCIs"
      )
    ) +
      ylab("") +
      theme(text=element_text(size=20)) +
      theme(plot.title = element_text(hjust = 0.5, size = 16))
    return(p)
  })
}

plot_TSA_ORA_network <- function(
  input
) {
  renderVisNetwork({
    req(input$TSA_ORA_DATASET_CHOICE, input$TSA_TISSUE_CHOICE)
    obj <- scAgeCom_data$DATASETS_COMBINED[[input$TSA_ORA_DATASET_CHOICE]]
    abbr <- scAgeCom_data$ABBR_CELLTYPE[[input$TSA_ORA_DATASET_CHOICE]]
    abbr <- unique(abbr[ORIGINAL_CELLTYPE %in% obj@cci_table_detected[ID == input$TSA_TISSUE_CHOICE][["EMITTER_CELLTYPE"]]])
    req(obj)
    BuildNetwork(
      object = obj,
      network_type = "ORA_network",
      layout_type = "bipartite",
      ID = input$TSA_TISSUE_CHOICE,
      abbreviation_table = abbr
    )
  })
}


