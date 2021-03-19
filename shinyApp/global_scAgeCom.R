## Libraries ####

library(shiny)
library(shinyWidgets)
library(DT)
library(data.table)
library(ggplot2)
library(ComplexUpset)
library(scDiffCom)
library(ggExtra)
library(igraph)
library(RColorBrewer)
library(purrr)
library(visNetwork)
library(htmlwidgets)

## Global Data ####

LRI_curated <- copy(scDiffCom::LRI_mouse$LRI_curated)
LRI_curated[, SOURCE := gsub("SCT:SingleCellSignalR|SCT:CellPhoneDB|SCT:DLRP", "", SOURCE)]
LRI_curated[, SOURCE := gsub(";;", ";", SOURCE)]
LRI_curated[, SOURCE := sub(";$", "", SOURCE)]
LRI_curated[, SOURCE := sub("^;", "", SOURCE)]

DATASETS_COMBINED <- readRDS("data/scagecom_results_processed.rds")
DATASETS_COMBINED <- lapply(
  DATASETS_COMBINED,
  function(dataset) {
    dt <- dataset@cci_table_detected
    dt[, LOG2FC_BASE := LOGFC*log2(exp(1))]
    dt[, LOG2FC_ID := {
      temp <- LOG2FC_BASE
      temp_max <- ceiling(max(temp[is.finite(temp)]))
      temp_min <- floor(min(temp[is.finite(temp)]))
      temp_max <- max(temp_max, -temp_min)
      temp_min <- min(-temp_max, temp_min)
      ifelse(
        is.infinite(LOG2FC_BASE) & LOG2FC_BASE > 0,
        temp_max,
        ifelse(
          is.infinite(LOG2FC_BASE) & LOG2FC_BASE < 0,
          temp_min,
          LOG2FC_BASE
        )
      )}, by = ID]
    dt[, LOG2FC_ALL := {
      temp <- LOG2FC_BASE
      temp_max <- ceiling(max(temp[is.finite(temp)]))
      temp_min <- floor(min(temp[is.finite(temp)]))
      temp_max <- max(temp_max, -temp_min)
      temp_min <- min(-temp_max, temp_min)
      ifelse(
        is.infinite(LOG2FC_BASE) & LOG2FC_BASE > 0,
        temp_max,
        ifelse(
          is.infinite(LOG2FC_BASE) & LOG2FC_BASE < 0,
          temp_min,
          LOG2FC_BASE
        )
      )}]
    dataset@cci_table_detected <- dt
    return(dataset)
  }
)

names(DATASETS_COMBINED) <- c(
  "Calico2019",
  "TMS Droplet (female)", "TMS Droplet (male)",
  "TMS FACS (female)", "TMS FACS (male)"
)

ABBR <- lapply(
  DATASETS_COMBINED,
  function(dataset) {
    dt <- unique(dataset@cci_table_detected[, c("EMITTER_CELLTYPE", "EMITTER_CELL_ABR")])
    setnames(
      dt,
      old = c("EMITTER_CELLTYPE", "EMITTER_CELL_ABR"),
      new = c("ORIGINAL_CELLTYPE", "ABBR_CELLTYPE")
    )
    dt
  }
)

##
LRI_REGULATION_SUMMARY <- rbindlist(
  lapply(
    DATASETS_COMBINED,
    function(i) {
      i@cci_table_detected[, c("ID", "ER_CELLTYPES", "LRI", "ER_CELLFAMILIES", "REGULATION")]
    }
  ),
  use.names = TRUE,
  idcol = "DATASET"
)

LRI_ORA_SUMMARY <- rbindlist(
  lapply(
    DATASETS_COMBINED,
    function(i) {
      i@ora_table$LRI
    }
  ),
  use.names = TRUE,
  idcol = "DATASET"
)

TISSUE_COUNTS_SUMMARY <- readRDS("data/tissue_cci_counts.rds")
TISSUE_COUNTS_SUMMARY[
  data.table(
    old_names = c(
      "calico",
      "droplet_female", "droplet_male",
      "facs_female", "facs_male"
    ),
    new_names = c(
      "Calico2019",
      "TMS Droplet (female)", "TMS Droplet (male)",
      "TMS FACS (female)", "TMS FACS (male)"
    )
  ),
  on = "DATASET==old_names",
  DATASET := i.new_names
]


##

ORA_KEYWORD_SUMMARY <- readRDS("data/ora_keyword_summary.rds")
ORA_KEYWORD_COUNTS <- ORA_KEYWORD_SUMMARY[
  ,
  .N,
  by = c("TYPE", "REGULATION", "GENDER", "DATASET", "VALUE")
]
ORA_KEYWORD_COUNTS <- dcast.data.table(
  ORA_KEYWORD_COUNTS,
  TYPE + VALUE + REGULATION ~ DATASET + GENDER,
  value.var = "N",
  fill = 0
)[,
  c(
    "TYPE", "VALUE", "REGULATION",
    "either_either",
    "facs_male", "facs_female",
    "droplet_male", "droplet_female",
    "calico_male")
]
setnames(
  ORA_KEYWORD_COUNTS,
  old = c("either_either",
          "facs_male", "facs_female",
          "droplet_male", "droplet_female",
          "calico_male"),
  new = c("Overall (union)",
          "TMS FACS (male)", "TMS FACS (female)",
          "TMS Droplet (male)", "TMS Droplet (female)",
          "Calico2019")
)
name_conversion <- data.table(
  old_names = c("LRI", "GO_TERMS", "KEGG_PWS", "ER_CELLFAMILIES"),
  new_names = c("LRI", "GO Terms", "KEGG Pathways", "Cell-Type Families")
)
ORA_KEYWORD_COUNTS[
  name_conversion,
  on = "TYPE==old_names",
  TYPE := i.new_names
]

#CCI_SUMMARY <- readRDS("data/scagecom_summary.rds")
#names(CCI_SUMMARY) <- names(DATASETS_COMBINED)

## Global functions ####

show_DT <- function(
  data,
  cols_to_show,
  cols_numeric = NULL,
  table_title = NULL,
  options = NULL,
  rownames = TRUE,
  callback = NULL
) {
  if (is.null(options)) {
    options <- list(
      pageLength = 10
      )
  }
  if (is.null(callback)) {
    callback <- htmlwidgets::JS("return table;")
  }
  res <- DT::datatable(
    data = data[, cols_to_show, with = FALSE],
    options = options,
    callback = callback,
    caption = tags$caption(style = 'caption-side: top; text-align: center; color:black; font-size:150% ;',table_title),
    rownames = rownames,
    extensions = c("Buttons")
  )
  if(!is.null(cols_numeric)) {
    res <- DT::formatSignif(
      table = res,
      columns = cols_numeric,
      digits = 3
    )
  }
  return(res)
}

show_volcano <- function(
  data,
  xlims,
  ylims
) {
  p <- ggplot(data, aes(
    x = LOG2FC,
    y = minus_log10_pval,
    color = `Age Regulation`
  )) +
    geom_point() +
    scale_color_manual(values = c(
      "UP" = "red", "DOWN" = "blue",
      "FLAT" = "green",
      "NON_SIGNIFICANT_CHANGE" = "gray")) +
    geom_hline(yintercept = -log10(0.05)) +
    geom_vline(xintercept = log2(1.5)) +
    geom_vline(xintercept = -log2(1.5)) +
    xlab(expression(paste(Log[2], "FC"))) +
    ylab(expression(paste(-Log[10], " ", p[BH]))) +
    xlim(xlims) + ylim(ylims) +
    ggtitle("Volcano Plot of detected CCI (interactive)") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(text=element_text(size=20)) #+
   # theme(legend.title = element_blank())
  return(p)
}

show_scores <- function(
  data,
  xlims,
  ylims
) {
  p <- ggplot(
    data,
    aes(
      x = `Score Young`,
      y = `Score Old`,
      color = `Age Regulation`
    )
  ) +
    geom_point() +
    scale_color_manual(values = c(
      "UP" = "red", "DOWN" = "blue",
      "FLAT" = "green",
      "NON_SIGNIFICANT_CHANGE" = "gray")) +
    scale_x_log10(limits = xlims) +
    scale_y_log10(limits = ylims) +
    geom_abline(slope = 1, intercept = 0) +
    xlab("Score Young") +
    ylab("Score Old") +
    ggtitle("Old vs Young Scores of detected CCIs (interactive)") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(text=element_text(size=20))
    #theme(legend.title = element_blank())
  return(p)
}

show_LRIFC <- function(
  data,
  xlims,
  ylims
) {
  p <- ggplot(
    data,
    aes(
      x = LOG2FC_L,
      y = LOG2FC_R,
      color = `Age Regulation`
    )
  ) +
    geom_point() +
    scale_color_manual(values = c(
      "UP" = "red", "DOWN" = "blue",
      "FLAT" = "green",
      "NON_SIGNIFICANT_CHANGE" = "gray")) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    xlim(xlims) + ylim(ylims) +
    xlab("Ligand LOG2FC") +
    ylab("Receptor LOG2FC") +
    ggtitle("Ligand vs Receptor Fold-Change of detected CCIs (interactive)") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(text=element_text(size=20))
  #theme(legend.title = element_blank())
  return(p)
}

get_plot_keyword_tissue_vs_dataset <- function(
  value
) {
  dt <- dcast.data.table(
    ORA_KEYWORD_SUMMARY[
      VALUE == value &
        GENDER %in% c("male", "female") &
        DATASET %in% c("facs", "droplet", "calico")],
    ID ~ DATASET + GENDER,
    value.var = "REGULATION",
    fun.aggregate = paste0,
    fill = "NOT_OR",
    collapse = ":"
  )
  dt <- melt.data.table(
    dt,
    id.vars = "ID"
  )
  dt[data.table(
    old_values = c(
      "UP", "DOWN", "FLAT",
      "UP:DOWN", "DOWN:UP",
      "UP:FLAT", "FLAT:UP",
      "DOWN:FLAT", "FLAT:DOWN",
      "NOT_OR"
    ),
    new_values = c(
      "UP", "DOWN", "FLAT",
      "UP:DOWN", "UP:DOWN",
      "UP", "UP",
      "DOWN", "DOWN",
      "NOT_OR"
    )
  ),
  on = "value==old_values",
  value := i.new_values
  ]
  p <- ggplot(dt, aes(variable, ID)) +
    geom_tile(aes(
      fill = value,
      width = 0.9,
      height = 0.9),
      colour = "black") +
    scale_fill_manual(values = c(
      "UP" = "red",
      "DOWN" = "blue",
      "FLAT" = "green",
      "UP:DOWN" = "yellow",
      "NOT_OR" = "gray")) +
    ggtitle(paste0("Over-representation of '", value, "'")) +
    scale_y_discrete(limits = sort(unique(dt$ID), decreasing = TRUE)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(text=element_text(size = 20)) +
    theme(axis.text=element_text(size = 18)) +
    xlab("Dataset") +
    ylab("Tissue")
  return(p)
}
