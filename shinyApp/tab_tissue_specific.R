tab_tissue_specific <- tabPanel(
  title = "Tissue-Specific Analysis",
  uiOutput("TSA_TOP_VIEW"),
  uiOutput("TSA_PANEL_VIEW"),
  value = "TSA_navbar"
)
