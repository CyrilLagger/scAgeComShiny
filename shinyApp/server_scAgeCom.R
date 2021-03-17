server_scAgeCom <- function(
  input,
  output,
  session
) {
  ## INTRO overall ####
  source("utils_description.R", local = TRUE)
  output$INTRO_TITLE <- get_INTRO_title(input)
  output$INTRO_OVERVIEW <- get_INTRO_OVERVIEW(input)
  ## INTRO METHOD mainpanel ####
  output$INTRO_METHOD_HTML <- get_INTRO_method_htlm(input)
  ## INTRO SCRNA mainpanel ####
  output$INTRO_SCRNA_HTML <- get_INTRO_scrna_htlm(input)
  ## INTRO LRdb mainpanel ####
  output$INTRO_LRI_HTML <- get_INTRO_lri_html(input)
  output$INTRO_LRI_DETAILS <- get_INTRO_lri_details(input)
  output$INTRO_LRI_TABLE <- get_INTRO_lri_table(input)
  output$INTRO_LRI_UPSET_PLOT <- plot_INTRO_lri_upset(input)
  ## TSA overall ####
  source("utils_tissue_specific.R", local = TRUE)
  output$TSA_TISSUE_CHOICE <- choose_TSA_tissue(input)
  output$TSA_TITLE <- get_TSA_title(input)
  output$TSA_OVERVIEW_TABLE <- get_TSA_overview_table(input)
  ## TSA CCI sidebar ####
  output$TSA_CCI_DATASET_CHOICE <- choose_TSA_dataset(input, "CCI")
  output$TSA_ORA_DATASET_CHOICE <- choose_TSA_dataset(input, "ORA")
  output$TSA_DOWNLOAD_TABLE <- download_TSA_table(input)
  output$TSA_EMITTER_CHOICE <- choose_TSA_emitter(input)
  output$TSA_RECEIVER_CHOICE <- choose_TSA_receiver(input)
  #output$TSA_LRI_CHOICE <- choose_TSA_lri(input, session)
  observe({
    req(input$TSA_CCI_DATASET_CHOICE, input$TSA_TISSUE_CHOICE)
    ALL_LRI_LABEL = 'All LRI'
    choices <-
      c(ALL_LRI_LABEL,
        sort(
          unique(
            DATASETS_COMBINED[[input$TSA_CCI_DATASET_CHOICE]]@cci_detected[
              ID == input$TSA_TISSUE_CHOICE][["LR_GENES"]]))
      )
    updateSelectizeInput(
      session = session,
      "TSA_LRI_CHOICE",
      #label = 'Filter by Ligand-Receptor Interactions',
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
  output$TSA_SLIDER_LOG2FC <- get_TSA_slider_log2fc(input)
  ## TSA CCIs mainpanel ####
  output$TSA_CCI_DETAILS <- get_TSA_cci_details(input)
  output$TSA_CCI_TEXTOUTPUT <- get_TSA_cci_text(input)
  output$TSA_INTERACTION_TABLE <- get_TSA_interaction_table(input)
  output$TSA_VOLCANO_PLOT <- plot_TSA_VOLCANO(input)
  output$TSA_VOLCANO_TEXTOUTPUT <- get_TSA_VOLCANO_text(input)
  output$TSA_SCORES_PLOT <- plot_TSA_SCORES(input)
  output$TSA_SCORES_TEXTOUTPUT <- get_TSA_SCORES_text(input)
  output$TSA_LRIFC_PLOT <- plot_TSA_LRIFC(input)
  output$TSA_LRIFC_TEXTOUTPUT <- get_TSA_LRIFC_text(input)
  ## TSA ORA sidebar ####
  output$TSA_ORA_CATEGORY_CHOICE <- choose_TSA_ORA_category(input)
  ## TSA ORA mainpanel ####
  output$TSA_ORA_DETAILS <- get_TSA_ora_details(input)
  output$TSA_ORA_TABLE <- get_TSA_ORA_table(input)
  output$TSA_ORA_PLOT <- plot_TSA_ORA(input)
  output$TSA_ORA_NETWORK_PLOT <- plot_TSA_ORA_network(input)
  ## TCA overall ####
  source("utils_combined_analysis.R", local = TRUE)
  output$TCA_DATASET_CHOICE <- choose_TCA_dataset(input)
  output$TCA_TITLE <- get_TCA_title(input)
  output$TCA_OVERVIEW_TABLE <- get_TCA_overview_table(input)
  ## TCA CCI sidebar ####
  output$TCA_DOWNLOAD_TABLE <- download_TCA_table(input)
  output$TCA_TISSUE_CHOICE <- choose_TCA_tissue(input)
  #output$TCA_LRI_CHOICE <- choose_TCA_lri(input)
  observe({
    req(input$TCA_DATASET_CHOICE)
    ALL_LRI_LABEL = 'All LRI'
    choices <-
      c(ALL_LRI_LABEL,
        sort(
          unique(
            DATASETS_COMBINED[[input$TCA_DATASET_CHOICE]]@cci_detected[["LR_GENES"]]))
      )
    updateSelectizeInput(
      session = session,
      inputId = 'TCA_LRI_CHOICE',
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
  ## TCA CCIs mainpanel ####
  output$TCA_CCI_DETAILS <- get_TCA_cci_details(input)
  output$TCA_CCI_TEXTOUTPUT <- get_TCA_cci_text(input)
  output$TCA_INTERACTION_TABLE <- get_TCA_interaction_table(input)
  output$TCA_VOLCANO_PLOT <- plot_TCA_VOLCANO(input)
  output$TCA_VOLCANO_TEXTOUTPUT <- get_TCA_VOLCANO_text(input)
  output$TCA_SCORES_PLOT <- plot_TCA_SCORES(input)
  output$TCA_SCORES_TEXTOUTPUT <- get_TCA_SCORES_text(input)
  output$TCA_LRIFC_PLOT <- plot_TCA_LRIFC(input)
  output$TCA_LRIFC_TEXTOUTPUT <- get_TCA_LRIFC_text(input)
  ## TCA GLOBAL sidebar ####
  ## TCA GLOBAL mainpanel ####
  output$TCA_GLOBAL_DETAILS <- get_TCA_global_details(input)
  output$TCA_GLOBAL_TABLE <- get_TCA_global_table(input)
  output$TCA_GLOBAL_ORA_PLOT <- plot_TCA_global_ora(input)
  ## TCA LRI sidebar ####
  output$TCA_LRI_SUMMARY_CHOICE <- choose_TCA_lri_summary(input)
  ## TCA LRI mainpanel
  output$TCA_LRI_SUMMARY <- get_TCA_lri_summary(input)
  # source("utils_LRdb.R", local = TRUE)
  # output$LRdb_TABLE <- show_LRdb_table(input)
  # output$LRdb_UPSET_PLOT <- show_LRdb_upset(input)
}

