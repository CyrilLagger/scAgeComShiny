
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
  renderPlot({
    req(
      input$TCA_KEYWORD_CATEGORY_CHOICE,
      input$TCA_KEYWORD_VALUE_CHOICE
      )
    scAgeCom_data$plot_keyword_tissue_vs_dataset(
      scAgeCom_data$ORA_KEYWORD_SUMMARY_UNIQUE,
      scAgeCom_data$ORA_KEYWORD_TEMPLATE,
      input$TCA_KEYWORD_CATEGORY_CHOICE,
      input$TCA_KEYWORD_VALUE_CHOICE
    )
  })
}
