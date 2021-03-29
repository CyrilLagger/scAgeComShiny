
## INTRO MAIN TITLE ####

get_intro_title <- function(
  input
) {
  renderUI({
      scAgeCom_data$shiny_html_content$intro_title
    })
}

## INTRO OVERVIEW mainpanel ####

get_intro_overview <- function(
  input
) {
  renderUI({
    scAgeCom_data$shiny_html_content$intro_overview
  })
}

## INTRO METHOD mainpanel ####

get_intro_method <- function(
  input
) {
  renderUI({
      scAgeCom_data$shiny_html_content$intro_method
    })
}

## INTRO SCRNA DATA mainpanel ####

get_intro_scrna_data <- function(
  input
) {
  renderUI({
    scAgeCom_data$shiny_html_content$intro_scrna_data
  })
}

## INTRO LRI sidebar ####

choose_intro_lri_database <- function(
  input
) {
  renderUI({
    pickerInput(
      inputId = "LRI_DATABASE",
      label = "Databases",
      choices = scAgeCom_data$LRI_DATABASES,
      selected = scAgeCom_data$LRI_DATABASES,
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    )
  })
}

## INTRO LRI mainpanel ####

get_intro_lri_table <- function(
  input
) {
  DT::renderDataTable({
    req(
      input$LRI_DATABASE,
      input$INTRO_LRI_SPECIES_CHOICE
    )
    if (input$INTRO_LRI_SPECIES_CHOICE == "Mouse") {
      dt <- scAgeCom_data$LRI_mouse_curated[
        apply(
          sapply(
            input$LRI_DATABASE,
            function(i) {
              grepl(i, `Database of Origin`)
            }
          ),
          MARGIN = 1,
          any
        )
      ]
    }
    if (input$INTRO_LRI_SPECIES_CHOICE == "Human") {
      dt <- scAgeCom_data$LRI_human_curated[
        apply(
          sapply(
            input$LRI_DATABASE,
            function(i) {
              grepl(i, `Database of Origin`)
            }
          ),
          MARGIN = 1,
          any
        )
      ]
    }
    options_LRI <- list(
      pageLength = 10,
      columnDefs = list(
        list(
          targets = c(6,7),
          render = htmlwidgets::JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 20 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
            "}")
        )
      )
    )
    scAgeCom_data$show_DT(
      data = dt,
      cols_to_show = colnames(dt)[1:7],
      cols_numeric = NULL,
      table_title = "Table of Ligand-Receptor Interactions",
      options = options_LRI
    )
  })
}

plot_intro_lri_upset <- function(
  input
) {
  renderPlot({
    req(input$INTRO_LRI_SPECIES_CHOICE)
    if (input$INTRO_LRI_SPECIES_CHOICE == "Mouse") {
      scAgeCom_data$plot_lri_upset(
        LRI_table = scAgeCom_data$LRI_mouse_curated,
        groups = colnames(scAgeCom_data$LRI_mouse_curated)[9:16]
      )
    } else if (input$INTRO_LRI_SPECIES_CHOICE == "Human") {
      scAgeCom_data$plot_lri_upset(
        LRI_table = scAgeCom_data$LRI_human_curated,
        groups = colnames(scAgeCom_data$LRI_human_curated)[9:16]
      )
    }
  })
}

get_intro_lri_html <- function(
  input
) {
  renderUI({
    scAgeCom_data$shiny_html_content$intro_lri
  })
}

get_intro_lri_details <- function(
  input
) {
  renderUI({
    req(input$INTRO_LRI_DETAILS_CHOICE)
    if (input$INTRO_LRI_DETAILS_CHOICE == "LRI Table") {
      DT::dataTableOutput("INTRO_LRI_TABLE")
    } else if (input$INTRO_LRI_DETAILS_CHOICE == "Upset Plot by Source") {
      plotOutput("INTRO_LRI_UPSET_PLOT", height = "600px")
    } else if (input$INTRO_LRI_DETAILS_CHOICE == "References") {
      htmlOutput("INTRO_LRI_HTML")
    }
  })
}
