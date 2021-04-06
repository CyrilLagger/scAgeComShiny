
output$INTRO_TOP_VIEW <- renderUI({
  fluidRow(
    column(
      width = 6,
      titlePanel(htmlOutput("INTRO_TITLE")),
      offset = 3
    ),
  )
})

output$INTRO_TITLE <- renderUI({
  scAgeCom_data$shiny_html_content$intro_title
})

output$INTRO_PANEL_VIEW <- renderUI({
  tabsetPanel(
    type = "tabs",
    tabPanel(
      title = "Overview",
      fluidRow(
        column(
          width = 12,
          htmlOutput("INTRO_OVERVIEW"),
          style = "padding:50px"
        )
      ),
      value = "INTRO_OVERVIEW_TAB"
    ),
    tabPanel(
      title = "Methodology",
      fluidRow(
        column(
          width = 12,
          htmlOutput("INTRO_METHOD"),
          style = "padding:50px"
        )
      ),
      value = "INTRO_METHOD_TAB"
    ),
    tabPanel(
      title = "Single-cell Data",
      fluidRow(
        column(
          width = 12,
          htmlOutput("INTRO_SCRNA_DATA"),
          style = "padding:50px"
        )
      ),
      value = "INTRO_SCRNA_DATA_TAB"
    ),
    tabPanel(
      title = "Ligand-Receptor Interaction Database",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(
            inputId = "INTRO_LRI_DETAILS_CHOICE",
            label = "Choose information to display",
            choices = c(
              "LRI Table",
              "Upset Plot by Source",
              "References"
            )
          ),
          conditionalPanel(
            condition = "input.INTRO_LRI_DETAILS_CHOICE != 'References'",
            selectInput(
              inputId = "INTRO_LRI_SPECIES_CHOICE",
              label = "Choose a Species",
              choices = c(
                "Mouse",
                "Human"
              )
            )
          ),
          conditionalPanel(
            condition = "input.INTRO_LRI_DETAILS_CHOICE == 'LRI Table'",
            uiOutput("INTRO_LRI_DATABASE_CHOICE")
          )
        ),
        mainPanel(
          fluidRow(
            column(
              width = 12,
              uiOutput("INTRO_LRI_DETAILS"),
              style = "padding:50px"
            )
          )
        )
      ),
      value = "INTRO_LRI_DATABASE_TAB"
    ),
    id = "active_INTRO_panel"
  )
})

output$INTRO_OVERVIEW <- renderUI({
  scAgeCom_data$shiny_html_content$intro_overview
})

output$INTRO_METHOD <- renderUI({
  scAgeCom_data$shiny_html_content$intro_method
})

output$INTRO_SCRNA_DATA <- renderUI({
  scAgeCom_data$shiny_html_content$intro_scrna_data
})

output$INTRO_LRI_DATABASE_CHOICE <- renderUI({
  pickerInput(
    inputId = "LRI_DATABASE",
    label = "Databases",
    choices = scAgeCom_data$LRI_DATABASES,
    selected = scAgeCom_data$LRI_DATABASES,
    options = list(`actions-box` = TRUE),
    multiple = TRUE
  )
})

output$INTRO_LRI_TABLE <- DT::renderDataTable({
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

output$INTRO_LRI_UPSET_PLOT <- renderPlot({
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

output$INTRO_LRI_HTML <- renderUI({
  scAgeCom_data$shiny_html_content$intro_lri
})

output$INTRO_LRI_DETAILS <- renderUI({
  req(input$INTRO_LRI_DETAILS_CHOICE)
  if (input$INTRO_LRI_DETAILS_CHOICE == "LRI Table") {
    DT::dataTableOutput("INTRO_LRI_TABLE")
  } else if (input$INTRO_LRI_DETAILS_CHOICE == "Upset Plot by Source") {
    plotOutput("INTRO_LRI_UPSET_PLOT", height = "600px")
  } else if (input$INTRO_LRI_DETAILS_CHOICE == "References") {
    htmlOutput("INTRO_LRI_HTML")
  }
})

