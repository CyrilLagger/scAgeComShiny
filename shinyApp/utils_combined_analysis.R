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
    div(style="display: inline-block;", "Choose a title for here: ")
  )
})

output$TCA_PANEL_VIEW <- renderUI({
  tabsetPanel(
    type = "tabs",
    tabPanel(
      title = "Summary Table",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          uiOutput("TCA_GLOBAL_TABLE_CHOICE"),
          uiOutput("TCA_GLOBAL_ORA_REGULATION_CHOICE")
        ),
        mainPanel(
          uiOutput("TCA_GLOBAL_DETAILS")
        )
      ),
      value = "TCA_SUMMARY_TABLE"
    ),
    tabPanel(
      title = "Keyword summary",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          uiOutput("TCA_KEYWORD_CATEGORY_CHOICE"),
          uiOutput("TCA_KEYWORD_VALUE_CHOICE")
        ),
        mainPanel(
          fluidRow(
            column(
              width = 12,
              plotly::plotlyOutput("TCA_KEYWORD_SUMMARY",  height = "800px"),
              style = "padding:50px"
            )
          )
        )
      ),
      value = "TCA_KEYWORD_SUMMARY"
    ),
    id = "active_TCA_panel"
  )
})

output$TCA_GLOBAL_TABLE_CHOICE <- renderUI({
  selectInput(
    inputId = "TCA_GLOBAL_TABLE_CHOICE",
    label = "Category",
    choices = scAgeCom_data$ALL_GLOBAL_CATEGORIES
  )
})

output$TCA_GLOBAL_ORA_REGULATION_CHOICE <- renderUI({
  selectInput(
    inputId = "TCA_GLOBAL_ORA_REGULATION_CHOICE",
    label = "ORA Regulation",
    choices = scAgeCom_data$ALL_ORA_TYPES
  )
})

output$TCA_GLOBAL_DETAILS <- renderUI({
  fluidRow(
    column(
      width = 12,
      DT::dataTableOutput("TCA_GLOBAL_TABLE"),
      style = "padding:50px"
    )
  )
})

output$TCA_GLOBAL_TABLE <- DT::renderDT({
  req(
    input$TCA_GLOBAL_TABLE_CHOICE,
    input$TCA_GLOBAL_ORA_REGULATION_CHOICE
    )
  scAgeCom_data$build_GLOBAL_display(
    ORA_KEYWORD_COUNTS = scAgeCom_data$ORA_KEYWORD_COUNTS,
    global_category = input$TCA_GLOBAL_TABLE_CHOICE,
    global_type = input$TCA_GLOBAL_ORA_REGULATION_CHOICE
  )
})

output$TCA_KEYWORD_CATEGORY_CHOICE <- renderUI({
  selectInput(
    inputId = "TCA_KEYWORD_CATEGORY_CHOICE",
    label = "Category",
    choices = scAgeCom_data$ALL_GLOBAL_CATEGORIES
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

output$TCA_KEYWORD_SUMMARY <- plotly::renderPlotly({
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

observeEvent(
  input$TCA_KEYWORD_CATEGORY_CHOICE,
  {
    req(input$TCA_KEYWORD_CATEGORY_CHOICE)
    updateSelectInput(
      session = session,
      'TCA_KEYWORD_CATEGORY_CHOICE',
      selected = input$TCA_KEYWORD_CATEGORY_CHOICE
    )
    choices <- sort(unique(scAgeCom_data$ORA_KEYWORD_COUNTS[
      TYPE == input$TCA_KEYWORD_CATEGORY_CHOICE
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






