
output$INTRO_PAGE_VIEW <- renderUI({
  fluidPage(
    fluidRow(
      column(
        width = 6,
        titlePanel(htmlOutput("INTRO_TITLE")),
        offset = 3
      )
    ),
    fluidRow(
      column(
        width = 8,
        offset = 2,
        htmlOutput("INTRO_OVERVIEW")#,
        #style = "padding:50px 100px 10px 100px"
      )
    ),
    fluidRow(
      column(
        width = 8,
        offset = 2,
        bs_accordion(id = "INTRO_METHOD_DETAIL") %>%
          bs_append(
            title = htmlOutput("INTRO_CODE_TITLE"),
            content = htmlOutput("INTRO_CODE_TEXT")
          ) %>%
          bs_append(
            title = htmlOutput("INTRO_SCRNA_TITLE"),
            content = htmlOutput("INTRO_SCRNA_TEXT")
          ) %>%
          bs_append(
            title = htmlOutput("INTRO_LRI_TITLE"),
            content = uiOutput("INTRO_LRI_TEXT")
          ) %>%
          bs_append(
            title = htmlOutput("INTRO_CONTACT_TITLE"),
            content = htmlOutput("INTRO_CONTACT_TEXT")
          ),
        style = "padding:10px"
      )
    )
  )
})

output$INTRO_TITLE <- renderUI({
  scAgeCom_data$shiny_html_content$intro_title
})

output$INTRO_OVERVIEW <- renderUI({
  scAgeCom_data$shiny_html_content$intro_overview
})

output$INTRO_CODE_TITLE<- renderUI({
  scAgeCom_data$shiny_html_content$intro_code_title
})

output$INTRO_CODE_TEXT <- renderUI({
  scAgeCom_data$shiny_html_content$intro_code_text
})

output$INTRO_SCRNA_TITLE <- renderUI({
  scAgeCom_data$shiny_html_content$intro_scrna_title
})

output$INTRO_SCRNA_TEXT <- renderUI({
  scAgeCom_data$shiny_html_content$intro_scrna_text
})

output$INTRO_LRI_TITLE <- renderUI({
  scAgeCom_data$shiny_html_content$intro_lri_title
})

output$INTRO_LRI_TEXT <- renderUI({
  fluidPage(
    fluidRow(
      column(
        width = 6,
        htmlOutput("INTRO_LRI_HTML_TEXT")
      ),
      column(
        width = 6,
        htmlOutput("INTRO_LRI_HTML_DB_LIST")
      )
    ),
    fluidRow(
      column(
        width = 12,
        DT::dataTableOutput("INTRO_LRI_TABLE")
      )
    ),
    fluidRow(
      column(
        width = 10,
        offset = 1,
        plotOutput("INTRO_LRI_UPSET_PLOT", height = "500px")
      )
    )
  )
})

output$INTRO_CONTACT_TITLE <- renderUI({
  scAgeCom_data$shiny_html_content$intro_contact_title
})

output$INTRO_CONTACT_TEXT <- renderUI({
  scAgeCom_data$shiny_html_content$intro_contact_text
})

output$INTRO_LRI_HTML_TEXT <- renderUI({
  scAgeCom_data$shiny_html_content$intro_lri_text
})

output$INTRO_LRI_HTML_DB_LIST <- renderUI({
  scAgeCom_data$shiny_html_content$intro_lri_db_list
})

output$INTRO_LRI_TABLE <- DT::renderDataTable({
  scAgeCom_data$build_LRI_display(
    LRI_table = scAgeCom_data$LRI_mouse_curated,
    LRI_database = scAgeCom_data$LRI_DATABASES
  )
})

output$INTRO_LRI_UPSET_PLOT <- renderPlot({
  scAgeCom_data$plot_lri_upset(
    LRI_table = scAgeCom_data$LRI_mouse_curated,
    groups = colnames(scAgeCom_data$LRI_mouse_curated)[9:16],
    min_size = 40
  )
})

