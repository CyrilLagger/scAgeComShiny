
output$INTRO_PAGE_VIEW <- renderUI({
  fluidRow(
    column(
      width = 6,
      titlePanel(htmlOutput("INTRO_TITLE")),
      offset = 3
    ),
    column(
      width = 12,
      htmlOutput("INTRO_OVERVIEW"),
      style = "padding:50px"
    ),
    # column(
    #   width = 12,
    #   htmlOutput("INTRO_METHOD_TITLE"),
    #   style = "padding:50px"
    # ),
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
          content = htmlOutput("INTRO_LRI_TEXT")
          ) %>%
        bs_append(
          title = htmlOutput("INTRO_CONTACT_TITLE"),
          content = htmlOutput("INTRO_CONTACT_TEXT")
        ),
      style = "padding:50px"
    )
  )
})

output$INTRO_TITLE <- renderUI({
  scAgeCom_data$shiny_html_content$intro_title
})

output$INTRO_OVERVIEW <- renderUI({
  scAgeCom_data$shiny_html_content$intro_overview
})

# output$INTRO_METHOD_TITLE <- renderUI({
#   scAgeCom_data$shiny_html_content$intro_method_title
# })

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
  fluidRow(
    # htmlOutput("INTRO_LRI_HTML"),
    # DT::dataTableOutput("INTRO_LRI_TABLE"),
    # plotOutput("INTRO_LRI_UPSET_PLOT", height = "600px")
    column(
      width = 12,
      htmlOutput("INTRO_LRI_HTML"),
      style = "padding:30px"
    ),
    column(
      width = 12,
      DT::dataTableOutput("INTRO_LRI_TABLE"),
      style = "padding:30px"
    ),
    column(
      width = 12,
      plotOutput("INTRO_LRI_UPSET_PLOT", height = "600px"),
      style = "padding:30px"
    )
  )
})

output$INTRO_CONTACT_TITLE <- renderUI({
  scAgeCom_data$shiny_html_content$intro_contact_title
})

output$INTRO_CONTACT_TEXT <- renderUI({
  scAgeCom_data$shiny_html_content$intro_contact_text
})

output$INTRO_LRI_HTML <- renderUI({
  scAgeCom_data$shiny_html_content$intro_lri_text
})

output$INTRO_LRI_TABLE <- DT::renderDataTable({
  # req(
  #   input$LRI_DATABASE,
  #   input$INTRO_LRI_SPECIES_CHOICE
  # )
  # if (input$INTRO_LRI_SPECIES_CHOICE == "Mouse") {
  #   dt <- scAgeCom_data$LRI_mouse_curated
  # }
  # if (input$INTRO_LRI_SPECIES_CHOICE == "Human") {
  #   dt <- scAgeCom_data$LRI_human_curated
  # }
  scAgeCom_data$build_LRI_display(
    LRI_table = scAgeCom_data$LRI_mouse_curated,
    LRI_database = scAgeCom_data$LRI_DATABASES
  )
})

output$INTRO_LRI_UPSET_PLOT <- renderPlot({
  #req(input$INTRO_LRI_SPECIES_CHOICE)
  # if (input$INTRO_LRI_SPECIES_CHOICE == "Mouse") {
  #   scAgeCom_data$plot_lri_upset(
  #     LRI_table = scAgeCom_data$LRI_mouse_curated,
  #     groups = colnames(scAgeCom_data$LRI_mouse_curated)[9:16]
  #   )
  # } else if (input$INTRO_LRI_SPECIES_CHOICE == "Human") {
  #   scAgeCom_data$plot_lri_upset(
  #     LRI_table = scAgeCom_data$LRI_human_curated,
  #     groups = colnames(scAgeCom_data$LRI_human_curated)[9:16]
  #   )
  # }
  scAgeCom_data$plot_lri_upset(
    LRI_table = scAgeCom_data$LRI_mouse_curated,
    groups = colnames(scAgeCom_data$LRI_mouse_curated)[9:16]
  )
})

# output$INTRO_LRI_DATABASE_CHOICE <- renderUI({
#   pickerInput(
#     inputId = "LRI_DATABASE",
#     label = "Databases",
#     choices = scAgeCom_data$LRI_DATABASES,
#     selected = scAgeCom_data$LRI_DATABASES,
#     options = list(`actions-box` = TRUE),
#     multiple = TRUE
#   )
# })

