#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    fluidPage(
      theme = shinythemes::shinytheme("cerulean"),
      tags$head(
        tags$style(
          type = 'text/css',
          "body {padding-bottom: 70px;}"
          #'.navbar-brand{display:none;}'
        )
        # tags$script(
        #   src = "https://kit.fontawesome.com/8deb5c53bb.js",
        #   crossorigin = "anonymous"
        # ),
      ),
      titlePanel(
        title = tags$table(
          style = "width: 100%",
          tags$tbody(
            tags$tr(
              tags$td(
                tags$span(
                  style = "font-size: 26px",
                  paste(
                    "A Murine Atlas of Age-related Changes in Intercellular",
                    "Communication"
                  )
                )
              ),
              tags$td(
                style = "text-align: left;",
                tags$div(
                  tags$img(
                    src = "img/uol_logo.png",
                    style = "width: 200px;"
                  ),
                  tags$img(
                    src = "img/hfsp_logo.jpg",
                    style = "width: 200px;"
                  )
                )
              )
            )
          )
        ),
        windowTitle = "scAgeCom"
      ),
      navbarPage(
        title = NULL,
        id = "navbarID",
        tabPanel(
          title = "scAgeCom",
          mod_introduction_ui("introduction_ui_1"),
          value = "INTRO_navbar"
        ),
        tabPanel(
          title = "Tissue-specific Results",
          mod_tsa_ui("tsa_ui_1"),
          value = "TSA_navbar"
        ),
        tabPanel(
          title = "Cross-tissue Results",
          mod_tca_ui("tca_ui_1"),
          value = "TCA_navbar"
        ),
        tabPanel(
          title = "Help/Glossary",
          mod_glossary_ui("glossary_ui_1"),
          value = "HELP_navbar"
        )
      )#,
      # navbarPage(
      #   title = tags$div(
      #     tags$p(
      #       paste(
      #         "Please consider citing our preprint: (in preparation, stay",
      #         "tuned!) "
      #       )
      #     )
      #     # tags$a(
      #     #   href = "aaa",
      #     #   paste(
      #     #     "Please consider citing our preprint: (in preparation, stay",
      #     #     "tuned!) "
      #     #   ),
      #     #   style = "text-align: center;color:white;font-size: 14px;"
      #     # )
      #   ),
      #   position = "fixed-bottom",
      #   id = "navbarBottom"
      # )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  add_resource_path(
    'www',
    app_sys('app/www')
  )
  addResourcePath(
  'img',
  app_sys('app/img')
  )
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'scAgeComShiny'
    )
  )
}