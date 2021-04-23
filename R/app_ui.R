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
          '.navbar-brand{display:none;}'
        )
        # tags$script(
        #   src = "https://kit.fontawesome.com/8deb5c53bb.js",
        #   crossorigin = "anonymous"
        # ),
      ),
      titlePanel(
        title = tags$div(
          style = "font-size: 26px",
          paste(
            "A Murine ATLAS of Age-related Changes in Intercellular",
            "Communication (Development Website!)."
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
          title = "Tissue Specific Results",
          mod_tsa_ui("tsa_ui_1"),
          value = "TSA_navbar"
        ),
        tabPanel(
          title = "Global Comparison",
          mod_tca_ui("tca_ui_1"),
          value = "TCA_navbar"
        ),
        tabPanel(
          title = "Glossary",
          mod_glossary_ui("glossary_ui_1"),
          value = "HELP_navbar"
        )
      )
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
    'www', app_sys('app/www')
  )
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'scAgeComShiny'
    )
  )
}