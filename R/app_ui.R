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
                    style = "max-width: 100%; width: 200px; height: auto;"
                  ),
                  tags$img(
                    src = "img/hfsp_logo.jpg",
                    style = "max-width: 100%; width: 200px; height: auto;"
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
      ),
      navbarPage(
        title = tags$div(
          tags$a(
            href = "https://www.nature.com/articles/s43587-023-00514-x",
            tags$p(
              paste(
                "Please consider citing:"
              ),
              tags$em(
                paste(
                  "scDiffCom: a tool for differential analysis of cell-cell",
                  "interactions provides a mouse atlas of aging changes in",
                  "intercellular communication."
                )
              ),
              paste(
                "Cyril Lagger, Eugen Ursu, Anais Equey, Roberto A Avelar,",
                "Angela O Pisco, Robi Tacutu, Joao Pedro de Magalhaes."
              ),
              style = "text-align: center;color:white;font-size: 12px;"
            ),
            target = "_blank"
          )
        ),
        position = "fixed-bottom",
        id = "navbarBottom"
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