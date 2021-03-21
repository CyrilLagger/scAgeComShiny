####################################################
##
## Project: scAgeComShiny
##
## Last update - March 2021
##
## cyril.lagger@liverpool.ac.uk
## ursu_eugen@hotmail.com
## anais.equey@etu.univ-amu.fr
##
##
####################################################
##

## Source code ####
source("global_scAgeCom.R", local = TRUE)
source("ui_scAgeCom.R", local = TRUE)
source("server_scAgeCom.R", local = TRUE)

## Options ####
#options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
#options(htmlwidgets.TOJSON_ARGS = NULL)
#options("DT.TOJSON_ARGS" = NULL)

## Main shinyApp call ####

shinyApp(
  ui = ui_scAgeCom,
  server = server_scAgeCom
)
