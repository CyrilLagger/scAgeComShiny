####################################################
##
## Project: scAgeComShiny
##
## Last update - April 2021
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

## Main shinyApp call ####

shinyApp(
  ui = ui_scAgeCom,
  server = server_scAgeCom
)
