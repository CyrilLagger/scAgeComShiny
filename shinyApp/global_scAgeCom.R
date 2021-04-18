## Load libraries ####

library(shiny)
library(shinythemes)
library(htmlwidgets)
library(shinyWidgets)
library(DT)
library(plotly)
library(bsplus)

library(data.table)
library(ggplot2)
library(ComplexUpset)

library(igraph)
library(RColorBrewer)
library(purrr)
library(kableExtra)
library(visNetwork)
library(stringr)

library(scDiffCom)


## Load data and functions ####

scAgeCom_data <- readRDS("data/scAgeCom_shiny_data.rds")
