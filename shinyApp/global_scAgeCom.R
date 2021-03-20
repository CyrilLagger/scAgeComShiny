## Load libraries ####

library(shiny)
library(htmlwidgets)
library(shinyWidgets)
library(DT)

library(data.table)
library(ggplot2)
library(ComplexUpset)
library(scDiffCom)

library(ggExtra)
library(igraph)
library(RColorBrewer)
library(purrr)
library(visNetwork)


## Load data and functions ####

scAgeCom_data <- readRDS("data/scAgeCom_shiny_data.rds")
