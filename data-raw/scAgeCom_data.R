## code to prepare `scAgeCom_data` dataset goes here

# the data for this app are produced in the scAgeCom project
# here we retrieve them locally

scAgeCom_data <- readRDS(
  "../data_scAgeCom/analysis/outputs_data/scAgeCom_shiny_data.rds"
)

usethis::use_data(scAgeCom_data, overwrite = TRUE)
