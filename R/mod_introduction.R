#' introduction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_introduction_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("INTRO_PAGE_VIEW"))
}

#' introduction Server Functions
#'
#' @noRd 
mod_introduction_server <- function(id) {
  moduleServer(
    id,
    function(
      input,
      output,
      session
    ) {
      ns <- session$ns
      
      ## define style to be reused multiple times ####
      
      style_intro_accordion_title <- paste(
        "font-size: 20px;",
        "margin-top: 0;",
        "margin-bottom: 0;"
      )
      style_intro_accordion_text <- paste(
        "margin:auto;",
        "font-size: 15px;",
        "text-align: justify;"
      )
      
      ## outputs #####
      
      output$INTRO_PAGE_VIEW <- renderUI({
        fluidPage(
          fluidRow(
            column(
              width = 6,
              offset = 3,
              titlePanel(htmlOutput(ns("INTRO_TITLE")))
            )
          ),
          fluidRow(
            column(
              width = 8,
              offset = 2,
              htmlOutput(ns("INTRO_OVERVIEW"))
            )
          ),
          fluidRow(
            column(
              width = 8,
              offset = 2,
              bsplus::bs_accordion(id = "INTRO_METHOD_DETAIL") %>%
                bsplus::bs_append(
                  title = htmlOutput(ns("INTRO_CODE_TITLE")),
                  content = htmlOutput(ns("INTRO_CODE_TEXT"))
                ) %>%
                bsplus::bs_append(
                  title = htmlOutput(ns("INTRO_SCRNA_TITLE")),
                  content = htmlOutput(ns("INTRO_SCRNA_TEXT"))
                ) %>%
                bsplus::bs_append(
                  title = htmlOutput(ns("INTRO_LRI_TITLE")),
                  content = uiOutput(ns("INTRO_LRI_CONTENT"))
                ) %>%
                bsplus::bs_append(
                  title = htmlOutput(ns("INTRO_CONTACT_TITLE")),
                  content = htmlOutput(ns("INTRO_CONTACT_TEXT"))
                ),
              style = "padding: 10px"
            )
          )
        )
      })
      
      output$INTRO_TITLE <- renderUI({
        tags$p(
          style = paste(
            "margin: 20px auto;",
            "font-size: 26px;",
            "text-align: center;"
          ),
          "Welcome to scAgeCom!"
        )
      })
      
      output$INTRO_OVERVIEW <- renderUI({
        tags$div(
          style = paste(
            "margin:auto;",
            "font-size: 15px;",
            "text-align: justify;"
          ),
          tags$p(
            tags$b(
              paste(
                "Explore how intercellular communication changes with age",
                "in 23 mouse tissues."
              )
            )
          ),
          tags$p(
            paste(
              "This website includes both tissue specific results and",
              "a global comparison of the changes shared accross",
              "the different organs.",
              "The full methodology behind this analysis is described in our",
              "(manuscript in preparation) but the most important steps",
              "are highligthed below."
            )
          )
        )
      })
      
      output$INTRO_CODE_TITLE<- renderUI({
        tags$h3(
          style = style_intro_accordion_title,
          "Methodology and Code",
        )
      })
      
      output$INTRO_CODE_TEXT <- renderUI({
        tags$div(
          style = style_intro_accordion_text,
          tags$h4("The package"),
          tags$p(
            "We built the R package",
            tags$a(
              href = "https://github.com/CyrilLagger/scDiffCom",
              "scDiffCom",
              target = "_blank"
            ),
            paste(
              "to inverstigate how intercellular communication varies between",
              "two biological conditions of interest (young/old, sick/healthy, etc.).",
              "Here, we have used scDiffCom to study differences in intercellular",
              "comunication between young and old murine cells. Please visit"
            ),
            tags$a(
              href = "https://cyrillagger.github.io/scDiffCom/articles/scDiffCom-vignette.html",
              "this tutorial",
              target = "_blank"
            ),
            "to apply this package to your own data."
          ),
          tags$h4("How does it work?"),
          tags$p(
            paste(
              "scDiffCom can be applied to any scRNA-seq dataset",
              "(defined as a "
            ),
            tags$a(
              href = "https://satijalab.org/seurat/index.html",
              "Seurat object",
              target = "_blank",
              .noWS = "outside"
            ),
            paste(
              "). It uses its own integrated database of ligand-receptor interactions",
              "to list all the potential signals between the cell types of the dataset."
            )
          ),
          tags$p(
            paste(
              "Statistical tests are then performed to only retain biologically",
              "significant interactions and to measure their change between",
              "the two conditions. Finally, an over-representation test allows the",
              "package to infer the dominant",
              "changing patterns on a gene-centric, cell type-centric or",
              "annotation-centric (GO/KEGG) level."
            ),
            .noWS = c("after-begin", "before-end")
          ),
          tags$h4("Find our scripts on GitHub"),
          tags$ol(
            tags$li(
              "Package for differential analysis on any dataset:",
              tags$a(
                href = "https://github.com/CyrilLagger/scDiffCom",
                "scDiffCom",
                target = "_blank"
              )
            ),
            tags$li(
              "Code for this aging analysis:",
              tags$a(
                href = "https://github.com/CyrilLagger/scAgeCom",
                "scAgeCom",
                target = "_blank"
              )
            ),
            tags$li(
              "Code for this website:",
              tags$a(
                href = "https://github.com/CyrilLagger/scAgeComShiny",
                "scAgeComShiny",
                target = "_blank"
              )
            )
          )
        )
      })
      
      output$INTRO_SCRNA_TITLE <- renderUI({
        tags$h3(
          style = style_intro_accordion_title,
          "Aging Single Cell Datasets"
        )
      })
      
      output$INTRO_SCRNA_TEXT <- renderUI({
        tags$div(
          style = style_intro_accordion_text,
          tags$p(
            paste(
              "We based our analysis on several murine scRNA-seq datasets",
              "provided by the two studies below:"
            )
          ),
          tags$ul(
            tags$li(
              "Tabula Muris Senis (TMS):",
              tags$ul(
                tags$li(
                  tags$a(
                    href = "https://tabula-muris-senis.ds.czbiohub.org/",
                    "webpage",
                    target = "_blank"
                  )
                ),
                tags$li(
                  tags$a(
                    href = "https://www.nature.com/articles/s41586-020-2496-1",
                    "Nature article",
                    target = "_blank",
                    .noWS = "outside"
                  )
                )
              )
            ),
            tags$li(
              "Calico2019:",
              tags$ul(
                tags$li(
                  tags$a(
                    href = "https://mca.research.calicolabs.com/",
                    "webpage",
                    target = "_blank"
                  )
                ),
                tags$li(
                  tags$a(
                    href = "https://genome.cshlp.org/content/29/12/2088",
                    "Genome Research article",
                    target = "_blank",
                    .noWS = "outside"
                  )
                )
              )
            )
          ),
          tags$p(
            "Overall, this represents data for 23 organs (give here a summary picture)"
          )
        )
      })
      
      output$INTRO_LRI_TITLE <- renderUI({
        tags$h3(
          style = style_intro_accordion_title,
          "Collection of Ligand-Receptor Interactions"
        )
      })
      
      output$INTRO_LRI_CONTENT <- renderUI({
        fluidPage(
          fluidRow(
            column(
              width = 6,
              htmlOutput(ns("INTRO_LRI_HTML_TEXT"))
            ),
            column(
              width = 6,
              htmlOutput(ns("INTRO_LRI_HTML_DB_LIST"))
            )
          ),
          fluidRow(
            column(
              width = 12,
              DT::dataTableOutput(ns("INTRO_LRI_TABLE"))
            )
          ),
          fluidRow(
            column(
              width = 10,
              offset = 1,
              plotOutput(
                ns("INTRO_LRI_UPSET_PLOT"),
                height = "500px"
              )
            )
          )
        )
      })
      
      output$INTRO_LRI_HTML_TEXT <- renderUI({
        tags$div(
          style = style_intro_accordion_text,
          tags$p(
            "scDiffCom internally relies on a list of ligand-receptor interactions",
            "that we have processed and combined from the eight studies in the list",
            "opposite. We carefully took into account the interactions involving",
            "heteromeric complexes (see our manuscript in preparation)."
          ),
          tags$p(
            "You will find below the mouse interactions for the scAgeCom analysis.",
            "This table and its human equivalent can also be directly accessed",
            "in the scDiffCom package."
          )
        )
      })
      
      output$INTRO_LRI_HTML_DB_LIST <- renderUI({
        tags$ul(
          tags$li(
            "CellChat:",
            tags$a(
              href = "http://www.cellchat.org/", "webpage",
              target = "_blank"
            ),
            " and ",
            tags$a(
              href = "https://www.nature.com/articles/s41467-021-21246-9",
              "Nature Communications article",
              target = "_blank",
              .noWS = "outside"
            ),
            "."
          ),
          tags$li(
            "CellPhoneDB:",
            tags$a(
              href = "https://www.cellphonedb.org/",
              "webpage",
              target = "_blank"
            ),
            " and ",
            tags$a(
              href = "https://www.nature.com/articles/s41596-020-0292-x",
              "Nature Protocol article",
              target = "_blank",
              .noWS = "outside"
            ),
            "."
          ),
          tags$li(
            "CellTalkDB:",
            tags$a(
              href = "http://tcm.zju.edu.cn/celltalkdb/",
              "webpage",
              target = "_blank"
            ),
            " and ",
            tags$a(
              href = "https://academic.oup.com/bib/advance-article-abstract/doi/10.1093/bib/bbaa269/5955941",
              "Briefings in Bioinformatics article",
              target = "_blank",
              .noWS = "outside"
            )
          ),
          tags$li(
            "connectomeDB2020:",
            tags$a(
              href = "https://github.com/forrest-lab/NATMI",
              "webpage",
              target = "_blank"
            ),
            " and ",
            tags$a(
              href = "https://www.nature.com/articles/s41467-020-18873-z",
              "Nature Communications article",
              target = "_blank",
              .noWS = "outside"
            ),
            "."
          ),
          tags$li(
            "ICELLNET:",
            tags$a(
              href = "https://github.com/soumelis-lab/ICELLNET",
              "webpage",
              target = "_blank"
            ),
            " and ",
            tags$a(
              href = "https://www.nature.com/articles/s41467-021-21244-x",
              "Nature Communications article",
              target = "_blank",
              .noWS = "outside"
            ),
            "."
          ),
          tags$li(
            "NicheNet:",
            tags$a(
              href = "https://github.com/saeyslab/nichenetr",
              "webpage",
              target = "_blank"
            ),
            " and ",
            tags$a(
              href = "https://www.nature.com/articles/s41592-019-0667-5",
              "Nature Methods article",
              target = "_blank",
              .noWS = "outside"
            ),
            "."
          ),
          tags$li(
            "SingleCellSignalR:",
            tags$a(
              href = "http://www.bioconductor.org/packages/release/bioc/html/SingleCellSignalR.html",
              "webpage",
              target = "_blank"
            ),
            " and ",
            tags$a(
              href = "https://academic.oup.com/nar/article/48/10/e55/5810485",
              "Nucleic Acids Research article",
              target = "_blank",
              .noWS = "outside"
            ),
            "."
          ),
          tags$li(
            "scTensor:",
            tags$a(
              href = "https://github.com/rikenbit/scTensor",
              "webpage",
              target = "_blank"
            ),
            " and ",
            tags$a(
              href = "https://www.biorxiv.org/content/10.1101/566182v1",
              "bioRxiv article",
              target = "_blank",
              .noWS = "outside"
            ),
            "."
          )
        )
      })
      
      output$INTRO_LRI_TABLE <- DT::renderDataTable({
        display_LRI_table(
          LRI_table = scAgeComShiny::scAgeCom_data$LRI_mouse_curated,
          LRI_database = scAgeComShiny::scAgeCom_data$LRI_DATABASES
        )
      })
      
      output$INTRO_LRI_UPSET_PLOT <- renderPlot({
        plot_upset_LRI(
          LRI_table = scAgeComShiny::scAgeCom_data$LRI_mouse_curated,
          groups = colnames(scAgeComShiny::scAgeCom_data$LRI_mouse_curated)[9:16],
          min_size = 40
        )
      })
      
      output$INTRO_CONTACT_TITLE <- renderUI({
        tags$h3(
          style = style_intro_accordion_title,
          "Contact"
        )
      })
      
      output$INTRO_CONTACT_TEXT <- renderUI({
        tags$div(
          style = style_intro_accordion_text,
          tags$p(
            "Please contact ..."
          )
        )
      })
      
    })
}

display_LRI_table <- function(
  LRI_table,
  LRI_database
) {
  `Database(s) of Origin` <- NULL
  dt <- LRI_table[
    apply(
      sapply(
        LRI_database,
        function(i) {
          grepl(i, `Database(s) of Origin`)
        }
      ),
      MARGIN = 1,
      any
    )
  ]
  DT::datatable(
    data = dt[, 1:7],
    class = "display compact",
    options = list(
      pageLength = 10,
      dom = '<"top"f>rt<"bottom"lip><"clear">',
      columnDefs = list(
        list(
          targets = c(6,7),
          render = htmlwidgets::JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 20 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
            "}"
          )
        )
      )
    ),
    caption = tags$caption(
      style = paste(
        "caption-side: top;",
        "text-align: center;",
        "color: black;",
        "font-size: 130%;"
        ),
      "Table of Mouse Ligand-Receptor Interactions"
    )
  )
}

plot_upset_LRI <- function(
  LRI_table,
  groups,
  min_size
) {
  COMPLEX <- NULL
  p <- ComplexUpset::upset(
    as.data.frame(LRI_table),
    groups,
    base_annotations = list(
      'Intersection size' = ComplexUpset::intersection_size(
        mapping = ggplot2::aes(fill = COMPLEX),
        counts = TRUE,
        bar_number_threshold = 100
      )
    ),
    themes = ComplexUpset::upset_default_themes(
      text = ggplot2::element_text(size = 16)
      ),
    min_size = min_size
  ) + ggplot2::ggtitle(
    "Number of Ligand-Receptor Interactions"
    )
  return(p)
}

