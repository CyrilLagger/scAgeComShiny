#' glossary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_glossary_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        width = 10,
        offset = 1,
        uiOutput(ns("GLOSSARY_PAGE")),
        style = "padding:10px"
      )
    )
  )
}
    
#' glossary Server Functions
#'
#' @noRd 
mod_glossary_server <- function(id){
  moduleServer(
    id,
    function(
      input,
      output,
      session
      ){
    ns <- session$ns
    
    output$GLOSSARY_PAGE <- renderUI({
      fluidPage(
        fluidRow(
          column(
            width = 8,
            titlePanel(htmlOutput(ns("GLOSSARY_TITLE_GT"))),
          )
        ),
        fluidRow(
          column(
            width = 8,
            htmlOutput(ns("GLOSSARY_CONTENT_GT"))
          )
        ),
        fluidRow(
          column(
            width = 8,
            titlePanel(htmlOutput(ns("GLOSSARY_TITLE_DATA"))),
          )
        ),
        fluidRow(
          column(
            width = 8,
            htmlOutput(ns("GLOSSARY_CONTENT_DATA"))
          )
        ),
        fluidRow(
          column(
            width = 8,
            titlePanel(htmlOutput(ns("GLOSSARY_TITLE_DA"))),
          )
        ),
        fluidRow(
          column(
            width = 8,
            htmlOutput(ns("GLOSSARY_CONTENT_DA"))
          )
        ),
        fluidRow(
          column(
            width = 8,
            titlePanel(htmlOutput(ns("GLOSSARY_TITLE_ORA"))),
          )
        ),
        fluidRow(
          column(
            width = 8,
            htmlOutput(ns("GLOSSARY_CONTENT_ORA"))
          )
        )
      )
    })
    
    output$GLOSSARY_TITLE_GT <- renderUI({
      tags$p(
        style = paste(
          "font-size: 22px;"
        ),
        "General Terminology"
      )
    })
    
    output$GLOSSARY_CONTENT_GT <- renderUI({
      acc <- bsplus::bs_accordion(
        id = "HELP_GT"
      ) %>%
        bsplus::bs_append(
          title = "LRI: ligand-receptor interaction",
          content = paste(
            "A LRI is formally defined as a pair of ligand gene(s)",
            "and receptor gene(s) coding for a known protein-protein",
            "interaction. It can be simple, namely involving a single ligand",
            "gene and receptor gene (e.g. Apoe:Ldlr), or complex if",
            "involving an heteromeric complex (e.g. Col3a1:Itgb1_Itga2)."
          )
        ) %>%
        bsplus::bs_append(
          title = "CCI: cell-cell interaction",
          content = paste(
            "A communication signal between two cell types mediated by a",
            "ligand-receptor interaction. For example, (B cell, T cell, Apoe:Ldlr)",
            "is a CCI that corresponds to the emission of Apoe from B cells and that",
            "will interact with Ldlr expressed by T cells."
          )
        ) 
      attrib_ <- acc$children[[1]]$children[[2]]$attribs
      attrib_[which(attrib_ == "in")] <- NULL
      acc$children[[1]]$children[[2]]$attribs <- attrib_
      acc
    })
    
    output$GLOSSARY_TITLE_DATA <- renderUI({
      tags$p(
        style = paste(
          "font-size: 22px;"
        ),
        "scRNA-seq dataset Terminology"
      )
    })
    
    output$GLOSSARY_CONTENT_DATA <- renderUI({
      acc <- bsplus::bs_accordion(
        id = "HELP_DATA"
      ) %>%
        bsplus::bs_append(
          title = "TMS FACS",
          content = paste(
            "Refers to the Tabula Muris Senis scRNA-seq datasets obtained by",
            "cell sorting in microtiter well plates followed by Smart-seq2",
            "library preparation and full-length sequencing."
          )
        ) %>%
        bsplus::bs_append(
          title = "TMS Droplet / Calico Droplet",
          content = paste(
            "Refers to the Tabula Muris Senis and Calico scRNA-seq datasets",
            "obtained by cell capture by microfluidic droplets as per the",
            "10x Genomics protocol followed by 3' end counting."
          )
        )
      attrib_ <- acc$children[[1]]$children[[2]]$attribs
      attrib_[which(attrib_ == "in")] <- NULL
      acc$children[[1]]$children[[2]]$attribs <- attrib_
      acc
    })
    
    output$GLOSSARY_TITLE_DA <- renderUI({
      tags$p(
        style = paste(
          "font-size: 22px;"
        ),
        "Detection/differential analysis Terminology"
      )
    })
    
    output$GLOSSARY_CONTENT_DA <- renderUI({
      acc <- bsplus::bs_accordion(
        id = "HELP_DA"
      ) %>%
        bsplus::bs_append(
          title = "UP/DOWN/FLAT/NSC CCIs",
          content = paste(
            "Refers to how each cell-cell interaction (CCI) is regulated",
            "with age. UP: increases with age. DOWN: decreases with age.",
            "FLAT: stable with age. NSC: non-significant change with age.",
            "Exact values of the thresholds used are given in our manuscript."
          )
        )
      attrib_ <- acc$children[[1]]$children[[2]]$attribs
      attrib_[which(attrib_ == "in")] <- NULL
      acc$children[[1]]$children[[2]]$attribs <- attrib_
      acc
    })
    
    output$GLOSSARY_TITLE_ORA <- renderUI({
      tags$p(
        style = paste(
          "font-size: 22px;"
        ),
        "Over-representation analysis Terminology"
      )
    })
    
    output$GLOSSARY_CONTENT_ORA <- renderUI({
     acc <- bsplus::bs_accordion(
        id = "HELP_ORA"
      ) %>%
        bsplus::bs_append(
          title = "ORA: Over-representation analysis",
          content = 
            tags$div(
              tags$p(
                paste(
                  "The method used to infer dominant changes among all detected CCIs.",
                  "For example, to test if the ligand",
                  "Apoe has the tendancy to be more associated with up-regulated",
                  "CCIs in a tissue, we perform a Fisher's exact test on the 2x2",
                  "contingency table made of",
                  "1) number of up-regulated CCIs that contain Apoe, 2) number of",
                  "up-regulated CCIs that do not contain Apoe, 3) number of not",
                  "up-regulated CCIs that contain Apoe and 4) number of not",
                  "up-regulated CCIs that do not contain Apoe."
                )
              ),
              tags$p(
                paste(
                  "The process can the be repeated not only on all other",
                  "ligands but on any keywords than can be attached to the CCIs.",
                  "Here, we did it on LRIs, ligands, receptors, emitter cell types,",
                  "receiver cell types, emitter-receiver cell-type pairs, GO",
                  "terms and KEGG pathways (on UP, DOWN and FLAT CCIs)."
                )
              )
            )
            
        )  %>%
        bsplus::bs_append(
          title = "Odds Ratios (OR), adj. p-values and ORA scores",
          content = paste(
            "The results of the Fisher's exact test described above.",
            "In addition to the standard odd ratio and p-value, we also defined",
            "an ORA score as log2(OR)*(-log10(adj. p-value)). This score",
            "allows us to sort the results based on a single value."
          )
        ) 
     attrib_ <- acc$children[[1]]$children[[2]]$attribs
     attrib_[which(attrib_ == "in")] <- NULL
     acc$children[[1]]$children[[2]]$attribs <- attrib_
     acc
    })
    
  })
}