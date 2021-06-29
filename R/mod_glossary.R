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
  moduleServer( id, function(input, output, session){
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
      bsplus::bs_accordion(
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
            "A communication signals between two cell types mediated by a",
            "ligand-receptor interaction. For example, (B cell, T cell, Apoe:Ldlr)",
            "is a CCI that corresponds to the emission of Apoe from B cells and that",
            "will interact with Ldlr expressed by T cells"
          )
        ) %>%
        bsplus::bs_append(
          title = "TODO",
          content = paste(
            "TODO"
          )
        )
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
      bsplus::bs_accordion(
        id = "HELP_DATA"
      ) %>%
        bsplus::bs_append(
          title = "TMS FACS",
          content = paste(
            "Refers to Tabula Muris Senis scRNA-seq datasets obtained by",
            "cell sorting in microtiter well plates followed by Smart-seq2",
            "library preparation and full-length sequencing."
          )
        ) %>%
        bsplus::bs_append(
          title = "TMS Droplet / Calico Droplet",
          content = paste(
            "Refers to Tabula Muris Senis and Calico scRNA-seq datasets",
            "obtained by cell capture by microfluidic droplets as per the",
            "10x Genomics protocol followed by 3' end counting."
          )
        )
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
      bsplus::bs_accordion(
        id = "HELP_DA"
      ) %>%
        bsplus::bs_append(
          title = "Detected CCIs",
          content = paste(
            "TODO"
          )
        ) %>%
        bsplus::bs_append(
          title = "UP/DOWN/FLAT/NSC CCIs",
          content = paste(
            "TODO"
          )
        )
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
      bsplus::bs_accordion(
        id = "HELP_ORA"
      ) %>%
        bsplus::bs_append(
          title = "ORA",
          content = paste(
            ""
          )
        )  %>%
        bsplus::bs_append(
          title = "Odds Ratio",
          content = paste(
            ""
          )
        )  %>%
        bsplus::bs_append(
          title = "Adj. p-value",
          content = paste(
            ""
          )
        )  %>%
        bsplus::bs_append(
          title = "ORA score",
          content = paste(
            ""
          )
        )
    })
    
  })
}