# App UI
helperUI <- function(id) {
  ns <- NS(id)
  fluidPage(theme = shinythemes::shinytheme("flatly"),
            checkboxInput(ns("show"), "view", value = T),
            fluidRow(
                     conditionalPanel("input.show", ns = ns,
                     column(4, style = "margin-top: 30px;",
                        div(style = "display: inline-block;",
                            textInput(ns("PMID"), "PMID"),
                            textAreaInput(ns("text"), "Resources", height = 600, width = 600),
                            actionButton(ns("go"), "Parse")
                        )
                      )),
                      column(8,
                          tabsetPanel(type = "pills",
                            tabPanel("SciCrunch Results",
                                     br(),
                                     div(
                                       DTOutput(ns("dt"))
                                       )
                                     ),
                            tabPanel("Add & Modify")
                          )
                      )
             )
           )
}




helper <- function(input, output, session) {

  abInfo <- reactiveVal(data.frame())

  observe({
    idformat <- gsub("^PMID:[ ]+", "PMID", input$PMID)
    updateTextInput(session, "PMID", value = idformat)
  })

  observeEvent(input$go, {
    withProgress(value = 0.4, message = "parsing...",
      { parsed <- text2Abs(input$text)
        dt <- supplementAbs(parsed$x, parsed$text)
        setProgress(value = 0.8)
        # indices <- unlist(lapply(parsed$text, function(x) grep(x, dt$VendorCat)))
        # dt[, DefiningManuscriptId := input$PMID]
        abInfo(dt)
      }
    )
  })

  output$dt <- renderDT({
    # fillcols <- c("Role", "Applications",	"IsoType", "Type", "AntigenSpecies", "TargetCells",	"PositiveControl",
    #               "DilutionUnits", "UsageNotes",	"Rating",	"Description")
    if(!length(abInfo())) return()
    abInfo()[, -c("Comments")]
  }, editable = T, style = "bootstrap", rownames = F)

  observeEvent(input$delete, {
    if(!is.null(input$dt_rows_selected)) {
      dt <- abInfo()[-as.numeric(input$dt_rows_selected), ]
      abInfo(dt)
    }
  })

  output$save <- downloadHandler(
    filename = function() {
        paste0(input$PMID, "_abs.txt")
      },
      content = function(file) {
        write.table(abInfo(), file, sep = "\t", row.names = F)
      }
  )

}


helperApp <- function() {
  ui <- helperUI("default")
  server <- function(input, output, session) { callModule(helper, "default") }
  shinyApp(ui = ui, server = server)
}



