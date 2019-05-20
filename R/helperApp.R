text2Abs <- function(text) {
  text <- strsplit(text, "(?<=\\)),", perl = T)[[1]]
  catmatch <-  regexpr("([a-zA-Z]{1,2})?[^,:( ][-0-9]{3,7}", text)
  vcats <- vector("character", length(catmatch))
  vcats[catmatch != -1] <- regmatches(text, catmatch)
  vcats[catmatch == -1] <- ""
  parsed <- list(vcats = vcats, text = text)
  parsed
}

fillCols <- function() {
  c("Role", "Applications",	"IsoType", "Type", "AntigenSpecies", "TargetCells",	"PositiveControl",
            "DilutionUnits", "UsageNotes",	"Rating",	"Description")
}

# App UI
helperUI <- function(id) {
  ns <- NS(id)
  navbarPage("Resource Sharing", theme = shinytheme("yeti"),
             tabPanel("Main",
                      fluidPage(style = "margin-left: 20px;",
                                fluidRow(textInput(ns("PMID"), "PMID")),
                                fluidRow(textAreaInput(ns("text"), "Methods text", height = 500, width = 800)),
                                fluidRow(actionButton(ns("go"), "Process"),
                                         actionButton(ns("delete"), "Selected", icon = icon("minus")),
                                         downloadButton(ns("save"), "Save")),
                                fluidRow(style = "margin-top: 30px;",
                                         DTOutput(ns("dt")))
                                )
             )
  )
}




helper <- function(input, output, session) {

  abInfo <- reactiveVal(data.frame())

  observeEvent(input$delete, {
    if(!is.null(input$dt_rows_selected)) {
      dt <- abInfo()[-as.numeric(input$dt_rows_selected), ]
      abInfo(dt)
    }
  })

  observe({
    idformat <- gsub("^PMID:[ ]+", "PMID", input$PMID)
    updateTextInput(session, "PMID", value = idformat)
  })

  observeEvent(input$go, {
    withProgress(value = 0.4, message = "parsing...",
      { parsed <- text2Abs(input$text)
        dt <- supplementAbs(ids = parsed$vcats, txt = parsed$text)
        setProgress(value = 0.8)
        # indices <- unlist(lapply(parsed$text, function(x) grep(x, dt$VendorCat)))
        dt[, DefiningManuscriptId := input$PMID]
        dt[, (fillCols()) := ""]
        abInfo(dt)
      }
    )
  })

  output$dt <- renderDT({
    if(!length(abInfo())) return()
    abInfo()[, -c("DefiningManuscriptId", fillCols())]
  }, editable = T)

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



