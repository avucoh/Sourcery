extractUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(4, style = "margin-top: 30px;",
             div(style = "forceInline",
                 fileInput(ns("file"), "Antibody List", placeholder = "  no file selected"),
                 textInput(ns("PMID"), "PMID", width = 250),
                 textAreaInput(ns("text"), "Resources", height = 500, width = 600),
                 actionButton(ns("parse"), "Parse")
             )
      ),
      column(8, #"SciCrunch Results",
             DTOutput(ns("dt"))
      )
    )
  )
}


extractR <- function(input, output, session) {
  
  abInfo <- reactiveVal(data.frame())
  
  observe({
    idformat <- gsub("^PMID:[ ]+", "PMID", input$PMID)
    updateTextInput(session, "PMID", value = idformat)
  })
  
  observeEvent(input$file, {
    inFile <- input$file
    text <- readLines(inFile$datapath)
    withProgress(value = 0.3, message = "parsing...", 
                { 
                 parsed <- text2Abs(text, split = F)
                 setProgress(value = 0.7, message = "using API...")
                 dt <- extraAbs(parsed$x, parsed$text)
                 abInfo(dt)
                 }
    )
  })
  
  observeEvent(input$parse, {
    withProgress(value = 0.3, message = "parsing...",
                 { parsed <- text2Abs(input$text)
                 setProgress(value = 0.7, message = "using API...")
                 dt <- extraAbs(parsed$x, parsed$text)
                 abInfo(dt)
                 }
    )
  })
  
  output$dt <- renderDT({
    if(!length(abInfo())) return()
    abInfo()[, -c("Comments")]
  }, editable = T, style = "bootstrap", rownames = F)
  
  extracted <- reactive({
    # dt[, DefiningManuscriptId := input$PMID]
    abInfo()[input$dt_rows_selected, ]
  })
  
  return(extracted)
}