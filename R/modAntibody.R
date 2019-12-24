antibodyLab <- function() {
  list(Name = "Name", VendorName = "Vendor", VendorCat = "Catalog#", RRID = "RRID",
       AntibodyType = "Antibody type", ClonalityStatus = "Clonality", CloneName = "Clone name", TargetAntigen = "Target antigen",
       AntigenSpecies = "Antigen species", RaisedIn = "Raised in", AntibodyConjugate = "Conjugate", Isotype = "Isotype",
       Publication = "PMID", Rating = "Rating", Usage = "Usage", UsageNotes = "Usage notes", TargetCells = "Target cells",
       PositiveControl = "Positive control")
}

# Create as many inputs as specified by n
batchInput <- function(ui, n = NULL, inputdata = NULL, ns = function(x) x) {
  if(!length(n)) n <- nrow(inputdata)
  lapply(1:n, function(i) do.call(ui, list(id = ns(i), inputdata = inputdata[i, ], labels = i == 1)))
}

antibodyInput <- function(id, inputdata = NULL, labels = T) {
  ns <- NS(id)
  if(labels) labels <- antibodyLab() else labels <- NULL
  div(
    div(class = "forceInline", textInput(ns("Name"), labels$Name, width = 300, value = inputdata$Name)),
    div(class = "forceInline", textInput(ns("VendorName"), labels$VendorName, width = 120, value = inputdata$VendorName)),
    div(class = "forceInline", textInput(ns("VendorCat"), labels$VendorCat, width = 120, value = inputdata$VendorCat)),
    div(class = "forceInline", textInput(ns("RRID"), labels$RRID, width = 120, value = inputdata$RRID))
  )
}

antibodyUsageInput <- function(id, inputdata = NULL, labels = T) {
  ns <- NS(id)
  if(labels) labels <- antibodyLab() else labels <- NULL
  div(# class = "usage",
      div(class = "forceInline", textInput(ns("Publication"), labels$Publication, width = 100, value = inputdata$Publication)),
      div(class = "forceInline", numericInput(ns("Rating"), labels$Rating, min = 1, max = 5, value = 5, width = 80)),
      div(class = "forceInline", selectInput(ns("Usage"), labels$Usage,
                                             choices = c("", "western blot analysis", "immunohistochemistry", "immunocytochemistry",
                                                         "ELISA", "CyTOF", "flow cytometry assay", "fluorescence imaging",
                                                         "immunoprecipitation", "biologic intervention"), width = 200,
                                             selected = "immunohistochemistry")),
      div(class = "forceInline", textInput(ns("UsageNotes"), labels$UsageNotes, value = inputdata$UsageNotes, width = 400)),
      div(class = "forceInline", selectizeInput(ns("TargetCells"), labels$TargetCells, choices = NULL, multiple = T, width = 400))
      # div(class = "forceInline", textInput(ns("PositiveControl"), labels$PositiveControl, width = 150))
  )
}

antibodyPropInput <- function(id, inputdata = NULL, labels = T) {
  ns <- NS(id)
  if(labels) labels <- antibodyLab() else labels <- NULL
  div(# class = "properties",
      div(class = "forceInline", selectInput(ns("AntibodyType"), labels$AntibodyType,  choices = c("", "primary", "secondary"), width = 120, selected = inputdata$AntibodyType)),
      div(class = "forceInline", selectInput(ns("ClonalityStatus"), labels$ClonalityStatus, choices = c("", "monoclonal", "polyclonal"), width = 120, selected = inputdata$ClonalityStatus)),
      div(class = "forceInline", textInput(ns("CloneName"), labels$CloneName, width = 120, value = inputdata$CloneName)),
      div(class = "forceInline", selectInput(ns("TargetAntigen"), labels$TargetAntigen, choices = "", width = 120, selected = inputdata$TargetAntigen)),
      div(class = "forceInline", selectInput(ns("AntigenSpecies"), labels$AntigenSpecies, choices = O[["NCBITAXON"]][, Name], width = 300, selected = inputdata$AntigenSpecies[[1]], multiple = T)),
      div(class = "forceInline", selectInput(ns("RaisedIn"), labels$RaisedIn, choices = O[["NCBITAXON"]][, Name], width = 150, selected = inputdata$RaisedIn)),
      div(class = "forceInline", textInput(ns("AntibodyConjugate"), labels$AntibodyConjugate, width = 80, value = inputdata$AntibodyConjugate)),
      div(class = "forceInline", selectInput(ns("Isotype"), labels$Isotype, choices = c("", O[["ERO"]][grep("^Ig", Name), Name]), width = 80, selected = inputdata$Isotype))
      # div(class = "forceInline", selectizeInput(ns("CrossReactiveSpecies"), "", choices = O[["NCBITAXON"]][, Name], selected = "", width = 160))
  )
}


modAntibodyUI <- function(id) {
  ns <- NS(id)
  fluidPage(includeCSS(system.file("www/", "styles.css", package = "Sourcery")), # theme = shinythemes::shinytheme("flatly"),
            fluidRow(
              column(4,
                     tabsetPanel(type = "pills",
                                 tabPanel("Main", br(),
                                          uiOutput(ns("antibodyMain")))
                     )
              ),
              column(8,
                     tabsetPanel(type = "pills",
                       tabPanel("Properties", class = "properties", style = "padding-left: 20px;", br(),
                                shinycssloaders::withSpinner(uiOutput(ns("antibodyProperties")))),
                       tabPanel("Usage", class = "usage", style = "padding-left: 20px;", br(),
                                shinycssloaders::withSpinner(uiOutput(ns("antibodyUsage"))))
                     )
              )
            )
  )
}

# Create a data entry UI for n rows of records;
# if inputdata is provided, n is calculated from inputdat instead.
modAntibody <- function(input, output, session,
                        n = 10, inputdata = reactive({ NULL }) ) {

  nrows <- reactive({
    if(length(inputdata())) nrow(inputdata()) else n
  })

  output$antibodyMain <- renderUI({
    batchInput("antibodyInput", n = nrows(), inputdata = inputdata(), ns = session$ns)
  })

  output$antibodyProperties <- renderUI({
    batchInput("antibodyPropInput", n = nrows(), inputdata = inputdata(), ns = session$ns)
  })

  output$antibodyUsage <- renderUI({
    tags$div(batchInput("antibodyUsageInput", n = nrows(), inputdata = inputdata(), ns = session$ns),
             actionButton(session$ns("done"), "Done"))
  })

  observe({
    if(!is.null(input$done) && input$done == 0) for(i in 1:n) {
      updateSelectizeInput(session, NS(i, "TargetCells"), choices = c("", O[["CL"]][, Name]), selected = "", server = T)
    }
  })

  output$out <- renderPrint({
    reactiveValuesToList(input)
  })

  inputs <- reactive({
    values <- reactiveValuesToList(input)
    values <- values[grep("^[0-9]", names(values))] # keep only real data fields, which are inputs starting with numbers representing row numbers
    values <- values[grep("selectized", names(values), invert = T)]
    if(length(values)) {
      splitnames <- strsplit(names(values), "-")
      rows <- sapply(splitnames, `[[`, 1)
      cols <- sapply(splitnames, `[[`, 2)
      names(values) <- cols
      values <- split(values, f = rows)
      values <- values[sort.list(as.integer(names(values)))]
      values <- rbindlist(values, use.names = T)
      return(values)
    }
  })

  return(inputs)
}


# App UI
modAntibodyAppUI <- function(id) {
  ns <- NS(id)
  navbarPage("", theme = shinythemes::shinytheme("flatly"),
             tabPanel("Extract/Import",
                      extractUI("ab")
             ),
             tabPanel("Transform",
                      modAntibodyUI("ab")
             ),
             tabPanel("Load",
                      downloadButton("export", "Export"),
                      h2("Preview"),
                      verbatimTextOutput("sample"))
  )
}


modAntibodyApp <- function() {
  ui <- modAntibodyAppUI("default")
  server <- function(input, output, session) {
    extracted <- callModule(extractR, "ab")

    inputdata <- reactive({
      if(length(extracted())) asInput(extracted())
    })

    transformed <- callModule(modAntibody, "ab", inputdata = inputdata)

    output$sample <- renderPrint({
      transformed()
    })

    output$export <- downloadHandler(
      filename = function() {
        paste0(input$PMID, "_abs.txt")
      },
      content = function(file) {
        write.table(transformed(), file, sep = "\t", row.names = F)
      }
    )

  }
  shinyApp(ui = ui, server = server)
}







