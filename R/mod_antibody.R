antibodyUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    div(class = "forceInline", textInput(ns("Name"), "")),
    div(class = "forceInline", selectInput(ns("AntibodyType"), "",  choices = c("", "primary", "secondary"))),
    div(class = "forceInline", selectInput(ns("TargetAntigen"), "", choices = "")),
    div(class = "forceInline", selectInput(ns("AntigenSpecies"), "", choices = O[["NCBITAXON"]][, Name])),
    div(class = "forceInline", selectizeInput(ns("RaisedIn"), "", choices = O[["NCBITAXON"]][, Name])),
    div(class = "forceInline", selectizeInput(ns("TargetCells"), "", choices = O[["NCBITAXON"]][, Name])),
    div(class = "forceInline", selectInput(ns("ClonalityStatus"), "", choices = c("", "monoclonal", "polyclonal"))),
    div(class = "forceInline", textInput(ns("CloneName"), "")),
    div(class = "forceInline", textInput(ns("AntibodyConjugate"), "")),
    div(class = "forceInline", selectizeInput(ns("Isotype"), "", choices = O[["ERO"]][grep("^Ig", Name), Name])),
    div(class = "forceInline", selectizeInput(ns("CrossReactiveSpecies"), "", choices = O[["NCBITAXON"]][, Name])),
    div(class = "forceInline", textInput(ns("PositiveControl"), "")),
    div(class = "forceInline", numericInput(ns("Rating"), "", min = 1, max = 5, value = 5)),
    div(class = "forceInline", selectInput(ns("Usage"), "", choices = )),
    div(class = "forceInline", textInput(ns("UsageNotes"), "")),
    div(class = "forceInline", textInput(ns("Publication"), "PMID"))
  )
}

antibody <- function(input, output, session) {
  
  properties <- c("Name", "AntibodyType", "TargetAntigen", "AntigenSpecies", "RaisedIn", "TargetCells", 
                  "ClonalityStatus", "CloneName", "AntibodyConjugate", "Isotype", "CrossReactiveSpecies",
                  "PositiveControl", "Rating", "Usage", "UsageNotes", "Publication")
  
  antibody <- reactive({
    lapply(properties, function(p) input[[p]])
  })
  
  return(antibody)

}

modAntibody <- function() {
  ui <- antibodyUI("1")
  server <- function(input, output, session) { callModule(antibody, "1") }
  shinyApp(ui = ui, server = server)
}