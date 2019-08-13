antibodyProps <- function() {
  list(Name = "Name", VendorName = "Vendor", VendorCat = "Catalog #",
       AntibodyType = "Antibody type", ClonalityStatus = "Clonality", CloneName = "Clone name", TargetAntigen = "Target antigen",
       AntigenSpecies = "Antigen species", RaisedIn = "Raised in", AntibodyConjugate = "Conjugate", Isotype = "Isotype",
       Publication = "PMID", Rating = "Rating", Usage = "Usage", UsageNotes = "Usage notes", TargetCells = "Target cells",
       PositiveControl = "Positive control")
}

antibodyBatchInput <- function(i) {
  lapply(as.character(1:i), antibodyInput)
}

antibodyInput <- function(id, labels = if(id == "1") antibodyProps() else NULL) {
  ns <- NS(id)
  fluidRow(
    div(class = "forceInline", textInput(ns("Name"), labels$Name), width = 300),
    div(class = "forceInline", textInput(ns("VendorName"), labels$VendorName, width = 180)),
    div(class = "forceInline", textInput(ns("VendorCat"), labels$VendorCat, width = 180)),
    div(class = "forceInline properties",
        conditionalPanel(condition = "input.section == 'Properties'",
                         div(class = "forceInline", selectInput(ns("AntibodyType"), labels$AntibodyType,  choices = c("", "primary", "secondary"), width = 120)),
                         div(class = "forceInline", selectInput(ns("ClonalityStatus"), labels$ClonalityStatus, choices = c("", "monoclonal", "polyclonal"), width = 120)),
                         div(class = "forceInline", textInput(ns("CloneName"), labels$CloneName, width = 120)),
                         div(class = "forceInline", selectInput(ns("TargetAntigen"), labels$TargetAntigen, choices = "", width = 120)),
                         div(class = "forceInline", selectInput(ns("AntigenSpecies"), labels$AntigenSpecies, choices = O[["NCBITAXON"]][, Name], width = 180)),
                         div(class = "forceInline", selectInput(ns("RaisedIn"), labels$RaisedIn, choices = O[["NCBITAXON"]][, Name], width = 180)),
                         div(class = "forceInline", textInput(ns("AntibodyConjugate"), labels$AntibodyConjugate, width = 80)),
                         div(class = "forceInline", selectizeInput(ns("Isotype"), labels$Isotype, choices = c("", O[["ERO"]][grep("^Ig", Name), Name]), width = 80))
                         # div(class = "forceInline", selectizeInput(ns("CrossReactiveSpecies"), "", choices = O[["NCBITAXON"]][, Name]))
        )),
    div(class = "forceInline usage"
        # conditionalPanel(condition = "input.section == 'Usage'",
        #                  div(class = "forceInline", textInput(ns("Publication"), labels$Publication, width = 100)),
        #                  div(class = "forceInline", numericInput(ns("Rating"), labels$Rating, min = 1, max = 5, value = 5, width = 50)),
        #                  div(class = "forceInline", selectInput(ns("Usage"), labels$Usage,
        #                                                         choices = c("", "western blot analysis", "immunohistochemistry", "immunocytochemistry",
        #                                                                     "ELISA", "CyTOF", "flow cytometry assay", "fluorescence imaging",
        #                                                                     "immunoprecipitation", "biologic intervention"), width = 200)),
        #                  div(class = "forceInline", textAreaInput(ns("UsageNotes"), labels$UsageNotes)),
        #                  div(class = "forceInline", selectizeInput(ns("TargetCells"), labels$TargetCells, choices = c("", O[["CL"]][, Name]), multiple = T, width = 250)),
        #                  div(class = "forceInline", textInput(ns("PositiveControl"), labels$PositiveControl, width = 160))
        # )
  ))
}

getInput <- function(input, output, session, properties) {

   reactive({
    lapply(properties, function(p) input[[p]])
  })

}

modAntibody <- function() {
  ui <- fluidPage(theme = shinythemes::shinytheme("flatly"),
                  includeCSS(system.file("www/", "styles.css", package = "Sourcery")),
                  fileInput("file", "Antibody List", placeholder = "  no file selected"), br(),
                  radioButtons("section", "Section", choices = c("Properties", "Usage"), inline = T),
                  antibodyBatchInput(6)
                  )
  server <- function(input, output, session) { callModule(getInput, "1") }
  shinyApp(ui = ui, server = server)
}

