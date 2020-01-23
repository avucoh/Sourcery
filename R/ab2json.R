#' @export
ab2json <- function(a, json = ab_temp, batch, sep = "|", CV = codedValues, HIRN = whichHIRN) {
  new <- json
  # Specifics
  new$CanonicalId <- if(length(a$RRID)) paste0("RRID:", a$RRID)
  new$Name <- a$Name
  new$Description <- a$Description
  # Characteristics
  # Expanded with CanonicalId Name Url Ontology Id
  new$RaisedIn <- map2O(name = a$RaisedIn, ontology = "NCBITAXON")
  # new$IsoType <- map2O(name = a$Isotype, ontology = "ERO")
  # new$AntibodyType <-  map2O(name = a$Type, ontology = "ERO")
  new$AntibodyType <- if(a$Type == "primary") CVs$AntibodyType[[1]] else CVs$AntibodyType[[2]]
  # new$TargetAntigen <- a$Antigen
  new$AntigenSpecies <- map2O(name = a$AntigenSpecies, ontology = "NCBITAXON")
  # new$ClonalityStatus <- map2O(name = a$Clonality, ontology = "ERO")
  new$ClonalityStatus <- if(a$Clonality == "monoclonal") CVs$ClonalityStatus[[1]] else if(a$Clonality == "polyclonal") CVs$ClonalityStatus[[2]] else NULL
  new$TargetCells <- lapply(unlist(strsplit(a$TargetCells, sep, fixed = T)), function(x) map2O(name = x, ontology = "CL"))
  # Applications
  new$Applications <- map2Application(Publication = a$Publication, Usage = a$Applications, UsageNotes = a$UsageNotes, Rating = a$Rating)
  # Strings only
  new$CloneName <- a$CloneName
  new$PositiveControl <- a$PositiveControl
  new$AntibodyConjugate <- a$AntibodyConjugate
  new$Purity <- a$Purity
  # Vendor
  new$Vendor$Name <- a$VendorName
  new$VendorCatNumber <- a$VendorCat
  # Paper and contributors
  new$Contributors <- map2Person(a$Publication, a$Role, HIRN)
  new$DefiningManuscriptId <- a$Publication
  # Index metadata
  new$Id <- uuid::UUIDgenerate()
  jsonlite::toJSON(new, pretty = T, null = "null", na = "null", auto_unbox = T)
}
