ab2json <- function(a, json = ab_temp, batch, sep = "|", CV = codedValues, HIRN) {
  new <- json
  # Name and description
  new$CanonicalId <- a$RRID
  new$Name <- a$Name
  new$Description <- a$Description
  # Characteristics
  new$AntibodyType <-  map2O(name = a$AntibodyType, ontology = "ERO")
  new$TargetAntigen <- a$Antigen
  new$AntigenSpecies <- map2O(name = a$AntigenSpecies, ontology = "NCBITAXON")
  new$RaisedIn <- map2O(name = a$RaisedIn, ontology = "NCBITAXON")
  new$ClonalityStatus <- map2O(name = a$Clonality, ontology = "ERO")
  new$CloneName <- a$CloneName
  new$PositiveControl <- a$PositiveControl
  new$AntibodyConjugate <- a$AntibodyConjugate
  new$IsoType <- map2O(name = a$Isotype, ontology = "ERO")
  new$Purity <- a$Purity
  new$TargetCells <- lapply(unlist(strsplit(a$TargetCells, sep, fixed = T)), function(x) map2O(name = x, ontology = "CL"))
  new$Applications <- lapply(strsplit(a$Applications, "|", fixed = T)[[1]], function(x) map2O(id = x))
  # Vendor
  new$VendorName <- a$VendorName
  new$VendorCat <- a$VendorCat
  # Paper and contributors
  new$Contributors <- map2Person(a$DefiningManuscriptId, a$Role, HIRN)
  new$DefiningManuscriptId <- a$DefiningManuscriptId
  # Index metadata
  new$id <- uuid::UUIDgenerate()
  new$PrimaryResourceType <- resourceTypes$ResourceTypes[[1]][resourceSchema]
  new$PrimaryResourceType$SerializationMode <- "full"
  new$SecondaryResourceType <- resourceTypes$ResourceTypes[[1]]$SubResourceTypes[[1]][resourceSchema]
  new$SecondaryResourceType$SerializationMode <- "full"
  new$DateCreated <- format_iso_8601(Sys.time())
  new$CurationStatus <- CV$CodedValuesByType$CurationStatus[[3]]
  new$CurationStatus$SerializationMode <- "full"
  new$BatchNumber <- batch
  toJSON(new, pretty = T, null = "null", na = "null", auto_unbox = T)
}

ab2json_batch <- function() {
  
}