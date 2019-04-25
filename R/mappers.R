#-- Templates -------------------------------------------------------------------------------------------------------#

boiler <- function(OntologyId = NA, id = NA, CanonicalId = NA, Url = NA, Name = NA,
                   Type = NA, SystemType = "CodedValue", Tags = character(0), Visibility = NA,
                   DateCreated = "", DateModified = "", VersionId = "1", SerializationMode = "full") {
  list(OntologyId = OntologyId, id = id, CanonicalId = CanonicalId, Url = Url, Name = Name,
       Type = Type, SystemType = SystemType,
       Tags = Tags, Visibility = Visibility,
       DateCreated = DateCreated, DateModified = DateModified,
       VersionId = VersionId, SerializationMode = SerializationMode)
}


#-- MAPPERS --------------------------------------------------------------------------#

# Use this for RaisedIn, AntigenSpecies, CrossReactiveSpecies
map2NCBITAXON <- function(name) {
  mapped <- taxon$IRI[match(name, taxon$name)]
  boiler(OntologyId = "NCBITAXON", CanonicalId = mapped, Url = mapped, Name = name)
}

map2CL <- function(name) {
  mapped <- cl$IRI[match(name, cl$name)]
  boiler(OntologyId = "CL", CanonicalId = mapped, Url = mapped, Name = name)
}

map2Isotype <- function(name) {
  mapped <- isotype$IRI[match(name, isotype$name)]
  boiler(OntologyId = "ERO", CanonicalId = mapped, Url = mapped, Name = name)
}

map2Clonality <- function(name) {
  mapped <- ifelse(grepl("monoclonal", name),
                   "http://purl.obolibrary.org/obo/REO_0000501",
                   "http://purl.obolibrary.org/obo/REO_0000502")
  list(boiler(OntologyId = "OBI_BCGO", CanonicalId = mapped, Url = mapped, Name = name))
}

map2Type <- function(name) {
  mapped <- ifelse(name == "primary",
                   "http://purl.obolibrary.org/obo/ERO_0000229",
                   "http://purl.obolibrary.org/obo/ERO_0000230")
  boiler(OntologyId = "ERO", CanonicalId = mapped, Url = mapped, Name = name)
}

# Since resources are curated by paper, lookup HIRN authors -- "Contact" as default role
map2Person <- function(pmid, role = "Contact", HIRN) {
  mapped <- Persons[HIRN[[pmid]]]
  roleCV <- codedValues$CodedValuesByType$ResourceRole[[match(role, c("Contact", "Producer", "Distributor", "Date Submitter"))]]
  roleCV$SerializationMode <- "full"
  mapped <- lapply(mapped,
                   function(x) c(Role = list(roleCV),
                                 Person = list(x),
                                 boiler(SystemType = "Contributor")[-1])
                   )
}

# If only name is given, ontology must be specified. Ontologies can be inferred from id.

map2O <- function(name = NA, id = NA, ontology = NULL) {
  if(is.null(ontology)) ontology <- gsub("_.*", "", id)
  if(ontology == "CVCL") ontology <- "Cellosaurus"
  if(ontology == "UBERON") ontology <- "CL"
  mapped <- O[[ontology]][Name == name | ID == id, ]
  boiler(OntologyId = ontology, CanonicalId = mapped$IRI, Url = mapped$IRI, Name = mapped$Name)
}
