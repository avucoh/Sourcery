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


#-- Antibody-specific mappers --------------------------------------------------------------------------------------------------#
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

#-- Mappers used for all resources -------------------------------------------------------------------------------------------#

# Create a mapping of PMID to authors
PMID2Authors <- function(pmids, forenamechars = 3) {
  authors <- RISmed::EUtilsGet(pmids, type = "efetch", db = "pubmed")
  authors <- Author(authors)
  authors <- lapply(authors, function(x) {
    forename <- regmatches(x$ForeName, regexpr(pattern = paste0("^[^ ]{1,", forenamechars, "}"), x$ForeName))
    x$Name <- iconv(paste(x$LastName, forename), from ="UTF-8", to="ASCII//TRANSLIT")
    x
  })
  names(authors) <- pmids
  return(authors)
}

# find HIRN authors among a publication's listed authors
authorCheck <- function(pmids = NULL, srctable, ...) {
  if(is.null(pmids)) pmids <- as.character(srctable$DefiningManuscriptId)
  authors <- PMID2Authors(pmids, ...)
  whichHIRN <- lapply(authors, function(x) unlist(sapply(x$Name, function(y) grep(y, HIRNauthors)), use.names = F))
  names(whichHIRN) <- pmids
  return(whichHIRN)
}

# Since resources are curated by paper, look up HIRN authors -- "Contact" as default role
map2Person <- function(pmid, role = "Contact", HIRN) {
  mapped <- Persons[HIRN[[gsub("PMID", "", pmid)]]]
  roleCV <- codedValues$CodedValuesByType$ResourceRole[[match(role, c("Contact", "Producer", "Distributor", "Date Submitter"))]]
  roleCV$SerializationMode <- "full"
  mapped <- lapply(mapped,
                   function(x) c(Role = list(roleCV),
                                 Participant = list(x),
                                 boiler(SystemType = "Contributor")[-1])
                   )
}

# map2Person <- function(pmid, pmid2author, role = "Contact") {
#   mapped <- Persons[HIRN[[pmid]]]
#   roleCV <- codedValues$CodedValuesByType$ResourceRole[[match(role, c("Contact", "Producer", "Distributor", "Date Submitter"))]]
#   roleCV$SerializationMode <- "full"
#   mapped <- lapply(mapped,
#                    function(x) c(Role = list(roleCV),
#                                  Participant = list(x),
#                                  boiler(SystemType = "Contributor")[-1])
#   )
# }

# If only name is given, ontology must be specified. Ontologies can be inferred from id.
map2O <- function(name = NA, id = NA, ontology = NULL) {
  if(is.null(ontology)) ontology <- gsub("_.*", "", id)
  if(ontology == "CVCL") ontology <- "Cellosaurus"
  if(ontology == "UBERON") ontology <- "CL"
  mapped <- O[[ontology]][Name == name | ID == id, ]
  boiler(OntologyId = ontology, CanonicalId = mapped$IRI, Url = mapped$IRI, Name = mapped$Name)
}

# ResourceApplication [Contacts, Publication, Usage, UsageNotes, Rating]
map2Application <- function(Publication, Usage, UsageNotes, Rating, authorlookup = whichHIRN_id) {
  list(Contacts = authorlookup[[gsub("PMID", "", Publication)]],
       Publication = Publication,
       Usage = map2O(name = Usage, ontology = "OBI"),
       UsageNotes = UsageNotes,
       Rating = Rating)
}


# list(Contacts = map2Person())

#-- Misc schema selection --------------------------------------------------------------------------------------------#

# resourceSchema <- c("TargetClassType", "ParentResourceTypeId", "id", "CanonicalId", "Url", "Name",
#                      "Type", "SystemType", "Tags", "Visibility", "DateCreated", "VersionId")
# usethis::use_data(resourceSchema)
