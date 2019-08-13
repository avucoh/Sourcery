# Summary: load ontologies, schemas, mapping functions, make validators, set batch

# Ontologies should already be .rda in package /Data, but perhaps for using with a different version or whatever,
# one can load ontologies and other dictionaries from the flat files into data.tables in the environment

# As-is flat files:
# 1. Cell Ontology --  https://bioportal.bioontology.org/ontologies/CL/
#    This is the "complete" version which contains inter-ontology axioms;
#    it already imports most of UBERON so don't need to obtain UBERON separately
# 2. Disease Ontology -- https://bioportal.bioontology.org/ontologies/DOID/
# 3. Ontology of Biomedical Investigations -- https://bioportal.bioontology.org/ontologies/OBI
# 4. Eagle-I Research Resource Ontology -- https://bioportal.bioontology.org/ontologies/ERO
# 5. OBI_BCGO -- https://bioportal.bioontology.org/ontologies/OBI_BCGO

# gets latest release from BioPortal
updateOntology <- function(acronym, apikey = "8b5b7825-538d-40e0-9e9e-5ab9274a9aeb") {
  url <- paste0("http://data.bioontology.org/ontologies/", acronym, "/download?apikey=", apikey, "&download_format=csv")
  tmp <- tempfile()
  tryCatch(download.file(url, tmp), error = function(e) {})
  o <- read.csv(gzfile(tmp), header = T, stringsAsFactors = F, check.names = F)
  o <- o[, c("Class ID", "Preferred Label", "Synonyms")]
  names(o) <- c("IRI", "Term", "Synonyms")
  o <- as.data.table(o)
  o[, ID := gsub("http://purl.obolibrary.org/obo/", "", IRI)]
  return(o)
}

batchUpdate <- function(os = c("CL", "GO", "OBI", "EFO", "MP")) {
  O <- lapply(os, updateOntology)
  names(O) <- os
  O
}

loadOntology <- function(o) {
  o <- fread(o, select = c("Class ID", "Preferred Label", "Synonyms"))
  setnames(o, c("IRI", "Name", "Synonyms"))
  o[, ID := gsub("http://purl.obolibrary.org/obo/", "", IRI)]
  o
}

# Wrapper around loadOntology to load all ontologies stored in current directory
# Expected that directory should contain c("CL.csv", "DOID.csv", "ERO.csv", "GO.csv", "OBI.csv", "CLO.csv")
loadOntologyDir <- function(dir = NULL) {
  if(is.null(dir)) dir <- getwd()
  os <- list.files(dir)
  O <- lapply(os, loadOntology)
  names(O) <- gsub("[.]csv|txt", "", os)
  O
}

# For loading pre-parsed ontology files without "Class ID", "Preferred Label", "Synonyms" column structure
# dir given is Ontologies2
# Used for Cellosaurus -- parsed from the obo file to csv -- ftp://ftp.expasy.org/databases/cellosaurus/cellosaurus.txt
# And a subset of NCBITAXON (the full ontology is huge)
loadOntologyDir2 <- function(dir = NULL) {
  if(is.null(dir)) dir <- getwd()
  os <- list.files(dir)
  O <- lapply(os, fread)
  names(O) <- gsub("[.](csv|txt)", "", os)
  O
}

# To update package data:
# Os <- loadOntologyDir()
# list2env(Os, envir = .GlobalEnv)
# usethis::use_data(CL); usethis::use_data(CLO); usethis::use_data(DOID); usethis::use_data(ERO); usethis::use_data(GO); usethis::use_data(OBI)

# Os2 <- loadOntologyDir2()
# list2env(Os2, envir = .GlobalEnv)
# usethis::use_data(Cellosaurus); usethis::use_data(NCBITAXON)

# The dictionaries from HIRN should also be .rda in package /Data,
# but these are the functions used for pulling the most recent versions

# codedValuess <- fromJSON(system.file("extdata/ResourceBrowser/codedValues.json", package = "Sourcery"), simplifyVector = F)
# usethis::use_data(codedValues)
# resourceTypes <- fromJSON(system.file("extdata/ResourceBrowser/codedValues.json", package = "Sourcery"), simplifyVector = F)
# usethis::use_data(resourceTypes)

# HIRN list
# Persons <- fromJSON(system.file("extdata/ResourceBrowser/contrib.json", package = "Sourcery"), simplifyVector = F)
# usethis::use_data(Persons, overwrite = T)

# HIRNauthors <- sapply(Persons, function(x) paste(x$LastName, x$FirstName))
# HIRNauthors[HIRNauthors == "Powers Al"] <- "Powers Alvin"
# usethis::use_data(HIRNauthors, overwrite = T)





