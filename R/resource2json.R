#-- Misc schema selection --------------------------------------------------------------------------------------------#

# resourceSchema <- c("TargetClassType", "ParentResourceTypeId", "id", "CanonicalId", "Url", "Name",
#                      "Type", "SystemType", "Tags", "Visibility", "DateCreated", "VersionId")

# Create a mapping of PMID to authors
PMID2Authors <- function(pmids) {
  authors <- EUtilsGet(pmids, type = "efetch", db = "pubmed")
  authors <- Author(authors)
  authors <- lapply(authors, function(x) { x$Name <- iconv(paste(x$LastName, substr(x$ForeName, 1, 3)), from ="UTF-8", to="ASCII//TRANSLIT"); x })
  names(authors) <- pmids
  HIRNauthors <- sapply(Persons, function(x) paste(x$LastName, x$FirstName))
  HIRNauthors[HIRNauthors == "Powers Al"] <- "Powers Alvin"
  return(authors)
}


ds2json <- function(d, dsjson, dscat, batch, sep = "|", CV = codedValues, HIRN) {
  new <- dsjson
  new$TechnologyType <- map2O(id = d$OBI_ERO)
  new$BiosampleCharacteristics <- c(lapply(unlist(strsplit(d$GOTerm, sep, fixed = T)), function(x) map2O(name = x, "GO")),
                                    lapply(unlist(strsplit(d$DOIDTerm, sep, fixed = T)), function(x) map2O(name = x, "DOID")))
  new$BiosampleType <- c(lapply(unlist(strsplit(d$CellTissue, sep, fixed = T)), function(x) map2O(name = x, ontology = "CL")),
                         lapply(unlist(strsplit(d$CellLine, sep, fixed = T)), function(x) map2O(name = x, ontology = "CVCL")),
                         lapply(unlist(strsplit(d$Species, sep, fixed = T)), function(x) map2O(name = x, ontology = "NCBITAXON")))
  new$DatasetValue$Name <- as.character(d$Value)
  new$Description <- d$Description
  new$Contributors <- map2Person(d$DefiningManuscriptId, "Producer", HIRN)
  new$PrimaryResourceType <- resourceTypes$ResourceTypes[[2]][resourceSchema]
  new$PrimaryResourceType$SerializationMode <- "full"
  new$SecondaryResourceType <- dscat[[match(d$Category, c("Epigenomics", "Genomics", "Metabolomics", "Proteomics", "Transcriptomics", "Cellomics"))]]
  new$SecondaryResourceType$SerializationMode <- "full"
  new$CurationStatus <- CV$CodedValuesByType$CurationStatus[[1]]
  new$CurationStatus$SerializationMode <- "full"
  new$DefiningManuscriptId <- d$DefiningManuscriptId
  new$id <- uuid::UUIDgenerate()
  new$CanonicalId <- d$ACC
  new$Url <- d$URL
  new$Name <- d$Name
  new$DateCreated <- format_iso_8601(Sys.time())
  new$SystemType <- d$Category
  new$BatchNumber <- batch
  toJSON(new, pretty = T, null = "null", na = "null", auto_unbox = T)
}

# Wrapper
ds2json_batch <- function(srctable = NULL, file = system.file("Curated/Ds.txt", package = "Sourcery"), batch) {
  if(is.null(srctable)) srctable <- fread(file)
  # find HIRN authors from list of authors for each publication
  pmids <- as.character(srctable$DefiningManuscriptId)
  authors <- PMID2Authors(pmids)
  whichHIRN <- lapply(authors, function(x) unlist(sapply(x$Name, function(y) grep(y, HIRNauthors))))
  names(whichHIRN) <- pmids
  authCount <- lengths(whichHIRN)
  notHIRN <- srctable[!authCount, ]
  srctable <- srctable[as.logical(authCount), ]
  # get latest template
  dsjson <- fromJSON("http://test-resourcebrowser.azurewebsites.net/api/v1.0/AdminResource/GetNew/Proteomics")
  dscat <- lapply(resourceTypes$ResourceTypes[[2]]$SubResourceTypes, function(x) x[resourceSchema])
  Cellomics <- dscat[[1]]
  Cellomics$TargetClassType
  Cellomics$id <- Cellomics$CanonicalId <- "HIRN-Ds-Cell"
  dscat[[6]] <- Cellomics

  rlist <- lapply(apply(srctable, 1, as.list), function(x) ds2json(x, dsjson, dscat, batch = batch, HIRN = whichHIRN))
  result <- list(passed = rlist, needreview = notHIRN)
  return(result)
}

# --------------------------------------------------------------------------------------------------------------------------------#

tech2json <- function(t, subtype, techjson, batch, HIRN) {
  ## CodePipeline mirrors SoftwareDb, DeviceEquipment mirrors Service
  ## Assay has the most additional fields
  new <- techjson[[subtype]]
  new$Name <- t$Name
  new$Dependencies <- lapply(strsplit(t$Dependencies, "|", fixed = T)[[1]], function(x) map2O(id = x))
  new$CompatibleTools <- lapply(strsplit(t$CompatibleTools, "|", fixed = T)[[1]], function(x) map2O(id = x))
  new$Applications <- lapply(strsplit(t$Applications, "|", fixed = T)[[1]], function(x) map2O(id = x))
  new$Contributors <- map2Person(gsub("PMID", "", t$DefiningManuscriptId), t$Role, HIRN)
  new$Description <- t$Description
  new$CanonicalId <- t$RRID
  new$Url <- t$Url
  new$CurationStatus <- codedValues$CodedValuesByType$CurationStatus[[1]]
  new$CurationStatus$SerializationMode <- "full"
  new$DefiningManuscriptId <- t$DefiningManuscriptId
  new$id <- UUIDgenerate()
  new$BatchNumber <- batch
  new$PrimaryResourceType <- resourceTypes$ResourceTypes[[4]][resourceSchema]
  new$PrimaryResourceType$SerializationMode <- "full"
  new$SecondaryResourceType <- resourceTypes$ResourceTypes[[4]]$SubResourceTypes[[match(subtype, c("Assay", "CodePipeline", "DeviceEquipment", "Service", "SoftwareDb"))]][resourceSchema]
  new$SecondaryResourceType$SerializationMode <- "full"

  if(grepl("Assay", subtype)) {
    new$BioSampleType <- lapply(strsplit(t$BioSampleType, "|", fixed = T)[[1]], function(x) map2O(x, "NCBITAXON"))
    new$AssayCategory <- map2O(id = t$AssayCategory, ontology = "OBI")
  }
  if(grepl("Assay|Device|Service", subtype)) {
    new$CellsTestedWith <- lapply(strsplit(t$CellsTestedWith, "|", fixed = T)[[1]], function(x) map2O(id = x, ontology = "CL"))
  }
  toJSON(new, pretty = T, null = "null", na = "null", auto_unbox = T)
}

# find HIRN authors from list of authors for each publication
authorCheck <- function(srctable) {
  pmids <- as.character(srctable$DefiningManuscriptId)
  authors <- PMID2Authors(pmids)
  whichHIRN <- lapply(authors, function(x) unlist(sapply(x$Name, function(y) grep(y, HIRNauthors))))
  names(whichHIRN) <- pmids
  return(whichHIRN)
}



# batch
tech2json_batch <- function(srctable = NULL, file = system.file("Curated/Tech.txt", package = "Sourcery"), batch) {
  if(is.null(srctable)) srctable <- fread(file)
  whichHIRN <- authorCheck(srctable)
  authcounts <- lengths(whichHIRN)
  notHIRN <- srctable[!as.logical(authcounts), ]
  srctable <- srctable[as.logical(authcounts), ]
  # get latest templates
  techjson <- lapply(c("http://test-resourcebrowser.azurewebsites.net/api/v1.0/AdminResource/GetNew/Assay",
                    "http://test-resourcebrowser.azurewebsites.net/api/v1.0/AdminResource/GetNew/CodePipeline",
                    "http://test-resourcebrowser.azurewebsites.net/api/v1.0/AdminResource/GetNew/DeviceEquipment",
                    "http://test-resourcebrowser.azurewebsites.net/api/v1.0/AdminResource/GetNew/Service",
                    "http://test-resourcebrowser.azurewebsites.net/api/v1.0/AdminResource/GetNew/SoftwareDb"
  ), fromJSON)
  names(techjson) <- c("Assay", "CodePipeline", "DeviceEquipment", "Service", "SoftwareDb")
  # Go through table
  rlist <- lapply(apply(srctable, 1, as.list), function(t) tech2json(t, t$Type, techjson, batch = batch, HIRN = whichHIRN))
  result <- list(passed = rlist, srctable = srctable, needreview = notHIRN)
  return(result)
}

# Example:
# export <- tech2json_batch(batch = "4")
# export$passed



# When one needs to modify the R list structure so that toJSON will be in the correct format
unboxR <- function(x) {
  if(length(x) == 0) return(NA)
  if(!is.list(x[[1]])) return(unlist(x))
  else {
    r <- lapply(x, unboxR)
    r[names(r) == "Tags"][[1]] <- list() ## Have to keep Tags as array
    r
  }
}
