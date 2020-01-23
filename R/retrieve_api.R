# Methods that interact with the retrieval-oriented endpoints of HIRN Resource Browser API

#' @export
getResourceType <- function(type = c("Antibody", "CellLine", "Construct", "DifferentiatedCell", "ModelOrganism", "PrimaryCell", "StemCell", "Tissue",
                        "Epigenomics", "Genomics", "Proteomics", "Metabolomics", "Transcriptomics", "Protocol", "Assay",
                        "CodePipeline", "DeviceEquipment", "Service", "SoftwareDb"),
                        verbose = F) {
  if(length(type) > 1) return(getResourceType2(type))
  response <- httr::GET(url = paste0("https://resourcebrowser.hirnetwork.org/api/v2/p_Resource/", type), add_headers(Accept = "application/json"))
  status <- httr::status_code(response)
  if(status != 200L) stop(sprintf("API request failed [%s]", status_code(response)))
  response <- content(response)
  if(verbose) cat(paste(length(response), "resources retrieved"))
  return(response)
}

getResourceType2 <- function(types) {
  lapply(types, getResourceType)
}

#' @export
CanonicalId <- function(content) sapply(content, function(x) x$CanonicalId )

#' @export
Description <- function(content) sapply(content, function(x) x$Description )

#' @export
PMID <- function(content) sapply(content, function(x) x$DefiningManuscriptId)


#' @export
Name <- function(content) sapply(content, function(x) x$Name )

getResourceID <- function(id) {

}

response <- httr::GET(url = paste0("https://resourcebrowser.hirnetwork.org/api/v2/p_Resource/", type), add_headers(Accept = "application/json"))
response <- jsonlite::fromJSON(paste0("https://resourcebrowser.hirnetwork.org/api/v2/p_Resource/", type))
