# Resource validator -------------------------------------------------------------------------------------------------#
#' @export
createValidator <- function(type) {
  switch(type,
         ## Antibody
         Antibody = jsonvalidate::json_validator(RCurl::getURL("https://test-resourcebrowser.azurewebsites.net/api/v1.0/Schema/GetSchema/Antibody?resourceType=BioReagent")),
         ## Cell types:
         CellLine = jsonvalidate::json_validator(RCurl::getURL("https://test-resourcebrowser.azurewebsites.net/api/v1.0/Schema/GetSchema/CellLine?resourceType=BioReagent")),
         DifferentiatedCell = jsonvalidate::json_validator(RCurl::getURL("https://test-resourcebrowser.azurewebsites.net/api/v1.0/Schema/GetSchema/DifferentiatedCell?resourceType=BioReagent")),
         PrimaryCell = jsonvalidate::json_validator(RCurl::getURL("https://test-resourcebrowser.azurewebsites.net/api/v1.0/Schema/GetSchema/PrimaryCell?resourceType=BioReagent")),
         StemCell = jsonvalidate::json_validator(RCurl::getURL("https://test-resourcebrowser.azurewebsites.net/api/v1.0/Schema/GetSchema/StemCell?resourceType=BioReagent")),
         ## Model Org
         ModelOrganism = jsonvalidate::json_validator(RCurl::getURL("https://test-resourcebrowser.azurewebsites.net/api/v1.0/Schema/GetSchema/ModelOrganism?resourceType=BioReagent")),
         ## Database types (schemas for Proteomics, Genomics, etc., are actually interchangeable, so just use Proteomics):
         Proteomics = jsonvalidate::json_validator(RCurl::getURL("https://test-resourcebrowser.azurewebsites.net/api/v1.0/Schema/GetSchema/Proteomics?resourceType=Dataset")),
         ## Technology
         Assay = jsonvalidate::json_validator(RCurl::getURL("https://test-resourcebrowser.azurewebsites.net/api/v1.0/Schema/GetSchema/Assay?resourceType=Technology")),
         CodePipeline = jsonvalidate::json_validator(RCurl::getURL("https://test-resourcebrowser.azurewebsites.net/api/v1.0/Schema/GetSchema/CodePipeline?resourceType=Technology")),
         DeviceEquipment = jsonvalidate::json_validator(RCurl::getURL("https://test-resourcebrowser.azurewebsites.net/api/v1.0/Schema/GetSchema/DeviceEquipment?resourceType=Technology")),
         Service = jsonvalidate::json_validator(RCurl::getURL("https://test-resourcebrowser.azurewebsites.net/api/v1.0/Schema/GetSchema/Service?resourceType=Technology")),
         SoftwareDb = jsonvalidate::json_validator(RCurl::getURL("https://test-resourcebrowser.azurewebsites.net/api/v1.0/Schema/GetSchema/SoftwareDb?resourceType=Technology"))
  )
}

#' @export
# Can take a list of jsons to validate
useValidator <- function(jsonlist, validator) {
  sapply(jsonlist, validator)
}

#' @export
batchValidator <- function(jsonlist, type) {
  Val <- createValidator(type)
  result <- useValidator(jsonlist, Val)
  return(result)
}

# Examples
# export <- ds2json_batch(batch = "4")
# batchValidator(export$passed, "Proteomics")
# validated <- batchValidator(export$passed, "Proteomics")
# all(validated)
# for(i in seq_along(export$passed)) write(export$passed[[i]], paste0(i, ".json"))


# export <- tech2json_batch(batch = "5")
# jsonlist <- split(export$passed, f = export$srctable$Type)
# validated <- mapply(batchValidator, jsonlist, names(jsonlist))
# jsonlist <- unlist(jsonlist)
# for(i in seq_along(jsonlist)) write(jsonlist[i], file = paste0(i, ".json"))
