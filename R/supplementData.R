## DKNet key
mykey <- "cfhrc2KVQfZVNYkC696DCX3MZj7QWXDX"

# Pulls supplementary info from Antibody Registry data using SciCrunch api
abCrunch <- function(id = NULL, catalog, maxhits = 1) {
  query <- if(!is.null(id)) id else gsub(",|Cat|#", "", catalog)
  if(query == "") {
   return(data.frame(RRID = ""))
  } else {
    # nif-0000-07730-1 is the SciCrunch Source id for Antibody Registry
    search <- GET("https://dknet.org/api/1/dataservices/federation/data/nif-0000-07730-1?",
                  query=list(q = query, key = mykey),
                  config = httr::config(ssl_verifypeer = FALSE)) %>% content(encoding = "UTF-8")
    abdata <- xml_find_first(search, "result/results") %>% xml_find_all("row")
    if(length(abdata) == 0) return (NULL)
    abdata <- t(sapply(abdata, function(x) xml_find_all(x, "data/value")
                       %>% xml_text())
                %>% gsub(pat = "<[^>]+>", rep = "")
                %>% trimws())
    abdata <- as.data.frame(abdata, stringsAsFactors = F)
    abdata <- abdata[, -12] # last column is the SciCrunch uuid
    n <- min(nrow(abdata), maxhits)
    abdata <- abdata[1:n, ]
    # DKnet antibody table data headers
    names(abdata) <- c("RRID", "Name", "TargetAntigen", "VendorName", "VendorCat", "RRIDCite", "Reference", "Clonality", "CloneName", "RaisedIn", "Comments")
    abdata$VendorName <- gsub(" Go To Vendor", "", abdata$VendorName)
    abdata$TargetAntigen <- gsub("See NCBI gene ", "", abdata$TargetAntigen)
    return(abdata)
  }
}

# Wrapper around absCrunch
supplementAbsFile <- function(file = NULL) {
  if(is.null(file)) {
    currentdata <- fread(system.file("Curated/Abs/Abs.csv", package = "Sourcery"))
  } else {
    currentdata <- fread(file)
  }
  ids <- currentdata$RRID
  sdata <- lapply(ids, abCrunch)
  sdata <- rbindlist(sdata, fill = T)
  indices <- match(sdata$RRID, currentdata$RRID)
  sdata[, AntigenSpecies := getAntigenSpecies(TargetAntigen)]
  scols <- c("Name", "AntigenSpecies", "CloneName", "Clonality", "RaisedIn", "VendorName", "VendorCat")
  currentdata[indices, (scols) := sdata[, scols, with = F] ]
  return(currentdata)
}

supplementAbs <- function(ids, txt) {
  sdata <- lapply(ids, function(x) abCrunch(x, maxhits = 5))
  if(is.null(txt)) {
    text <- dilutions <- ""
  } else {
    text <- rep.int(txt, sapply(sdata, function(x) nrow(x)))
    dilutions <- rep.int(getDilution(txt), sapply(sdata, function(x) nrow(x)))
  }
  sdata <- rbindlist(sdata, fill = T)
  sdata[, Text := text]
  sdata[, Dilution := dilutions]
  sdata[, AntigenSpecies := getAntigenSpecies(TargetAntigen)]
  sdata[, AntibodyConjugate := getAbConjugate(Name)]
  sdata[, RRIDCite := NULL]
  sdata[, Reference := NULL]
  setcolorder(sdata, c("Text", setdiff(names(sdata), "Text")))
  return(sdata)
}

getAntigenSpecies <- function(txt) {
  AS <- lapply(txt,
               function(x) {
                  if(grepl("produced in", txt)) return("") else O[["NCBITAXON"]]$Name[sapply(O[["NCBITAXON"]]$Name, function(s) grepl(s, x))]
               })
  AS <- sapply(AS, function(x) paste0(x, collapse = "|"))
  return(AS)
}

getAbConjugate <- function(txt) {
  common <- "Alexa.*[0-9]{1,3}|FITC|Dy.*[0-9]{1,3}|^APC$|Rhodamine|Cy[0-9]|Pacific ?(Blue|Orange)"
  conj <- sapply(txt, function(x) regmatches(x, regexpr(common, x)) )
  return(conj)
}

getDilution <- function(txt) {
  dilution <- sapply(txt, function(x) regmatches(x, regexpr("1:[0-9,]{1,6}", x)) )
  dilution <- gsub(",", "", dilution)
  return(dilution)
}

# write.table(currentdata, file = "Abs_supplemented.txt", sep = "\t", row.names = F)



