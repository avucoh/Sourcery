## DKNet key
mykey <- "cfhrc2KVQfZVNYkC696DCX3MZj7QWXDX"

# Pulls supplementary info from Antibody Registry data using SciCrunch api
abCrunch <- function(query = NULL, maxhits = 1) {
  if(!length(query)) {
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
    abdata <- abdata[, -12] # last column is SciCrunch uuid
    n <- min(nrow(abdata), maxhits)
    abdata <- abdata[1:n, ]
    # DKnet antibody table data headers
    names(abdata) <- c("RRID", "Name", "TargetAntigen", "RRIDCite", "Clonality", "Reference", "Comments", "CloneName", "RaisedIn", "VendorName", "VendorCat")
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

supplementAbs <- function(x, txt) {
  sdata <- lapply(x, function(x) abCrunch(x, maxhits = 5))
  if(is.null(txt)) {
    text <- dilutions <- ""
  } else {
    dilutions <- Map(rep, sapply(txt, getDilution), sapply(sdata, function(x) nrow(x))) %>% unlist()
  }
  sdata <- rbindlist(sdata, fill = T)
  # sdata[, Text := text]
  # sdata[, Dilution := dilutions]
  # sdata[, AntigenSpecies := sapply(TargetAntigen, getAntigenSpecies)]
  # sdata[, AntibodyConjugate := sapply(Name, getAbConjugate)]
  sdata[, RRIDCite := NULL]
  sdata[, Reference := NULL]
  # setcolorder(sdata, c("Text", setdiff(names(sdata), "Text")))
  return(sdata)
}

getAntigenSpecies <- function(txt) {
  species <- if(grepl("produced in", txt)) return("") else O[["NCBITAXON"]]$Name[sapply(O[["NCBITAXON"]]$Name, function(s) grepl(s, txt))]
  species <- paste0(species, collapse = "|")
  return(species)
}

getAbConjugate <- function(txt) {
  common <- "Alexa.*[0-9]{1,3}|FITC|Dy.*[0-9]{1,3}|^APC$|Rhodamine|Cy[0-9]|Pacific ?(Blue|Orange)"
  conj <- regmatches(txt, regexpr(common, txt))
  return(conj)
}

getDilution <- function(txt) {
  dilution <- regmatches(txt, regexpr("1(:|/)[0-9,]{1,6}", txt))
  dilution <- gsub(",", "", dilution)
  if(!length(dilution)) dilution <- ""
  return(dilution)
}

# write.table(currentdata, file = "Abs_supplemented.txt", sep = "\t", row.names = F)



