## DKNet key
mykey <- "cfhrc2KVQfZVNYkC696DCX3MZj7QWXDX"

# Pulls supplementary info from Antibody Registry data using SciCrunch api
abCrunch <- function(query = NULL, maxhits = 10) {
  if(!length(query)) {
   return(data.frame(RRID = ""))
  } else {
    # nif-0000-07730-1 is the SciCrunch Source id for Antibody Registry
    search <- httr::GET("https://dknet.org/api/1/dataservices/federation/data/nif-0000-07730-1?",
                  query=list(q = query, key = mykey),
                  config = httr::config(ssl_verifypeer = FALSE)) %>% content(encoding = "UTF-8")
    abdata <- xml_find_first(search, "result/results") %>% xml_find_all("row")
    if(length(abdata) == 0) return (NULL)
    abdata <- sapply(abdata, function(x) xml_find_all(x, "data/value") %>% xml_text()) %>%
      gsub(pat = "<[^>]+>", rep = "") %>%
      trimws() %>%
      t()
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

# Wrapper around abCrunch
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

extraAbs <- function(x, text = NULL) {
  result <- lapply(x, function(x) abCrunch(x, maxhits = 10))
  abs <- rbindlist(result, fill = T)
  if(!is.null(text)) {
    text <- unlist(Map(rep, text, sapply(result, function(x) if(length(x)) nrow(x) else 0)),
                       use.names = F)
    abs[, Text := text]
  }
  abs[, RRIDCite := NULL]
  abs[, Reference := NULL]
  return(abs)
}


# input dt should be output of abCrunch, a dt with:
# RRID, Name, TargetAntigen, RRIDCite, Clonality, Reference, Comments, CloneName, RaisedIn, VendorName, VendorCat
# which will be used to populate/derive new columns:
# UsageNotes (dilution), AntigenSpecies, etc.
asInput <- function(dt) {
  dt[, ClonalityStatus := gsub(" antibody", "", Clonality)]
  dt[, UsageNotes := sapply(Text, getDilution)]
  dt[, AntigenSpecies := lapply(TargetAntigen, getAntigenSpecies)]
  dt[, AntibodyConjugate := sapply(Name, getAbConjugate)]
  dt[, AntibodyType := ifelse(AntibodyConjugate == "", "primary", "secondary")]
  return(dt)
}

getAntigenSpecies <- function(txt) {
  species <- if(grepl("produced in", txt)) return("") else O[["NCBITAXON"]]$Name[sapply(O[["NCBITAXON"]]$Name, function(s) grepl(s, txt))]
  # species <- paste0(species, collapse = "|")
  return(species)
}

getAbConjugate <- function(txt) {
  common <- "Alexa.*[0-9]{1,3}|FITC|Dy.*[0-9]{1,3}|^APC$|Rhodamine|Cy[0-9]|Pacific ?(Blue|Orange)"
  conj <- regmatches(txt, regexpr(common, txt))
  if(!length(conj)) conj <- ""
  return(conj)
}

getDilution <- function(txt) {
  dilution <- regmatches(txt, regexpr("1(:|/)[0-9,]{1,6}", txt))
  dilution <- gsub(",", "", dilution)
  dilution <- gsub("/", ":", dilution)
  if(!length(dilution)) dilution <- "" else dilution <- paste("dilution", dilution)
  return(dilution)
}

# write.table(currentdata, file = "Abs_supplemented.txt", sep = "\t", row.names = F)



