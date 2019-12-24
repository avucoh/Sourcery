# Parse mentions of antibodies in different formats for an RRID or catalog number/name
text2Abs <- function(text, regx = "(?<=\\))(;|,|\\n)", split = T) {
  # assumes relevant mentions will be wrapped in () and separated by a comma or line break
  # i.e. "(Mercodia Cat# 10-1201-01, RRID:AB_2636872), (R and D Systems Cat# AF789, RRID:AB_2092786)"
  if(split) text <- strsplit(text, regx, perl = T)[[1]]
  x <- matchRRID(text)
  # if no RRID, resort to cat# match
  x[x == ""] <- matchAb(text[x == ""])
  parsed <- list(x = x, text = text)
  parsed
}

matchRRID <- function(text) {
  rridmatch <- regexpr("RRID:[A-Z]+_[0-9]+", text)
  x <- vector("character", length(rridmatch))
  x[rridmatch != -1] <- regmatches(text, rridmatch)
  x[rridmatch == -1] <- ""
  return(x)
}

matchAb <- function(text) {
  m <-  regexpr("([a-zA-Z]{1,2})?[^,:( /][-0-9]{3,7}", text)
  x <- vector("character", length(m))
  x[m != -1] <- gsub(",|Cat|#", "", regmatches(text, m))
  x[m == -1] <- ""
  return(x)
}
