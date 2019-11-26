library(httr)
library(xml2)
library(stringr)


# pug-rest failed (returns huge objects)
# pug view via JSON would work, but quite complicated to reach the right nodes
# try with xml response

classification_query <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/712/XML?heading=GHS+Classification"

cont <- try(content(POST(classification_query),
                    type = 'text', encoding = 'UTF-8'),
            silent = TRUE
)

#saveRDS(cont, "data/content_ghs_temp.rda") # temp, for offline testing
cont <- readRDS("data/content_ghs_temp.rda")

ghs_xml <- xml_ns_strip(read_xml(cont))

# of interest are all
# <StringWithMarkup>
# within <Information> Tags that have a
# <Name>GHS Hazard Statements</Name>


# extract all available references containing GHS hazard statements
ghs_refs <- xml_text(xml_find_all(ghs_xml, xpath = "//Information[Name][contains(., 'GHS Hazard Statements')]//ReferenceNumber"))
# note: returned reference ID are not consistent accross substance, reference ID is not suitable as identifier of sources

# make named vector of the references, names are the id, values to be used for selection
# (yet to be implemnted to work this way in the function)
names(ghs_refs) <- ghs_refs
ghs_refs <- sapply(ghs_refs, function(ref) xml_text(xml_find_all(ghs_xml, xpath = paste0("Reference[ReferenceNumber[text()='", ref, "']]//SourceName"))))



# select references
ref_selection <- c("ECHA", "CLP")

# example without function
# probably best solution; create a list of available references, already
# compatible with the final repsonse list
# then use an additional list element serving as a flag if it is to be included
# and at the end remove any references not to be included
# then use this list as frame for the final return/repsonse list

# ja, am besten hier schon die liste uafbauen, obwohl angangs mit absicht dagegen entshcieden:
# named list element, mit name = id, mit jeweils eintrag  $ source_name wie unten. dnn diese Liste filtern



get_ref_ids <- function(refs_available, ref_selected) { # gets a named vector of available reference IDs, with the string description in the value and the numeric id as names; a reference ID to be searched,
                            # returns the ID(s) of the reference, if included

# refs_available entspricht ghs_refs
# ref_selected entspricht z.B. "ECHA"

# temporary: df for lookup reference-detectionstring,
#  needs to be outside of fct

  searchstring <- c(
    "European Chemicals Agency", # string to look for, for "ECHA"
    "EU REGULATION \\(EC\\) No 1272/2008" # string to look for, for "CLP"
  )
  names(searchstring) <- c(
    "ECHA",
    "CLP"
  )

  # for now with a loop # ToDo: continue here
  returned_ids <- NULL
  for (i in seq_along(searchstring)) {
    if (any(str_detect(ref_selected, names(searchstring[i])))) { # geht so auc für vektor ref_selection, statt string ref_secetion
      response <- refs_available[str_detect(refs_available, searchstring[i])] # precise string that needs to be present
      returned_ids <- c(returned_ids, response)
    }

  }
  return(returned_ids)
}

get_ref_ids(ghs_refs, ref_selected = c("CLP", "ECHA"))

# lookup tables using named vectors
# https://www.infoworld.com/article/3323006/do-more-with-r-quick-lookup-tables-using-named-vectors.html




out <-  list()
for(ref in names(ghs_refs)) {

  source <- xml_find_all(ghs_xml, xpath = paste0("Reference[ReferenceNumber[text()='", ref, "']]"))
  source_name <- xml_text(xml_find_all(source, xpath = "SourceName"))
  source_substancename <- xml_text(xml_find_all(source, xpath = "Name"))
  source_description <- xml_text(xml_find_all(source, xpath = "Description"))
  source_url <- xml_text(xml_find_all(source, xpath = "URL"))

  # at least the ECHA source requires the license note to be transported
  source_license <- xml_text(xml_find_all(source, xpath = "LicenseNote"))

  hazard_statements <- xml_text(xml_find_all(ghs_xml, xpath = paste0("//Information[ReferenceNumber[text()='", ref, "']][Name][contains(., 'GHS Hazard Statements')]//StringWithMarkup/String")))
  hazard_statements <- hazard_statements[hazard_statements != ""]
  hazard_codes <- str_extract(hazard_statements, "^H\\d{3}(\\s?\\*|[A-z]{0,2})(\\+H\\d{3}[A-z]{0,2})*")
  hazard_codes <- sort(unique(hazard_codes[hazard_codes != ""]))

  prec_codes <- xml_text(xml_find_all(ghs_xml, xpath = paste0("//Information[ReferenceNumber[text()='", ref, "']][Name][contains(., 'Precautionary Statement Codes')]//StringWithMarkup/String") ))
  prec_codes <- unlist(str_extract_all(prec_codes, "P\\d{3}(\\+P\\d{3})*"))

  # build output list(s)

  out[[length(response) + 1]] <-  list(
    substance = source_substancename,
    hazard_codes = hazard_codes,
    hazard_statements = hazard_statements,
    precautionary_statement_codes = prec_codes,
    source_name = source_name,
    source_desc = source_description,
    url = source_url,
    license = source_license)


}




##### bring to function

# temp
cid <- 6341
verbose = TRUE



pc_ghs <- function(cid,
                   sources = "all",
                   verbose = TRUE){
  # cid: for now only a single cid, ToDo: implementation of multiple cid like in other pc_ functions
  # sources: for now only "all", which returns a list of lists from all available sources containing GHS information

  prolog <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/"
  input <- paste0("compound/", cid,"/")
  output <- "XML?heading=GHS+Classification"

  qurl <- paste0(prolog, input, output)
  if (verbose)
    message(qurl)
  Sys.sleep(0.2) # pubchem asks to limit requests to 5/sec

  cont <- try(content(POST(qurl),
                      type = 'text', encoding = 'UTF-8'),
              silent = TRUE
  )
  if (inherits(cont, "try-error")) {
    warning('Problem with web service encountered... Returning NA.')
    return(NA)
  }
  # fault handling (e.g. non existant CID)
  if (length(xml_find_all(ghs_xml, xpath = "/Fault")) != 0) {
    warning(xml_text(xml_find_all(ghs_xml, xpath = '/Fault/Message')), ". Returning NA.")
    return(NA)
  }

  # extract relevant xml
  ghs_xml <- xml_ns_strip(read_xml(cont))
  # extract all available references containing GHS hazard statements
  # returned reference ID are not consistent accross substance, reference ID is not suitable as identifier of sources
  ghs_refs <- xml_text(xml_find_all(ghs_xml, xpath = "//Information[Name][contains(., 'GHS Hazard Statements')]//ReferenceNumber"))

  out <-  list()
  for(ref in ghs_refs) {
    source <- xml_find_all(ghs_xml, xpath = paste0("Reference[ReferenceNumber[text()='", ref, "']]"))
    source_name <- xml_text(xml_find_all(source, xpath = "SourceName"))
    source_substancename <- xml_text(xml_find_all(source, xpath = "Name"))
    source_description <- xml_text(xml_find_all(source, xpath = "Description"))
    source_url <- xml_text(xml_find_all(source, xpath = "URL"))

    # at least the ECHA source requires the license note to be transported
    source_license <- xml_text(xml_find_all(source, xpath = "LicenseNote"))

    hazard_statements <- xml_text(xml_find_all(ghs_xml, xpath = paste0("//Information[ReferenceNumber[text()='", ref, "']][Name][contains(., 'GHS Hazard Statements')]//StringWithMarkup/String")))
    hazard_statements <- hazard_statements[hazard_statements != ""]
    hazard_codes <- str_extract(hazard_statements, "^H\\d{3}[A-z]{0,2}(\\+H\\d{3}[A-z]{0,2})*")
    hazard_codes <- sort(unique(hazard_codes[hazard_codes != ""]))

    prec_codes <- xml_text(xml_find_all(ghs_xml, xpath = paste0("//Information[ReferenceNumber[text()='", ref, "']][Name][contains(., 'Precautionary Statement Codes')]//StringWithMarkup/String") ))
    prec_codes <- unlist(str_extract_all(prec_codes, "P\\d{3}(\\+P\\d{3})*"))

    # build output list(s)

    out[[length(out) + 1]] <-  list(
      substance = source_substancename,
      hazard_codes = hazard_codes,
      hazard_statements = hazard_statements,
      precautionary_statement_codes = prec_codes,
      source_name = source_name,
      source_desc = source_description,
      url = source_url,
      license = source_license
      )
  }
    return(out)
}

pc_ghs(cid)
final_response_list <- pc_ghs(cid)

# ToDo: variable/list with sources to extract: named specificaly (returning empty when not available)
# or option 'all' for all available
  # needed for this: explicit list of names which can occur as sources

#ToDo: add support for multipe CIDs: in the outer list, each element is the response from one CID