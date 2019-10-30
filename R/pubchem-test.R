library(httr)
library(xml2)
library(stringr)


# pug-rest failed (returns huge objects)
# pug view via JSON would work, but quite complicated to reach the right nodes
# try with xml response

# classification_query <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/6341/XML?heading=GHS+Classification"
#
# cont <- try(content(POST(classification_query),
#                     type = 'text', encoding = 'UTF-8'),
#             silent = TRUE
# )
#
# saveRDS(cont, "data/content_ghs_temp.rda")
#cont <- readRDS("data/content_ghs_temp.rda")

ghs_xml <- xml_ns_strip(read_xml(cont))

# of interest are all
# <StringWithMarkup>
# within <Information> Tags that have a
# <Name>GHS Hazard Statements</Name>


# extract all available references containing GHS hazard statements
ghs_refs <- xml_text(xml_find_all(ghs_xml, xpath = "//Information[Name][contains(., 'GHS Hazard Statements')]//ReferenceNumber"))
# note: returned reference ID are not consistent accross substance, reference ID is not suitable as identifier of sources

# access hazard statements of referencenumbers directly
#response <- xml_find_all(ghs_xml, xpath = "//Information[ReferenceNumber[text()='52']][Name][contains(., 'GHS Hazard Statements')]//StringWithMarkup/String") # <- works
#xml_text(response)

# dynamically get GHS statements
for (ref in ghs_refs) {
  print(ref)
  response <- xml_find_all(ghs_xml, xpath = paste0("//Information[ReferenceNumber[text()='", ref, "']][Name][contains(., 'GHS Hazard Statements')]//StringWithMarkup/String") )
  print(response)
}


# build response list for each reference
ref <- 11
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


out[1]



##### bring to function

# temp
cid <- 6341
verbose = TRUE


#' Retrieve GHS classification from a pubchem CID
#'
#' Retrieve available information on GHS classification of compound from Pubchem CID, see \url{https://pubchem.ncbi.nlm.nih.gov/}
#' @import httr xml2 stringr
#'
#' @param cid character; Pubchem ID (CID).
#' @param sources character vector; sources of GHS information to return,
#'                for now only accepts "all" for all available sources.
#'                Available sources vary depending on the substance.
#' @param verbose logical; should a verbose output be printed to the console?
#'
#' @md
#' @return a list of lists, one list for each source, with the elements:
#'  * substance character; name of the substance according to the current source.
#'  * hazard_codes character vector; hazard statement codes ("H Codes").
#'  * hazard_statements character vector; hazard statement sentences ("H Phrases").
#'  * precautionary_statement_codes character vector; precautionary statement codes ("P Codes").
#'  * source_name character; name of the source.
#'  * source_desc character; description of the source.
#'  * url character; URL of the source.
#   * license character; license of the source. Returns an empty character vector for many sources.
#' @author Marco Dilger, \email{marco.dilger@@gmail.com}
#' @seealso \code{\link{get_cid}} to retrieve Pubchem IDs.
#' @references Wang, Y., J. Xiao, T. O. Suzek, et al. 2009 PubChem: A Public Information System for
#' Analyzing Bioactivities of Small Molecules. Nucleic Acids Research 37: 623–633.
#'
#' Kim, Sunghwan, Paul A. Thiessen, Evan E. Bolton, et al. 2016
#' PubChem Substance and Compound Databases. Nucleic Acids Research 44(D1): D1202–D1213.
#'
#' Kim, S., Thiessen, P. A., Bolton, E. E., & Bryant, S. H. (2015).
#' PUG-SOAP and PUG-REST: web services for programmatic access to chemical information in PubChem. Nucleic acids research, gkv396.
#'
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' pc_ghs(8148)
#' }
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
  # ToDo: fault handling

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
    hazard_codes <- str_extract(hazard_statements, "^H\\d{3}(\\s?\\*|[A-z]{0,2})(\\+H\\d{3}[A-z]{0,2})*")
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


# ToDo: variable/list with sources to extract: named specificaly (returning empty when not available)
# or option 'all' for all available
  # needed for this: explicit list of names which can occur as sources



# entweder es wird an der Stelle dann allgemein gehalten,
# oder: Fallunterscheidung je nach Quelle (ECHA, NITE-CMC, HCIS)
# Liste mit allenauftretenden Sources?

# todo: add roxygen documentation
# https://github.com/r-lib/roxygen2#roxygen2