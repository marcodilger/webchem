  #' Retrieve Pubchem Id (CID)
#'
#' Return CompoundID (CID) for a search query using PUG-REST,
#' see \url{https://pubchem.ncbi.nlm.nih.gov/}.
#' @import httr
#'
#' @param query character; search term.
#' @param from character; type of input, can be one of 'name' (default), 'cid', 'sid', 'aid', 'smiles', 'inchi', 'inchikey'
#' @param first logical; If TRUE return only first result.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param arg character; optinal arguments like 'name_type=word' to match individual words.
#' @param ... optional arguments
#' @return a list of cids. If first = TRUE a vector.
#'
#' @references Wang, Y., J. Xiao, T. O. Suzek, et al. 2009 PubChem: A Public Information System for
#' Analyzing Bioactivities of Small Molecules. Nucleic Acids Research 37: 623–633.
#'
#' Kim, Sunghwan, Paul A. Thiessen, Evan E. Bolton, et al. 2016
#' PubChem Substance and Compound Databases. Nucleic Acids Research 44(D1): D1202–D1213.
#'
#' Kim, S., Thiessen, P. A., Bolton, E. E., & Bryant, S. H. (2015).
#' PUG-SOAP and PUG-REST: web services for programmatic access to chemical information in PubChem. Nucleic acids research, gkv396.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' get_cid('Triclosan')
#' get_cid('Triclosan', arg = 'name_type=word')
#' get_cid("BPGDAMSIGCZZLK-UHFFFAOYSA-N", from = 'inchikey')
#' get_cid("CCCC", from = 'smiles')
#'
#' # multiple inputs
#' comp <- c('Triclosan', 'Aspirin')
#' get_cid(comp)
#'
#' }
get_cid <- function(query, from = 'name', first = FALSE, verbose = TRUE, arg = NULL, ...) {
  # from can be cid | name | smiles | inchi | sdf | inchikey | formula
  # query <- c('Aspirin')
  # from = 'name'

  foo <- function(query, from, first, verbose, ...){
    prolog <- 'https://pubchem.ncbi.nlm.nih.gov/rest/pug'
    input <- paste0('/compound/', from)
    output <- '/cids/JSON'
    if (!is.null(arg))
      arg <- paste0('?', arg)
    qurl <- paste0(prolog, input, output, arg)
    if (verbose)
      message(qurl)
    Sys.sleep(0.2) # pubchem asks to limit requests to 5/sec
    cont <- try(content(POST(qurl,
                             body = paste0(from, '=', query)
                             ), type = 'text', encoding = 'UTF-8'),
                silent = TRUE
    )
    if (inherits(cont, "try-error")) {
      warning('Problem with web service encountered... Returning NA.')
      return(NA)
    }
    cont <- fromJSON(cont)
    if (names(cont) == 'Fault') {
      warning(cont$Fault$Details, '. Returning NA.')
      return(NA)
    }
    out <- unlist(cont)
    if (first)
      out <- out[1]
    names(out) <- NULL
    return(out)
  }
  out <- lapply(query, foo, from = from, first = first, verbose = verbose)
  out <- setNames(out, query)
  if (first)
    out <- unlist(out)
  return(out)
}



#' Retrieve compound properties from a pubchem CID
#'
#' Retrieve compound information from pubchem CID, see \url{https://pubchem.ncbi.nlm.nih.gov/}
#' @import httr jsonlite
#'
#' @param cid character; Pubchem ID (CID).
#' @param properties character vector; properties to retrieve, e.g. c('MolecularFormula', 'MolecularWeight').
#' If NULL (default) all available properties are retrieved.
#' See \url{https://pubchem.ncbi.nlm.nih.gov/pug_rest/PUG_REST.html#_Toc409516770} for a list of all available properties.
#' @param verbose logical; should a verbose output be printed to the console?
#' @param ... currently not used.
#'
#' @return a data.frame
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
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
#' pc_prop(5564)
#'
#' ###
#' # multiple CIDS
#' comp <- c('Triclosan', 'Aspirin')
#' cids <- unlist(get_cid(comp))
#' pc_prop(cids, properties = c('MolecularFormula', 'MolecularWeight', 'CanonicalSMILES'))
#' }
pc_prop <- function(cid, properties = NULL, verbose = TRUE, ...){
  # cid <- c('5564', '7843')
  napos <- which(is.na(cid))
  cid_o <- cid
  cid <- cid[!is.na(cid)]
  prolog <- 'https://pubchem.ncbi.nlm.nih.gov/rest/pug'
  input <- '/compound/cid'
  if (is.null(properties))
    properties <- c('MolecularFormula', 'MolecularWeight', 'CanonicalSMILES',
                  'IsomericSMILES', 'InChI', 'InChIKey', 'IUPACName',
                  'XLogP', 'ExactMass', 'MonoisotopicMass', 'TPSA',
                  'Complexity', 'Charge', 'HBondDonorCount', 'HBondAcceptorCount',
                  'RotatableBondCount', 'HeavyAtomCount', 'IsotopeAtomCount',
                  'AtomStereoCount', 'DefinedAtomStereoCount', 'UndefinedAtomStereoCount',
                  'BondStereoCount', 'DefinedBondStereoCount', 'UndefinedBondStereoCount',
                  'CovalentUnitCount', 'Volume3D', 'XStericQuadrupole3D',
                  'YStericQuadrupole3D', 'ZStericQuadrupole3D', 'FeatureCount3D',
                  'FeatureAcceptorCount3D', 'FeatureDonorCount3D', 'FeatureAnionCount3D',
                  'FeatureCationCount3D', 'FeatureRingCount3D', 'FeatureHydrophobeCount3D',
                  'ConformerModelRMSD3D', 'EffectiveRotorCount3D', 'ConformerCount3D',
                  'Fingerprint2D')
  properties <- paste(properties, collapse = ',')
  output <- paste0('/property/', properties, '/JSON')

  qurl <- paste0(prolog, input, output)
  if (verbose)
    message(qurl)
  Sys.sleep(0.2) # pubchem asks to limit requests to 5/sec
  cont <- try(content(POST(qurl,
                           body = list("cid" = paste(cid, collapse = ',')
                                       )),
                      type = 'text', encoding = 'UTF-8'),
              silent = TRUE
  )
  if (inherits(cont, "try-error")) {
    warning('Problem with web service encountered... Returning NA.')
    return(NA)
  }
  cont <- fromJSON(cont)
  if (names(cont) == 'Fault') {
    warning(cont$Fault$Message, '. ', cont$Fault$Details, '. Returning NA.')
    return(NA)
  }
  out <- cont$PropertyTable[[1]]
  # insert NA rows
  narow <- rep(NA, ncol(out))
  for (i in seq_along(napos)) {
    #capture NAs at beginning
    firstnna <- min(which(!is.na(cid_o)))
    if (napos[i] <  firstnna) {
      out <- rbind(narow, out)
    } else {
      # capture NAs at end
      if (napos[i] > nrow(out)) {
        # print(napos[i])
        out <- rbind(out, narow)
      } else {
        out <- rbind(out[1:(napos[i] - 1), ], narow, out[napos[i]:nrow(out), ])
      }
    }}
  rownames(out) <- NULL
  class(out) <- c('pc_prop','data.frame')
  return(out)
}


#' Search synonyms in pubchem
#'
#' Search synonyms using PUG-REST,
#' see \url{https://pubchem.ncbi.nlm.nih.gov/}.
#' @import httr jsonlite
#' @importFrom utils menu
#'
#' @param query character; search term.
#' @param from character; type of input, can be one of 'name' (default), 'cid',
#'     'sid', 'aid', 'smiles', 'inchi', 'inchikey'
#' @param interactive deprecated.  Use the `choices` argument instead
#' @param choices to get only the first synonym, use `choices = 1`, to get a number of synonyms to choose from in an interactive menu, provide the number of choices you want or "all" to choose from all synonyms.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param arg character; optinal arguments like 'name_type=word' to match individual words.
#' @param ... optional arguments
#' @return a character vector.
#'
#' @references Wang, Y., J. Xiao, T. O. Suzek, et al. 2009 PubChem: A Public Information System for
#' Analyzing Bioactivities of Small Molecules. Nucleic Acids Research 37: 623–633.
#'
#' Kim, Sunghwan, Paul A. Thiessen, Evan E. Bolton, et al. 2016
#' PubChem Substance and Compound Databases. Nucleic Acids Research 44(D1): D1202–D1213.
#'
#' Kim, S., Thiessen, P. A., Bolton, E. E., & Bryant, S. H. (2015).
#' PUG-SOAP and PUG-REST: web services for programmatic access to chemical information in PubChem. Nucleic acids research, gkv396.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \donttest{
#' pc_synonyms('Aspirin')
#' pc_synonyms(c('Aspirin', 'Triclosan'))
#' pc_synonyms(5564, from = 'cid')
#' pc_synonyms(c('Aspirin', 'Triclosan'), choices = 10)
#' }
pc_synonyms <- function(query, from = 'name', choices = NULL, verbose = TRUE, arg = NULL, interactive = 0, ...) {
  # from can be cid | name | smiles | inchi | sdf | inchikey | formula
  # query <- c('Aspirin')
  # from = 'name'
  if(!missing("interactive"))
    stop("'interactive' is deprecated. Use 'choices' instead.")
  foo <- function(query, from, verbose, ...){
    prolog <- 'https://pubchem.ncbi.nlm.nih.gov/rest/pug'
    input <- paste0('/compound/', from)
    output <- '/synonyms/JSON'
    if (!is.null(arg))
      arg <- paste0('?', arg)
    qurl <- paste0(prolog, input, output, arg)
    if (verbose)
      message(qurl)
    Sys.sleep(0.2) # pubchem asks to limit requests to 5/sec
    cont <- try(content(POST(qurl,
                             body = paste0(from, '=', query)
    )), silent = TRUE
    )
    if (inherits(cont, "try-error")) {
      warning('Problem with web service encountered... Returning NA.')
      return(NA)
    }
    if (names(cont) == 'Fault') {
      warning(cont$Fault$Details, '. Returning NA.')
      return(NA)
    }
    out <- unlist(cont)
    names(out) <- NULL

    out <- chooser(out, choices)

  }
  out <- lapply(query, foo, from = from, verbose = verbose)
  out <- setNames(out, query)
  if(!is.null(choices)) #if only one choice is returned, convert list to vector
    out <- unlist(out)
  return(out)
}


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
#' @return a list of lists, one list for each source, with the elements:
#' \item{name a}{description a}
#' \item{substance} {character; name of the substance according to the current source.}
#' \item{hazard_codes} {character vector; hazard statement codes ("H Codes").}
#' \item{hazard_statements} {character vector; hazard statement sentences ("H Phrases").}
#' \item{precautionary_statement_codes} {character vector; precautionary statement codes ("P Codes").}
#' \item{source_name} {character; name of the source.}
#' \item{source_desc} {character; description of the source.}
#' \item{url} {character; URL of the source.}
#' \item{license} {character; license of the source. Returns an empty character vector for many sources.}
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
