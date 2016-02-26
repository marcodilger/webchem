#' Get Drugbank ID
#'
#' Query Drugbank
#' \url{http://www.drugbank.ca/} for their Drugbank ID
#'
#' @import xml2 httr
#' @importFrom stats rgamma
#'
#' @param query character; The searchterm
#' @param match character; How should multiple hits be handeled? 'all' returns all matched IDs,
#' 'first' only the first match, 'best' the best matching (by name) ID, 'ask' is a interactive mode and the user is asked for input,
#' 'na' returns NA if multiple hits are found.
#' @param verbose logical; print message during processing to console?
#'
#' @return if match = 'all' a list with etoxids, otherwise a dataframe with 4 columns:
#' etoxID, matched substance, string distance to match and the queried string
#'
#' @note Before using this function, please read the disclaimer \url{http://www.drugbank.ca/about}.
#'
#' @seealso
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' # might fail if API is not available
#' get_dbid('Triclosan', match = 'all')
#' get_dbid('Atrazin', match = 'all')
#' get_dbid('Atrazin', match = 'best')
#' get_dbid('Atrazin', match = 'na')
#' get_dbid('Atrazin', match = 'first')
#' get_dbid('Atrazin', match = 'ask')
#' get_dbid(c('Triclosan', 'Atrazin'), match = 'best')
#' }
get_dbid <- function(query, match = c('best', 'all', 'first', 'ask', 'na'), verbose = TRUE) {
  match <- match.arg(match)
  foo <- function(query, match, verbose){
    # query <- 'Triclosan'
    # query <- 'Atrazin'
    # query <- 'xxxxxxx'
    if (verbose)
      message('Searching ', query)
    baseurl <- 'http://www.drugbank.ca/unearth/q?searcher=drugs'
    qurl <- paste0(baseurl, '&query=', query)
    Sys.sleep( rgamma(1, shape = 15, scale = 1/10))
    h <- GET(qurl)
    # get substances and links
    tt <- content(h)
    ids <- xml_text(xml_find_all(tt, '//a[@class="btn-card"]'))

    if (length(ids) == 0) {
      if (verbose)
        message('Not found! Returing NA. \n')
      out <- NA
      attr(out, "matched") <- NA
      attr(out, "distance") <- NA
      return(out)
    }

    # multiple hits
    if (length(ids) > 1) {
      if (verbose)
        message("More then one Link found. \n")
      if (match == 'na') {
        if (verbose)
          message("Returning NA. \n")
        id <- NA
        matched_sub <- NA
        d <- NA
      }
      if (match == 'all') {
        if (verbose)
          message("Returning all matches. \n")
        id <- hits
        matched_sub <- xml_text(xml_find_all(tt, '//div[@class="hit-name"]/a'))
        d <- 'all'
      }
      if (match == 'first') {
        if (verbose)
          message("Returning first match. \n")
        id <- ids[1]
        matched_sub <- xml_text(xml_find_all(tt, '//div[@class="hit-name"]/a'))[1]
        d <- 'first'
      }
      if (match == 'best') {
        if (verbose)
          message("Returning best match. \n")
        msubs <- xml_text(xml_find_all(tt, '//div[@class="hit-name"]/a'))
        dd <- adist(query, msubs) / nchar(msubs)
        id <- ids[which.min(dd)]
        d <- round(dd[which.min(dd)], 2)
        matched_sub <- msubs[which.min(dd)]
      }
      if (match == 'ask') {
        msubs <- xml_text(xml_find_all(tt, '//div[@class="hit-name"]/a'))
        tochoose <- data.frame(match = msubs,
                               cas = xml_text(xml_find_all(tt, '//div[@class="cas-number"]')),
                               dbid = ids)
        print(tochoose)
        message("\nEnter rownumber of compounds (other inputs will return 'NA'):\n") # prompt
        take <- as.numeric(scan(n = 1, quiet = TRUE))
        if (length(take) == 0) {
          id <- NA
          matched_sub <- NA
          d <- NA
        }
        if (take %in% seq_len(nrow(tochoose))) {
          id <- ids[take]
          matched_sub <- msubs[take]
          d <- 'interactive'
        }
      }
    } else {
      id <- ids
      d <- 0
      matched_sub <- xml_text(xml_find_all(tt, '//div[@class="hit-name"]/a'))
    }
    # return object
    names(id) <- NULL
    attr(id, "matched") <- matched_sub
    attr(id, "distance") <- d
    return(id)
  }
  out <- lapply(query, foo, match = match, verbose = verbose)
  if (match != 'all') {
    out <- data.frame(t(sapply(out, function(y) {
      c(y, attr(y, 'matched'), attr(y, 'distance'))
    })), stringsAsFactors = FALSE)
    names(out) <- c('dbid', 'match', 'distance')
    out[['query']] <- query
  }
  return(out)
}




