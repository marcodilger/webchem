#' Query Data bank of Environmental Properties of Chemicals (EnviChem)
#'
#' Query Data bank of Environmental Properties of Chemicals (EnviChem)
#' \url{https://wwwp.ymparisto.fi/scripts/kemrek/language.asp?Language=UK}
#' @import httr xml2
#' @importFrom stats rgamma
#'
#' @param query character; The searchterm
#' @param type character; Type of searchterm. Can be 'cas', 'name' or 'free' (for freetext)
#' @param verbose logical; print message during processing to console?
#' @return a list with lists of 7 entries: name, cas, synonyms, formula, einecs,
#' molecular weigth and references
#'
#' @note see also \url{http://www.ymparisto.fi/en-US/Maps_and_statistics/Data_systems/Data_bank_of_Environmental_Properties_of%2830591%29}
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' h <- ec_query('50-00-0', 'cas')
#' h[['50-00-0']]$name
#'
#' h <- ec_query(c('50-00-0', '3380-34-5'), 'cas')
#' }


ec_query <- function(query, type = c('cas', 'name', 'free'), verbose = TRUE) {
  type <- match.arg(type)
  foo <- function(query, type, verbose) {
    cas <- ifelse(type == 'cas', query, '')
    name <- ifelse(type == 'name', query, '')
    free <- ifelse(type == 'free', query, '')
    bd <- list(Method = 'MAKECHEMLIST',
               txtCasnro = cas,
               txtFreetext = free,
               txtname = name,
               txteditmode = 0
               )
    if (verbose)
      message('Searching ', query)
    Sys.sleep( rgamma(1, shape = 15, scale = 1/10))
    h <- try(POST(baseurl, body = bd, encode = 'form'), silent = TRUE)
    if (inherits(res, "try-error")) {
      warning('Problem with service... Returning NA.')
      return(NA)
    } else {
      tt <- read_html(h)
    }

    require(xmlview)
    xml_view(tt, add_filter = TRUE)

    name <- xml_text(xml_find_all(tt, "//tr[contains(td[1],'Chemical')]/following-sibling::tr[1]/td[1]"))
    cas <- xml_text(xml_find_all(tt, "//tr[contains(td[1],'CAS-number')]/following-sibling::tr[1]/td[1]"))

    synonyms <- xml_text(xml_find_all(tt, "//tr[contains(td[1],'Synonyms')]/following-sibling::tr[following-sibling::tr[contains(td, 'Sumformula')]]"), trim = TRUE)
    # = all tr after 'Synonyms' undtil 'Sumformula' is found
    synonyms <- synonyms[-length(synonyms)]

    formula <- xml_text(xml_find_all(tt, "//tr[contains(td[1],'Sumformula')]/following-sibling::tr[1]/td[1]"))
    einecs <- xml_text(xml_find_all(tt, "//tr[contains(td[1],'EINECS')]/following-sibling::tr[1]/td[1]"))
    mw <- as.numeric(xml_text(xml_find_all(tt, "//tr[contains(td[1],'Molecular weight')]/following-sibling::tr[1]/td[1]")))

    # Water Solubility

    # # log POW
    # logpow <- xml_text(xml_find_all(tt,
    #                                 "//tr[contains(td[1],' log Pow')]/following-sibling::tr[following-sibling::tr[contains(td, 'Henry')]]/td[1]"),
    #                    trim = TRUE)
    # logpowref <- xml_text(xml_find_all(tt, "//tr[contains(td[1],' log Pow')]/following-sibling::tr[following-sibling::tr[contains(td, 'Henry')]]/td[2]"))
    # logpow <- data.frame(value = logpow[-length(logpow)], ref = logpowref)
    #
    # # Henry
    # hr <- xml_text(xml_find_all(tt, "//tr[contains(td[1],'Henry')]/following-sibling::tr[following-sibling::tr[contains(td, 'Adsorption')]]"))
    # hr <- hr[-length(hr)]
    # hr <- strsplit(hr, '\r\n')
    #
    # # DT50air
    # dt50air <- xml_text(xml_find_all(tt, "//tr[contains(td[1],'Half-life in air, days')]/following-sibling::tr[following-sibling::tr[contains(td, 'Half-life in soil, days ')]]"))
    # dt50air <- dt50air[-length(dt50air)]
    # dt50air <- strsplit(dt50air, '\r\n')
    # references
    refsno <- xml_text(xml_find_all(tt, "//tr[contains(td[1],'References')]/following-sibling::tr/td[1]"))
    refs <- xml_text(xml_find_all(tt, "//tr[contains(td[1],'References')]/following-sibling::tr/td[2]"))
    refs <- gsub('\r\n', ' ', refs)
    references <- data.frame(refsno, refs)

    out <- list(name = name, cas = cas, synonyms = synonyms, formula = formula,
                einecs = einecs, mw = mw, refs = references)
    return(out)
  }
  out <- lapply(query, foo, type = type, verbose = verbose)
  out <- setNames(out, query)
  class(out) <- c('list', 'ec_query')
  return(out)
}