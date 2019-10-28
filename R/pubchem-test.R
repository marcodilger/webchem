library(httr)
library(jsonlite)
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
readRDS("data/content_ghs_temp.rda")

ghs_xml <- read_xml(cont)

ghs_xml <- xml_ns_strip(ghs_xml)
xml_children(ghs_xml)

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

response <-  list()
for(ref in ghs_refs) {

source <- xml_find_all(ghs_xml, xpath = paste0("Reference[ReferenceNumber[text()='", ref, "']]"))
source_name <- xml_text(xml_find_all(source, xpath = "SourceName"))
source_substancename <- xml_text(xml_find_all(source, xpath = "Name"))
source_description <- xml_text(xml_find_all(source, xpath = "Description"))
source_url <- xml_text(xml_find_all(source, xpath = "URL"))
source_license <- xml_text(xml_find_all(source, xpath = "LicenseNote"))

hazard_statements <- xml_text(xml_find_all(ghs_xml,
                      xpath = paste0("//Information[ReferenceNumber[text()='", ref, "']][Name][contains(., 'GHS Hazard Statements')]//StringWithMarkup/String")
                      )
         )
hazard_statements <- hazard_statements[hazard_statements != ""]
hazard_codes <- str_extract(hazard_statements, "^H\\d{3}(\\+H\\d{3})*") # needs to be improved to capture minimum classification and subclasses as well, see https://en.wikipedia.org/wiki/GHS_hazard_statements
hazard_codes <- sort(unique(hazard_codes[hazard_codes != ""]))

prec_codes <- xml_text(xml_find_all(ghs_xml, xpath = paste0("//Information[ReferenceNumber[text()='", ref, "']][Name][contains(., 'Precautionary Statement Codes')]//StringWithMarkup/String") ))
prec_codes <- prec_codes[str_detect(prec_codes, "^P\\d{3}")]
prec_codes <- unlist(str_extract_all(prec_codes, "P\\d{3}(\\+P\\d{3})*"))


info <- list(source_name = source_name, substance = source_substancename, codes = hazard_codes, hazard_statements = hazard_statements, precautionary_statement_codes = prec_codes, source_desc = source_description, url = source_url, license = source_license)

response[[length(response) + 1]] <-  info # ansprechen via info$name ist KEINE gute Idee (z.B. ECHA doppelte beiträge möglich)
}


response[2]






# ToDo: variable/list with sources to extract: named specificaly (returning empty when not available)
# or option 'all' for all available
  # needed for this: explicit list of names which can occur as sources



# entweder es wird an der Stelle dann allgemein gehalten,
# oder: Fallunterscheidung je nach Quelle (ECHA, NITE-CMC, HCIS)
# Liste mit allenauftretenden Sources?

# todo: add roxygen documentation
# https://github.com/r-lib/roxygen2#roxygen2