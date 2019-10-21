library(httr)
library(jsonlite)
library(tidyverse) #to avoid
library(xml2)

# pug-rest failed (returns huge objects)
# pug view via JSON would work, but quite complicated to reach the right nodes
# try with xml response

classification_query <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/2244/XML?heading=GHS+Classification"

cont <- try(content(POST(classification_query),
                    type = 'text', encoding = 'UTF-8'),
            silent = TRUE
)

# looks much!! easier accessible via xpaths

ghs_xml <- read_xml(cont)

xml_children(ghs_xml)

# of interest are all 
# <StringWithMarkup>
# within <Information> Tags that have a 
# <Name>GHS Hazard Statements</Name>

ghs_xml <- xml_ns_strip(ghs_xml)

xml_find_all(ghs_xml, "Section")
ghs_info <- xml_find_all(ghs_xml, xpath = "//Information[Name][contains(., 'GHS Hazard Statements')]//StringWithMarkup/String")
xml_find_all(ghs_xml, xpath = "//Information[ReferenceNumber][contains(., '52')][Name][contains(., 'GHS Hazard Statements')]")

xml_text(ghs_info)

# extract all available references
ghs_refs <- xml_text(xml_find_all(ghs_xml, xpath = "//Information[Name][contains(., 'GHS Hazard Statements')]//ReferenceNumber"))


# access hazard statements of referencenumbers directly
xml_find_all(ghs_xml, xpath = "//Information[ReferenceNumber][contains(., '52')][Name][contains(., 'GHS Hazard Statements')]//StringWithMarkup/String")

# dynamically
for (ref in ghs_refs) {
  print(
    xml_find_all(ghs_xml, xpath = paste0("//Information[ReferenceNumber][contains(., ", ghs_refs[ref], ")][Name][contains(., 'GHS Hazard Statements')]//StringWithMarkup/String") )
  )
} # error, spits out everything for all references


# entweder es wird an der Stelle dann allgemein gehalten,
# oder: Fallunterscheidung je nach Quelle (ECHA, NITE-CMC, HCIS)
# Liste mit allenauftretenden Sources?