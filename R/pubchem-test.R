library(httr)
library(jsonlite)
library(tidyverse) #to avoid
library(xml2)

example_query <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/712/property/CanonicalSMILES/JSON/" # construction already handled by webchem
classification_query <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/712/classification/JSON?classification_type=original"
cont <- try(content(POST(classification_query
                         ),
                    type = 'text', encoding = 'UTF-8'),
            silent = TRUE
)

# this generates a > 9 mb object for formaldehyde -> not feasible

# should be feasible using PUG-VIEW, which can access
# all headings listed here:

# https://pubchem.ncbi.nlm.nih.gov/classification/#hid=72
# via e.g. https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/2244/JSON?heading=UN+GHS+Classification
# to check

classification_query <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/2244/JSON?heading=GHS+Classification"

cont <- try(content(POST(classification_query),
                    type = 'text', encoding = 'UTF-8'),
            silent = TRUE
)


ghs <- fromJSON(cont)
ghs_layer2 <- ghs$Record$Section

ghs_layer3 <- ghs_layer2[[3]][[1]]

ghs_layer4 <- ghs_layer3[[3]][[1]]

ghs_layer5 <- ghs_layer4$Information

ghs_layer6 <- ghs_layer5[[1]] # this contains for each available reference the pictograms, signal words, hazard statements etc.

# getting directly to this layer

ghs_layer6 <- ghs$Record$Section[[3]][[1]][[3]][[1]]$Information[[1]]
 # alternative like: ghs$Record$Section['Section']['Section']['Section']['Section']$Section
# this layer defines sources (coded) & types of the information ( pictograms, hazard statements...etcs)

ghs_layer6['Name' == "GHS Hazard Statement", 3]

str(list(ghs_layer6))

ghs_layer7 <- ghs_layer6$Value$StringWithMarkup

ghs_layer7
# goal a list with a list element for each reference




enframe(unlist(cont))



# highly relevant
# https://stackoverflow.com/questions/32634430/r-parse-json-xml-exported-compound-properties-from-pubchem


# try with xml response

classification_query <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/2244/XML?heading=GHS+Classification"

cont <- try(content(POST(classification_query),
                    type = 'text', encoding = 'UTF-8'),
            silent = TRUE
)

# looks much!! easier accessible vi xpaths

ghs_xml <- read_xml(cont)

xml_children(ghs_xml)
# of interest are all 
# <StringWithMarkup>
# within <Information> Tags that have a 
# <Name>GHS Hazard Statements</Name>

xml_find_all(ghs_xml, "./*")
xml_find_all(ghs_xml, xpath = "./StringWithMarkup")

# need to brush up xpath