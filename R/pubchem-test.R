library(httr)

example_query <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/712/property/CanonicalSMILES/JSON/" # construction already handled by webchem
classification_query <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/712/classification/JSON?classification_type=original"
cont <- try(content(POST(classification_query
                         ),
                    type = 'text', encoding = 'UTF-8'),
            silent = TRUE
)

# this generates a > 9 mb object -> not feasible

# should be feasible using PUG-VIEW, which can access
# all headings listed here:

# https://pubchem.ncbi.nlm.nih.gov/classification/#hid=72
# via e.g. https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/2244/JSON?heading=UN+GHS+Classification
# to check






# old stuff as template for testing



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
  Sys.sleep(0.2)
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


