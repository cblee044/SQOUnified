# This script is going to be a wrapper function for the RIVPACS scripts to be implemented into
# SQOUnified package.

RIVPACS_wrapper <- function(DB, station){
# Need SampleDepth in data frame for RIVPACS

}

# Rscript SccwrpRivpacs_script.R 1359585702 user.filename

library(SccwrpRivpacs)

  # RIVPACS calculations. By default the functions use the example user data.
  socal <- SoCalRivpacs(observed.predictors = scb.predictors, observed.taxa = scb.taxa)

  # Create the HTML files displaying the output.
  HtmlOutput(rivpacs = socal, timestamp = ts, user.filename = uf, path = "/var/www/sqo/files/")

}

if(nrow(sfb.station) > 0) {

  sfb.predictors <- data.frame(SampleDepth = sfb.station$SampleDepth,
                               Hab_G = rep(0, times = nrow(sfb.station)),
                               Longitude = sfb.station$Longitude)

  row.names(sfb.predictors) <- sfb.station$StationID

  sfb.predictors <- as.matrix(sfb.predictors)

  sfb.taxa <- benthic[benthic$StationID %in% sfb.station$StationID, ]

  sfb.taxa$Taxa <- gsub(" ", "_", sfb.taxa$Taxa, fixed = TRUE)
  sfb.taxa$Taxa <- gsub("(", "_", sfb.taxa$Taxa, fixed = TRUE)
  sfb.taxa$Taxa <- gsub(")", "_", sfb.taxa$Taxa, fixed = TRUE)

  sfb.taxa <- reshape(data = sfb.taxa, v.names = "Abundance", timevar = "Taxa",
                      idvar = "StationID", direction = "wide")

  row.names(sfb.taxa) <- sfb.taxa$StationID

  sfb.taxa <- sfb.taxa[, -1]

  colnames(sfb.taxa) <- gsub("Abundance.", "", colnames(sfb.taxa))

  # Replace NAs with zero.
  sfb.taxa[is.na(sfb.taxa)] <- 0

  # RIVPACS calculations. By default the functions use the example user data.
  sfbay <- SFBayRivpacs(observed.predictors = sfb.predictors, observed.taxa = sfb.taxa)

  # Create the HTML files displaying the output.
  HtmlOutput(rivpacs = sfbay, timestamp = ts, user.filename = uf, path = "/var/www/sqo/files/")

}
