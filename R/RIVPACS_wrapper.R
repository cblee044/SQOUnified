# This script is going to be a wrapper function for the RIVPACS scripts to be implemented into
# SQOUnified package.

RIVPACS_wrapper <- function(DB, station){

  socalrivpacs_output <- SoCalRivpacs(Pcutoff = 0.5,
                                      reference.groups = socal.reference.groups,
                                      observed.predictors = socal.example.habitat,
                                      reference.taxa = socal.reference.taxa,
                                      group.means = socal.reference.group.means,
                                      reference.cov = socal.reference.covariance,
                                      observed.taxa = socal.example.taxa)
  # Note that the default settings for the SoCalRivpacs() function are the examples included in the
  # original package.
  # TODO: Joana -- Be sure to include these examples in the SQOUnified package or we might get errors.


}

# Rscript SccwrpRivpacs_script.R 1359585702 user.filename

#########################
## We will not be loading in this package. Instead, we will include the functions in the SQOUnified
## package and call them directly using this script.
#library(SccwrpRivpacs)
#########################
#rm(list = ls())

#ts <- commandArgs(TRUE)[1] # timestamp
#uf <- commandArgs(TRUE)[2] # user file name

# Read in user files.
#########################
## We will not to read in these files because we will already have them loaded.
#station <- read.csv(paste("/var/www/sqo/files/", ts, ".station.csv", sep = ""), stringsAsFactors = FALSE)
#benthic <- read.csv(paste("/var/www/sqo/files/", ts, ".benthic.csv", sep = ""), stringsAsFactors = FALSE)
#########################

# Split to SoCal and SFBay.

scb.station <- station[toupper(station$HabitatCode) == "C", ]

sfb.station <- station[toupper(station$HabitatCode) == "D", ]

# If data exists for habitat, format data.

if(nrow(scb.station) > 0) {

  scb.predictors <- data.frame(Latitude = scb.station$Latitude,
                               Longitude = scb.station$Longitude,
                               SampleDepth = scb.station$SampleDepth)

  row.names(scb.predictors) <- scb.station$StationID

  scb.predictors <- as.matrix(scb.predictors)

  scb.taxa <- benthic[benthic$StationID %in% scb.station$StationID, ]

  scb.taxa$Taxa <- gsub(" ", "_", scb.taxa$Taxa, fixed = TRUE)
  scb.taxa$Taxa <- gsub("(", "_", scb.taxa$Taxa, fixed = TRUE)
  scb.taxa$Taxa <- gsub(")", "_", scb.taxa$Taxa, fixed = TRUE)

  scb.taxa <- reshape(data = scb.taxa, v.names = "Abundance", timevar = "Taxa",
                      idvar = "StationID", direction = "wide")

  row.names(scb.taxa) <- scb.taxa$StationID

  scb.taxa <- scb.taxa[, -1]

  colnames(scb.taxa) <- gsub("Abundance.", "", colnames(scb.taxa))

  # Replace NAs with zero.
  scb.taxa[is.na(scb.taxa)] <- 0

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
