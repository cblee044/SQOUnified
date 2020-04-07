#' create a vector with all the RHMP stations of interest
#' Note: We pasted the stations into a text file and created a csv.
#'
#' @param BenthicData a data frame with the following information with these headings:
#'    \code{StationID} - an alpha-numeric identifier of the location;
#'    \code{Replicate} - a numeric identifying the replicate number of samples taken at the location;
#'    \code{SampleDate} - the date of sample collection;
#'    \code{Latitude} - latitude in decimal degrees;
#'    \code{Longitude} - longitude in decimal degrees. Make sure there is a negative sign for the Western coordinates;
#'    \code{Species} - name of the fauna, ideally in SCAMIT ed12 format, do not use sp. or spp.,
#'        use sp only or just the Genus. If no animals were present in the sample use
#'        NoOrganismsPresent with 0 abundance;
#'    \code{Abundance} - the number of each Species observed in a sample;
#'    \code{Salinity} - the salinity observed at the location in PSU, ideally at time of sampling.
#'
#' @usage data(benthic_data)
#'
require(tidyverse)
load("data/benthic_data.Rdata")
rhmp = read.csv("RHMP_data.csv", fileEncoding="UTF-8-BOM", header = FALSE)

# stations are read in as factors. We coerce them into characters.
rhmp = as.character(rhmp$V1)

# filter the data base qeuery by the RHMP stations of interest
DB = benthic_data %>%
  dplyr::filter(StationID %in% rhmp)

save(DB, file = "data/rhmp_data.Rdata")
write.csv(benthic_data, file = "data/rhmp_data.csv", row.names = FALSE)

# Run the SQOUnified code to get the calculated benthic indices
# NOTE: We don't have RIVPACS score in yet, so this is NOT the final scores.
library(devtools)
install()
library(SQOUnified)
final <- SQOUnified(DB, SQO = "all")

DB_infauna = infauna %>% dplyr::rename(StationID = stationid) %>%
  dplyr::filter(StationID %in% rhmp)

DB_assignment = assignment %>% dplyr::rename(StationID = stationid) %>%
  dplyr::filter(StationID %in% rhmp)

DB_grab = grab %>% dplyr::rename(StationID = stationid) %>%
  dplyr::filter(StationID %in% rhmp)

DB_station_occupation = station_occupation %>% dplyr::rename(StationID = stationid) %>%
  dplyr::filter(StationID %in% rhmp)


DB_rhmp <- DB_grab %>%
  dplyr::inner_join(DB_station_occupation, by = c('StationID','sampledate' = 'occupationdate')) %>%
  dplyr::inner_join(DB_infauna, by = c('StationID','sampledate')) %>%
  dplyr::select('StationID','replicate','sampledate','latitude','longitude','taxon','abundance','salinity', 'stratum', 'exclude') %>%
  dplyr::mutate_if(is.numeric, list(~na_if(., -88))) %>%
  dplyr::rename(species = taxon) %>%
  dplyr::rename(Replicate = replicate, SampleDate = sampledate, Latitude = latitude, Longitude = longitude, Species = species, Abundance = abundance, Salinity = salinity, Stratum = stratum, Exclude = exclude)

save(DB_rhmp, file = "data/rhmp_data.RData")
