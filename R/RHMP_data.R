# create a vector with all the RHMP stations of interest
# Note: We pasted the stations into a text file and created a csv.
#       This csv file is what is read in.
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
