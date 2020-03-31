#' Pull the necessary benthic data from SCCWRP database to compute SQO Benthic indices.
#'
#' @param BenthicData a data frame with the following information with these headings:
#'    \code{StationID} - an alpha-numeric identifier of the location;
#'    \code{Replicat} - a numeric identifying the replicate number of samples taken at the location;
#'    \code{SampleDate} - the date of sample collection;
#'    \code{Latitude} - latitude in decimal degrees;
#'    \code{Longitude} - longitude in decimal degrees. Make sure there is a negative sign for the Western coordinates;
#'    \code{Species} - name of the fauna, ideally in SCAMIT ed12 format, do not use sp. or spp.,
#'        use sp only or just the Genus. If no animals were present in the sample use
#'        NoOrganismsPresent with 0 abundance;
#'    \code{Abundance} - the number of each Species observed in a sample;
#'    \code{Salinity} - the salinity observed at the location in PSU, ideally at time of sampling.

  require(DBI) # needed to connect to database
  require(dbplyr) # needed to connect to database
  require(RPostgreSQL) # needed to connect to our database
  require(rstudioapi) # just so we can type the password as we run the script, so it is not written in the clear
  require(tidyverse)

  # con is short for connection
  # Create connection to the database
  con <- DBI::dbConnect(
    PostgreSQL(),
    host = "192.168.1.16",
    dbname = 'bight2018',
    user = 'b18read',
    password = '1969$Harbor' # if we post to github, we might want to do rstudioapi::askForPassword()
  )

  # Bring in our tables from the database
  infauna <- tbl(con, "tbl_infaunalabundance_initial") %>% as_tibble
  save(infauna, file = 'data/infauna_data.Rdata')

  grab <- tbl(con, "tbl_grabevent") %>%
    as_tibble %>%
    dplyr::filter(grabeventnumber == 1)
  save(grab, file = 'data/grab_data.Rdata')

  assignment <- tbl(con, "field_assignment_table") %>%
    as_tibble %>%
    dplyr::filter(stratum == "Bays" | stratum == "Ports" | stratum == "Estuaries" | stratum == "Brackish Estuaries" | stratum == "Marinas")
  save(assignment, file = 'data/assignment_data.Rdata')

  station_occupation <- tbl(con, "tbl_stationoccupation") %>%
    as_tibble %>%
    inner_join(assignment, by = 'stationid')
  save(station_occupation, file = 'data/station_occupation_data.Rdata')

  # Create the dataset needed to compute all the SQO benthic indices
  benthic_data <- grab %>%
    dplyr::filter(benthicinfauna == 'Yes') %>%
    dplyr::inner_join(station_occupation, by = c('stationid','sampledate' = 'occupationdate')) %>%
    dplyr::inner_join(infauna, by = c('stationid','sampledate')) %>%
    dplyr::select('stationid','replicate','sampledate','latitude','longitude', 'stationwaterdepth', 'taxon','abundance','salinity', 'stratum', 'exclude') %>%
    dplyr::mutate_if(is.numeric, list(~na_if(., -88))) %>%
    dplyr::rename(species = taxon) %>%
    dplyr::rename(StationID = stationid, Replicate = replicate, SampleDate = sampledate, Latitude = latitude, Longitude = longitude, SampleDepth = stationwaterdepth, Species = species, Abundance = abundance, Salinity = salinity, Stratum = stratum, Exclude = exclude)

  #mutate(EG_Test=ifelse(is.na(EG),"NoEG", "YesEG"))

  save(benthic_data, file = "data/benthic_data.Rdata")
  write.csv(benthic_data, file = "data/benthic_data.csv", row.names = FALSE)
