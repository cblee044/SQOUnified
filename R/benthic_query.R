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
#' @param EG_File_Name A quoted string with the name of the csv file with the suite of US Ecological Groups assigned
#'     initially in Gillett et al. 2015. This EG file has multiple versions of the EG values and a Yes/No designation
#'     if the fauna are Oligochaetes or not. The default file is the Ref - EG Values 2018.csv file included with this
#'     code. Replace with other files as you see fit, but make sure the file you use is in a similar format and uses
#'     the same column names. Additionally, new taxa can be added at the bottom of the list with the EG values the user
#'     feels appropriate, THOUGH THIS IS NOT RECOMMENDED
#' @param EG_Scheme A quoted string with the name of the EG Scheme to be used in the AMBI scoring. The default is
#'     Hybrid, though one could use US (all coasts), Standard (Values from Angel Borja and colleagues),
#'     US_East (US East Coast), US_Gulf (US Gulf of Mexico Coast), or US_West (US West Coast).
#'
#' @usage data(benthic_data)
#' @usage data(EG_Ref)
#' @usage data(Saline_Standards)
#' @usage data(TidalFresh_Standards)
#'
#' @examples
#' MAMBI(benthic_data, EG_File_Name="data/Ref - EG Values 2018.csv", EG_Scheme="Hybrid")
#' MAMBI(benthic_data, EG_File_Name="data/Ref - EG Values 2018.csv", EG_Scheme="US_Gulf")


################################################################################################################
# We are going to make benthic_query a function so that we call the database every time that we run the package.
#       Input:     NA
#                       We do not require any inputs to run this function.
#       Output(s): benthic_data
#                       A data frame living in the R environment. This data frame will be stored in the "data"
#                       folder of the package. Note that this data frame will have all of the necessary fields
#                       with the correct headings that are needed to compute ALL of the indices. This dataset
#                       will be the default for all of other functions to run and compute the indices.
# The data will contain the following information with these headings:
#           StationID -
#           Replicate -
#           SampleDate -
#           Latitude -
#           Longitude -
#           Species -
#           Abundance -
#           Salinity -
#           Stratum -
#           Exclude -
#
################################################################################################################

benthic_query <- function()
{
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

  assignment <- tbl(con, "field_assignment_table") %>%
    as_tibble %>%
    dplyr::filter(stratum == "Bays" | stratum == "Ports" | stratum == "Estuaries" | stratum == "Brackish Estuaries")

  station_occupation <- tbl(con, "tbl_stationoccupation") %>%
    as_tibble %>%
    inner_join(assignment, by = 'stationid')

  # Create the dataset needed to compute all the SQO benthic indices
  benthic_data <- grab %>%
    dplyr::filter(benthicinfauna == 'Yes') %>%
    dplyr::inner_join(station_occupation, by = c('stationid','sampledate' = 'occupationdate')) %>%
    dplyr::inner_join(infauna, by = c('stationid','sampledate')) %>%
    dplyr::select('stationid','replicate','sampledate','latitude','longitude','taxon','abundance','salinity', 'stratum', 'exclude') %>%
    dplyr::mutate_if(is.numeric, list(~na_if(., -88))) %>%
    dplyr::rename(species = taxon) %>%
    dplyr::rename(StationID = stationid, Replicate = replicate, SampleDate = sampledate, Latitude = latitude, Longitude = longitude, Species = species, Abundance = abundance, Salinity = salinity, Stratum = stratum, Exclude = exclude)

  #mutate(EG_Test=ifelse(is.na(EG),"NoEG", "YesEG"))

  save(benthic_data, file = "data/benthic_data.Rdata")
  write.csv(benthic_data, file = "data/benthic_data.csv", row.names = FALSE)


}
