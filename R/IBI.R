#' Compute the multivariate AMBI (M-AMBI) index score.
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
#'
#' @usage data(benthic_data)
#'
#' @examples
#' MAMBI.DJG.alt(benthic_data, EG_File_Name="data/Ref - EG Values 2018.csv", EG_Scheme="Hybrid")
#' MAMBI.DJG.alt(benthic_data, EG_File_Name="data/Ref - EG Values 2018.csv", EG_Scheme="US_Gulf")

##########################################################################################################################
## This is a function to calculate multivariate AMBI (M-AMBI) index scores following Pelletier et al. 2018
## which is in turn built upon the work of Sigovini et al. 2013 and Muxica et al. 2007.  This is an alternate version that
## allows for manipulation of the data within R before submitting it to the function in lieu of directly reading
## in an excel file to the function. The function is designed for use in US estuarine waters and requires three arguments:
##          BenthicData - A data file living in the R environment.
##                        The data MUST contain the following information with these headings:
##
##                                  StationID - an alpha-numeric identifier of the location
##                                  Replicate - a numeric identifying the replicate number of samples taken at the
##                                              location
##                                  SampleDate - the date of sample collection
##                                  Latitude - latitude in decimal degrees
##                                  Longitude - longitude in decimal degrees make sure there is a negative sign
##                                              for the Western coordinates
##                                  Species - name of the fauna, ideally in SCAMIT ed12 format, do not use sp. or spp.,
##                                            use sp only or just the Genus. If no animals were present in the sample
##                                            use NoOrganismsPresent with 0 abundance
##                                  Abundance - the number of each Species observed in a sample
##                                  Salinity - the salinity observed at the location in PSU, ideally at time of sampling
##
##          EG_File_Name - A quoted string with the name of the csv file with the suite of US Ecological Groups
##                          assigned initially in Gillett et al. 2015. This EG file has multiple versions of the EG
##                          values and a Yes/No designation if the fauna are Oligochaetes or not. The default file is
##                          the Ref - EG Values 2018.csv file included with this code. Replace with other files as you
##                          see fit, but make sure the file you use is in a similar format and uses the same column names.
##                          Additionally, new taxa can be added at the bottom of the list with the EG values the user
##                          feels appropriate, THOUGH THIS IS NOT RECOMMENDED
##
##          EG_Scheme - A quoted string with the name of the EG Scheme to be used in the AMBI scoring. The default is
##                      Hybrid, though one could use US (all coasts), Standard (Values from Angel Borja and colleagues),
##                      US_East (US East Coast), US_Gulf (US Gulf of Mexico Coast), or US_West (US West Coast).
##
## Two additional files are also needed to run the script: Saline and Tidal freshwater good-bad standards for the M-AMBI
## that are in the Pelletier2018_Standards.xlsx work book and included along with this code.
##
## For the function to run, the following packages NEED to be installed:  tidyverse, reshape2, vegan, and readxl.
## Additionally the EQR.R function must also be installed and is included with this code.
##
## The output of the function will be a dataframe with StationID, Replicate, SampleDate, Latitude, Longitude,
## SalZone (The Salinity Zone assigned by M-AMBI), AMBI_Score, S (Species Richness), H (Species Diversity),
## Oligo_pct (Relative Abundance of Oligochaetes), MAMBI_Score, Orig_MAMBI_Condition, New_MAMBI_Condition,
## Use_MAMBI (Can M-AMBI be applied?), Use_AMBI (Can AMBI be applied?), and YesEG (% of Abundance with a EG value)
##########################################################################################################################

IBI <- function(BenthicData)
{
  require(DBI) # needed to connect to database
  require(dbplyr) # needed to connect to database
  require(RPostgreSQL) # needed to connect to our database
  require(rstudioapi) # just so we can type the password as we run the script, so it is not written in the clear
  require(tidyverse)
  require(sqldf)

  "benthic_data"
  "Taxonomic_Info"
  # Relevant Queries
  # SQO RBI -2
  # Bring in our tables from the database

  # con is short for connection
  # Create connection to the database
  con <- DBI::dbConnect(
    PostgreSQL(),
    host = "192.168.1.16",
    dbname = 'bight2018',
    user = 'b18read',
    password = '1969$Harbor' # if we post to github, we might want to do rstudioapi::askForPassword()
  )

  infauna <- tbl(con, "tbl_infaunalabundance_initial") %>%
    as_tibble %>%
    dplyr::filter(exclude == 'No') %>%
    dplyr::filter(replicate == 1)

  grab <- tbl(con, "tbl_grabevent") %>%
    as_tibble %>%
    dplyr::filter(grabeventnumber == 1)

  assignment <- tbl(con, "field_assignment_table") %>%
    as_tibble %>%
    dplyr::filter(stratum == "Bays" | stratum == "Ports" | stratum == "Estuaries" | stratum == "Brackish Estuaries")

  station_occupation <- tbl(con, "tbl_stationoccupation") %>%
    as_tibble %>%
    inner_join(assignment, by = 'stationid')


  # had to change this query so that we also saved the IBISensitive information
  # TODO --> Change the benthic_query.R file so that we get this information, too.
  ibi_data <- grab %>%
    dplyr::filter(benthicinfauna == 'Yes') %>%
    dplyr::inner_join(station_occupation, by = c('stationid','sampledate' = 'occupationdate')) %>%
    dplyr::inner_join(infauna, by = c('stationid','sampledate')) %>%
    dplyr::select('stationid','replicate','sampledate','latitude','longitude','taxon','abundance','salinity', 'stratum', 'exclude') %>%
    dplyr::inner_join(Taxonomic_Info, by = c('taxon' = 'Taxon')) %>%
    dplyr::mutate_if(is.numeric, list(~na_if(., -88))) %>%
    dplyr::add_count(taxon) %>%
    dplyr::select('stationid','replicate','taxon','abundance','stratum', 'Phylum', 'Subphylum', 'IBISensitive', 'n') %>%
    dplyr::group_by(stratum, stationid, replicate, taxon, abundance, Phylum, Subphylum) %>%
    dplyr::rename(NumOfTaxa = n) %>%
    dplyr::rename(species = taxon) %>%
    dplyr::rename(StationID = stationid, Replicate = replicate, Species = species, Abundance = abundance, B13_Stratum = stratum)



  ### SQO IBI - 1
  # columns needed in RBI: B13_Stratum, StationID, Replicate, Phylum, NumofTaxa
  ibi1 <- ibi_data %>%
    group_by(B13_Stratum, StationID, Replicate) %>%
    summarise(NumOfTaxa = sum(NumOfTaxa))


  ### SQO IBI - 2
  ibi2 <- ibi_data %>%
    dplyr::filter(Phylum == "MOLLUSCA") %>%
    dplyr::group_by(B13_Stratum, StationID, Replicate, Phylum, NumOfTaxa) %>%
    dplyr::select(B13_Stratum, StationID, Replicate, Phylum, NumOfTaxa) %>%
    dplyr::group_by(B13_Stratum, StationID, Replicate, Phylum) %>%
    dplyr::summarise(NumOfMolluscTaxa = sum(NumOfTaxa))


  ### SQO RBI - 3 - 1
  ibi3_1 <- ibi_data %>%
    dplyr::filter(grepl("Notomastus", Species)) %>%
    dplyr::group_by(B13_Stratum, StationID, Replicate, Species, Abundance) %>%
    dplyr::select(B13_Stratum, StationID, Replicate, Species, Abundance)


  ### SQO IBI - 3 - 2
  ibi3_2 <- ibi3_1 %>%
    dplyr::group_by(B13_Stratum, StationID, Replicate) %>%
    dplyr::summarise(NotomastusAbun = sum(Abundance))


  ### SQO IBI - 4 - 1
  ibi4_1 <- ibi_data %>%
    dplyr::filter(IBISensitive == "TRUE") %>%
    dplyr::group_by(B13_Stratum, StationID, Replicate, IBISensitive, Abundance) %>%
    dplyr::add_count(Abundance) %>%
    dplyr::rename(SensTaxa = n) %>%
    dplyr::group_by(B13_Stratum, StationID, Replicate, IBISensitive) %>%
    dplyr::summarise(SensTaxa = sum(SensTaxa))


  ### SQO IBI - 4 - 2
  ibi4_2 <- ibi1 %>%
    dplyr::inner_join(ibi4_1, by = c("B13_Stratum", "StationID", "Replicate")) %>%
    dplyr::mutate(PctSensTaxa = (SensTaxa/NumOfTaxa)*100) %>%
    dplyr::select(B13_Stratum, StationID, Replicate, PctSensTaxa)

  ### B13 IBI Metrics
  ibi_metrics <- ibi1 %>%
    dplyr::full_join(ibi2, by = c("B13_Stratum", "StationID", "Replicate")) %>%
    dplyr::full_join(ibi3_1, by = c("B13_Stratum", "StationID", "Replicate")) %>%
    dplyr::full_join(ibi3_2, by = c("B13_Stratum", "StationID", "Replicate")) %>%
    dplyr::full_join(ibi4_1, by = c("B13_Stratum", "StationID", "Replicate")) %>%
    dplyr::full_join(ibi4_2, by = c("B13_Stratum", "StationID", "Replicate")) %>%
    dplyr::select(B13_Stratum, StationID, Replicate, NumOfTaxa, NumOfMolluscTaxa, NotomastusAbun, PctSensTaxa)


  #write.csv(ibi_metrics, file = "data/ibi-metrics.csv", row.names = FALSE)

}
