
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
