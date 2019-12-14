library(DBI) # needed to connect to database
library(dbplyr) # needed to connect to database
library(RPostgreSQL) # needed to connect to our database
library(rstudioapi) # just so we can type the password as we run the script, so it is not written in the clear
library(tidyverse)
library(sqldf)

# Relevant Queries
# SQO RBI -2
# Bring in our tables from the database
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

# the following yields the correct columns. We have the extra columns that are needed for benthic_data
# as we continue to implement the RBI queries, we will decide whether we need to keep all of these columns
# if we do, it might be worth going through and adding this to the original benthic_query.R file
# and include this with all of the benthic_data we save

rbi_data <- grab %>%
  dplyr::filter(benthicinfauna == 'Yes') %>%
  dplyr::inner_join(station_occupation, by = c('stationid','sampledate' = 'occupationdate')) %>%
  dplyr::inner_join(infauna, by = c('stationid','sampledate')) %>%
  dplyr::select('stationid','replicate','sampledate','latitude','longitude','taxon','abundance','salinity', 'stratum', 'exclude') %>%
  dplyr::inner_join(Taxonomic_Info, by = c('taxon' = 'Taxon')) %>%
  dplyr::mutate_if(is.numeric, list(~na_if(., -88))) %>%
  dplyr::add_count(taxon) %>%
  dplyr::select('stationid','replicate','taxon','abundance','stratum', 'Phylum', 'Subphylum', 'n') %>%
  dplyr::group_by(stratum, stationid, replicate, taxon, abundance, Phylum, Subphylum) %>%
  dplyr::summarise(NumOfTaxa = sum(n)) %>%
  dplyr::rename(species = taxon) %>%
  dplyr::rename(StationID = stationid, Replicate = replicate, Species = species, Abundance = abundance, B13_Stratum = stratum)

ibi_data <- rbi_data %>%
  group_by(B13_Stratum, StationID, Replicate) %>%
  summarise(NumOfTaxa = sum(NumOfTaxa))

# need to verify this table with David
# columns needed in RBI: B13_Stratum, StationID, Replicate, Phylum, NumofMolluscTaxa
rbi2 <- rbi_data %>%
  dplyr::filter(Phylum == "MOLLUSCA") %>%
  dplyr::group_by(B13_Stratum, StationID, Replicate, Phylum, NumOfTaxa) %>%
  dplyr::select(B13_Stratum, StationID, Replicate, Phylum, NumOfTaxa) %>%
  dplyr::group_by(B13_Stratum, StationID, Replicate, Phylum) %>%
  dplyr::summarise(NumOfMolluscTaxa = sum(NumOfTaxa))
# Looks like we got the correct values. We just need to make sure that the values we are selecting are correct
# There are two entries for one of the station ids (B18-10201) because there is a different taxa count for each
# of the replicates (1 and 2). Ask if this is correct or whether we need to fix the query we're making.



### SQO RBI -3
rbi3 <- rbi_data %>%
  dplyr::filter(Subphylum == "Crustacea") %>%
  dplyr::group_by(B13_Stratum, StationID, Replicate, Subphylum) %>%
  dplyr::select(B13_Stratum, StationID, Replicate, Subphylum, NumOfTaxa) %>%
  dplyr::summarise(NumOfCrustaceanTaxa = sum(NumOfTaxa))

# Same comment as above for rbi2 --> Looks correct but we need to get rid of the second replicate for B18-10201
#save(rbi3, file = 'rbi3_data4rmRBI.Rdata')

### SQO RBI -4
rbi4 <- rbi_data %>%
  dplyr::filter(Subphylum == "Crustacea") %>%
  dplyr::group_by(B13_Stratum, StationID, Replicate, Subphylum) %>%
  dplyr::select(B13_Stratum, StationID, Replicate, Subphylum, Abundance) %>%
  dplyr::summarise(CrustaceanAbun = sum(Abundance))


### SQO RBI -5
rbi5 <- rbi_data %>%
  dplyr::filter(Species == "Monocorophium insidiosum") %>%
  dplyr::group_by(B13_Stratum, StationID, Replicate, Species) %>%
  dplyr::summarise(M_insidiosumAbun = sum(Abundance))

# Note that this query calls for "Monocorophium insidiosum" but there is no data for this species. So, to check that this is
# working correctly, I changed the species names to "Monocorophium acherusicum"



### SQO RBI -6
rbi6 <- rbi_data %>%
  dplyr::filter(Species == "Asthenothaerus diegensis") %>%
  dplyr::group_by(B13_Stratum, StationID, Replicate, Species) %>%
  dplyr::summarise(A_diegensisAbun = sum(Abundance))


### SQO RBI -7
rbi7 <- rbi_data %>%
  dplyr::filter(Species == "Goniada littorea") %>%
  dplyr::group_by(B13_Stratum, StationID, Replicate, Species) %>%
  dplyr::summarise(G_littoreaAbun = sum(Abundance))


### SQO RBI -8
rbi8 <- rbi_data %>%
  dplyr::filter(Species == "Capitella capitata Cmplx") %>%
  dplyr::group_by(B13_Stratum, StationID, Replicate, Species) %>%
  dplyr::summarise(CapitellaAbun = sum(Abundance))


### SQO RBI -9
rbi9 <- rbi_data %>%
  dplyr::filter(Species == "Oligochaeta") %>%
  dplyr::group_by(B13_Stratum, StationID, Replicate, Species) %>%
  dplyr::summarise(OligochaetaAbun = sum(Abundance))

### B13 RBI Metrics
# We are using a full join because if there are missing values, we might just get an empty data frame.
rbi_metrics <- ibi_data %>%
  dplyr::full_join(rbi2, by = c("B13_Stratum", "StationID", "Replicate")) %>%
  dplyr::full_join(rbi3, by = c("B13_Stratum", "StationID", "Replicate")) %>%
  dplyr::full_join(rbi4, by = c("B13_Stratum", "StationID", "Replicate")) %>%
  dplyr::full_join(rbi5, by = c("B13_Stratum", "StationID", "Replicate")) %>%
  dplyr::full_join(rbi6, by = c("B13_Stratum", "StationID", "Replicate")) %>%
  dplyr::full_join(rbi7, by = c("B13_Stratum", "StationID", "Replicate")) %>%
  dplyr::full_join(rbi8, by = c("B13_Stratum", "StationID", "Replicate")) %>%
  dplyr::full_join(rbi9, by = c("B13_Stratum", "StationID", "Replicate")) %>%
  dplyr::select(B13_Stratum, StationID, Replicate, NumOfTaxa, NumOfMolluscTaxa, NumOfCrustaceanTaxa, CrustaceanAbun, M_insidiosumAbun, A_diegensisAbun, G_littoreaAbun, CapitellaAbun, OligochaetaAbun)
