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
  dplyr::filter(exclude == 'No')

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
  dplyr::select('stationid','replicate','sampledate','latitude','longitude','taxon','abundance','salinity', 'stratum', 'Phylum', 'Subphylum', 'exclude', 'n') %>%
  dplyr::group_by(stratum, stationid, replicate) %>%
  dplyr::rename(species = taxon) %>%
  dplyr::rename(StationID = stationid, Replicate = replicate, SampleDate = sampledate, Latitude = latitude, Longitude = longitude, Species = species, Abundance = abundance, Salinity = salinity, B13_Stratum = stratum)


# need to verify this table with David
# columns needed in RBI: B13_Stratum, StationID, Replicate, Phylum, NumofMolluscTaxa
rbi2 <- rbi_data %>%
  dplyr::filter(Phylum == "MOLLUSCA") %>%
  dplyr::group_by(B13_Stratum, StationID, Replicate, Phylum, n) %>%
  dplyr::select(B13_Stratum, StationID, Replicate, Phylum, n) %>%
  dplyr::group_by(B13_Stratum, StationID, Replicate, Phylum) %>%
  dplyr::summarise(NumOfMolluscTaxa = sum(n))
# Looks like we got the correct values. We just need to make sure that the values we are selecting are correct
# There are two entries for one of the station ids (B18-10201) because there is a different taxa count for each
# of the replicates (1 and 2). Ask if this is correct or whether we need to fix the query we're making.



### SQO RBI -3
rbi3 <- rbi_data %>%
  dplyr::filter(Subphylum == "Crustacea") %>%
  dplyr::select(B13_Stratum, StationID, Replicate, Subphylum, n) %>%
  dplyr::group_by(B13_Stratum, StationID, Replicate, Subphylum) %>%
  dplyr::summarise(NumOfCrustaceanTaxa = sum(n))

# Same comment as above for rbi2 --> Looks correct but we need to get rid of the second replicate for B18-10201
#save(rbi3, file = 'rbi3_data4rmRBI.Rdata')

### SQO RBI -4
rbi4 <- rbi_data %>%
  dplyr::filter(Subphylum == "Crustacea") %>%
  dplyr::select(B13_Stratum, StationID, Replicate, Subphylum, Abundance) %>%
  dplyr::group_by(B13_Stratum, StationID, Replicate, Subphylum) %>%
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
SELECT [SQO IBI - 1].B13_Stratum, [SQO IBI - 1].StationID, [SQO IBI - 1].Replicate, [SQO IBI - 1].NumOfTaxa, [SQO RBI - 2].NumOfMolluscTaxa, [SQO RBI - 3].NumOfCrustaceanTaxa, [SQO RBI - 4].CrustaceanAbun, [SQO RBI - 5].M_insidiosumAbun, [SQO RBI - 6].A_diegensisAbun, [SQO RBI - 7].G_littoreaAbun, [SQO RBI - 8].CapitellaAbun, [SQO RBI - 9].OligochaetaAbun
FROM ((((((([SQO IBI - 1] LEFT JOIN [SQO RBI - 2] ON ([SQO IBI - 1].Replicate = [SQO RBI - 2].Replicate) AND ([SQO IBI - 1].StationID = [SQO RBI - 2].StationID)) LEFT JOIN [SQO RBI - 3] ON ([SQO IBI - 1].Replicate = [SQO RBI - 3].Replicate) AND ([SQO IBI - 1].StationID = [SQO RBI - 3].StationID)) LEFT JOIN [SQO RBI - 4] ON ([SQO IBI - 1].Replicate = [SQO RBI - 4].Replicate) AND ([SQO IBI - 1].StationID = [SQO RBI - 4].StationID)) LEFT JOIN [SQO RBI - 5] ON ([SQO IBI - 1].Replicate = [SQO RBI - 5].Replicate) AND ([SQO IBI - 1].StationID = [SQO RBI - 5].StationID)) LEFT JOIN [SQO RBI - 6] ON ([SQO IBI - 1].Replicate = [SQO RBI - 6].Replicate) AND ([SQO IBI - 1].StationID = [SQO RBI - 6].StationID)) LEFT JOIN [SQO RBI - 7] ON ([SQO IBI - 1].Replicate = [SQO RBI - 7].Replicate) AND ([SQO IBI - 1].StationID = [SQO RBI - 7].StationID)) LEFT JOIN [SQO RBI - 8] ON ([SQO IBI - 1].Replicate = [SQO RBI - 8].Replicate) AND ([SQO IBI - 1].StationID = [SQO RBI - 8].StationID)) LEFT JOIN [SQO RBI - 9] ON ([SQO IBI - 1].Replicate = [SQO RBI - 9].Replicate) AND ([SQO IBI - 1].StationID = [SQO RBI - 9].StationID);

rbi_metrics <-
