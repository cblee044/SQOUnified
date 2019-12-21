#' Compute the Indec of Biotic Integrity (IBI) and IBI condition category.
#'
#' @param BenthicData a data frame with AT LEAST the following information with these headings:
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
#' @examples
#' MAMBI.DJG.alt(benthic_data, EG_File_Name="data/Ref - EG Values 2018.csv", EG_Scheme="Hybrid")
#' MAMBI.DJG.alt(benthic_data, EG_File_Name="data/Ref - EG Values 2018.csv", EG_Scheme="US_Gulf")

##########################################################################################################################
#
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

  ### Reference ranges for IBI metrics in Southern California Marine Bays
  ### [ Table 5.4 (p. 77, Technical Manual, 2014) ]
  ibi_ref_ranges_table <- data.frame(ref_low = c(13, 2, 0, 19),
                                     ref_high = c(99, 25, 59, 47.1))
  row.names(ibi_ref_ranges_table) <- c("NumOfTaxa", "NumOfMolluscTaxa", "NotomastusAbun", "PctSensTaxa")


  ### IBI category response ranges for Southern California Marine Bays
  ### [ Table 5.5 (p. 77, Technical Manual, 2014) ]
  ibi_category_response_table <- data.frame(ibi_score = as.factor(c(0, 1, 2, 3, 4)),
                                            category = as.factor(c("Reference",
                                                                   "Low Disturbance",
                                                                   "Moderate Disturbance",
                                                                   "High Disturbance",
                                                                   "High Disturbance")),
                                            category_score = as.factor(c(1, 2, 3, 4, 4)))

  ### B13 IBI Metrics:
  # We stitch together all the necessary IBI metrics to determine the IBI index.
  # Each of the metrics is then compared to the tables listed above (Table 5.4 and Table 5.5) to determine the IBI score,
  # the IBI Category, and IBI Category Score
  ibi_metrics <- ibi1 %>%
    dplyr::full_join(ibi2, by = c("B13_Stratum", "StationID", "Replicate")) %>%
    dplyr::full_join(ibi3_1, by = c("B13_Stratum", "StationID", "Replicate")) %>%
    dplyr::full_join(ibi3_2, by = c("B13_Stratum", "StationID", "Replicate")) %>%
    dplyr::full_join(ibi4_1, by = c("B13_Stratum", "StationID", "Replicate")) %>%
    dplyr::full_join(ibi4_2, by = c("B13_Stratum", "StationID", "Replicate")) %>%
    dplyr::select(B13_Stratum, StationID, Replicate, NumOfTaxa, NumOfMolluscTaxa, NotomastusAbun, PctSensTaxa) %>%
    # We replace any NAs with 0 so that we can compare the values to the tables listed above
    dplyr::mutate(NotomastusAbun = replace_na(NotomastusAbun, 0)) %>%
    # The IBI score is set to zero before comparison the reference range.
    dplyr::mutate(IBI_Score = 0) %>%
    # For each metric that is out of the reference range (above or below), the IBI score goes up by one.
    dplyr::mutate(IBI_Score = if_else((NumOfTaxa < ibi_ref_ranges_table["NumOfTaxa",]$ref_low  | NumOfTaxa > ibi_ref_ranges_table["NumOfTaxa",]$ref_high),
                                      IBI_Score + 1, IBI_Score)) %>%
    dplyr::mutate(IBI_Score = if_else((NumOfMolluscTaxa < ibi_ref_ranges_table["NumOfMolluscTaxa",]$ref_low  | NumOfMolluscTaxa > ibi_ref_ranges_table["NumOfMolluscTaxa",]$ref_high),
                                      IBI_Score + 1, IBI_Score)) %>%
    dplyr::mutate(IBI_Score = if_else((NotomastusAbun < ibi_ref_ranges_table["NotomastusAbun",]$ref_low  | NotomastusAbun > ibi_ref_ranges_table["NotomastusAbun",]$ref_high),
                                      IBI_Score + 1, IBI_Score)) %>%
    dplyr::mutate(IBI_Score = if_else((PctSensTaxa < ibi_ref_ranges_table["PctSensTaxa",]$ref_low  | PctSensTaxa > ibi_ref_ranges_table["PctSensTaxa",]$ref_high),
                                      IBI_Score + 1, IBI_Score)) %>%
    # The IBI score is then compared to condition category response ranges (Table 5.5) to determine the IBI category and category score.
    dplyr::mutate(IBI_Category = case_when(IBI_Score == 0 ~ "Reference", IBI_Score == 1 ~ "Low Disturbance", IBI_Score == 2 ~ "Moderate Disturbance", (IBI_Score == 3 | IBI_Score == 4) ~ "High Disturbance")) %>%
    dplyr::mutate(IBI_Category_Score = case_when(IBI_Score == 0 ~ 1, IBI_Score == 1 ~ 2, IBI_Score == 2 ~ 3, (IBI_Score == 3 | IBI_Score == 4) ~ 4))



  #write.csv(ibi_metrics, file = "data/ibi-metrics.csv", row.names = FALSE)

}
