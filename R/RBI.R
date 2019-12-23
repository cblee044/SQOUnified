#' Compute the relative benthic index (RBI) score.
#'
#' The RBI is the weighted sum of: (a) four community metrics related to biodiversity (total number of taxa, number of crustacean taxa, abundance
#' of crustacean individuals, and number of mollusc taxa), (b) abundances of three positive indicator taxa, and (c) the presence of two negative
#' indicator species.
#'
#' The data needed to calculate the RBI are:
#' (1) Total number of taxa,
#' (2) Number of mollusc taxa,
#' (3) Number of crustacean individuals,
#' (4) Number of individuals of \emph{Monocorophium insidiosum},
#' (5) Number of individuals of \emph{Asthenothaerus diegensis},
#' (6) Number of individuals of \emph{Goniada littorea},
#' (7) Whether the data has the presence of \emph{Capitella capitata} complex, and
#' (8) Whether the data has the presence of Oligochaeta.
#'
#' To compute the RBI, the first step is to normalize the values for the benthic community metrics relative to maxima for the data used to develop
#' the RBI for the Southern California Marine Bays habitat, to produce values relative to the maxima that are referred to as scaled values. The
#' scaled value calculations use the following formulae:
#'
#' Total Number of Taxa / 99
#' Number of Mollusc Taxa / 28
#' Number of Crustacean Taxa / 29
#'
#'
#' The next step is to calculate the Taxa Richness Weighted Value (TWV) from the scaled values by the equation:
#'
#' TWV = Scaled Total Number of Taxa + Scaled Number of Mollusc Taxa + Scaled Number of Crustacean Taxa + (0.25 * Scaled Abundance of Crustacea)
#'
#' Next, the value for the two negative indicator taxa (NIT) is calculated. The two negative indicator taxa are \emph{Capitella capitata} complex
#' and Oligochaeta. For each of these taxa that are present, in any abundance, the NIT is decreased by 0.1. Therefore, if neither were found the
#' NIT = 0, if both are found the NIT = -0.2.
#'
#' The next step is to calculate the value for the three positive indicator taxa (PIT).
#'
#' @usage data(benthic_data)
#' @usage data(EG_Ref)
#' @usage data(Taxonomic_Info)
#'
#' @param BenthiCData a data frame stored in the R environment. Note that this data frame MUST contain the following
#'                    information with these headings:
#'                         \code{StationID} - an alpha-numeric identifier of the location;
#'                         \code{Replicate} - a numeric identifying the replicate number of samples taken at the location;
#'                         \code{SampleDate} - the date of sample collection;
#'                         \code{Latitude} - latitude in decimal degrees;
#'                         \code{Longitude} - longitude in decimal degrees. Make sure there is a negative sign for the Western coordinates;
#'                         \code{Taxon} - name of the fauna, ideally in SCAMIT ed12 format, do not use sp. or spp.,
#'        use sp only or just the Genus. If no animals were present in the sample use
#'        NoOrganismsPresent with 0 abundance;
#'                         \code{Abundance} - the number of each Species observed in a sample;
#'                         \code{Salinity} - the salinity observed at the location in PSU, ideally at time of sampling;
#'                         \code{Stratum} - ;
#'                         \code{Exclude} - ;
#'
#' @examples
#' RBI(benthic_data)
#' RBI(DB)

##########################################################################################################################
## This is a function to calculate relative benthic index (RBI). The RBI is the weighted sum of: 1) four community metrics
## related to biodiversity (total number of taxa, number of crustacean taxa, abundance of crustacean individuals, and
## number of mollusc taxa); 2) abundance of three positive indicator taxa; and 3) the presence of two negative indicator
## species.
##                        The data MUST contain the following information with these headings:
##
##                                  StationID - an alpha-numeric identifier of the location
##                                  Replicate - a numeric identifying the replicate number of samples taken at the
##                                              location
##                                  SampleDate - the date of sample collection
##                                  Latitude - latitude in decimal degrees
##                                  Longitude - longitude in decimal degrees make sure there is a negative sign
##                                              for the Western coordinates
##                                  Taxon - name of the fauna, ideally in SCAMIT ed12 format, do not use sp. or spp.,
##                                            use sp only or just the Genus. If no animals were present in the sample
##                                            use NoOrganismsPresent with 0 abundance
##                                  Abundance - the number of each Species observed in a sample
##                                  Salinity - the salinity observed at the location in PSU, ideally at time of sampling
##                                  Stratum -
##                                  Exclude -
##
##
##
## For the function to run, the following packages NEED to be installed:  tidyverse, reshape2, vegan, and readxl.
## Additionally the EQR.R function must also be installed and is included with this code.
##
## The output of the function will be a dataframe with StationID, Replicate, SampleDate, Latitude, Longitude,
## SalZone (The Salinity Zone assigned by M-AMBI), AMBI_Score, S (Species Richness), H (Species Diversity),
## Oligo_pct (Relative Abundance of Oligochaetes), MAMBI_Score, Orig_MAMBI_Condition, New_MAMBI_Condition,
## Use_MAMBI (Can M-AMBI be applied?), Use_AMBI (Can AMBI be applied?), and YesEG (% of Abundance with a EG value)
##########################################################################################################################


RBI <- function(BenthicData)
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
    dplyr::filter(exclude == 'No')
  # %>% dplyr::filter(replicate == 1)

  grab <- tbl(con, "tbl_grabevent") %>%
    as_tibble %>%
    dplyr::filter(grabeventnumber == 1)

  assignment <- tbl(con, "field_assignment_table") %>%
    as_tibble %>%
    dplyr::filter(stratum == "Bays" | stratum == "Ports" | stratum == "Estuaries" | stratum == "Brackish Estuaries")

  station_occupation <- tbl(con, "tbl_stationoccupation") %>%
    as_tibble %>%
    inner_join(assignment, by = 'stationid')

# TODO --> Be sure to go back and update the benthic_query.R file so that we get the data included below. We need this
# to run the RBI function. Other changes will also need to be included if other info is needed for IBI, BRI, and RIVPACS
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
    dplyr::rename(NumOfTaxa = n) %>%
    dplyr::rename(species = taxon) %>%
    dplyr::rename(StationID = stationid, Replicate = replicate, Species = species, Abundance = abundance, B13_Stratum = stratum)

  ibi_data <- rbi_data %>%
    group_by(B13_Stratum, StationID, Replicate) %>%
    summarise(NumOfTaxa = sum(NumOfTaxa))

  # columns needed in RBI: B13_Stratum, StationID, Replicate, Phylum, NumofMolluscTaxa
  rbi2 <- rbi_data %>%
    dplyr::filter(Phylum == "MOLLUSCA") %>%
    dplyr::group_by(B13_Stratum, StationID, Replicate, Phylum, NumOfTaxa) %>%
    dplyr::select(B13_Stratum, StationID, Replicate, Phylum, NumOfTaxa) %>%
    dplyr::group_by(B13_Stratum, StationID, Replicate, Phylum) %>%
    dplyr::summarise(NumOfMolluscTaxa = sum(NumOfTaxa))


  ### SQO RBI -3
  rbi3 <- rbi_data %>%
    dplyr::filter(Subphylum == "Crustacea") %>%
    dplyr::group_by(B13_Stratum, StationID, Replicate, Subphylum) %>%
    dplyr::select(B13_Stratum, StationID, Replicate, Subphylum, NumOfTaxa) %>%
    dplyr::summarise(NumOfCrustaceanTaxa = sum(NumOfTaxa))

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

  ### RBI Category Thresholds for Southern California Marine Bays
  RBI_category_thresholds <- data.frame(ref_low = c(0.27, 0.16, 0.08, 0.08),
                                        ref_high = c(0.27, 0.27, 0.16, 0.08),
                                        category = as.factor(c("Reference",
                                                               "Low Disturbance",
                                                               "Moderate Disturbance",
                                                               "High Disturbance")),
                                        category_score = c(1, 2, 3, 4))

  # Compute the RBI scores.
  # This was not included in the queries that D. Gillet listed. We went through the Technical Manual (p. 77-78)
  # to find the appropriate calculations.
  rbi_scores <- rbi_metrics %>%
    mutate(scaled_NumTaxa = NumOfTaxa/99) %>%
    mutate(scaled_NumMolluscTaxa = NumOfMolluscTaxa/28) %>%
    mutate(scaled_NumCrustaceanTaxa = NumOfCrustaceanTaxa/29) %>%
    mutate(scaled_CrustaceanAbun = CrustaceanAbun/1693) %>%
    mutate(scaled_NumTaxa = replace_na(scaled_NumTaxa, 0), scaled_NumMolluscTaxa = replace_na(scaled_NumMolluscTaxa, 0), scaled_NumCrustaceanTaxa = replace_na(scaled_NumCrustaceanTaxa, 0), scaled_CrustaceanAbun = replace_na(scaled_CrustaceanAbun, 0)) %>%
    # TWV = Taxa Richness Weighted Value
    mutate(TWV = scaled_NumTaxa + scaled_NumMolluscTaxa + scaled_NumCrustaceanTaxa + (0.25 * scaled_CrustaceanAbun)) %>%
    # NIT = Negative Indicator Taxa
    mutate(NIT =
             case_when(
               !is.na(CapitellaAbun) & !is.na(OligochaetaAbun) ~ -0.2,
               !is.na(CapitellaAbun) | !is.na(OligochaetaAbun) ~ -0.1,
               is.na(CapitellaAbun) & is.na(OligochaetaAbun) ~ 0
             )) %>%
    mutate(M_insidiosumAbun = replace_na(M_insidiosumAbun, 0), A_diegensisAbun = replace_na(A_diegensisAbun, 0), G_littoreaAbun = replace_na(G_littoreaAbun, 0)) %>%
    # PIT = Positive Indicator Taxa
    mutate(PIT = ( (M_insidiosumAbun)^(1/4) / (473)^(1/4) ) + ( (A_diegensisAbun)^(1/4) / (27)^(1/4) ) + ( (G_littoreaAbun)^(1/4) / (15)^(1/4) )) %>%
    mutate(Raw_RBI = TWV + NIT + (2 * PIT)) %>%
    dplyr::mutate(RBI_Score = (Raw_RBI - 0.03)/ 4.69) %>%
    # RBI Categories based on RBI scores
    dplyr::mutate(RBI_Category = case_when( (RBI_Score > 0.27) ~ "Reference",
                                            (RBI_Score > 0.16 & RBI_Score <= 0.27) ~ "Low Disturbance",
                                            (RBI_Score > 0.08 & RBI_Score <= 0.16) ~ "Moderate Disturbance",
                                            (RBI_Score <= 0.08)  ~ "High Disturbance" )) %>%
    # RBI Category Scores based on RBI scores
    dplyr::mutate(RBI_Category_Score = case_when( (RBI_Category == "Reference") ~ 1,
                                                  (RBI_Category == "Low Disturbance") ~ 2,
                                                  (RBI_Category == "Moderate Disturbance") ~ 3,
                                                  (RBI_Category == "High Disturbance") ~ 4))

}

