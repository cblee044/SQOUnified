#' This script is going to be a wrapper function for the RIVPACS scripts to be implemented into
#' SQOUnified package.
#'
#' @param benthic_data data frame stored in the R environment. Note that this data frame MUST contain the following
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
#' I saved the SoCalRivpacs function in an RData file and import it here
#' @usage
#' data(SoCalRivpacs)
#' data(benthic_data)
#' @import dplyr
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' RIVPACS(benthic_data)
#'
#' @export
#'


# This is what we will use for RIVPACS
RIVPACS <- function(benthic_data){

  # Split to SoCal and SFBay.
  ## We are only working with SoCal data so we don't need to do this!

  #scb.station <- station[toupper(station$HabitatCode) == "C", ]

  #########################
  # At this point of the SQOUnified package, we are only working with SoCal data so we don't need sfb
  #sfb.station <- station[toupper(station$HabitatCode) == "D", ]

  # If data exists for habitat, format data.

  #if(nrow(scb.station) > 0) {

  scb.predictors <- data.frame(Latitude = benthic_data$Latitude,
                               Longitude = benthic_data$Longitude,
                               SampleDepth = benthic_data$SampleDepth) %>%
    dplyr::distinct()

  benthic_data <- benthic_data %>% dplyr::rename(Taxa = Species)

  scb.taxa <- benthic_data %>% dplyr::select(StationID, Latitude, Longitude, SampleDepth) %>%
    dplyr::distinct()

  row.names(scb.predictors) <- scb.taxa$StationID

  scb.predictors <- as.matrix(scb.predictors)

  # Don't need this line because all needed info is in benthic_data data frame
  # and we're only doing SCB data, so no need to classify them
  #scb.taxa <- benthic_data[benthic_data$StationID %in% scb.station$StationID, ]
  scb.taxa <- benthic_data %>%
    dplyr::filter(Replicate == 1) %>%
    dplyr::select(StationID, Taxa, Abundance) %>%
    dplyr::distinct()

  scb.taxa$Taxa <- gsub(" ", "_", scb.taxa$Taxa, fixed = TRUE)
  scb.taxa$Taxa <- gsub("(", "_", scb.taxa$Taxa, fixed = TRUE)
  scb.taxa$Taxa <- gsub(")", "_", scb.taxa$Taxa, fixed = TRUE)

  scb.taxa <- scb.taxa %>%
    tidyr::pivot_wider(id_cols = "StationID", names_from = "Taxa",
                       values_from = "Abundance", values_fn = list(Abundance = list))
  scb.taxa <- as.data.frame(scb.taxa)

  scb.taxa <- scb.taxa[, -1]

  colnames(scb.taxa) <- gsub("Abundance.", "", colnames(scb.taxa))

  # Replace NAs with zero.
  scb.taxa[scb.taxa == "NULL"] <- 0
  scb.taxa = as.data.frame(lapply(scb.taxa, as.numeric))
  row.names(scb.taxa) <- row.names(scb.predictors)

  # RIVPACS calculations. By default the functions use the example user data.
  socal <- SoCalRivpacs(observed.predictors = scb.predictors, observed.taxa = scb.taxa)

  # the stations column of the oe table dataframe wwas being returned as a factor. Need to make that a character
  socal$oe.table <- socal$oe.table %>%
    mutate_if(is.factor,as.character)

  benthic_data <- benthic_data %>%
    dplyr::rename(B13_Stratum = Stratum) %>%
    dplyr::select(StationID, Replicate, SampleDate, B13_Stratum) %>%
    dplyr::distinct()

  rivpacs.score <- socal$oe.table %>%
    dplyr::select(stations, O.over.E) %>%
    dplyr::rename(StationID = stations, Score = O.over.E) %>%
    dplyr::full_join(benthic_data) %>%
    dplyr::mutate(Index = "RIVPACS") %>%
    dplyr::mutate(Category = case_when((Score > 0.90 | Score < 1.10) ~ "Reference",
                                       ((Score > 0.74 & Score <= 0.90) | Score >= 1.10 & Score < 1.26) ~ "Low Disturbance",
                                       ((Score > 0.32 & Score <= 0.74) | (Score >= 1.26)) ~ "Moderate Disturbance",
                                       (Score <= 0.32) ~ "High Disturbance")) %>%
    dplyr::mutate(`Category Score` = case_when(Category == "Reference" ~ 1,
                                             Category == "Low Disturbance" ~ 2,
                                             Category == "Moderate Disturbance" ~ 3,
                                             Category == "High Disturbance" ~ 4)) %>%
    dplyr::select(StationID, SampleDate, Replicate, B13_Stratum, Score, Category, `Category Score`)


  return(rivpacs.score)
}
