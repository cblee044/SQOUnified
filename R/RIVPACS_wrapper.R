# This script is going to be a wrapper function for the RIVPACS scripts to be implemented into
# SQOUnified package.

RIVPACS_wrapper <- function(DB){

  # Split to SoCal and SFBay.
  ## We are only working with SoCal data so we don't need to do this!

  #scb.station <- station[toupper(station$HabitatCode) == "C", ]

  #########################
  # At this point of the SQOUnified package, we are only working with SoCal data so we don't need sfb
  #sfb.station <- station[toupper(station$HabitatCode) == "D", ]

  # If data exists for habitat, format data.

  #if(nrow(scb.station) > 0) {

  scb.predictors <- data.frame(Latitude = DB$Latitude,
                               Longitude = DB$Longitude,
                               SampleDepth = DB$SampleDepth) %>%
    dplyr::distinct()

  DB <- DB %>% dplyr::rename(Taxa = Species)

  scb.taxa <- DB %>% dplyr::select(StationID, Latitude, Longitude, SampleDepth) %>%
    dplyr::distinct()

  row.names(scb.predictors) <- scb.taxa$StationID

  scb.predictors <- as.matrix(scb.predictors)

  # Don't need this line because all needed info is in DB data frame
  # and we're only doing SCB data, so no need to classify them
  #scb.taxa <- DB[DB$StationID %in% scb.station$StationID, ]
  scb.taxa <- DB %>%
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

  rivpacs.score <- socal$oe.table %>%
    dplyr::select(stations, O.over.E) %>%
    dplyr::rename(StationID = stations, Score = O.over.E) %>%
    dplyr::mutate(Index = "RIVPACS") %>%
    dplyr::mutate(Category = case_when((Score > 0.90 | Score < 1.10) ~ "Reference",
                                       ((Score > 0.74 & Score <= 0.90) | Score >= 1.10 & Score < 1.26) ~ "Low Disturbance",
                                       ((Score > 0.32 & Score <= 0.74) | (Score >= 1.26)) ~ "Moderate Disturbance",
                                       (Score <= 0.32) ~ "High Disturbance")) %>%
    dplyr::mutate(Category_Score = case_when(Category == "Reference" ~ 1,
                                             Category == "Low Disturbance" ~ 2,
                                             Category == "Moderate Disturbance" ~ 3,
                                             Category == "High Disturbance" ~ 4))

  return(rivpacs.score)
}
