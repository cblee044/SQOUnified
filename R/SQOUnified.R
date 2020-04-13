#' Compute the SQO index scores.
#'
#' @param benthic_data A data file string name that we want to compute SQO scores for.
#' @param SQO A list of the type of SQO scores that we want to compute
#'     (e.g., \code{c("MAMBI", "RBI")}).
#'     The default is \code{"all"}, meaning that all scores will be computed.
#' @usage
#' data(benthic_data)
#' data(site_assessment_criteria)
#' @examples
#' SQOUnified(benthic_data = benthic_data, LOE = "all")
#' SQOUnified(benthic_data = benthic_data, LOW = 'tox')
#'
#' @importFrom dplyr case_when full_join select rename mutate arrange
#' @importFrom purrr map
#' @importFrom tidyr spread


#' @export
SQOUnified <- function(benthic = NULL, chem = NULL, tox = NULL) {

  load("data/site_assessment_criteria.Rdata")

  if (all(is.null(c(benthic,chem,tox)))){
    stop("
      No data was provided.
      Please provide benthic, chemistry and toxicity data to get the integrated site assessments
    ")
  }
  # check the data coming in before anything
  checkdata(benthic, chem, tox)



  # Compute ALL SQO scores

  # ---- Benthic ----
  if (!is.null(benthic)) {
    benthic <- benthic.sqo(benthic) %>%
      mutate(LOE = 'Benthic') %>%
      select(StationID, Replicate, SampleDate, LOE, Index, Score, Category, `Category Score`) %>%
      # David says only keep replicate 1.
      # 999 times out of 1000 there should only be one replicate
      filter(
        Replicate == 1
      ) %>%
      # The way i see it now is, Replicate becomes a useless field.
      # The other datatypes dont use it
      # SampleDate is not very useful either, not in the Bight program at least
      # The fact that the station is called B18 shows it was sampled in 2018, which is all we care about
      # besides, I queried the tbl_infaunalabundance_initial table,
      # there are not two sampledates for the same station, unlike SMC, which has permanent station names,
      # and sampledates are used to distinguish the station at different times
      # The Bight program however, which only samples every 5 years, puts the year of the sample in the stationid
      select(-c(Replicate,SampleDate))

  } else {
    benthic = data.frame(
      StationID = c(),
      LOE = c(),
      Index = c(),
      Score = c(),
      Category = c(),
      `Category Score` = c()
    )
  }

  # ---- Chemistry ----
  if (!is.null(chem)) {
    chem <- chem.sqo(chem) %>%
      mutate(LOE = 'Chemistry') %>%
      select(StationID, LOE, Index, Score, Category, `Category Score`)
  } else {
    chem = data.frame(
      StationID = c(),
      LOE = c(),
      Index = c(),
      Score = c(),
      Category = c(),
      `Category Score` = c()
    )
  }

  # ---- Toxicity ----
  if (!is.null(tox)) {
    tox <- tox.sqo(tox) %>%
      mutate(LOE = 'Toxicity') %>%
      select(StationID, LOE, Index, Score, Category, `Category Score`)
  } else {
    tox <- data.frame(
      StationID = c(),
      LOE = c(),
      Index = c(),
      Score = c(),
      Category = c(),
      `Category Score` = c()
    )
  }

  integrated <- bind_rows(benthic, chem, tox) %>%
    filter(
      grepl("SQO",Index)
    ) %>%
    select(
      StationID, LOE, Category
    ) %>%
    group_by(
      StationID
    ) %>%
    spread(
      LOE, Category
    ) %>%
    inner_join(
      site_assessment_criteria,
      by = c("Benthic","Chemistry","Toxicity")
    ) %>%
    select(
      StationID, `Site Assessment`
    ) %>%
    mutate(
      LOE = "Integrated",
      Index = "Site Assessment",
      Score = NA_real_,
      `Category Score` = NA_real_
    ) %>%
    select(
      StationID,
      LOE,
      Index,
      Score,
      Category = `Site Assessment`,
      `Category Score`
    )

  out <- bind_rows(
    benthic, chem, tox, integrated
  ) %>%
  arrange(
    StationID, LOE, Index
  )

  return(out)


}


# Commented out older code. We will need to update this function to output the final
# SQO scores for benthic, toxicity, and chemistry data.
#   else {
#     mambi.score <- MAMBI(benthic_data, EG_File_Name="data/Ref - EG Values 2018.csv", EG_Scheme="Hybrid")
#     final.scores <- mambi.score %>%
#       dplyr::left_join(rbi.scores, by = c("StationID", "Replicate")) %>%
#       dplyr::left_join(ibi.scores, by = c("StationID", "Replicate")) %>%
#       dplyr::left_join(bri.scores, by = c("StationID", "Replicate")) %>%
#       dplyr::group_by(StationID, SampleDate, Replicate)
#     # will add other scores to this data frame as they are computed
#   }
#
#   ####
#   # Will create a for loop with the other indices to make sure that all wanted indices are given
#   # in the final csv file. For now, we just have the MAMBI score (see "else" condition above).
#   # Maybe there's a better way to do this?
#   ####
#   for (index in SQO)
#   {
#     final.scores <- data.frame(matrix(vector(), 0, 3),
#                                dimnames = list(c(), c("Date", "File", "User")),
#                                stringsAsFactors = F)
#     if (SQO == "all"){
#       mambi.score <- MAMBI(benthic_data, EG_File_Name="data/Ref - EG Values 2018.csv", EG_Scheme="Hybrid")
#       rbi.scores <- RBI(benthic_data)
#       ibi.scores <- IBI(benthic_data)
#       bri.scores <- BRI(benthic_data)
#       final.scores <- mambi.score %>%
#         dplyr::full_join(bri.scores) %>%
#         dplyr::full_join(rbi.scores) %>%
#         dplyr::full_join(ibi.scores) %>% # will add other scores to this data frame as they are computed
#         dplyr::select("StationID", "Replicate", "SampleDate", "B13_Stratum", "Index", "Score",
#                       "Category", "Category_Score", "MAMBI_Score", "New_MAMBI_Condition",
#                       "Use_AMBI", "Use_MAMBI", "YesEG")
#     } else {
#       for (item in SQO)
#       {
#         if(item == "MAMBI") {
#           mambi.score <- MAMBI(benthic_data, EG_File_Name="data/Ref - EG Values 2018.csv", EG_Scheme="Hybrid")
#         }
#         else if(item == "RBI"){
#           rbi.scores <- RBI(benthic_data)
#         }
#         else if(item == "IBI"){
#           ibi.scores <- IBI(benthic_data)
#         }
#         else if(item == "BRI"){
#           bri.scores <- BRI(benthic_data)
#         }
#         #else if(item == "RIVPACS"){
#          # rivpacs = RIVPACS(benthic_data)
#         #}
#         else {
#           # Add a check in chkinp. This is just for testing right now.
#           print("Not a valid SQO score that can be computed.")
#         }
#       }
#   }
#
#   # OUTPUT: Write an xlsx file to current working directory
#
#   write.csv(final.scores, file = "SQO-Unified.csv", row.names = FALSE)
#   return(final.scores)
#   }
# }
