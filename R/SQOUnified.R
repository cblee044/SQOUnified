"benthic_data"
#' Compute the SQO index scores.
#'
#' @param DB A data file string name that we want to compute SQO scores for.
#' @param SQO A list of the type of SQO scores that we want to compute
#'     (e.g., \code{c("MAMBI", "RBI")}).
#'     The default is \code{"all"}, meaning that all scores will be computed.
#' @usage data(DB)
#' @examples
#' SQOUnified(DB = benthic_data, SQO = "all")
#' SQOUnified(DB = benthic_data, SQO = "MAMBI")
#' SQOUnified(DB = benthic_data, SQO = c("MAMBI", "RBI")) --> To be implemented in
#' the future



SQOUnified <- function(DB = benthic_data, SQO = "all"){

  # Compute ALL SQO scores
  if (SQO == "all"){
    mambi.score <- MAMBI(DB, EG_File_Name="data/Ref - EG Values 2018.csv", EG_Scheme="Hybrid")
    rbi.scores <- RBI(DB)
    ibi.scores <- IBI(DB)
    bri.scores <- BRI(DB)
    final.scores <- mambi.score %>%
      dplyr::full_join(rbi.scores, by = c("StationID", "Replicate")) %>%
      dplyr::full_join(ibi.scores, by = c("StationID", "Replicate")) %>%
      dplyr::full_join(bri.scores, by = c("StationID", "Replicate")) # will add other scores to this data frame as they are computed
  } else {
    mambi.score <- MAMBI(DB, EG_File_Name="data/Ref - EG Values 2018.csv", EG_Scheme="Hybrid")
    final.scores <- mambi.score %>%
      dplyr::left_join(rbi.scores, by = c("StationID", "Replicate")) %>%
      dplyr::left_join(ibi.scores, by = c("StationID", "Replicate")) %>%
      dplyr::left_join(bri.scores, by = c("StationID", "Replicate"))
    # will add other scores to this data frame as they are computed
  }

  ####
  # Will create a for loop with the other indices to make sure that all wanted indices are given
  # in the final csv file. For now, we just have the MAMBI score (see "else" condition above).
  # Maybe there's a better way to do this?
  ####
  for (index in SQO)
  {
    if (SQO == "all"){
      mambi.score <- MAMBI(DB, EG_File_Name="data/Ref - EG Values 2018.csv", EG_Scheme="Hybrid")
      rbi.scores <- RBI(DB)
      ibi.scores <- IBI(DB)
      bri.scores <- BRI(DB)
      final.scores <- mambi.score %>%
        dplyr::left_join(rbi.scores, by = c("StationID", "Replicate")) %>%
        dplyr::left_join(ibi.scores, by = c("StationID", "Replicate")) %>%
        dplyr::left_join(bri.scores, by = c("StationID", "Replicate")) %>%
        # only select the final scores to be written to return to user
        dplyr::select("StationID", "Replicate", "SampleDate", "Index",
                      "Score", "Category", "Category_Score", "Orig_MAMBI_Condition", "New_MAMBI_Condition")
    } else {
      for (item in SQO)
      {
        if(item == "MAMBI") {
          mambi.score <- MAMBI(DB, EG_File_Name="data/Ref - EG Values 2018.csv", EG_Scheme="Hybrid")
        }
        else if(item == "RBI"){
          rbi.scores <- RBI(DB)
        }
        else if(item == "IBI"){
          ibi.scores <- IBI(DB)
        }
        else if(item == "BRI"){
          bri.scores <- BRI(DB)
        }
        else if(item == "RIVPACS"){
          rivpacs = RIVPACS(DB)
        }
        else {
          # Add a check in chkinp. This is just for testing right now.
          print("Not a valid SQO score that can be computed.")
        }
      }
  }

  # OUTPUT: Write an xlsx file to current working directory

  write.csv(final.scores, file = "SQO-Unified.csv", row.names = FALSE)
  return(final.scores)
  }
}
