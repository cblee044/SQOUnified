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
    mambi.score <- MAMBI(DB, EG_File_Name="data/Ref - EG Values 2018.csv", EG_Scheme="Hybrid") %>%
      dplyr::mutate(Score = MAMBI_Score, Category = New_MAMBI_Condition)
    rbi.scores <- RBI(DB)
    ibi.scores <- IBI(DB)
    bri.scores <- BRI(DB)
    # We need to incorporate RIVPACS. Once we have this we will take the median of RIVPACS, IBI, BRI, RBI and report back one overall score (MBI?)
    # Once this is done, we will add this to the final output.
    # For RIVPACS, we're simply going to write a wrapper function and add this to SQOUnified.
    # If you run into problems, call D. Gillet to get more clarifaction.
    final.scores <- mambi.score %>%
      dplyr::full_join(bri.scores) %>%
      dplyr::full_join(rbi.scores) %>%
      dplyr::full_join(ibi.scores) %>% # will add other scores to this data frame as they are computed
      dplyr::select("StationID", "Replicate", "SampleDate", "B13_Stratum", "Index", "Score",
                    "Category", "Category_Score",
                    "Use_AMBI", "Use_MAMBI")
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
    final.scores <- data.frame(matrix(vector(), 0, 3),
                               dimnames = list(c(), c("Date", "File", "User")),
                               stringsAsFactors = F)
    if (SQO == "all"){
      mambi.score <- MAMBI(DB, EG_File_Name="data/Ref - EG Values 2018.csv", EG_Scheme="Hybrid")
      rbi.scores <- RBI(DB)
      ibi.scores <- IBI(DB)
      bri.scores <- BRI(DB)
      final.scores <- mambi.score %>%
        dplyr::full_join(bri.scores) %>%
        dplyr::full_join(rbi.scores) %>%
        dplyr::full_join(ibi.scores) %>% # will add other scores to this data frame as they are computed
        dplyr::select("StationID", "Replicate", "SampleDate", "B13_Stratum", "Index", "Score",
                      "Category", "Category_Score", "MAMBI_Score", "New_MAMBI_Condition",
                      "Use_AMBI", "Use_MAMBI", "YesEG")
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
        #else if(item == "RIVPACS"){
         # rivpacs = RIVPACS(DB)
        #}
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
