"benthic_data"
#' Compute the SQO index scores.
#'
#' @param DB A data file string name that we want to compute SQO scores for.
#' @param SQO A list of the type of SQO scores that we want to compute
#'     (e.g., \code{c("MAMBI", "RBI")}).
#'     The default is \code{"all"}, meaning that all scores will be computed.
#' @usage data(benthic_data)
#' @examples
#' SQOUnified(DB = benthic_data, SQO = "all")
#' SQOUnified(DB = benthic_data, SQO = "MAMBI")
#' SQOUnified(DB = benthic_data, SQO = c("MAMBI", "RBI")) --> To be implemented in
#' the future



SQOUnified <- function(DB = benthic_data, SQO = "all"){

  # Compute ALL SQO scores
  if (SQO == "all"){
    mambi.score <- MAMBI(DB, EG_File_Name="data/Ref - EG Values 2018.csv", EG_Scheme="Hybrid")
    rbi.scores <- RBI()
    ibi.scores <- IBI()
    bri.scores <- BRI()
    final.scores <- mambi.score # will add other scores to this data frame as they are computed
  } else {
    mambi.score <- MAMBI(DB, EG_File_Name="data/Ref - EG Values 2018.csv", EG_Scheme="Hybrid")
    final.scores <- mambi.score # will add other scores to this data frame as they are computed
  }

  ####
  # Will create a for loop with the other indices to make sure that all wanted indices are given
  # in the final csv file. For now, we just have the MAMBI score (see "else" condition above).
  # Maybe there's a better way to do this?
  ####
  for (index in SQO)
  {
    if (index == "all"){

    }
  }

  # OUTPUT: Write an xlsx file to current working directory
  write.csv(mambi.score, file = "SQO-Unified.csv", row.names = FALSE)
}
