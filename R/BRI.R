#' Compute the relative benthic index (RBI) score.
#'
#'
#' @usage data(benthic_data)
#' @usage data(EG_Ref)
#' @usage data(Taxonomic_Info)
#' @usage data(assignment)
#'
#' @examples
#'
#'
"assignment"
"benthic_data"
"EG_Ref"
"Taxonomic_Info"


BRI <- function(BenthicData)
{
  out <- BenthicData %>%
  dplyr::left_join(Taxonomic_Info, by = c('taxon' = 'Taxon')) %>%
  dplyr::right_join(assignment, by = 'stationid') %>%
  # I assume that the next line is something they had in there as a method of removing duplicates
  # for this reason, this next line will likely be eliminated.
  # They grouped by all the columns that were selected (In query BRI - 1)
  # Instead, if need be we can use something from dplyr that deals with duplicates
  # I actually found that it didn't appear to make a difference
  #dplyr::group_by(stratum, stationid, replicate, taxon, abundance, `B-CodeScore`) %>%
  dplyr::filter(stratum %in% c("Estuaries", "Marinas", "Bays", "Ports")) %>%
  dplyr::filter(!is.na(`B-CodeScore`)) %>%
  dplyr::select(stratum, stationid, replicate, taxon, abundance, `B-CodeScore`)  %>%
  # End of BRI - 1 query. Begin BRI - 2 query
  dplyr::mutate(
    fourthroot_abun = abundance ** 0.25,
    tolerance_score = fourthroot_abun * `B-CodeScore`
  ) %>%
  # End of BRI - 2. Begin BRI - 3
  dplyr::group_by(
    stratum, stationid, replicate
  ) %>%
  dplyr::summarize(
    BRI_Score = sum(tolerance_score, na.rm = T) / sum(fourthroot_abun, na.rm = T)
  )

  return(out)
}


