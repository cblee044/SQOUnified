#' Calulate the Indices for Chemistry Line of Evidence to determine Station Assessment
#' in other words, how impacted a site is
#'
#' This is done by getting the :
#'   LRM (Logistic Regression Model) Score
#'   CSI (Chemical Score Index)
#'   The Average of the Above two Scores, which is hard to come up with a name for
#'
#' The ultimate guide for this thing is the CASQO Technical Manual Chapter 3 (page 18 to 36)
#'
#' @param chemdata a dataframe with the following headings
#'    \code{StationID} - an alpha-numeric identifier of the location;
#'    \code{AnalyteName} - Name of the Chemical Compound
#'    \code{Result} - the concentration of the Analyte at that station
#'
#' @usage data(lrm_table)
#' @usage data(csi_weight)
#'
#' @importFrom dplyr mutate group_by ungroup arrange select rename left_join case_when summarize summarise
#' @export

# ------------ LRM --------------
# uncomment to make it a function again

LRM <- function(chemdata) {
  "lrm_table"



  # Take the Log10 of the chemistry concentration.
  chemdata <- chemdata %>%
    mutate(
      logResult = log10(Result)
    )

  # Combine LRM values with data based on the compound. Exclude compounds not in lrm.

  # Page 32 of Technical Manual
  chemdata_lrm <- lrm_table %>% left_join(chemdata, by = 'AnalyteName') %>%
    mutate(
      p = (exp(B0 + B1 * logResult) / (1 + exp(B0 + B1 * logResult))) %>% round(2)
    ) %>%
    # Page 33 of Technical Manual
    group_by(StationID) %>%
    summarize(
      Score = max(p, na.rm = T)
    ) %>%
    ungroup() %>%
    mutate(
      # Page 34 of Technical Manual
      `Category Score` = case_when(
        Score < 0.33 ~ 1,
        Score >= 0.33 & Score <= 0.49 ~ 2,
        Score > 0.49 & Score <= 0.66 ~ 3,
        Score > 0.66 ~ 4,
        TRUE ~ NA_real_
      ),
      Category = case_when(
        `Category Score` == 1 ~ "Minimal Exposure",
        `Category Score` == 2 ~ "Low Exposure",
        `Category Score` == 3 ~ "Moderate Exposure",
        `Category Score` == 4 ~ "High Exposure",
        TRUE ~ NA_character_
      )
    ) %>%
    mutate(
      Index = 'LRM'
    ) %>%
    select(
      StationID, Index, Score, Category, `Category Score`
    )

  return(chemdata_lrm)

# uncomment to make the above block a function again
}


# ------------ CSI (Chemical Score Index) --------------
#' @export
CSI <- function(chemdata) {
  "csi_weight"

  # Combine CSI Weight values with data based on the compound. Exclued compounds not in CSI calculation.
  chemdata_csi <- csi_weight %>% left_join(chemdata, by = "AnalyteName")

  chemdata_csi <- csi_weight %>%
    left_join(chemdata, by = "AnalyteName") %>%
    mutate(
      weight = case_when(
        # we needed this because the weights are only summed for mean CSI if the Result value exists
        # So we have to make it NA_real_ where the Result is NA
        !is.na(Result) ~ weight,
        TRUE ~ NA_real_
      ),
      # Page 34 of Technical Manual
      exposure_score = case_when(
        Result <= BDC1 ~ 1,
        (Result > BDC1) & (Result <= BDC2) ~ 2,
        (Result > BDC2) & (Result <= BDC3) ~ 3,
        Result > BDC3 ~ 4,
        TRUE ~ NA_real_
      ),
      # Page 35 of Technical Manual
      weighted_score = exposure_score * weight # manual calls this the weighted score (page 35)
    ) %>%
    group_by(
      # We need to also group by SampleDate.
      # Current SampleData does not have a date in it
      StationID #,SampleDate ?
    ) %>%
    summarize(
      # Page 35 of Technical Manual (Chart)
      weighted_score_sum = sum(weighted_score, na.rm = T),
      weight = sum(weight, na.rm = T),
      Score = round(weighted_score_sum / weight, 2)
    ) %>%
    ungroup() %>%
    select(-c(weighted_score_sum, weight)) %>%
    mutate(
      # Page 36 of Technical Manual
      `Category Score` = case_when(
        Score < 1.69 ~ 1,
        (Score >= 1.69) & (Score <= 2.33) ~ 2,
        (Score >= 2.33) & (Score <= 2.99) ~ 3,
        Score > 2.99 ~ 4,
        TRUE ~ NA_real_
      ),
      Category = case_when(
        `Category Score` == 1 ~ "Minimal Exposure",
        `Category Score` == 2 ~ "Low Exposure",
        `Category Score` == 3 ~ "Moderate Exposure",
        `Category Score` == 4 ~ "High Exposure",
        TRUE ~ NA_character_
      )
    ) %>%
    mutate(
      Index = "CSI"
    ) %>%
    select(
      StationID, Index, Score, Category, `Category Score`
    )

  return(chemdata_csi)
}

# ------------ Integrated Score --------------
#' @export
chem.sqo <- function(chemdata) {

  chemdata_lrm <- LRM(chemdata)
  chemdata_csi <- CSI(chemdata)

  # We should probably put some checks on the input data here
  # if it doesn't meet requirements, call stop function

  rbind(chemdata_lrm, chemdata_csi) %>%
    arrange(StationID) %>%
    group_by(StationID) %>%
    summarize(
      # we call ceiling because we are always wanting to round up
      # err on the side of determining that a site is more impacted, rather than not
      Score = ceiling(mean(`Category Score`)),
      `Category Score` = ceiling(mean(`Category Score`)) # Category Score is the same as the Score in this case
    ) %>%
    ungroup() %>%
    mutate(
      Index = "Integrated SQO",
      Category = case_when(
        Score == 1 ~ "Minimal Exposure",
        Score == 2 ~ "Low Exposure",
        Score == 3 ~ "Moderate Exposure",
        Score == 4 ~ "High Exposure",
        TRUE ~ NA_character_
      )
    ) %>%
    select(
      StationID, Index, Score, Category, `Category Score`
    ) %>%
    rbind(chemdata_lrm, chemdata_csi) %>%
    mutate_if(is.factor,as.character) %>%
    arrange(
      StationID, Index
    )
}

