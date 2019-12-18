#' Compute the relative benthic index (RBI) score.
#'
#'
#' @usage data(benthic_data)
#' @usage data(EG_Ref)
#' @usage data(Taxonomic_Info)
#'
#' @examples
#' MAMBI.DJG.alt(benthic_data, EG_File_Name="data/Ref - EG Values 2018.csv", EG_Scheme="Hybrid")
#' MAMBI.DJG.alt(benthic_data, EG_File_Name="data/Ref - EG Values 2018.csv", EG_Scheme="US_Gulf")


library(DBI) # needed to connect to database
library(dbplyr) # needed to connect to database
library(RPostgreSQL) # needed to connect to our database
library(rstudioapi) # just so we can type the password as we run the script, so it is not written in the clear
library(tidyverse)

# con is short for connection
# Create connection to the database
con <- DBI::dbConnect(
  PostgreSQL(),
  host = "192.168.1.16",
  dbname = 'bight2018',
  user = 'b18read',
  password = '1969$Harbor' # if we post to github, we might want to do rstudioapi::askForPassword()
)


infauna <- tbl(con, "tbl_infaunalabundance_initial") %>% as_tibble()
assignment <- tbl(con, "field_assignment_table") %>% as_tibble()

BRI1 <- infauna %>%
  dplyr::left_join(Taxonomic_Info, by = c('taxon' = 'Taxon')) %>%
  dplyr::right_join(assignment, by = 'stationid') %>%
  # I assume that the next line is something they had in there as a method of removing duplicates
  # for this reason, this next line will likely be eliminated.
  # They grouped by all the columns that were selected (In query BRI - 1)
  # Instead, if need be we can use something from dplyr that deals with duplicates
  dplyr::group_by(stratum, stationid, replicate, taxon, abundance, `B-CodeScore`) %>%
  dplyr::filter(stratum %in% c("Estuaries", "Marinas", "Bays", "Ports")) %>%
  dplyr::filter(!is.na(`B-CodeScore`)) %>%
  dplyr::select(stratum, stationid, replicate, taxon, abundance, `B-CodeScore`)


