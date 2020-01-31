# create a vector with all the RHMP stations of interest
# Note: We pasted the stations into a text file and created a csv.
#       This csv file is what is read in.
rhmp = read.csv("RHMP_data.csv", fileEncoding="UTF-8-BOM", header = FALSE)

# stations are read in as factors. We coerce them into characters.
rhmp = as.character(rhmp$V1)

# filter the data base qeuery by the RHMP stations of interest
DB = benthic_data %>%
  dplyr::filter(StationID %in% rhmp)

save(DB, file = "data/rhmp_data.Rdata")
write.csv(benthic_data, file = "data/rhmp_data.csv", row.names = FALSE)

# Run the SQOUnified code to get the calculated benthic indices
# NOTE: We don't have RIVPACS score in yet, so this is NOT the final scores.
library(devtools)
install()
library(SQOUnified)
final <- SQOUnified(DB, SQO = "all")
