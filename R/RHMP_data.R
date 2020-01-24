# create a vector with all the RHMP stations of interest
# Note: We pasted the stations into a text file and created a csv.
#       This csv file is what is read in.
rhmp = read.csv("RHMP_data.csv", fileEncoding="UTF-8-BOM", header = FALSE)

# stations are read in as factors. We coerce them into characters.
rhmp = as.character(rhmp$V1)

# filter the data base qeuery by the RHMP stations of interest
DB = test %>%
  dplyr::filter(StationID %in% rhmp)

# Run the SQOUnified code to get the calculated benthic indices

mambi.scores <- MAMBI(DB, EG_File_Name="data/Ref - EG Values 2018.csv", EG_Scheme="Hybrid")
