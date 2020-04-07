library(readxl)
# Good-Bad Benchmarks following Pelletier et al. 2018
Saline_Standards <- read_xlsx("data/Pelletier2018_Standards.xlsx", sheet = "Saline Sites")
#Good-Bad Benchmarks following Pelletier et al. 2018
TidalFresh_Standards <- read_xlsx("data/Pelletier2018_Standards.xlsx", sheet = "Tidal Fresh Sites")
# suite of US Ecological Groups assigned initially in Gillett et al. 2015
EG_Ref <- read.csv("data/Ref - EG Values 2018.csv", stringsAsFactors = F, na.strings = "")
# Taxonomic information (provided by Gillet)
Taxonomic_Info <- read.csv("data/Master - Taxonomic Info 1-2-20 for Robert.csv")

save(Saline_Standards, file ='data/Saline_Standards.Rdata')
save(TidalFresh_Standards, file ='data/TidalFresh_Standards.Rdata')
save(EG_Ref, file = 'data/EG_Ref.Rdata')
save(Taxonomic_Info, file = 'data/Taxonomic_Info.Rdata')
