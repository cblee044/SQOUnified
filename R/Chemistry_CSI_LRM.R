# Caluculate Logistic Regression Model (LRM) values.


# ------------ Function should start here --------------
# Input should be the dataframe called chemistry, which is saved as an RData file
library(tidyverse)
load("data/chem_sampledata.RData") # chem_sampledata is the input for the function
load("data/lrm_table.RData") # this dataframe is going to be merged with the input data


chemdata <- chem_sampledata
# uncomment to make it a function again
#chem_lrm <- function(chemdata) {

# Take the Log10 of the chemistry concentration.
#chemdata$logConc <- log10(chemistry$Conc)
chemdata <- chemdata %>%
  mutate(
    logResult = log10(Result)
  )


# Combine LRM values with data based on the compound. Exclude compounds not in lrm.
# chemistry_lrm <- merge(x = lrm, y = chemistry, by = "Compound", all.x = TRUE, all.y = FALSE)
# chemdata_lrm refers to the original raw chemistry data merged together with the static global lrm_table
chemdata_lrm <- lrm_table %>% left_join(chemdata, by = 'AnalyteName')



# Calculate p_max.
#chemdata_lrm$p_max <- with(chemdata_lrm, (exp(B0 + (B1 * logResult))) / (1 + exp(B0 + (B1 * logResult))))
chemdata_lrm <- chemdata_lrm %>%
  mutate(
    p_max = (exp(B0 + B1 * logResult) / (1 + exp(B0 + B1 * logResult))) %>% round(2)
  )

# rounded it to two devimal places in previous step
#chemdata_lrm$p_max_r <- round(chemdata_lrm$p_max, digits=2)

# For each station, find the maximum p_max.
#chem_p_max <- aggregate(chemdata_lrm$p_max_r, by = list(chemdata_lrm$StationID), FUN = max, na.rm = TRUE)
#names(chem_p_max) <- c("StationID", "lrm_p_max")
chem_lrm_output <- chemdata_lrm %>%
  group_by(StationID) %>%
  summarize(
    lrm_p_max = max(p_max, na.rm = T)
  ) %>%
  ungroup() %>% # the below steps are going to be combined into this chain of pipes
  mutate(
    p_category_value = case_when(
      lrm_p_max < 0.33 ~ 1,
      (lrm_p_max >= 0.33) & (lrm_p_max <= 0.49) ~ 2,
      (lrm_p_max > 0.49) & (lrm_p_max <= 0.66) ~ 3,
      lrm_p_max > 0.66 ~ 4,
      TRUE ~ NA_real_
    ),
    p_category_text = case_when(
      p_category_value == 1 ~ "Minimal Exposure",
      p_category_value == 2 ~ "Low Exposure",
      p_category_value == 3 ~ "Moderate Exposure",
      p_category_value == 4 ~ "High Exposure",
      TRUE ~ NA_character_
    )
  )


# Assign category values.
#chem_p_max$p_category_value[chem_p_max$lrm_p_max < 0.33]  <- 1
#chem_p_max$p_category_value[chem_p_max$lrm_p_max >= 0.33 & chem_p_max$lrm_p_max <= 0.49]  <- 2
#chem_p_max$p_category_value[chem_p_max$lrm_p_max > 0.49 & chem_p_max$lrm_p_max <= 0.66]  <- 3
#chem_p_max$p_category_value[chem_p_max$lrm_p_max > 0.66]  <- 4

#chem_p_max$p_category_text[chem_p_max$p_category_value == 1] <- "Minimal Exposure"
#chem_p_max$p_category_text[chem_p_max$p_category_value == 2] <- "Low Exposure"
#chem_p_max$p_category_text[chem_p_max$p_category_value == 3] <- "Moderate Exposure"
#chem_p_max$p_category_text[chem_p_max$p_category_value == 4] <- "High Exposure"




# uncomment to make the above block a function again
#}

################# Caluculate Chemical Score Index (CSI) values. ###################

# uncomment to make the block of code a function again
#chem_csi <- function(chemdata) {
# Table of csi weight values. May want to read this in as a .csv file.

csi_weight <- data.frame(Compound = c("Copper", "Lead", "Mercury", "Zinc", "HPAH", "LPAH", "Alpha_Chlordane",
                                      "Gamma_Chlordane", "DDDs_total", "DDEs_total", "DDTs_total", "PCBs_total"),
                         weight = c(100, 88, 30, 98, 16, 5, 55, 58, 45, 33, 20, 55),
                         BDC1 = c(52.8, 26.4, 0.09, 113, 313, 85.4, 0.50, 0.54, 0.77, 1.19, 0.61, 11.9),
                         BDC2 = c(96.5, 60.8, 0.45, 201, 1325, 312, 1.23, 1.45, 3.56, 6.01, 2.79, 24.7),
                         BDC3 = c( 406, 154, 2.18, 629, 9320, 2471, 11.1, 14.5, 26.37, 45.84, 34.27, 288))


# Combine CSI Weight values with data based on the compound. Exclued compounds not in CSI calculation.

chemistry_csi <- merge(x = csi_weight, y = chemistry, by = "Compound", all.x = TRUE, all.y = FALSE)

# Calculate CSI category.

chemistry_csi$csi_category[chemistry_csi$Conc <= chemistry_csi$BDC1]  <- 1
chemistry_csi$csi_category[chemistry_csi$Conc > chemistry_csi$BDC1 & chemistry_csi$Conc <= chemistry_csi$BDC2]  <- 2
chemistry_csi$csi_category[chemistry_csi$Conc > chemistry_csi$BDC2 & chemistry_csi$Conc <= chemistry_csi$BDC3]  <- 3
chemistry_csi$csi_category[chemistry_csi$Conc > chemistry_csi$BDC3]  <- 4

chemistry_csi$CSI <- chemistry_csi$csi_category * chemistry_csi$weight

# For each station, sum the CSI values (csi_category_value*weight), then divide sum of the CSI by the sum of the weight.

chem_csi <- aggregate(cbind(CSI, weight)~StationID, data=chemistry_csi, sum, na.rm=TRUE)
chem_csi$csi_mean <- round(chem_csi$CSI/chem_csi$weight, digits = 2)

# Assign category values.

chem_csi$csi_category_value[chem_csi$csi_mean < 1.69]  <- 1
chem_csi$csi_category_value[chem_csi$csi_mean >= 1.69 & chem_csi$csi_mean <= 2.33]  <- 2
chem_csi$csi_category_value[chem_csi$csi_mean > 2.33 & chem_csi$csi_mean <= 2.99]  <- 3
chem_csi$csi_category_value[chem_csi$csi_mean > 2.99]  <- 4

chem_csi$csi_category_text[chem_csi$csi_category_value == 1] <- "Minimal Exposure"
chem_csi$csi_category_text[chem_csi$csi_category_value == 2] <- "Low Exposure"
chem_csi$csi_category_text[chem_csi$csi_category_value == 3] <- "Moderate Exposure"
chem_csi$csi_category_text[chem_csi$csi_category_value == 4] <- "High Exposure"

# uncomment to make the block of code a function again
#}

# Merge LRM and CSI values into one table.

lrm_csi <- merge(x = chem_p_max, y = chem_csi, by = "StationID", all.x = TRUE, all.y = FALSE)
lrm_csi$lrm_csi_mean <- (lrm_csi$p_category_value + lrm_csi$csi_category_value)/2

#Compute the overall integrated chemistry category.

lrm_csi$lrm_csi_category_value[lrm_csi$lrm_csi_mean == 0.5 | lrm_csi$lrm_csi_mean == 1.0] <- 1
lrm_csi$lrm_csi_category_value[lrm_csi$lrm_csi_mean == 1.5 | lrm_csi$lrm_csi_mean == 2.0] <- 2
lrm_csi$lrm_csi_category_value[lrm_csi$lrm_csi_mean == 2.5 | lrm_csi$lrm_csi_mean == 3.0] <- 3
lrm_csi$lrm_csi_category_value[lrm_csi$lrm_csi_mean == 3.5 | lrm_csi$lrm_csi_mean == 4.0] <- 4

lrm_csi$lrm_csi_category_text[lrm_csi$lrm_csi_category_value == 1] <- "Minimal Exposure"
lrm_csi$lrm_csi_category_text[lrm_csi$lrm_csi_category_value == 2] <- "Low Exposure"
lrm_csi$lrm_csi_category_text[lrm_csi$lrm_csi_category_value == 3] <- "Moderate Exposure"
lrm_csi$lrm_csi_category_text[lrm_csi$lrm_csi_category_value == 4] <- "High Exposure"

DataOutput <- data.frame(StationID = lrm_csi$StationID, CA_LRM_value = lrm_csi$lrm_p_max,
                         CA_LRM_category = lrm_csi$p_category_text, CSI_value = lrm_csi$csi_mean,
                         CSI_category = lrm_csi$csi_category_text,
                         Integrated_Chemistry_Indicator = lrm_csi$lrm_csi_category_text)

# Print to screen.

print(DataOutput)
