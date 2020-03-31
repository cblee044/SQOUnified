# Caluculate Logistic Regression Model (LRM) values.

rm(list = ls())

# Read in data.

chem_orig <- read.csv("data/chemistry.csv", na.strings = "x")

# Convert to long format.

chemistry <- reshape(data = chem_orig, varying = list(names(chem_orig)[-1]), v.names = "Conc", timevar = "Compound",
                     idvar = "StationID", times = names(chem_orig)[-1], direction = "long")

# Take the Log10 of the chemistry concentration.

chemistry$logConc <- log10(chemistry$Conc)

# Table of lrm values. May want to read this in as a .csv file.

lrm <- data.frame(Compound = c("Cadmium", "Copper", "Lead", "Mercury", "Zinc", "HPAH", "LPAH", "Alpha_Chlordane",
                               "Dieldrin", "Trans_Nonachlor", "PCBs_total", "X4_4_DDT"),
                  B0 = c(0.2894, -5.5931, -4.7228, -0.0618, -5.1337, -8.1922, -6.8071, -3.408, -1.8344, -4.259, -4.4144, -3.5531),
                  B1 = c(3.1764, 2.5885, 2.8404, 2.6837, 2.4205, 1.9995, 1.8827, 4.457, 2.589, 5.3135, 1.4837, 3.2621))

# Combine LRM values with data based on the compound. Exclude compounds not in lrm.

chemistry_lrm <- merge(x = lrm, y = chemistry, by = "Compound", all.x = TRUE, all.y = FALSE)

# Calculate p_max.

chemistry_lrm$p_max <- with(chemistry_lrm, (exp(B0 + (B1 * logConc))) / (1 + exp(B0 + (B1 * logConc))))
chemistry_lrm$p_max_r <- round(chemistry_lrm$p_max, digits=2)

# For each station, find the maximum p_max.

chem_p_max <- aggregate(chemistry_lrm$p_max_r, by = list(chemistry_lrm$StationID), FUN = max, na.rm = TRUE)

names(chem_p_max) <- c("StationID", "lrm_p_max")

# Assign category values.

chem_p_max$p_category_value[chem_p_max$lrm_p_max < 0.33]  <- 1
chem_p_max$p_category_value[chem_p_max$lrm_p_max >= 0.33 & chem_p_max$lrm_p_max <= 0.49]  <- 2
chem_p_max$p_category_value[chem_p_max$lrm_p_max > 0.49 & chem_p_max$lrm_p_max <= 0.66]  <- 3
chem_p_max$p_category_value[chem_p_max$lrm_p_max > 0.66]  <- 4

chem_p_max$p_category_text[chem_p_max$p_category_value == 1] <- "Minimal Exposure"
chem_p_max$p_category_text[chem_p_max$p_category_value == 2] <- "Low Exposure"
chem_p_max$p_category_text[chem_p_max$p_category_value == 3] <- "Moderate Exposure"
chem_p_max$p_category_text[chem_p_max$p_category_value == 4] <- "High Exposure"

################# Caluculate Chemical Score Index (CSI) values. ###################

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
