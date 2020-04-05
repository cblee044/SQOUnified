#May 6, 2014 Logic and Syntax Now Working Correctly
#Fixed logic error for nontoxic samples
# 
#Empty memory
rm(list = ls())

#Set directory for analysis
setwd("C:/Data/My Documents/SQO/RTool/Toxicity")

# Read in data.
tox_orig <- read.csv("ToxTestDataset.csv", na.strings = "x")

#Calculate Percent of Control
tox_orig$Test1PctCon<-round((tox_orig$ToxResult1/tox_orig$ToxControl1)*100, digits=0)
tox_orig$Test2PctCon<-round((tox_orig$ToxResult2/tox_orig$ToxControl2)*100, digits=0)
tox_orig$Test3PctCon<-round((tox_orig$ToxResult3/tox_orig$ToxControl3)*100, digits=0)
tox_orig$Test4PctCon<-round((tox_orig$ToxResult4/tox_orig$ToxControl4)*100, digits=0)

#Look at data so far
tox_orig

#New Dataframe for calculations
tox_thresholds <- tox_orig

#Set Variable Values for Thresholds

Species <- c("Eohaustorius", "Mytilus", "Leptocheirus", "Rhepoxynius", "Neanthes")

Low <- c(90, 80, 90, 90, 90)
Med <- c(82, 77, 78, 83, 68)
High <- c(59, 42, 56, 70, 42)



#Loop Through the Tox Tests and  Thresholds For Test No. 1 Calculating Category Score
for (i in 1:5) {
tox_thresholds$Test1_Category_Value[tox_thresholds$ToxTest1 == Species[i] & tox_thresholds$Test1PctCon >= Med[i] & tox_thresholds$SigDiff1 == "No"] <- 1
tox_thresholds$Test1_Category_Value[tox_thresholds$ToxTest1 == Species[i] & tox_thresholds$Test1PctCon >= Med[i] & tox_thresholds$SigDiff1 == "Yes"] <- 2
tox_thresholds$Test1_Category_Value[tox_thresholds$ToxTest1 == Species[i] & tox_thresholds$Test1PctCon < Med[i] & tox_thresholds$Test1PctCon >= High[i] & tox_thresholds$SigDiff1 == "No"] <- 2
tox_thresholds$Test1_Category_Value[tox_thresholds$ToxTest1 == Species[i] & tox_thresholds$Test1PctCon < Med[i] & tox_thresholds$Test1PctCon >= High[i] & tox_thresholds$SigDiff1 == "Yes"] <- 3
tox_thresholds$Test1_Category_Value[tox_thresholds$ToxTest1 == Species[i] & tox_thresholds$Test1PctCon < High[i]] <- 4
tox_thresholds$Test1_Category_Value[tox_thresholds$ToxTest1 == Species[i] & tox_thresholds$ToxResult1 >= Low[i]] <- 1}

#tox_thresholds

#Loop Through the Tox Tests and  Thresholds For Test No. 2 Calculating Category Score
for (i in 1:5) {  
  tox_thresholds$Test2_Category_Value[tox_thresholds$ToxTest2 == Species[i] & tox_thresholds$Test2PctCon >= Med[i] & tox_thresholds$SigDiff2 == "No"] <- 1
  tox_thresholds$Test2_Category_Value[tox_thresholds$ToxTest2 == Species[i] & tox_thresholds$Test2PctCon >= Med[i] & tox_thresholds$SigDiff2 == "Yes"] <- 2
  tox_thresholds$Test2_Category_Value[tox_thresholds$ToxTest2 == Species[i] & tox_thresholds$Test2PctCon < Med[i] & tox_thresholds$Test2PctCon >= High[i] & tox_thresholds$SigDiff2 == "No"] <- 2
  tox_thresholds$Test2_Category_Value[tox_thresholds$ToxTest2 == Species[i] & tox_thresholds$Test2PctCon < Med[i] & tox_thresholds$Test2PctCon >= High[i] & tox_thresholds$SigDiff2 == "Yes"] <- 3
  tox_thresholds$Test2_Category_Value[tox_thresholds$ToxTest2 == Species[i] & tox_thresholds$Test2PctCon < High[i]] <- 4
  tox_thresholds$Test2_Category_Value[tox_thresholds$ToxTest2 == Species[i] & tox_thresholds$ToxResult2 >= Low[i]] <- 1}

#tox_thresholds

#Loop Through the Tox Tests and  Thresholds For Test No. 3 Calculating Category Score
for (i in 1:5) {  
  tox_thresholds$Test3_Category_Value[tox_thresholds$ToxTest3 == Species[i] & tox_thresholds$Test3PctCon >= Med[i] & tox_thresholds$SigDiff3 == "No"] <- 1
  tox_thresholds$Test3_Category_Value[tox_thresholds$ToxTest3 == Species[i] & tox_thresholds$Test3PctCon >= Med[i] & tox_thresholds$SigDiff3 == "Yes"] <- 2
  tox_thresholds$Test3_Category_Value[tox_thresholds$ToxTest3 == Species[i] & tox_thresholds$Test3PctCon < Med[i] & tox_thresholds$Test3PctCon >= High[i] & tox_thresholds$SigDiff3 == "No"] <- 2
  tox_thresholds$Test3_Category_Value[tox_thresholds$ToxTest3 == Species[i] & tox_thresholds$Test3PctCon < Med[i] & tox_thresholds$Test3PctCon >= High[i] & tox_thresholds$SigDiff3 == "Yes"] <- 3
  tox_thresholds$Test3_Category_Value[tox_thresholds$ToxTest3 == Species[i] & tox_thresholds$Test3PctCon < High[i]] <- 4
  tox_thresholds$Test3_Category_Value[tox_thresholds$ToxTest3 == Species[i] & tox_thresholds$ToxResult3 >= Low[i]] <- 1}

#tox_thresholds

#Loop Through the Tox Tests and  Thresholds For Test No. 4 Calculating Category Score
for (i in 1:5) {  
  tox_thresholds$Test4_Category_Value[tox_thresholds$ToxTest4 == Species[i] & tox_thresholds$Test4PctCon >= Med[i] & tox_thresholds$SigDiff4 == "No"] <- 1
  tox_thresholds$Test4_Category_Value[tox_thresholds$ToxTest4 == Species[i] & tox_thresholds$Test4PctCon >= Med[i] & tox_thresholds$SigDiff4 == "Yes"] <- 2
  tox_thresholds$Test4_Category_Value[tox_thresholds$ToxTest4 == Species[i] & tox_thresholds$Test4PctCon < Med[i] & tox_thresholds$Test3PctCon >= High[i] & tox_thresholds$SigDiff4 == "No"] <- 2
  tox_thresholds$Test4_Category_Value[tox_thresholds$ToxTest4 == Species[i] & tox_thresholds$Test4PctCon < Med[i] & tox_thresholds$Test3PctCon >= High[i] & tox_thresholds$SigDiff4 == "Yes"] <- 3
  tox_thresholds$Test4_Category_Value[tox_thresholds$ToxTest4 == Species[i] & tox_thresholds$Test4PctCon < High[i]] <- 4
  tox_thresholds$Test4_Category_Value[tox_thresholds$ToxTest4 == Species[i] & tox_thresholds$ToxResult4 >= Low[i]] <- 1}


tox_thresholds$Tox1_Category_Text[tox_thresholds$Test1_Category_Value == 1] <- "Nontoxic"
tox_thresholds$Tox1_Category_Text[tox_thresholds$Test1_Category_Value == 2] <- "Low Toxicity"
tox_thresholds$Tox1_Category_Text[tox_thresholds$Test1_Category_Value == 3] <- "Moderate Toxicity"
tox_thresholds$Tox1_Category_Text[tox_thresholds$Test1_Category_Value == 4] <- "High Toxicity"

tox_thresholds$Tox2_Category_Text[tox_thresholds$Test2_Category_Value == 1] <- "Nontoxic"
tox_thresholds$Tox2_Category_Text[tox_thresholds$Test2_Category_Value == 2] <- "Low Toxicity"
tox_thresholds$Tox2_Category_Text[tox_thresholds$Test2_Category_Value == 3] <- "Moderate Toxicity"
tox_thresholds$Tox2_Category_Text[tox_thresholds$Test2_Category_Value == 4] <- "High Toxicity"

tox_thresholds$Tox3_Category_Text[tox_thresholds$Test3_Category_Value == 1] <- "Nontoxic"
tox_thresholds$Tox3_Category_Text[tox_thresholds$Test3_Category_Value == 2] <- "Low Toxicity"
tox_thresholds$Tox3_Category_Text[tox_thresholds$Test3_Category_Value == 3] <- "Moderate Toxicity"
tox_thresholds$Tox3_Category_Text[tox_thresholds$Test3_Category_Value == 4] <- "High Toxicity"

tox_thresholds$Tox4_Category_Text[tox_thresholds$Test4_Category_Value == 1] <- "Nontoxic"
tox_thresholds$Tox4_Category_Text[tox_thresholds$Test4_Category_Value == 2] <- "Low Toxicity"
tox_thresholds$Tox4_Category_Text[tox_thresholds$Test4_Category_Value == 3] <- "Moderate Toxicity"
tox_thresholds$Tox4_Category_Text[tox_thresholds$Test4_Category_Value == 4] <- "High Toxicity"

#Take the mean of Category Scores for all four tests ignoring NAs
tox_thresholds$Tox_Category_Mean <- rowMeans(subset(tox_thresholds, select = c(Test1_Category_Value, Test2_Category_Value, Test3_Category_Value, Test4_Category_Value)), na.rm = TRUE)

#Calculate Final Toxicity LOE Score
tox_thresholds$Tox_Category_Score[tox_thresholds$Tox_Category_Mean < 1.5] <- 1
tox_thresholds$Tox_Category_Score[tox_thresholds$Tox_Category_Mean >= 1.5 & tox_thresholds$Tox_Category_Mean < 2.5] <- 2
tox_thresholds$Tox_Category_Score[tox_thresholds$Tox_Category_Mean >= 2.5 & tox_thresholds$Tox_Category_Mean < 3.5] <- 3
tox_thresholds$Tox_Category_Score[tox_thresholds$Tox_Category_Mean >= 3.5] <- 4

#Make Category Scores Text
tox_thresholds$Tox_LOE_Text[tox_thresholds$Tox_Category_Score == 1] <- "Nontoxic"
tox_thresholds$Tox_LOE_Text[tox_thresholds$Tox_Category_Score == 2] <- "Low Toxicity"
tox_thresholds$Tox_LOE_Text[tox_thresholds$Tox_Category_Score == 3] <- "Moderate Toxicity"
tox_thresholds$Tox_LOE_Text[tox_thresholds$Tox_Category_Score == 4] <- "High Toxicity"

tox_thresholds

ToxOutput <- data.frame(StationID = tox_thresholds$StationID, tox_thresholds$ToxTest1, tox_thresholds$Tox1_Category_Text, tox_thresholds$ToxTest2, tox_thresholds$Tox2_Category_Text,
                        tox_thresholds$ToxTest3, tox_thresholds$Tox3_Category_Text, tox_thresholds$ToxTest4, tox_thresholds$Tox4_Category_Text, tox_thresholds$Tox_LOE_Text)

print(ToxOutput)

write.table(ToxOutput, file="ToxtestOutputFile.csv", sep=",", row.names=FALSE)