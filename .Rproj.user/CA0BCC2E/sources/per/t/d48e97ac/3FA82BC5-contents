library(dplyr)
library(reshape2)



#read in data
rawData <- read_excel("AACG_BAL_trimmed.xlsx")

#trim " 1: " from the values in Tier 3 column, set the column as numeric values
rawData$`Tier 3` <- as.numeric(as.character(substring(rawData$`Tier 3`, 3)))




#subjects missing baselines


baseline <- filter(rawData, Visit == input$baselineVisits)

allSubjects <- distinct(rawData, Subject)

missingSubjects <- left_join(allSubjects, baseline, by = "Subject")
missingSubjects <- filter(missingSubjects, is.na(Visit))
missingSubjects <- subset(missingSubjects, select = "Subject")


firstInst <- rawData[match(unique(rawData$Subject), rawData$Subject),]
missingSubjects <- filter(firstInst, Visit != "BL/V2")









#pull unique Subject values and unique visit values
rawData <- group_by(rawData, Subject, Visit)

#prepare Tier.3 values to be populated in the table
rawData <- summarise(rawData,
                     sumTiter = sum(rawData$Tier.3))

#transform the rawData table to a pivot table, using Subjects as rows, Visits as columns, Tier.3 values as cells
rawDataTrans <- dcast(rawData, Subject ~ Visit, value.var = "sumTiter")

#R DOES NOT LIKE FORWARD SLASHES IN COLUMN HEADERS, MUST BE RENAMED
colnames(rawDataTrans)[colnames(rawDataTrans)==input$baselineVisits] <- "Baseline"

#set NA values to 0
rawDataTrans[is.na(rawDataTrans)] <- 0

#rename and drop the NA column
colnames(rawDataTrans)[colnames(rawDataTrans)=="NA"] <- "Blank"
rawDataTrans$Blank <- NULL

mrd <- input$mrdIn #takes in user input MRD value

maxTiter <- apply(rawDataTrans[,-1], 1, max)

bindedTiter <- cbind(maxTiter, rawDataTrans)

negBaseTE <- filter(bindedTiter, Baseline == 0 & maxTiter >= 2*mrd)

posBaseTE <- filter(bindedTiter, Baseline != 0 & bindedTiter$maxTiter >= 4*bindedTiter$Baseline)

emerTable <- rbind(negBaseTE, posBaseTE)