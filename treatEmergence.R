library(dplyr)
library(reshape2)



#read in data
rawData <- read_excel("AMAC_PBI_trimmed.xlsx")

#trim " 1: " from the values in Tier 3 column, set the column as numeric values
rawData$Tier3 <- as.numeric(as.character(substring(rawData$Tier3, 3)))

#set NA values to 0
rawData$Tier3[is.na(rawData$Tier3)] <- 0






######################
#MISSING BASELINES
######################



baseline <- filter(rawData, Visit == "BL/V2")

allSubjects <- distinct(rawData, Subject)

missingSubjects <- left_join(allSubjects, baseline, by = "Subject")
missingSubjects <- filter(missingSubjects, is.na(Visit))
missingSubjects <- subset(missingSubjects, select = "Subject")


firstInst <- rawData[match(unique(rawData$Subject), rawData$Subject),]
missingSubjects <- filter(firstInst, Visit != "BL/V2")









#########################
#TITER PIVOT TABLE
#########################




#pull unique Subject values and unique visit values
groupRawData <- group_by(rawData, Subject, Visit)

#prepare Tier.3 values to be populated in the table
summRawData <- summarise(groupRawData,
                         sumTiter = sum(Tier3))

#transform the rawData table to a pivot table, using Subjects as rows, Visits as columns, Tier.3 values as cells
rawDataTrans <- dcast(summRawData, Subject ~ Visit, value.var = "sumTiter")


#chronologically reorder columns by visit code
#get unique list of "Visit" codes and reorder the pivot table columns 
nams <- as.character(unique(rawData$Visit)) 
nums <- sapply(nams, function(nm) which(names(rawDataTrans) %in% nm)) 
rawDataTrans[, sort(nums)] <- rawDataTrans[, nams] 
names(rawDataTrans)[sort(nums)] <- nams



#R DOES NOT LIKE FORWARD SLASHES IN COLUMN HEADERS, MUST BE RENAMED
colnames(rawDataTrans)[colnames(rawDataTrans) == "BL/V2"] <- "Baseline"





#########################
#TREATMENT EMERGENCE
#########################



mrd <- 10 #takes in user input MRD value

maxTiter <- apply(rawDataTrans[,-1], 1, max, na.rm=TRUE)

bindedTiter <- cbind(maxTiter, rawDataTrans)

negBaseTE <- filter(bindedTiter, Baseline == 0 & maxTiter >= 2*mrd)

posBaseTE <- filter(bindedTiter, Baseline != 0 & bindedTiter$maxTiter >= 4*bindedTiter$Baseline)

emerTable <- rbind(negBaseTE, posBaseTE)

emerTable[is.na(emerTable)] <- "-"
return(emerTable)


