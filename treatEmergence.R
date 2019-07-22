library(dplyr)
library(DT)
library(ggplot2)
library(readxl)
library(reshape2)



#########################
#LOAD AND PREPROCESS DATA
#########################



#read in data
rawData <- read_excel("AMAC_PBI_trimmed.xlsx")

#trim " 1: " from the values in Tier 3 column, set the column as numeric values
rawData$Tier3 <- as.numeric(as.character(substring(rawData$Tier3, 3)))

#set NA values to 0
rawData$Tier3[is.na(rawData$Tier3)] <- 0

rawData[rawData=="BL/V2"] <- "Baseline"



##################
#MISSING BASELINES
##################



baseline <- filter(rawData, Visit == "Baseline")

allSubjects <- distinct(rawData, Subject)

missingSubjects <- left_join(allSubjects, baseline, by = "Subject")
missingSubjects <- filter(missingSubjects, is.na(Visit))
missingSubjects <- subset(missingSubjects, select = "Subject")

if (dim(missingSubjects)[1] == 0) {
  
  placeHolder <- data.frame("EMPTY")
  names(placeHolder) <- "Subject"
  return(placeHolder)
  
}



##################
#TITER PIVOT TABLE
##################



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



####################
#TREATMENT EMERGENCE
####################



#takes in user input MRD value
mrd <- 10

maxTiter <- apply(rawDataTrans[-c(1:2)], 1, max, na.rm=TRUE)

bindedTiter <- cbind(maxTiter, rawDataTrans)

negBaseTE <- filter(bindedTiter, Baseline == 0 & maxTiter >= 2*mrd)

posBaseTE <- filter(bindedTiter, Baseline != 0 & bindedTiter$maxTiter >= 4*bindedTiter$Baseline)

emerTable <- rbind(negBaseTE, posBaseTE)

emerTable[is.na(emerTable)] <- "-"



###################
#TITER COUNTS TABLE
###################



bpSubs <- filter(bindedTiter, !(is.na(Baseline)), maxTiter != "-Inf")

bpSubs <- data.frame(bpSubs$Baseline, bpSubs$maxTiter)

titerPiv <- as.data.frame.matrix(addmargins(table(bpSubs[,1], bpSubs[,2])))
titerPiv <- cbind("Baseline Titer" = rownames(titerPiv), titerPiv)



################
#TITER HISTOGRAM
################



countTiter <- as.data.frame(table(bindedTiter$maxTiter))
names(countTiter) <- c("Titer", "Count")
countTiter <- countTiter[!(countTiter$Titer == 0 | countTiter$Titer == "-Inf"), ]

plot1 <- ggplot(countTiter, aes(x = Titer, y = Count)) + 
  geom_bar(stat = "identity", color = "#337ab7", size = 0.6, fill = "#18bc9c", alpha = 0.7) + 
  geom_text(aes(label = Count), vjust = -0.3, color = "#2c3e50", size = 4.5) + 
  ggtitle("Frequency of Titers") + 
  theme_minimal()


plot1 + theme(
  plot.title = element_text(color = "#2c3e50", size = 24, face = "bold"),
  axis.title.x = element_text(color = "#2c3e50", size = 18, face = "bold"),
  axis.title.y = element_text(color = "#2c3e50", size = 18, face = "bold"),
  axis.text.x = element_text(size = 12),
  axis.text.y = element_text(size = 12),
  axis.line = element_line(color = "#337ab7", size = 1, linetype = "solid"),
  panel.background = element_rect(fill = "#cccccc", color = "#cccccc"),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank()
)



##########
#QC CHECKS
##########



QC1 <- subset(rawData, Tier1 != "NOTDETEC" & Tier1 != "DNR")
QC2 <- subset(rawData, Tier1 == "NOTDETEC" & Tier2 == "DETECTED")
QC3 <- subset(rawData, Tier1 == "NOTDETEC" & Tier3 != 0)

QC4 <- subset(rawData, Tier2 != "NOTDETEC" & Tier2 != "DETECTED")
QC5 <- subset(rawData, Tier2 == "NOTDETEC" & Tier3 != 0)
QC6 <- subset(rawData, Tier2 == "DETECTED" & Tier3 == 0)

errorTable <- rbind(QC1, QC2, QC3, QC4, QC5, QC6)


