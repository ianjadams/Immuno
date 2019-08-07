library(dplyr)
library(DT)
library(ggplot2)
library(readxl)
library(reshape2)
library(shiny)



#########################
#LOAD AND PREPROCESS DATA
#########################



#read in data
rawData <- read_excel("CGAM_PBI_trimmed.xlsx")

#trim " 1: " from the values in Tier 3 column, set the column as numeric values
rawData$Tier3 <- as.numeric(as.character(substring(rawData$Tier3, 3)))

#set NA values to 0
rawData$Tier3[is.na(rawData$Tier3)] <- 0

rawData[rawData=="V2 BL"] <- "Baseline"



##################
#MISSING BASELINES
##################

unevalFunc <- function() {
  
  noBL <- function() {
    
    baseline <- filter(rawData, Visit == "Baseline")
    
    allSubjects <- distinct(rawData, Subject)
    
    missingSubjects <- left_join(allSubjects, baseline, by = "Subject")
    missingSubjects <- filter(missingSubjects, is.na(Visit))
    missingSubjects <- subset(missingSubjects, select = "Subject")
    
    if (dim(missingSubjects)[1] > 0) {
      
      missingSubjects$Premise <- "Baseline"
      return(missingSubjects)
      
    } else if (dim(missingSubjects)[1] == 0) {
      
      placeHolder <- data.frame("Subject" = "EMPTY", "Premise" = "Missing Baseline")
      return(placeHolder)
      
    }
  }
  
  noPostBL <- function() {
    
    subsNoPostBL <- filter(bindedTiter, maxTiter == "-Inf")
    subsNoPostBL <- subset(subsNoPostBL, select = "Subject")
    
    if (dim(subsNoPostBL)[1] > 0) {
      
      subsNoPostBL$Premise <- "Post-Baseline Visit"
      return(subsNoPostBL)
      
    } else if (dim(subsNoPostBL)[1] == 0) {
      
      placeHolder <- data.frame("Subject" = "EMPTY", "Premise" = "Baseline w/o All Follow-Up Visits")
      return(placeHolder)
      
    }
  }
  
  unEvalTable <- rbind(noBL(), noPostBL())
  return(unEvalTable)
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



rawData$Premise <- "exp"

QC1 <- subset(rawData, Tier1 != "Not Detected" & Tier1 != "DNR")
QC2 <- subset(rawData, Tier1 == "Not Detected" & Tier2 == "Detected")
QC3 <- subset(rawData, Tier1 == "Not Detected" & Tier3 != 0)

QC4 <- subset(rawData, Tier2 != "Not Detected" & Tier2 != "Detected")
QC5 <- subset(rawData, Tier2 == "Not Detected" & Tier3 != 0)
QC6 <- subset(rawData, Tier2 == "Detected" & Tier3 == 0)

QC7 <- try(if("Tier4" %in% colnames(rawData)) {
  
  subset(rawData, Tier2 == "Not Detected" & Tier4 == "DETECTED")
  
})

QC8 <- try(if("Tier4" %in% colnames(rawData)) {
  
  subset(rawData, Tier4 != "DETECTED" & Tier4 != "NOTDETEC")
  
})

QC9 <- rawData[duplicated(rawData[, c("Subject", "Visit")]),]



try(if(QC1$Premise == "exp") {
  QC1$Premise <- "T1 Discrepant Value"
}, silent = TRUE)

try(if(QC2$Premise == "exp") {
  QC2$Premise <- "T1(-) with T2(+)"
}, silent = TRUE)

try(if(QC3$Premise == "exp") {
  QC3$Premise <- "T1(-) with T3(+)"
}, silent = TRUE)

try(if(QC4$Premise == "exp") {
  QC4$Premise <- "T2 Discrepant Value"
}, silent = TRUE)

try(if(QC5$Premise == "exp") {
  QC5$Premise <- "T2(-) with T3(+)"
}, silent = TRUE)

try(if(QC6$Premise == "exp") {
  QC6$Premise <- "T2(+) with T3(-)"
}, silent = TRUE)

try(if(QC7$Premise == "exp") {
  QC7$Premise <- "T2(-) with T4(+)"
}, silent = TRUE)

try(if(QC8$Premise == "exp") {
  QC8$Premise <- "T4 Discrepant Value"
}, silent = TRUE)

try(if(QC9$Premise == "exp") {
  QC9$Premise <- "Duplicate Visit for Subject"
}, silent = TRUE)



errortable <- try(rbind(QC1, QC2, QC3, QC4, QC5, QC6, QC7, QC8, QC9))

listErrors <- distinct(errortable, Premise)

output <- c("The following QC checks appear in the table: ", listErrors)



############
#STATS TABLE
############



# num of samples
allSamples <- nrow(subset(rawData, Tier1 == "Detected" | Tier1 == "Not Detected"))

# num of Tier 1 detected samples
t1Pos <- nrow(subset(rawData, Tier1 == "Detected"))

# putative positive rate (num of Tier 1 detected samples / total samples)
putPR <- (t1Pos/allSamples) * 100
putPR <- round(putPR, 2)

# num of Tier 2 samples tested
t2Tested <- nrow(subset(rawData, Tier2 == "Detected" | Tier2 == "Not Detected"))

# num of Tier 2 detected samples
t2Pos <- nrow(subset(rawData, Tier2 == "Detected"))

# confirmed positive rate (num of Tier 2 detected samples / num of Tier 1 detected samples)
conPR <- (t2Pos/t1Pos) * 100
conPR <- round(conPR, 2)

tierTable <- data.frame("SamplesTested" = c(allSamples, t2Tested),
                        "Detected" = c(t1Pos, t2Pos),
                        "PostiveRate" = c(putPR, conPR),
                        row.names = c("Tier 1", "Tier 2"))



if("Tier2b" %in% colnames(rawData)) {
  
  cat("Yep, it's in there!\n");
  
  # num of Tier 2 samples tested
  t2bTested <- nrow(subset(rawData, Tier2b == "Detected" | Tier2b == "Not Detected"))
  
  # num of Tier 2 detected samples
  t2bPos <- nrow(subset(rawData, Tier2b == "Detected"))
  
  # confirmed positive rate (num of Tier 2 detected samples / num of Tier 1 detected samples)
  t2bPR <- (t2bPos/t2Pos) * 100
  t2bPR <- round(t2bPR, 2)
  
  # tier2bDataTable <- data.frame("SamplesTested" = (t2bTested),
  #                               "Detected" = (t2bPos),
  #                               "PostiveRate" = (t2bPR),
  #                               row.names = ("Tier 2b"))
  
  tierTable[nrow(tierTable) + 1,] = list(t2bTested, t2bPos, t2bPR)
  
  
  
}

if ("Tier2c" %in% colnames(rawData)) {
  
  cat("Yep, it's in there!\n");
  
  # num of Tier 2 samples tested
  t2cTested <- nrow(subset(rawData, Tier2c == "Detected" | Tier2c == "Not Detected"))
  
  # num of Tier 2 detected samples
  t2cPos <- nrow(subset(rawData, Tier2c == "Detected"))
  
  # confirmed positive rate (num of Tier 2 detected samples / num of Tier 1 detected samples)
  t2cPR <- (t2cPos/t2bPos) * 100
  t2cPR <- round(t2cPR, 2)
  
  tier2cDataTable <- data.frame("SamplesTested" = (t2cTested),
                                "Detected" = (t2cPos),
                                "PostiveRate" = (t2cPR),
                                row.names = ("Tier 2c"))
  
  tierTable[nrow(tierTable) + 1,] = list(t2cTested, t2cPos, t2cPR)
  
}

if("Tier2d" %in% colnames(rawData)) {
  
  cat("Yep, it's in there!\n");
  
  # num of Tier 2 samples tested
  t2dTested <- nrow(subset(rawData, Tier2d == "Detected" | Tier2d == "Not Detected"))
  
  # num of Tier 2 detected samples
  t2dPos <- nrow(subset(rawData, Tier2d == "Detected"))
  
  # confirmed positive rate (num of Tier 2 detected samples / num of Tier 1 detected samples)
  t2dPR <- (t2dPos/t2cPos) * 100
  t2dPR <- round(t2dPR, 2)
  
  tier2dDataTable <- data.frame("SamplesTested" = (t2dTested),
                                "Detected" = (t2dPos),
                                "PostiveRate" = (t2dPR),
                                row.names = ("Tier 2d"))
  
  tierTable[nrow(tierTable) + 1,] = list(t2dTested, t2dPos, t2dPR)
  
}

finalTierTable <- try(rbind(tier1_2a_DataTable, tier2bDataTable, tier2cDataTable, tier2dDataTable))


