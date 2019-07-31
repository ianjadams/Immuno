library(dplyr)
library(DT)
library(ggplot2)
library(readxl)
library(reshape2)
library(shiny)
library(shinythemes)
library(xlsx)



#begin Shiny UI interface
ui<- shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Upload Vendor Data for Processing"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Load a Dataset:',
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv')),
      
      #input: text input for specifying values
      textInput("baselineVisits", label = "Complete the Fields Below:", placeholder = "Name of 'Baseline Visit' value"),
      textInput("t1D", label = NULL, placeholder = "Name of 'Tier 1 Detected' value"),
      textInput("t1ND", label = NULL, placeholder = "Name of 'Tier 1 NOT Detected' value"),
      textInput("t2D", label = NULL, placeholder = "Name of 'Tier 2 Detected' value"),
      textInput("t2ND", label = NULL, placeholder = "Name of 'Tier 2 NOT Detected' value"),
      textInput("t4D", label = NULL, placeholder = "Name of 'Tier 4 Detected' value"),
      textInput("t4ND", label = NULL, placeholder = "Name of 'Tier 4 NOT Detected' value"),
      
      #input: checkboxes for Tier 4 column
      checkboxGroupInput("checkT4", "If applicable, include Tier 4 column:",
                         c("No Tier 4" = "noT4",
                           "Tier 4" = "T4"),
                         selected = "noT4", inline = TRUE),
      
      #input: MRD value field
      numericInput("mrdIn", "Enter Minimum Required Dilution:", 10, min = 1, max = 100000000),
      verbatimTextOutput('mrdOut'),
      
      
      #input: select input for changing current view of the table
      selectInput("dropdown", "Select a View",
                  choices = c(Original = "original",
                              "Baselines" = "baseline",
                              "Baseline Positives" = "bp",
                              "Unevaluated Subjects" = "uneval",
                              "Subject Pivot Table" = "subjects",
                              "Treatment Emergent Pivot Table" = "te",
                              "Titer Pivot Table" = "titercounts"),
                  selected = "original"),
      
      
      #download button
      downloadButton("downloadData", "Download All Tables")
      
    ),
    mainPanel(
      
      #output: separate the page into specific tabs
      tabsetPanel(type = "tabs",
                  tabPanel("Table", varSelectInput("col", "Reorganize Visit Codes:", character(0), multiple = TRUE),
                           DT::dataTableOutput('contents')),
                  tabPanel("Flags", DT::dataTableOutput('flag')),
                  tabPanel("Plot", plotOutput('plot'),
                           verbatimTextOutput('sampleSize')),
                  tabPanel("Summary", DT::dataTableOutput('summary1'),
                           br(),
                           DT::dataTableOutput('summary2'),
                           br(),
                           DT::dataTableOutput('summary3'))
      )
    )
  )
)
)
#end Shiny UI interface



#begin Shiny server function
server <- function(input, output, session){
  
  
  #create reactive table on data load
  #change Tier3 datatype to numeric and trim off "1:" from all values
  #insert 0 into all empty (NA) fields in Tier3
  #update data to change user input BL value to "Baseline"
  myData <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL) 
    initialData <- data.frame(read_excel(inFile$datapath))
    initialData$Tier3 <- as.numeric(as.character(substring(initialData$Tier3, 3)))
    initialData$Tier3[is.na(initialData$Tier3)] <- 0
    initialData[initialData==input$baselineVisits] <- "Baseline"
    initialData
    
  })
  
  
  
  #begin global functions for table calculations and views
  
  #function: baseline visits
  baselineFunc <- function() {
    
    baseline <- filter(myData(), Visit == "Baseline")
    return(baseline)
    
  }
  
  
  
  #function: subjects who are Tier2 positive at baseline
  bpFunc <- function() {
    
    bp <- filter(myData(), Visit == "Baseline" & Tier2 == input$t2D)
    
    if (dim(bp)[1] == 0) {
      
      bp <- data.frame("EMPTY")
      names(bp) <- "Subject"
      return(bp)
      
    } else {
      
      return(bp)
      
    }
    
  }
  
  
  
  #function: create pivot table for subjects across all visits
  pivotTableFunc <- function() {
    
    #pull unique Subject values and unique visit values
    groupRawData <- group_by(myData(), Subject, Visit)
    
    #prepare Tier3 values to be populated in the table
    summRawData <- summarise(groupRawData,
                             sumTiter = sum(Tier3))
    
    #transform the rawData table to a pivot table, using Subjects as rows, Visits as columns, Tier3 values as cells
    rawDataTrans <- dcast(summRawData, Subject ~ Visit, value.var = "sumTiter")
    
    #gets each unique visit code and closely reorganizes them in chronological order 
    nams <- as.character(unique(myData()$Visit)) 
    nums <- sapply(nams, function(nm) which(names(rawDataTrans) %in% nm)) 
    rawDataTrans[, sort(nums)] <- rawDataTrans[, nams] 
    names(rawDataTrans)[sort(nums)] <- nams
    
    #get max titer of each subject after baseline
    maxTiter <- apply(rawDataTrans[-c(1:2)], 1, max, na.rm=TRUE)
    
    #append maxTiter to first column of pivot table
    bindedTiter <- cbind(maxTiter, rawDataTrans)
    
  }
  
  
  
  #function: creates treatment emergent table
  treatEmerFunc <- function() {
    
    treatData <- pivotTableFunc()
    
    #takes in user input MRD value
    mrd <- input$mrdIn 
    
    #creates table of treatment induced subjects
    negBaseTE <- filter(treatData, Baseline == 0 & maxTiter >= 2*mrd)
    
    #creates table of treatment boosted subjects
    posBaseTE <- filter(treatData, Baseline != 0 & treatData$maxTiter >= 4*treatData$Baseline)
    
    #combined table of treatment induced/boosted subjects
    emerTable <- rbind(negBaseTE, posBaseTE)
    
    if (dim(emerTable)[1] == 0) {
      
      emerTable <- data.frame("EMPTY")
      names(emerTable) <- "Subject"
      return(emerTable)
      
    } else {
      
      return(emerTable)
      
    }
    
  }
  
  
  
  #function: creates titer pivot table
  titerPivot <- function() {
    
    #remove missing baseline subjects and subjects without visits after baseline
    #only evaluable subjects are included in this table
    bpSubs <- filter(pivotTableFunc(), !(is.na(Baseline)), maxTiter != "-Inf")
    
  } 
  
  
  
  #gets the unique visit codes once the original dataset has been loaded
  observeEvent(myData(), {
    updateSelectInput(session, "col", choices = names(pivotTableFunc()))
  })
  
  
  #create new titer pivot table with reorganized visit codes from user input
  pivTableView <- function() {
    
    #if user does not reorganize any columns, return the original pivot table
    if (length(input$col) == 0) return(pivotTableFunc())
    #arranges the table based on order user selects visit codes
    newPivTable <- pivotTableFunc() %>% dplyr::select(!!!input$col)
    return(newPivTable)
    
  }
  
  
  
  #create new treatment emergence table with reorganized visit codes from user input
  pivTreatView <- function() {
    
    #if user does not reorganize any columns, return the original pivot table
    if (length(input$col) == 0) return(treatEmerFunc())
    #arranges the table based on order user selects visit codes
    newTreatTable <- treatEmerFunc() %>% dplyr::select(!!!input$col)
    return(newTreatTable)
    
  }
  
  
  
  #function: subjects who are unevaluable
  unEvalFunc <- function () {
    
    #function: subjects who are missing baseline visits
    noBL <- function() {
      
      #find subjects who missed baseline
      baseline <- filter(myData(), Visit == "Baseline")
  
      allSubjects <- distinct(myData(), Subject)
  
      missingSubjects <- left_join(allSubjects, baseline, by = "Subject")
      missingSubjects <- filter(missingSubjects, is.na(Visit))
      missingSubjects <- subset(missingSubjects, select = "Subject")
      
      #create column for missing info
      if (dim(missingSubjects)[1] > 0) {
  
        missingSubjects$Premise <- "Missing Baseline"
        return(missingSubjects)
      
        #populate empty table
      } else if (dim(missingSubjects)[1] == 0) {
  
        placeHolder <- data.frame("No Missing Baselines")
        names(placeHolder) <- "Subject"
        return(placeHolder)
  
      }
    
   }
    
  
  
    
    #function: subjects who have no post-BL visits
    noPostBL <- function() {
    
      #find subjects who missed post-baseline visits
      subsNoPostBL <- filter(pivotTableFunc(), maxTiter == "-Inf")
      subsNoPostBL <- subset(subsNoPostBL, select = "Subject")
      
      #create column for missing info
      if (dim(subsNoPostBL)[1] > 0) {
        
        subsNoPostBL$Premise <- "Baseline w/o Follow-Up Visits"
        return(subsNoPostBL)
        
        #populate empty table
      } else if (dim(subsNoPostBL)[1] == 0) {
        
        placeHolder <- data.frame("All Subjects w/Post-BL Follow-Up Visits")
        names(placeHolder) <- "Subject"
        return(placeHolder)
        
      }
    
   }
    
    unEvalTable <- rbind(noBL(), noPostBL())
    return(unEvalTable)
  }
  #end global functions for table views
  
  
  
  #begin "MRD" input field
  output$mrdOut <- renderText({ 
    
    paste("MRD set at 1:", input$mrdIn, sep="")
    
  })
  #end "MRD" input field
  
  
  
  #begin "Table" tab
  output$contents <- DT::renderDataTable({
    
    
    
    #SHOW ORIGINAL TABLE
    if(input$dropdown == "original") {
      
      return(myData())
      
    }
    #SHOW BASELINE VISITS TABLE
    else if(input$dropdown == "baseline") {
      
      return(baselineFunc())
      
    }
    #SHOW UNEVALUATED SUBJECTS TABLE
    else if(input$dropdown == "uneval") {
      
      return(unEvalFunc())
      
    }
    #SHOW BASELINE POSITIVE VISITS TABLE
    else if(input$dropdown == "bp") {
      
      return(bpFunc())
      
    }
    #SHOW TITERS BY SUBJECT TABLE
    else if(input$dropdown == "subjects") {
      
      bindedTiter <- pivTableView()
      
      #set NA and "-Inf" values to "-" for visibility
      bindedTiter[is.na(bindedTiter)] <- "-"
      bindedTiter[bindedTiter == "-Inf"] <- "-"
      
      return(bindedTiter)
      
    } 
    #SHOW TREATMENT EMERGENT TABLE
    else if(input$dropdown == "te") {
      
      emerTable <- pivTreatView()
      
      #set NA values to "-" for visibility
      emerTable[is.na(emerTable)] <- "-"
      
      return(emerTable)
      
    }
    #SHOW TITER COUNTS PIVOT TABLE
    else if(input$dropdown == "titercounts") {
      
      bpSubs <- titerPivot()
      
      bpSubs <- data.frame(bpSubs$Baseline, bpSubs$maxTiter)
      
      #create pivot table for Baseline and maxTiter columns, append Sum of rows and columns to the table
      titerPiv <- as.data.frame.matrix(addmargins(table(bpSubs[,1], bpSubs[,2])))
      titerPiv <- cbind("Baseline Titer" = rownames(titerPiv), titerPiv)
      return(titerPiv)
      
    }
    
    
    
  }, rownames = FALSE)
  #end "Table" tab
  
  
  
  #begin "Flags" tab
  output$flag <- DT::renderDataTable({ 
    
    #create column "Premise"
    allFlags <- myData()
    allFlags$Premise <- "exp"
    
    
    
    #begin logical QC checks
    QC1 <- subset(allFlags, Tier1 != input$t1D & Tier1 != input$t1ND & Tier1 != "N/A" & Tier1 != "NA")
    QC2 <- subset(allFlags, Tier1 == input$t1ND & Tier2 == input$t2D)
    QC3 <- subset(allFlags, Tier1 == input$t1ND & Tier3 != 0)

    QC4 <- subset(allFlags, Tier2 != input$t2D & Tier2 != input$t2ND & Tier2 != "N/A" & Tier2 != "NA")
    QC5 <- subset(allFlags, Tier2 == input$t2ND & Tier3 != 0)
    QC6 <- subset(allFlags, Tier2 == input$t2D & Tier3 == 0)
    
    QC7 <- allFlags[duplicated(allFlags[, c("Subject", "Visit")]), ]
    #end logical QC checks

    
    
    #begin populating Premise column with reasoning for each error in the table
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
      QC7$Premise <- "Duplicate Visit for Subject"
    }, silent = TRUE)
    
    
    
    #combine all rows that have any of the errors above
    errorTable <- try(rbind(QC1, QC2, QC3, QC4, QC5, QC6, QC7))
    
  }, rownames = FALSE)
  #end "Flags" tab
  
  
  
  #begin "Plot" tab
  output$plot <- renderPlot({
    
    #use titer pivot table to create frequency table of each unique titer AND drop titer value of zero or "-Inf" (AKA no maxTiter)
    countTiter <- as.data.frame(table(pivotTableFunc()$maxTiter))
    names(countTiter) <- c("Titer", "Count")
    countTiter <- countTiter[!(countTiter$Titer == 0 | countTiter$Titer == "-Inf"), ]
    
    #plot titer counts
    
    plot1 <- ggplot(countTiter, aes(x = Titer, y = Count)) + 
      geom_bar(stat = "identity", color = "#337ab7", size = 0.6, fill = "#18bc9c", alpha = 0.7) + 
      geom_text(aes(label = Count), vjust = -0.3, color = "#2c3e50", size = 4.5) + 
      ggtitle("Frequency of Highest Titer (Post-BL) per Subject*") + 
      theme_minimal()
    
    
    plot1 + theme(
      plot.title = element_text(color = "#2c3e50", size = 24, face = "bold"),
      axis.title.x = element_text(color = "#2c3e50", size = 20, face = "bold"),
      axis.title.y = element_text(color = "#2c3e50", size = 20, face = "bold"),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      axis.line = element_line(color = "#337ab7", size = 1, linetype = "solid"),
      panel.background = element_rect(fill = "#cccccc", color = "#cccccc"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
    
  })
  
  
  
  #begin "sample size" output field
  output$sampleSize <- renderText({
    
    plotCount <- filter(pivotTableFunc(), maxTiter != 0, maxTiter != "-Inf")
    
    paste("n = ", nrow(plotCount), "    *may include subjects who had titers after missing baseline", sep="")
    
  })
  #end "sample size" output field
  #end "Plot" tab
  
  
  
  #begin "Summary" tab
  
  #begin Tier table results
  output$summary1 <- DT::renderDataTable({
    
    statsData <- myData()
    
    # num of samples
    allSamples <- nrow(subset(statsData, Tier1 == input$t1D | Tier1 == input$t1ND))
    
    
    
    # num of Tier 1 detected samples
    t1Pos <- nrow(subset(statsData, Tier1 == input$t1D))
    
    # putative positive rate (num of Tier 1 detected samples / total samples)
    putPR <- (t1Pos/allSamples) * 100
    putPR <- round(putPR, 2)
    
    
    
    # num of Tier 2 samples tested
    t2Tested <- nrow(subset(statsData, Tier2 == input$t2D | Tier2 == input$t2ND))
    
    # num of Tier 2 detected samples
    t2Pos <- nrow(subset(statsData, Tier2 == input$t2D))
    
    # confirmed positive rate (num of Tier 2 detected samples / num of Tier 1 detected samples)
    conPR <- (t2Pos/t1Pos) * 100
    conPR <- round(conPR, 2)
    
    
    
    if(input$checkT4 == "noT4") {
      
      # output table for Tier 1 and Tier 2 results
      data.frame("SamplesTested" = c(allSamples, t2Tested),
                 "Detected" = c(t1Pos, t2Pos),
                 "PostiveRate" = c(putPR, conPR),
                 row.names = c("Tier 1", "Tier 2"))
      
    }
    
    else if(input$checkT4 == "T4") {
      
      # num of Tier 4 samples tested
      t4Tested <- nrow(subset(statsData, Tier4 == input$t4D | Tier4 == input$t4ND))
      
      # num of Tier 4 detected samples
      t4Pos <- nrow(subset(statsData, Tier4 == input$t4D))
      
      # positive rate (num of Tier 4 detected samples / num of Tier 2 detected samples)
      t4PR <- (t4Pos/t2Pos) * 100
      t4PR <- round(t4PR, 2)
      
      # output table for Tier 1, Tier 2, and Tier 4 results
      data.frame("SamplesTested" = c(allSamples, t2Tested, t4Tested),
                 "Detected" = c(t1Pos, t2Pos, t4Pos),
                 "PostiveRate" = c(putPR, conPR, t4PR),
                 row.names = c("Tier 1", "Tier 2", "Tier 4"))
      
    } 
    
    
    
  }, options = list(dom = 't', ordering = FALSE)
  ) 
  #end Tier table results
  
  
  
  #begin Treatment Emergent table
  output$summary2 <- DT::renderDataTable({
    
    # num of evaluable subjects in dataset
    numEvalSubjects <- nrow(titerPivot())
    
    # percent of subjects evaluable for TE ADA
    baseRate <- (numEvalSubjects/numEvalSubjects) * 100
    baseRate <- round(baseRate, 2)
    
    
    
    # num of subjects with positive baselines
    if(bpFunc()$Subject=="EMPTY") {
      
      numBLPosSubjects <- 0
      
    } else {
      
      numBLPosSubjects <- nrow(bpFunc())
      
    }
    
    # percent of subjects positive at baseline
    basePosRate <- (numBLPosSubjects/numEvalSubjects) * 100
    basePosRate <- round(basePosRate, 2)
    
    
    
    # num of treatment emergent subjects
    if(treatEmerFunc()$Subject=="EMPTY") {
      
      numTESubjects <- 0
      
    } else {
      
      numTESubjects <- nrow(treatEmerFunc())
      
    }
    
    # percent of subjects that are treatment emergent
    teRate <- (numTESubjects/numEvalSubjects) * 100
    teRate <- round(teRate, 2)
    
    
    
    # num of treatment induced subjects
    numTISubjects <- nrow(subset(treatEmerFunc(), Baseline == 0))
    
    # percent of subjects that are treatment induced
    tiRate <- (numTISubjects/numEvalSubjects) * 100
    tiRate <- round(tiRate, 2)
    
    
    
    # num of treatment boosted subjects
    numTBSubjects <- nrow(subset(treatEmerFunc(), Baseline != 0))
    
    # percent of subjects that are treatment boosted
    tbRate <- (numTBSubjects/numEvalSubjects) * 100
    tbRate <- round(tbRate, 2)
    
    
    
    # output table for Treatment Emergence results
    data.frame("Count" = c(numEvalSubjects, numBLPosSubjects, numTESubjects, numTISubjects, numTBSubjects),
               "Rate" = c(baseRate, basePosRate, teRate, tiRate, tbRate),
               row.names = c("Subjects Evaluable for TE ADA", "Evaluable Subs with ADA Present at Baseline",
                             "Subjects TE ADA", "Treatment-Induced", "Treatment-Boosted"))
    
    
    
  }, options = list(dom = 't', ordering = FALSE)
  )
  #end Treatment Emergent table
  
  
  
  #begin General Stats table
  output$summary3 <- DT::renderDataTable({
    
    # num of unique subjects in dataset
    numAllSubjects <- nrow(pivotTableFunc())
    
    # num of unevaluable subjects in dataset
    numUnEvalSubjects <- nrow(pivotTableFunc()) - nrow(titerPivot())
    
    # max titer of treatment emergence table
    highTiter <- max(treatEmerFunc()$maxTiter)
    
    
    
    # output table for General Stats results
    data.frame("Sort" = c(numAllSubjects, numUnEvalSubjects, highTiter),
               row.names = c("Total Unique Subjects", "Unevaluated Subjects", "Highest Titer"))
    
    
    
  }, options = list(dom = 't')
  )
  #end General Stats table
  #end "Summary" tab
  
  
  
  #begin "Download All Tables" button
  output$downloadData <- downloadHandler(
    
    #define file name and dump all rows from tables into separate excel sheets
    'appDownloadData.xlsx', content = function(file) {
      these = input$contents_rows_all
      
      write.xlsx(myData(), file, sheetName="Original", row.names=FALSE, showNA = FALSE)
      write.xlsx(baselineFunc(), file, sheetName="Baselines", append=TRUE, row.names=FALSE, showNA = FALSE)
      write.xlsx(unEvalFunc(), file, sheetName="Unevaluated Subjects", append=TRUE, row.names=FALSE, showNA = FALSE)
      write.xlsx(bpFunc(), file, sheetName="Baseline Positives", append=TRUE, row.names=FALSE, showNA = FALSE)
      write.xlsx(pivTableView(), file, sheetName="Subject Pivot Table", append=TRUE, row.names=FALSE, showNA = FALSE)
      write.xlsx(pivTreatView(), file, sheetName="Treatment Emergent Pivot Table", append=TRUE, row.names=FALSE, showNA = FALSE)
      write.xlsx(titerPivot(), file, sheetName="Titer Pivot Table", append=TRUE, row.names=FALSE, showNA = FALSE)
      write.xlsx(myData()[these, , drop = FALSE], file, sheetName="Search Results", append=TRUE, row.names=FALSE, showNA = FALSE)
      
      
    })
  #end "Download All Tables" button
  
  
  
}
#end Shiny server function

shinyApp(ui,server)


