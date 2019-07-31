library(dplyr)
library(DT)
library(readxl)
library(reshape2)
library(shiny)
library(shinythemes)
library(xlsx)



ui<- shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Upload Vendor Data for Processing"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Load a Dataset:',
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv')),
      
      # text inputs for specifying values
      textInput("baselineVisits", label = "Complete the Fields Below:", placeholder = "Name of 'Baseline Visit' value"),
      textInput("t1D", label = NULL, placeholder = "Name of 'Tier 1 Detected' value"),
      textInput("t2D", label = NULL, placeholder = "Name of 'Tier 2 Detected' value"),
      textInput("t1ND", label = NULL, placeholder = "Name of 'Tier 1 NOT Detected' value"),
      textInput("t2ND", label = NULL, placeholder = "Name of 'Tier 2 NOT Detected' value"),
      
      #uiOutput('select'),
      numericInput("mrdIn", "Enter Minimum Required Dilution:", 10, min = 1, max = 100000000),
      verbatimTextOutput('mrdOut'),
      
      
      # Input: Select method of filtering data ----
      selectInput("dropdown", "Select a Filter",
                  choices = c(None = "none",
                              "Baseline Visits" = "baseline",
                              "Missing Baseline Visits" = "nobaseline",
                              "Baseline Positive Visits" = "bp",
                              "Titers by Subject" = "subjects",
                              "Treatment Emergent Subjects" = "te"),
                  selected = "none"),
      
      
      # Button
      downloadButton("downloadData", "Download All Tables")
      
    ),
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Table", DT::dataTableOutput('contents')),
                  tabPanel("Plot", plotOutput('plot')),
                  tabPanel("Summary", DT::dataTableOutput('summary1'),
                           DT::dataTableOutput('summary2'))
      )
    )
  )
)
)


server <- function(input, output, session){
  
  myData <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL) 
    initialData <- data.frame(read_excel(inFile$datapath))
    initialData$Tier3 <- as.numeric(as.character(substring(initialData$Tier3, 3)))
    initialData$Tier3[is.na(initialData$Tier3)] <- 0
    initialData
    
  })
  
  
  
  #begin global functions for table views
  
  #creates table for baseline visits
  baselineFunc <- function() {
    
    baseline <- filter(myData(), Visit == input$baselineVisits)
    return(baseline)
    
  }
  
  
  
  #subjects missing baseline visits
  noBaselineFunc <- function() {
    
    rawData <- myData()
    
    baseline <- filter(rawData, Visit == input$baselineVisits)
    
    allSubjects <- distinct(rawData, Subject)
    
    missingSubjects <- left_join(allSubjects, baseline, by = "Subject")
    missingSubjects <- filter(missingSubjects, is.na(Visit))
    missingSubjects <- subset(missingSubjects, select = "Subject")
    
  }
  
  
  
  #creates table for baseline positive visits
  bpFunc <- function() {
    
    bp <- filter(myData(), Visit == input$baselineVisits & Tier2 == input$t2D)
    return(bp)
    
  }
  
  
  
  #begin pivot table for titers by subject
  pivotTableFunc <- function() {
    
    #pull unique Subject values and unique visit values
    groupRawData <- group_by(myData(), Subject, Visit)
    
    #prepare Tier.3 values to be populated in the table
    summRawData <- summarise(groupRawData,
                             sumTiter = sum(Tier3))
    
    #transform the rawData table to a pivot table, using Subjects as rows, Visits as columns, Tier.3 values as cells
    rawDataTrans <- dcast(summRawData, Subject ~ Visit, value.var = "sumTiter")
    
    #get unique list of "Visit" codes and reorder the pivot table columns into chronological order by visit 
    nams <- as.character(unique(myData()$Visit)) 
    nums <- sapply(nams, function(nm) which(names(rawDataTrans) %in% nm)) 
    rawDataTrans[, sort(nums)] <- rawDataTrans[, nams] 
    names(rawDataTrans)[sort(nums)] <- nams
    
    
    #R DOES NOT LIKE FORWARD SLASHES IN COLUMN HEADERS, MUST BE RENAMED
    colnames(rawDataTrans)[colnames(rawDataTrans)==input$baselineVisits] <- "Baseline"
    
    #set NA values to 0
    rawDataTrans[is.na(rawDataTrans)] <- "-"
    
    #rename and drop the NA column
    colnames(rawDataTrans)[colnames(rawDataTrans)=="NA"] <- "Blank"
    rawDataTrans$Blank <- NULL
    return(rawDataTrans)
    
  }
  
  
  
  #begin treatment emergent calculations
  treatEmerFunc <- function() {
    
    #pull unique Subject values and unique visit values
    groupRawData <- group_by(myData(), Subject, Visit)
    
    #prepare Tier.3 values to be populated in the table
    summRawData <- summarise(groupRawData,
                             sumTiter = sum(Tier3))
    
    #transform the rawData table to a pivot table, using Subjects as rows, Visits as columns, Tier.3 values as cells
    rawDataTrans <- dcast(summRawData, Subject ~ Visit, value.var = "sumTiter")
    
    
    #chronologically reorder columns by visit code
    #get unique list of "Visit" codes and reorder the pivot table columns 
    nams <- as.character(unique(myData()$Visit)) 
    nums <- sapply(nams, function(nm) which(names(rawDataTrans) %in% nm)) 
    rawDataTrans[, sort(nums)] <- rawDataTrans[, nams] 
    names(rawDataTrans)[sort(nums)] <- nams
    
    
    
    #R DOES NOT LIKE FORWARD SLASHES IN COLUMN HEADERS, MUST BE RENAMED
    colnames(rawDataTrans)[colnames(rawDataTrans)==input$baselineVisits] <- "Baseline"
    
    
    
    mrd <- input$mrdIn #takes in user input MRD value
    
    maxTiter <- apply(rawDataTrans[,-1], 1, max, na.rm=TRUE)
    
    bindedTiter <- cbind(maxTiter, rawDataTrans)
    
    negBaseTE <- filter(bindedTiter, Baseline == 0 & maxTiter >= 2*mrd)
    
    posBaseTE <- filter(bindedTiter, Baseline != 0 & bindedTiter$maxTiter >= 4*bindedTiter$Baseline)
    
    emerTable <- rbind(negBaseTE, posBaseTE)
    
    emerTable[is.na(emerTable)] <- "-"
    # emerTable <-data.frame(emerTable)
    return(emerTable)
    
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
    if(input$dropdown == "none") {
      
      return(myData())
      
    }
    #SHOW BASELINE VISITS TABLE
    else if(input$dropdown == "baseline") {
      
      return(baselineFunc())
      
    }
    #SHOW MISSING BASELINE VISITS TABLE
    else if(input$dropdown == "nobaseline") {
      
      return(noBaselineFunc())
      
    }
    #SHOW BASELINE POSITIVE VISITS TABLE
    else if(input$dropdown == "bp") {
      
      return(bpFunc())
      
    }
    #SHOW TITERS BY SUBJECT TABLE
    else if(input$dropdown == "subjects") {
      
      return(pivotTableFunc())
      
    } 
    #SHOW TREATMENT EMERGENT TABLE
    else if(input$dropdown == "te") {
      
      return(treatEmerFunc())
      
    }
    
    
    
  })
  #end "Table" tab
  
  
  
  output$plot <- renderPlot({
    
    # df <- myData()
    # df <- df[,input$variable]
    # hist(df)
    
    
  })
  
  
  
  #begin "Summary" tab
  
  #SHOW T1, T2, BASELINE TABLE
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
    
    
    
    # num of subjects with baseline visits
    baselines <- nrow(subset(statsData, Visit == input$baselineVisits))
    
    # num of subjects detected in Tier 2 at baseline visit
    basePos <- nrow(subset(statsData, Tier2 == input$t2D & Visit == input$baselineVisits))
    
    # baseline positive rate (num of baseline visits that are T2 positive / num of all baseline visits)
    basePR <- (basePos/baselines) * 100
    basePR <- round(basePR, 2)
    
    
    
    # output table for Tier 1, Tier 2, and Baseline results
    data.frame("SamplesTested" = c(allSamples, t2Tested, baselines),
               "Detected" = c(t1Pos, t2Pos, basePos),
               "PostiveRate" = c(putPR, conPR, basePR),
               row.names = c("Tier 1", "Tier 2", "Baseline"))
    
    
    
  })
  
  
  
  #SHOW TREATMENT EMERGENT RATE TABLE
  output$summary2 <- DT::renderDataTable({
    
    # max titer of treatment emergence table
    highTiter <- max(treatEmerFunc()$maxTiter)
    
    # num of treatment emergent subjects in dataset
    numTESubjects <- nrow(treatEmerFunc())
    
    # num of subjects in dataset
    numSubjects <- nrow(pivotTableFunc())
    
    # treatment emergence rate
    teRate <- (numTESubjects/numSubjects) * 100
    teRate <- round(teRate, 2)
    
    
    
    # output table for Treatment Emergence results
    data.frame("Sort" = c(highTiter, numTESubjects, numSubjects, teRate),
               row.names = c("Highest Titer", "Treatment Emergent Subjects", "Total Subjects", 
                             "Treatment Emergent Rate"))
    
    
    
  })
  #end "Summary" tab
  
  
  
  
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    
    'appDownloadData.xlsx', content = function(file) {
      these = input$contents_rows_all
      # write.xlsx(baselineFunc()[these, , drop = FALSE], file)
      
      write.xlsx(myData(), file, sheetName="Original", row.names=FALSE)
      write.xlsx(baselineFunc(), file, sheetName="Baselines", append=TRUE, row.names=FALSE)
      write.xlsx(noBaselineFunc(), file, sheetName="Missing Baseline", append=TRUE, row.names=FALSE)
      write.xlsx(bpFunc(), file, sheetName="Baseline Positive", append=TRUE, row.names=FALSE)
      write.xlsx(pivotTableFunc(), file, sheetName="Titer Pivot Table", append=TRUE, row.names=FALSE)
      write.xlsx(treatEmerFunc(), file, sheetName="Treatment Emergent", append=TRUE, row.names=FALSE)
      write.xlsx(myData()[these, , drop = FALSE], file, sheetName="Search Results", append=TRUE, row.names=FALSE)
      
      
    })
  
  
  
}

shinyApp(ui,server)


