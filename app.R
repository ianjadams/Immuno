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
      selectInput("dropdown", "Select a View",
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
                  tabPanel("Table", varSelectInput("col", "Reorganize Visit Codes:", character(0), multiple = TRUE),
                           DT::dataTableOutput('contents')),
                  tabPanel("Plot", plotOutput('plot')),
                  tabPanel("Summary", DT::dataTableOutput('summary1'),
                           DT::dataTableOutput('summary2'))
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
  #insert a 0 into all empty (NA) fields in Tier3
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
  
  #creates table for baseline visits
  baselineFunc <- function() {
    
    baseline <- filter(myData(), Visit == "Baseline")
    return(baseline)
    
  }
  
  
  
  #subjects missing baseline visits
  noBaselineFunc <- function() {
    
    rawData <- myData()
    
    baseline <- filter(rawData, Visit == "Baseline")
    
    allSubjects <- distinct(rawData, Subject)
    
    missingSubjects <- left_join(allSubjects, baseline, by = "Subject")
    missingSubjects <- filter(missingSubjects, is.na(Visit))
    missingSubjects <- subset(missingSubjects, select = "Subject")
    
    if (dim(missingSubjects)[1] == 0) {
      
      placeHolder <- data.frame("EMPTY")
      names(placeHolder) <- "Subject"
      return(placeHolder)
      
    } else {
      
      return(missingSubjects)
      
    }
    
  }
  
  
  
  #creates table for baseline positive visits
  bpFunc <- function() {
    
    bp <- filter(myData(), Visit == "Baseline" & Tier2 == input$t2D)
    
    if (dim(bp)[1] == 0) {
      
      placeHolder <- data.frame("EMPTY")
      names(placeHolder) <- "Subject"
      return(placeHolder)
      
    } else {
      
      return(bp)
      
    }
    
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
    # colnames(rawDataTrans)[colnames(rawDataTrans)==input$baselineVisits] <- "Baseline"
    
    #get max titer of each subject 
    maxTiter <- apply(rawDataTrans[,-1], 1, max, na.rm=TRUE)
    
    #append maxTiter to first column of pivot table
    bindedTiter <- cbind(maxTiter, rawDataTrans)
    
    #set NA values to 0
    bindedTiter[is.na(bindedTiter)] <- "-"
    
    #rename and drop the NA column
    # colnames(bindedTiter)[colnames(bindedTiter)=="NA"] <- "Blank"
    # bindedTiter$Blank <- NULL
    return(bindedTiter)
    
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
    
    
    
    #takes in user input MRD value
    mrd <- input$mrdIn 
    
    maxTiter <- apply(rawDataTrans[,-1], 1, max, na.rm=TRUE)
    
    bindedTiter <- cbind(maxTiter, rawDataTrans)
    
    negBaseTE <- filter(bindedTiter, Baseline == 0 & maxTiter >= 2*mrd)
    
    posBaseTE <- filter(bindedTiter, Baseline != 0 & bindedTiter$maxTiter >= 4*bindedTiter$Baseline)
    
    emerTable <- rbind(negBaseTE, posBaseTE)
    
    emerTable[is.na(emerTable)] <- "-"
    
    if (dim(emerTable)[1] == 0) {
      
      placeHolder <- data.frame("EMPTY")
      names(placeHolder) <- "Subject"
      return(placeHolder)
      
    } else {
      
      return(emerTable)
      
    }
    
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
      
      return(pivTableView())
      
    } 
    #SHOW TREATMENT EMERGENT TABLE
    else if(input$dropdown == "te") {
      
      return(pivTreatView())
      
    }
    
    
    
  }, rownames = FALSE)
  #end "Table" tab
  
  
  
  #begin "Plot" tab
  output$plot <- renderPlot({
    
    #use titer pivot table to create frequency table of each unique titer AND drop titer value of zero
    countTiter <- as.data.frame(table(pivotTableFunc()$maxTiter))
    names(countTiter) <- c("Titer", "Count")
    countTiter <- countTiter[!(countTiter$Titer == 0), ]
    
    #plot titer counts
    
    plot1 <- ggplot(countTiter, aes(x = Titer, y = Count)) + 
      geom_bar(stat = "identity", color = "#337ab7", size = 0.6, fill = "#18bc9c", alpha = 0.7) + 
      geom_text(aes(label = Count), vjust = -0.3, color = "#2c3e50", size = 4.5) + 
      ggtitle("Frequency of Highest Titer per Subject") + 
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
  #end "Plot" tab
  
  
  
  #begin "Summary" tab
  
  #begin T1, T2, BASELINE table
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
    baselines <- nrow(subset(statsData, Visit == "Baseline"))
    
    # num of subjects detected in Tier 2 at baseline visit
    basePos <- nrow(subset(statsData, Tier2 == input$t2D & Visit == "Baseline"))
    
    # baseline positive rate (num of baseline visits that are T2 positive / num of all baseline visits)
    basePR <- (basePos/baselines) * 100
    basePR <- round(basePR, 2)
    
    
    
    # output table for Tier 1, Tier 2, and Baseline results
    data.frame("SamplesTested" = c(allSamples, t2Tested, baselines),
               "Detected" = c(t1Pos, t2Pos, basePos),
               "PostiveRate" = c(putPR, conPR, basePR),
               row.names = c("Tier 1", "Tier 2", "Baseline"))
    
    
    
  })
  #end T1, T2, BASELINE table
  
  
  
  #begin Treatment Emergent table
  output$summary2 <- DT::renderDataTable({
    
    # max titer of treatment emergence table
    highTiter <- max(treatEmerFunc()$maxTiter)
    
    # num of treatment emergent subjects in dataset
    numTESubjects <- nrow(treatEmerFunc())
    
    # num of subjects in dataset
    numBLSubjects <- nrow(baselineFunc())
    
    # treatment emergence rate
    teRate <- (numTESubjects/numBLSubjects) * 100
    teRate <- round(teRate, 2)
    
    
    
    # output table for Treatment Emergence results
    data.frame("Sort" = c(highTiter, numTESubjects, numBLSubjects, teRate),
               row.names = c("Highest Titer", "Treatment Emergent Subjects", "Total Subjects w/Baseline", 
                             "Treatment Emergent Rate"))
    
    
    
  })
  #end Treatment Emergent table
  
  #end "Summary" tab
  
  
  
  #begin "Download All Tables" button
  output$downloadData <- downloadHandler(
    
    #define file name and dump all rows from tables into separate excel sheets
    'appDownloadData.xlsx', content = function(file) {
      these = input$contents_rows_all
      
      write.xlsx(myData(), file, sheetName="Original", row.names=FALSE)
      write.xlsx(baselineFunc(), file, sheetName="Baselines", append=TRUE, row.names=FALSE)
      write.xlsx(noBaselineFunc(), file, sheetName="Missing Baselines", append=TRUE, row.names=FALSE)
      write.xlsx(bpFunc(), file, sheetName="Baseline Positives", append=TRUE, row.names=FALSE)
      write.xlsx(pivTableView(), file, sheetName="Titer Pivot Table", append=TRUE, row.names=FALSE)
      write.xlsx(pivTreatView(), file, sheetName="Treatment Emergent", append=TRUE, row.names=FALSE)
      write.xlsx(myData()[these, , drop = FALSE], file, sheetName="Search Results", append=TRUE, row.names=FALSE)
      
      
    })
  #end "Download All Tables" button
  
  
  
}
#end Shiny server function

shinyApp(ui,server)


