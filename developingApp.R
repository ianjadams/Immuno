library(dplyr)
library(DT)
library(ggplot2)
library(readxl)
library(reshape2)
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(xlsx)



#begin Shiny UI interface
ui<- shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  
  tags$head(
    tags$style(HTML("
    
      #downloadData, #fileInput {
        background-color: #18bc9c;
      }

    "))
  ),
  
  useShinyjs(),
  titlePanel("LEM's Immuno Analysis Now for Vendor Data"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", 'Load a dataset:',
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv')),
      
      #input: text input for specifying values
      textInput("baselineVisits", label = "Complete the fields below:", placeholder = "Enter 'Baseline Visit' value"),
      textInput("t1D", label = NULL, placeholder = "Enter Tier 1 'Detected' value"),
      textInput("t1ND", label = NULL, placeholder = "Enter Tier 1 'NOT Detected' value"),
      textInput("t2aD", label = NULL, placeholder = "Enter Tier 2(a) 'Detected' value"),
      textInput("t2aND", label = NULL, placeholder = "Enter Tier 2(a) 'NOT Detected' value"),
      textInput("t2bD", label = NULL, placeholder = "Enter Tier 2b 'Detected' value"),
      textInput("t2bND", label = NULL, placeholder = "Enter Tier 2b 'NOT Detected' value"),
      textInput("t2cD", label = NULL, placeholder = "Enter Tier 2c 'Detected' value"),
      textInput("t2cND", label = NULL, placeholder = "Enter Tier 2c 'NOT Detected' value"),
      textInput("t2dD", label = NULL, placeholder = "Enter Tier 2d 'Detected' value"),
      textInput("t2dND", label = NULL, placeholder = "Enter Tier 2d 'NOT Detected' value"),
      textInput("t4aD", label = NULL, placeholder = "Enter Tier 4(a) 'Detected' value"),
      textInput("t4aND", label = NULL, placeholder = "Enter Tier 4(a) 'NOT Detected' value"),
      textInput("t4bD", label = NULL, placeholder = "Enter Tier 4b 'Detected' value"),
      textInput("t4bND", label = NULL, placeholder = "Enter Tier 4b 'NOT Detected' value"),
      textInput("t4cD", label = NULL, placeholder = "Enter Tier 4c 'Detected' value"),
      textInput("t4cND", label = NULL, placeholder = "Enter Tier 4c 'NOT Detected' value"),
      textInput("t4dD", label = NULL, placeholder = "Enter Tier 4d 'Detected' value"),
      textInput("t4dND", label = NULL, placeholder = "Enter Tier 4d 'NOT Detected' value"),
      
      strong("If applicable, include additional columns:"),
      
      br(),
      
      #input: checkboxes to include Tier 2 columns
      prettyCheckbox("checkT2B", "Tier 2b", FALSE, inline = TRUE, shape = "curve", status = "primary", animation = "pulse"),
      prettyCheckbox("checkT2C", "Tier 2c", FALSE, inline = TRUE, shape = "curve", status = "primary", animation = "pulse"),
      prettyCheckbox("checkT2D", "Tier 2d", FALSE, inline = TRUE, shape = "curve", status = "primary", animation = "pulse"),
      
      br(),
      
      #input: checkboxes to include Tier 4 columns
      prettyCheckbox("checkT4A", "Tier 4(a)", FALSE, inline = TRUE, shape = "curve", status = "primary", animation = "pulse"),
      prettyCheckbox("checkT4B", "Tier 4b", FALSE, inline = TRUE, shape = "curve", status = "primary", animation = "pulse"),
      prettyCheckbox("checkT4C", "Tier 4c", FALSE, inline = TRUE, shape = "curve", status = "primary", animation = "pulse"),
      prettyCheckbox("checkT4D", "Tier 4d", FALSE, inline = TRUE, shape = "curve", status = "primary", animation = "pulse"),
      
      
      #input: MRD value field
      numericInput("mrdIn", "Enter Minimum Required Dilution:", 10, min = 1, max = 100000000),
      verbatimTextOutput('mrdOut'),
      
      
      #input: select input for changing current view of the table
      selectInput("dropdown", "Select a view",
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
                  tabPanel("Flags", DT::dataTableOutput('flag'),
                           verbatimTextOutput('premises')),
                  tabPanel("Plot", plotOutput('plot'),
                           verbatimTextOutput('sampleSize')),
                  tabPanel("Summary", DT::dataTableOutput('summary1'),
                           br(),
                           DT::dataTableOutput('summary2'),
                           br(),
                           DT::dataTableOutput('summary3')),
                  tabPanel("Help", uiOutput('help'))
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
    initialData$Tier3 <- ifelse(substring(initialData$Tier3, 1, 2) == "1:", 
                                as.numeric(as.character(substring(initialData$Tier3, 3))),
                                as.numeric(as.character(initialData$Tier3)))
    initialData$Tier3[is.na(initialData$Tier3)] <- 0
    initialData[initialData==input$baselineVisits] <- "Baseline"
    as.data.frame(initialData)
    
  })
  
  
  
  #begin global functions for table calculations and views
  
  #function: baseline visits
  baselineFunc <- function() {
    
    baseline <- filter(myData(), Visit == "Baseline")
    return(baseline)
    
  }
  
  
  
  #function: subjects who are Tier2 positive at baseline
  bpFunc <- function() {
    
    bp <- filter(myData(), Visit == "Baseline" & Tier2 == input$t2aD)
    
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
  
  
  
  #create new titer pivot table with counts of each titer and sums in the rows and columns
  pivTiterView <- function() {
    
    bpSubs <- titerPivot()
    
    bpSubs <- data.frame(bpSubs$Baseline, bpSubs$maxTiter)
    
    #create pivot table for Baseline and maxTiter columns, append Sum of rows and columns to the table
    titerPiv <- as.data.frame.matrix(addmargins(table(bpSubs[,1], bpSubs[,2])))
    titerPiv <- cbind("Baseline Titer" = rownames(titerPiv), titerPiv)
    return(titerPiv)
    
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
        
        placeHolder <- data.frame("Subject" = "EMPTY", "Premise" = "No Subjects Missing Baseline")
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
        
        placeHolder <- data.frame("Subject" = "EMPTY", "Premise" = "No Subjects w/Baseline Visits Only")
        return(placeHolder)
        
      }
      
    }
    
    unEvalTable <- rbind(noBL(), noPostBL())
    return(unEvalTable)
  }
  #end global functions for table views
  
  
  
  #display or hide Tier 2b input fields based on user checking the box
  #display or hide Tier 2c checkbox based on user checking the box
  observe({
    toggle("t2bD", condition = input$checkT2B, anim = TRUE, time = 0.5, animType = "slide")
    toggle("t2bND", condition = input$checkT2B, anim = TRUE, time = 0.5, animType = "slide")
    toggle("checkT2C", condition = input$checkT2B, anim = TRUE, time = 0.5, animType = "slide")
  })
  
  
  
  #display or hide Tier 2c input fields based on user checking the box
  #display or hide Tier 2d checkbox based on user checking the box
  observe({
    toggle("t2cD", condition = input$checkT2C, anim = TRUE, time = 0.5, animType = "slide")
    toggle("t2cND", condition = input$checkT2C, anim = TRUE, time = 0.5, animType = "slide")
    toggle("checkT2D", condition = input$checkT2C, anim = TRUE, time = 0.5, animType = "slide")
    
  })
  
  
  
  #display or hide Tier 2d input fields based on user checking the box
  observe({
    toggle("t2dD", condition = input$checkT2D, anim = TRUE, time = 0.5, animType = "slide")
    toggle("t2dND", condition = input$checkT2D, anim = TRUE, time = 0.5, animType = "slide")
  })
  
  
  
  #display or hide Tier 4(a) input fields based on user checking the box
  #display or hide Tier 4b checkbox based on user checking the box
  observe({
    toggle("t4aD", condition = input$checkT4A, anim = TRUE, time = 0.5, animType = "slide")
    toggle("t4aND", condition = input$checkT4A, anim = TRUE, time = 0.5, animType = "slide")
    toggle("checkT4B", condition = input$checkT4A, anim = TRUE, time = 0.5, animType = "slide")
  })
  
  
  
  #display or hide Tier 4b input fields based on user checking the box
  #display or hide Tier 4c checkbox based on user checking the box
  observe({
    toggle("t4bD", condition = input$checkT4B, anim = TRUE, time = 0.5, animType = "slide")
    toggle("t4bND", condition = input$checkT4B, anim = TRUE, time = 0.5, animType = "slide")
    toggle("checkT4C", condition = input$checkT4B, anim = TRUE, time = 0.5, animType = "slide")
  })
  
  
  
  #display or hide Tier 4c input fields based on user checking the box
  #display or hide Tier 4d checkbox based on user checking the box
  observe({
    toggle("t4cD", condition = input$checkT4C, anim = TRUE, time = 0.5, animType = "slide")
    toggle("t4cND", condition = input$checkT4C, anim = TRUE, time = 0.5, animType = "slide")
    toggle("checkT4D", condition = input$checkT4C, anim = TRUE, time = 0.5, animType = "slide")
  })
  
  
  
  #display or hide Tier 4d input fields based on user checking the box
  observe({
    toggle("t4dD", condition = input$checkT4D, anim = TRUE, time = 0.5, animType = "slide")
    toggle("t4dND", condition = input$checkT4D, anim = TRUE, time = 0.5, animType = "slide")
  })
  
  
  
  #function: flag rows for QC checks
  flagFunc <- function() {
    
    #create column "Premise"
    allFlags <- myData()
    allFlags$Premise <- "exp"
    
    
    
    #begin logical QC checks
    #tier1
    QC1 <- subset(allFlags, Tier1 != input$t1D & Tier1 != input$t1ND & Tier1 != "N/A" & Tier1 != "NA")
    QC2 <- subset(allFlags, Tier1 == input$t1D & is.na(Tier2))
    QC3 <- subset(allFlags, Tier1 == input$t1ND & Tier2 == input$t2aD)
    QC4 <- subset(allFlags, Tier1 == input$t1ND & Tier3 != 0)
    
    #tier2
    QC5 <- subset(allFlags, Tier2 != input$t2aD & Tier2 != input$t2aND & Tier2 != "N/A" & Tier2 != "NA")
    QC6 <- subset(allFlags, Tier2 == input$t2aND & Tier3 != 0)
    QC7 <- subset(allFlags, Tier2 == input$t2aD & Tier3 == 0)
    
    QC8 <- try(if("Tier4" %in% colnames(allFlags)) {
      
      subset(allFlags, Tier2 == input$t2aD & Tier4 == is.na(Tier4))
      
    })
    
    QC9 <- try(if("Tier4" %in% colnames(allFlags)) {
      
      subset(allFlags, Tier2 == input$t2aND & Tier4 == input$t4aD)
      
    })
    
    #tier3
    QC10 <- subset(allFlags, Tier3 %% input$mrdIn != 0 & Tier3 != 0)
    
    #tier4
    QC11 <- try(if("Tier4" %in% colnames(allFlags)) {
      
      subset(allFlags, Tier4 != input$t4aD & Tier4 != input$t4aND)
      
    })
    
    #duplicate visits
    QC12 <- allFlags[duplicated(allFlags[, c("Subject", "Visit")]), ]
    
    #end logical QC checks
    
    
    
    #begin populating Premise column with reasoning for each error in the table
    try(if(QC1$Premise == "exp") {
      QC1$Premise <- "T1 Discrepant Value"
    }, silent = TRUE)
    
    try(if(QC2$Premise == "exp") {
      QC2$Premise <- "T1(+) w/o Result in T2"
    }, silent = TRUE)
    
    try(if(QC3$Premise == "exp") {
      QC3$Premise <- "T1(-) with T2(+)"
    }, silent = TRUE)
    
    try(if(QC4$Premise == "exp") {
      QC4$Premise <- "T1(-) with T3(+)"
    }, silent = TRUE)
    
    try(if(QC5$Premise == "exp") {
      QC5$Premise <- "T2 Discrepant Value"
    }, silent = TRUE)
    
    try(if(QC6$Premise == "exp") {
      QC6$Premise <- "T2(-) with T3(+)"
    }, silent = TRUE)
    
    try(if(QC7$Premise == "exp") {
      QC7$Premise <- "T2(+) with T3(-)"
    }, silent = TRUE)
    
    try(if(QC8$Premise == "exp") {
      QC8$Premise <- "T2(+) w/o Result in T4"
    }, silent = TRUE)
    
    try(if(QC9$Premise == "exp") {
      QC9$Premise <- "T2(-) with T4(+)"
    }, silent = TRUE)
    
    try(if(QC10$Premise == "exp") {
      QC10$Premise <- "T3 Discrepant Value"
    }, silent = TRUE)
    
    try(if(QC11$Premise == "exp") {
      QC11$Premise <- "T4 Discrepant Value"
    }, silent = TRUE)
    
    try(if(QC12$Premise == "exp") {
      QC12$Premise <- "Duplicate Visit for Subject"
    }, silent = TRUE)
    
    
    
    #combine all rows that have any of the errors above
    errorTable <- try(rbind(QC1, QC2, QC3, QC4, QC5, QC6, QC7, QC8, QC9, QC10, QC11, QC12))
    return(errorTable)
    
  }
  
  
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
      
      
      return(pivTiterView())
      
    }
    
    
    
  }, rownames = FALSE)
  #end "Table" tab
  
  
  
  #begin "Flags" tab
  output$flag <- DT::renderDataTable({
    
    return(flagFunc())
    
  }, rownames = FALSE)
  #end "Flags" tab
  
  
  
  #begin list of "Premises" output field
  output$premises <- renderText({
    
    errorTable <- flagFunc()
    
    listErrors <- distinct(errorTable, Premise)
    
    paste(nrow(listErrors), "unique QC checks appear in the table:", listErrors)
    
  })
  #end list of "Premises" output field
  
  
  
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
    putPR <- round((t1Pos/allSamples * 100), 2)
    
    # num of Tier 2 samples tested
    t2aTested <- nrow(subset(statsData, Tier2 == input$t2aD | Tier2 == input$t2aND))
    
    # num of Tier 2 detected samples
    t2aPos <- nrow(subset(statsData, Tier2 == input$t2aD))
    
    # confirmed positive rate (num of Tier 2 detected samples / num of Tier 1 detected samples)
    conPR <- round((t2aPos/t1Pos* 100), 2)
    
    #rows for Tier 1 and Tier 2
    tier1and2Rows <- data.frame("SamplesTested" = c(allSamples, t2aTested),
                                "Detected" = c(t1Pos, t2aPos),
                                "PostiveRate" = c(putPR, conPR),
                                row.names = c("Tier 1", "Tier 2"))
    
    
    
    tier2bRow <- try(if("Tier2b" %in% colnames(statsData)) {
      
      rowFunc <- function() {
        
        cat("T2b exists\n");
        
        # num of Tier 2b samples tested
        t2bTested <- nrow(subset(statsData, Tier2b == input$t2bD | Tier2b == input$t2bND))
        
        # num of Tier 2b detected samples
        t2bPos <- nrow(subset(statsData, Tier2b == input$t2bD))
        
        # Tier 2b positive rate (num of Tier 2b detected samples / num of Tier 2 detected samples)
        t2bPR <- round((t2bPos/t2aPos * 100), 2)
        
        #row for Tier 2b
        t2bTable<- data.frame("SamplesTested" = (t2bTested),
                              "Detected" = (t2bPos),
                              "PostiveRate" = (t2bPR),
                              row.names = c("Tier 2b"))
        
        return(t2bTable)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        cat("T2b column does not exist\n");
        
        #dummy row with NAs
        t2bTable<- data.frame("SamplesTested" = ("NA"),
                              "Detected" = ("NA"),
                              "PostiveRate" = (0),
                              row.names = c("Tier 2b"))
        
        return(t2bTable)
      }
      
    })
    
    
    
    tier2cRow <- try(if("Tier2c" %in% colnames(statsData)) {
      
      rowFunc <- function() {
        
        cat("T2c exists\n");
        
        # num of Tier 2c samples tested
        t2cTested <- nrow(subset(statsData, Tier2c == input$t2cD | Tier2c == input$t2cND))
        
        # num of Tier 2c detected samples
        t2cPos <- nrow(subset(statsData, Tier2c == input$t2cD))
        
        # Tier 2c positive rate (num of Tier 2c detected samples / num of Tier 2 detected samples)
        t2cPR <- round((t2cPos/t2aPos * 100), 2)
        
        #row for Tier 2c
        t2cTable<- data.frame("SamplesTested" = (t2cTested),
                              "Detected" = (t2cPos),
                              "PostiveRate" = (t2cPR),
                              row.names = c("Tier 2c"))
        
        return(t2cTable)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        cat("T2c column does not exist\n");
        
        #dummy row with NAs
        t2cTable<- data.frame("SamplesTested" = ("NA"),
                              "Detected" = ("NA"),
                              "PostiveRate" = (0),
                              row.names = c("Tier 2c"))
        
        return(t2cTable)
      }
      
    })
    
    
    
    tier2dRow <- try(if("Tier2d" %in% colnames(statsData)) {
      
      rowFunc <- function() {
        
        cat("T2d exists\n");
        
        # num of Tier 2d samples tested
        t2dTested <- nrow(subset(statsData, Tier2d == input$t2dD | Tier2d == input$t2dND))
        
        # num of Tier 2d detected samples
        t2dPos <- nrow(subset(statsData, Tier2d == input$t2dD))
        
        # Tier 2d positive rate (num of Tier 2d detected samples / num of Tier 2 detected samples)
        t2dPR <- round((t2dPos/t2aPos * 100), 2)
        
        #row for Tier 2d
        t2dTable<- data.frame("SamplesTested" = (t2dTested),
                              "Detected" = (t2dPos),
                              "PostiveRate" = (t2dPR),
                              row.names = c("Tier 2d"))
        
        return(t2dTable)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        cat("T2d column does not exist\n");
        
        #dummy row with NAs
        t2dTable<- data.frame("SamplesTested" = ("NA"),
                              "Detected" = ("NA"),
                              "PostiveRate" = (0),
                              row.names = c("Tier 2d"))
        
        return(t2dTable)
      }
      
    })
    
    
    
    tier4aRow <- try(if("Tier4" %in% colnames(statsData)) {
      
      rowFunc <- function() {
        
        cat("T4 exists\n");
        
        # num of Tier 4 samples tested
        t4aTested <- nrow(subset(statsData, Tier4 == input$t4aD | Tier4 == input$t4aND))
        
        # num of Tier 4 detected samples
        t4aPos <- nrow(subset(statsData, Tier4 == input$t4aD))
        
        # Tier 4 positive rate (num of Tier 4 detected samples / num of Tier 2 detected samples)
        t4aPR <- round((t4aPos/t2aPos * 100), 2)
        
        #row for Tier 4
        t4aTable<- data.frame("SamplesTested" = (t4aTested),
                             "Detected" = (t4aPos),
                             "PostiveRate" = (t4aPR),
                             row.names = c("Tier 4"))
        
        return(t4aTable)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        cat("T4 column does not exist\n");
        
        #dummy row with NAs
        t4aTable<- data.frame("SamplesTested" = ("NA"),
                             "Detected" = ("NA"),
                             "PostiveRate" = (0),
                             row.names = c("Tier 4"))
        
        return(t4aTable)
      }
      
    })
    
    
    
    tier4bRow <- try(if("Tier4b" %in% colnames(statsData)) {
      
      rowFunc <- function() {
        
        cat("T4b exists\n");
        
        # num of Tier 4b samples tested
        t4bTested <- nrow(subset(statsData, Tier4b == input$t4bD | Tier4b == input$t4bND))
        
        # num of Tier 4b detected samples
        t4bPos <- nrow(subset(statsData, Tier4b == input$t4bD))
        
        # Tier 4b positive rate (num of Tier 4b detected samples / num of Tier 2 detected samples)
        t4bPR <- round((t4bPos/t2aPos * 100), 2)
        
        #row for Tier 4b
        t4bTable<- data.frame("SamplesTested" = (t4bTested),
                              "Detected" = (t4bPos),
                              "PostiveRate" = (t4bPR),
                              row.names = c("Tier 4b"))
        
        return(t4bTable)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        cat("T4b column does not exist\n");
        
        #dummy row with NAs
        t4bTable<- data.frame("SamplesTested" = ("NA"),
                              "Detected" = ("NA"),
                              "PostiveRate" = (0),
                              row.names = c("Tier 4b"))
        
        return(t4bTable)
      }
      
    })
    
    
    
    tier4cRow <- try(if("Tier4c" %in% colnames(statsData)) {
      
      rowFunc <- function() {
        
        cat("T4c exists\n");
        
        # num of Tier 4c samples tested
        t4cTested <- nrow(subset(statsData, Tier4c == input$t4cD | Tier4c == input$t4cND))
        
        # num of Tier 4c detected samples
        t4cPos <- nrow(subset(statsData, Tier4c == input$t4cD))
        
        # Tier 4c positive rate (num of Tier 4c detected samples / num of Tier 2b detected samples)
        t4cPR <- round((t4cPos/tier2bRow()$Detected * 100), 2)
        
        #row for Tier 4c
        t4cTable<- data.frame("SamplesTested" = (t4cTested),
                              "Detected" = (t4cPos),
                              "PostiveRate" = (t4cPR),
                              row.names = c("Tier 4c"))
        
        return(t4cTable)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        cat("T4c column does not exist\n");
        
        #dummy row with NAs
        t4cTable<- data.frame("SamplesTested" = ("NA"),
                              "Detected" = ("NA"),
                              "PostiveRate" = (0),
                              row.names = c("Tier 4c"))
        
        return(t4cTable)
      }
      
    })
    
    
    
    tier4dRow <- try(if("Tier4d" %in% colnames(statsData)) {
      
      rowFunc <- function() {
        
        cat("T4d exists\n");
        
        # num of Tier 4d samples tested
        t4dTested <- nrow(subset(statsData, Tier4d == input$t4dD | Tier4d == input$t4dND))
        
        # num of Tier 4d detected samples
        t4dPos <- nrow(subset(statsData, Tier4d == input$t4dD))
        
        # Tier 4d positive rate (num of Tier 4d detected samples / num of Tier 2c detected samples)
        t4dPR <- round((t4dPos/tier2cRow()$Detected * 100), 2)
        
        #row for Tier 4d
        t4dTable<- data.frame("SamplesTested" = (t4dTested),
                              "Detected" = (t4dPos),
                              "PostiveRate" = (t4dPR),
                              row.names = c("Tier 4d"))
        
        return(t4dTable)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        cat("T4d column does not exist\n");
        
        #dummy row with NAs
        t4dTable<- data.frame("SamplesTested" = ("NA"),
                              "Detected" = ("NA"),
                              "PostiveRate" = (0),
                              row.names = c("Tier 4d"))
        
        return(t4dTable)
      }
      
    })
    
    
    #combine all rows for each Tier
    finalTierTable <- try(rbind(tier1and2Rows, tier2bRow(), tier2cRow(), tier2dRow(),
                                tier4aRow(), tier4bRow(), tier4cRow(), tier4dRow()))
    
    #drop rows that do not have statistics (they do not appear in the dataset)  
    subset(finalTierTable, SamplesTested != "NA")

    
    
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
               row.names = c("Total Unique Subjects", "Unevaluated Subjects", "Highest Post-Baseline Titer"))
    
    
    
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
      write.xlsx(bpFunc(), file, sheetName="Baseline Positives", append=TRUE, row.names=FALSE, showNA = FALSE)
      write.xlsx(unEvalFunc(), file, sheetName="Unevaluated Subjects", append=TRUE, row.names=FALSE, showNA = FALSE)
      write.xlsx(pivTableView(), file, sheetName="Subject Pivot Table", append=TRUE, row.names=FALSE, showNA = FALSE)
      write.xlsx(pivTreatView(), file, sheetName="Treatment Emergent Pivot Table", append=TRUE, row.names=FALSE, showNA = FALSE)
      write.xlsx(pivTiterView(), file, sheetName="Titer Pivot Table", append=TRUE, row.names=FALSE, showNA = FALSE)
      write.xlsx(myData()[these, , drop = FALSE], file, sheetName="Search Results", append=TRUE, row.names=FALSE, showNA = FALSE)
      
      
    })
  #end "Download All Tables" button
  
  
  output$help <- renderUI({
    
    HTML( 
      
      '<h4>List of all possible QC flags are included below:</h4>',
      '<br />',
      
        '<ol>',
      
            '<li><b>T1 Discrepant Value</b></li>',
              '<ul>',
                '<li>T1 value is neither of the user-entered values for T1 Detected and T1 Not Detected (this will flag rows such as QNS, CANCEL, PEND, etc.)</li>',
              '</ul>',
            '<li><b>T1(+) w/o Result in T2</b></li>',
              '<ul>',
                '<li>T1 value is Detected but result in T2 is missing</li>',
              '</ul>',
            '<li><b>T1(-) with T2(+)</b></li>',
              '<ul>',
                '<li>T1 value is Not Detected but T2 value is Detected</li>',
              '</ul>',
            '<li><b>T1(-) with T3(+)</b></li>',
              '<ul>',
                '<li>T1 value is Not Detected but T3 value is not 0 (indicating there was a reported titer)</li>',
              '</ul>',
            '<li><b>T2 Discrepant Value</b></li>',
              '<ul>',
                '<li>T2 value is neither of the user-entered values for T2 Detected and T2 Not Detected (this will flag rows such as QNS, CANCEL, PEND, etc.)</li>',
              '</ul>',
            '<li><b>T2(-) with T3(+)</b></li>',
              '<ul>',
                '<li>T2 value is Not Detected but T3 value is not 0 (indicating there was a reported titer)</li>',
              '</ul>',
            '<li><b>T2(+) with T3(-)</b></li>',
              '<ul>',
                '<li>T2 value is Detected but T3 value is 0 (indicating there should be a reported titer, but it might be missing)</li>',
              '</ul>',
            '<li><b>T2(+) w/o Result in T4</b></li>',
              '<ul>',
                '<li>T2 value is Detected but result in T4 is missing</li>',
              '</ul>',
            '<li><b>T2(-) with T4(+)</b></li>',
              '<ul>',
                '<li>T2 value is Not Detected but T4 value is Detected</li>',
              '</ul>',
            '<li><b>T3 Discrepant Value</b></li>',
              '<ul>',
                '<li>Titer value is below MRD or not multiple of MRD</li>',
              '</ul>',
            '<li><b>T4 Discrepant Value</b></li>',
              '<ul>',
                '<li>T4 value is neither of the user-entered values for T4 Detected and T4 Not Detected (this will flag rows such as QNS, CANCEL, PEND, etc.)</li>',
              '</ul>',
            '<li><b>Duplicate Visit for Subject</b></li>',
              '<ul>',
                '<li>Subjects who have duplicate instances of the same visit code</li>',
              '</ul>',
      
        '</ol>',
      
      '<br />',
      
        '<h4>Premises for summary statistics are included below:</h4>',
        '<br />',
      
          '<ol>',
          
            '<li><b>SamplesTested</b> is sum of user-entered Detected & Not Detected values in each Tier</li>',
            '<li><b>Detected</b> is sum of user-entered Detected values in each Tier</li>',
            '<li><b>PositiveRate</b> is # of <b>Detected</b> / # of <b>SamplesTested</b></li>',
            '<li><b>Evaluable Subjects</b> are those that have Baseline visit and at least 1 follow-up visit</li>',
            '<li><b>Unevaluated Subjects</b> are those that either:</li>',
              '<ul>',
                '<li>missed Baseline</li>',
                '<li>have Baseline visit without any follow-up visits</li>',
              '</ul>',
            '<li><b>Total Unique Subjects</b> is sum of <b>Evaluable</b> and <b>Unevaluated</b> subjects in the study</li>',
            '<li><b>Highest Titer</b> is the maximum Post-Baseline titer in the study</li>',
      
          '</ol>'
          
          )
    
  })
  
  
  
}
#end Shiny server function

shinyApp(ui,server)


