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
ui <- shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  
  tags$head(
    tags$style(HTML("
    
      .btn {
          background-color: #18bc9c;
          border-radius: 4px;
      }
      
      .btn:hover {
        background-color: #30857c;
      }
      
      .progress-bar {
        background-color: #2c3e50;
      }
      
      pre {
        background-color: #cae0e5;
        font-family: Helvetica;
      }
      
      #format, #flags, #summary {
        border: 1px solid #337ab7;
        border-radius: 20px;
        padding: 10px;
        margin: 15px;
      }
      
      

    "))
  ),
  
  useShinyjs(),
  headerPanel(title = ("LEM's Immunogenicity Analysis Now Tool"),
              tags$head(tags$link(rel = "icon", type = "image/png", href = "antibody.ico"),
                        windowTitle = "Immuno_Analysis_Now")),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", 'Load a dataset:',
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv')),
      
      strong("Select all columns that appear in the dataset:"),
      
      br(),
      
      #input: checkboxes to include Tier 2 columns
      prettyCheckbox("checkT1", "Tier 1", FALSE, inline = TRUE, shape = "curve", status = "primary", animation = "pulse"),
      
      br(),
      
      #input: checkboxes to include Tier 2 columns
      prettyCheckbox("checkT2A", "Tier 2", FALSE, inline = TRUE, shape = "curve", status = "primary", animation = "pulse"),
      prettyCheckbox("checkT2B", "Tier 2b", FALSE, inline = TRUE, shape = "curve", status = "primary", animation = "pulse"),
      prettyCheckbox("checkT2C", "Tier 2c", FALSE, inline = TRUE, shape = "curve", status = "primary", animation = "pulse"),
      prettyCheckbox("checkT2D", "Tier 2d", FALSE, inline = TRUE, shape = "curve", status = "primary", animation = "pulse"),
      
      br(),
      
      #input: checkboxes to include Tier 4 columns
      prettyCheckbox("checkT4A", "Tier 4", FALSE, inline = TRUE, shape = "curve", status = "primary", animation = "pulse"),
      prettyCheckbox("checkT4B", "Tier 4b", FALSE, inline = TRUE, shape = "curve", status = "primary", animation = "pulse"),
      prettyCheckbox("checkT4C", "Tier 4c", FALSE, inline = TRUE, shape = "curve", status = "primary", animation = "pulse"),
      prettyCheckbox("checkT4D", "Tier 4d", FALSE, inline = TRUE, shape = "curve", status = "primary", animation = "pulse"),
      prettyCheckbox("checkT4E", "Tier 4e", FALSE, inline = TRUE, shape = "curve", status = "primary", animation = "pulse"),
      prettyCheckbox("checkT4F", "Tier 4f", FALSE, inline = TRUE, shape = "curve", status = "primary", animation = "pulse"),
      
      #input: text input for specifying values
      textInput("baselineVisits", label = "Complete the fields below:", placeholder = "Enter 'Baseline Visit' value"),
      textInput("t1D", label = NULL, placeholder = "Enter Tier 1 'Detected' value"),
      textInput("t1ND", label = NULL, placeholder = "Enter Tier 1 'NOT Detected' value"),
      textInput("t2aD", label = NULL, placeholder = "Enter Tier 2 'Detected' value"),
      textInput("t2aND", label = NULL, placeholder = "Enter Tier 2 'NOT Detected' value"),
      textInput("t2bD", label = NULL, placeholder = "Enter Tier 2b 'Detected' value"),
      textInput("t2bND", label = NULL, placeholder = "Enter Tier 2b 'NOT Detected' value"),
      textInput("t2cD", label = NULL, placeholder = "Enter Tier 2c 'Detected' value"),
      textInput("t2cND", label = NULL, placeholder = "Enter Tier 2c 'NOT Detected' value"),
      textInput("t2dD", label = NULL, placeholder = "Enter Tier 2d 'Detected' value"),
      textInput("t2dND", label = NULL, placeholder = "Enter Tier 2d 'NOT Detected' value"),
      textInput("t4aD", label = NULL, placeholder = "Enter Tier 4 'Detected' value"),
      textInput("t4aND", label = NULL, placeholder = "Enter Tier 4 'NOT Detected' value"),
      textInput("t4bD", label = NULL, placeholder = "Enter Tier 4b 'Detected' value"),
      textInput("t4bND", label = NULL, placeholder = "Enter Tier 4b 'NOT Detected' value"),
      textInput("t4cD", label = NULL, placeholder = "Enter Tier 4c 'Detected' value"),
      textInput("t4cND", label = NULL, placeholder = "Enter Tier 4c 'NOT Detected' value"),
      textInput("t4dD", label = NULL, placeholder = "Enter Tier 4d 'Detected' value"),
      textInput("t4dND", label = NULL, placeholder = "Enter Tier 4d 'NOT Detected' value"),
      textInput("t4eD", label = NULL, placeholder = "Enter Tier 4e 'Detected' value"),
      textInput("t4eND", label = NULL, placeholder = "Enter Tier 4e 'NOT Detected' value"),
      textInput("t4fD", label = NULL, placeholder = "Enter Tier 4f 'Detected' value"),
      textInput("t4fND", label = NULL, placeholder = "Enter Tier 4f 'NOT Detected' value"),
      
      
      #input: MRD value field
      numericInput("mrdIn", "Enter Minimum Required Dilution:", 10, min = 1, max = 1000000000),
      verbatimTextOutput('mrdOut'),
      
      varSelectInput("col", "Reorganize visit codes:", character(0), multiple = TRUE),
      
      #input: select input for changing current view of the table
      selectInput("dropdown", "Select a view",
                  choices = c(Original = "original",
                              "Baselines" = "baseline",
                              "Baseline Positives" = "bp",
                              "Unevaluated Subjects" = "uneval",
                              list("Shift Tables" = c(
                                "Subject Pivot Table" = "subjects",
                                "Treatment Emergent Pivot Table" = "te",
                                "Titer Pivot Table" = "titercounts"))),
                  selected = "original"),
      
      
      #download button
      downloadButton("downloadData", "Download All Tables")
      
    ),
    mainPanel(
      
      #output: separate the page into specific tabs
      tabsetPanel(type = "tabs",
                  tabPanel("Table", DT::dataTableOutput('contents')),
                  tabPanel("Flags", DT::dataTableOutput('flag'),
                           br(),
                           tableOutput('premises')),
                  tabPanel("Plot", plotOutput('plot'),
                           verbatimTextOutput('sampleSize'),
                           br(),
                           varSelectInput("subs", "Select Subject:", character(0)),
                           varSelectInput("vars", "Choose 2 Variables to Plot:", character(0), multiple = TRUE),
                           numericInput("scaleIn", "Multiplier for Y-Axis Scale", 1, min = 0, max = 1000000000),
                           verbatimTextOutput('scaleOut'),
                           plotOutput('plot2'),
                           br()),
                  tabPanel("Summary", DT::dataTableOutput('summary1'),
                           br(),
                           DT::dataTableOutput('summary2'),
                           br(),
                           DT::dataTableOutput('summary3'),
                           br()),
                  tabPanel("Help", downloadButton("downloadGuide", "Documentation"),
                           uiOutput('help'),
                           br(),
                           br())
                  
      )
    )
  )
)
)
#end Shiny UI interface



#begin Shiny server function
server <- function(input, output, session) {
  
  
  #create reactive table on data load
  originalData <- reactive({
    
    #automatically enter text values for rows with missing Subject or missing Visit
    #update data to change user input BL value to "Baseline"
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    initialData <- data.frame(read_excel(inFile$datapath))
    initialData$Subject[is.na(initialData$Subject)] <- "Unknown Subject"
    initialData$Visit[is.na(initialData$Visit)] <- "Unknown Visit"
    initialData$Visit[initialData$Visit==input$baselineVisits] <- "Baseline"
    initialData$Tier3 <- ifelse(substring(initialData$Tier3, 1, 2) == "1:",
                            as.numeric(as.character(substring(initialData$Tier3, 3))),
                            as.numeric(as.character(initialData$Tier3)))
    as.data.frame(initialData)
    
  })
  
  
  
  #change Tier3 datatype to numeric and trim off "1:" from all values
  #insert 0 into all empty (NA) fields in Tier3
  myData <- function() {
    
    rawData <- originalData()
    
    rawData$Tier3[is.na(rawData$Tier3)] <- 0
    as.data.frame(rawData)
    
  }
  
  
  
  #begin global functions for table calculations and views
  
  #function: baseline visits
  baselineFunc <- function() {
    
    baseline <- filter(myData(), Visit == "Baseline")
    return(baseline)
    
  }
  
  
  
  postBaselineFunc <- function() {
    
    postBaseline <- filter(myData(), Visit != "Baseline")
    return(postBaseline)
    
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
                             maxTiter = max(Tier3))
    
    #transform the rawData table to a pivot table, using Subjects as rows, Visits as columns, Tier3 values as cells
    rawDataTrans <- dcast(summRawData, Subject ~ Visit, value.var = "maxTiter")
    
    #gets each unique visit code and closely reorganizes them in chronological order 
    nams <- as.character(unique(myData()$Visit)) 
    nums <- sapply(nams, function(nm) which(names(rawDataTrans) %in% nm)) 
    rawDataTrans[, sort(nums)] <- rawDataTrans[, nams]
    names(rawDataTrans)[sort(nums)] <- nams
    
    subsetRawDataTrans <- try(if("Baseline" %in% colnames(rawDataTrans)) {
      
      newTable <- subset(rawDataTrans, select = -c(Subject, Baseline))
      maxTiter <- apply(newTable, 1, max, na.rm=TRUE)
      bindedTiter <- cbind(maxTiter, rawDataTrans)
      
    })
    
    return(subsetRawDataTrans)
    
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
  observeEvent(originalData(), {
    updateSelectInput(session, "col", choices = names(pivotTableFunc()), selected = names(pivotTableFunc()))
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
  
  
  
  #display or hide Tier 1 input fields based on user checking the box
  observe({
    toggle("t1D", condition = input$checkT1, anim = TRUE, time = 0.5, animType = "slide")
    toggle("t1ND", condition = input$checkT1, anim = TRUE, time = 0.5, animType = "slide")
  })
  
  
  
  #display or hide Tier 2a input fields based on user checking the box
  #display or hide Tier 2b checkbox based on user checking the box
  observe({
    toggle("t2aD", condition = input$checkT2A, anim = TRUE, time = 0.5, animType = "slide")
    toggle("t2aND", condition = input$checkT2A, anim = TRUE, time = 0.5, animType = "slide")
    toggle("checkT2B", condition = input$checkT2A, anim = TRUE, time = 0.5, animType = "slide")
  })
  
  
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
    toggle("checkT4E", condition = input$checkT4D, anim = TRUE, time = 0.5, animType = "slide")
    
  })
  
  #display or hide Tier 4e input fields based on user checking the box
  observe({
    toggle("t4eD", condition = input$checkT4E, anim = TRUE, time = 0.5, animType = "slide")
    toggle("t4eND", condition = input$checkT4E, anim = TRUE, time = 0.5, animType = "slide")
    toggle("checkT4F", condition = input$checkT4E, anim = TRUE, time = 0.5, animType = "slide")
    
  })
  
  #display or hide Tier 4d input fields based on user checking the box
  observe({
    toggle("t4fD", condition = input$checkT4F, anim = TRUE, time = 0.5, animType = "slide")
    toggle("t4fND", condition = input$checkT4F, anim = TRUE, time = 0.5, animType = "slide")
    
  })
  
  
  
  #function: flag rows for QC checks
  flagFunc <- function() {
    
    #create column "Premise"
    allFlags <- myData()
    allFlags$Premise <- "exp"
    
    
    
    #begin logical QC checks
    #tier1
    QC1 <- try(if("Tier1" %in% colnames(allFlags)) {
      subset(allFlags, Tier1 != input$t1D & Tier1 != input$t1ND & Tier1 != "N/A" & Tier1 != "NA")
    })
    
    QC2 <- try(if("Tier1" %in% colnames(allFlags)) {
      subset(allFlags, Tier1 == input$t1D & is.na(Tier2))
    })
    
    QC3 <- try(if("Tier1" %in% colnames(allFlags)) { 
      subset(allFlags, Tier1 == input$t1ND & Tier2 == input$t2aD)
    })
    
    QC4 <- try(if("Tier1" %in% colnames(allFlags)) {
      subset(allFlags, Tier1 == input$t1ND & Tier3 != 0)
    })
    
    #tier2
    QC5 <- subset(allFlags, Tier2 != input$t2aD & Tier2 != input$t2aND & Tier2 != "N/A" & Tier2 != "NA")
    QC6 <- subset(allFlags, Tier2 == input$t2aND & Tier3 != 0)
    QC7 <- subset(allFlags, Tier2 == input$t2aD & Tier3 == 0)
    
    QC8 <- try(if("Tier4" %in% colnames(allFlags)) {
      
      subset(allFlags, Tier2 == input$t2aD & is.na(Tier4))
      
    })
    
    QC9 <- try(if("Tier4" %in% colnames(allFlags)) {
      
      subset(allFlags, Tier2 == input$t2aND & Tier4 == input$t4aD)
      
    })
    
    #tier3
    QC10 <- subset(allFlags, Tier3 %% input$mrdIn != 0 & Tier3 != 0)
    
    #tier4
    QC11 <- try(if("Tier4" %in% colnames(allFlags)) {
      
      subset(allFlags, Tier4 != input$t4aD & Tier4 != input$t4aND & Tier4 != "N/A" & Tier4 != "NA")
      
    })
    
    #duplicate visits
    QC12 <- allFlags[duplicated(allFlags[, c("Subject", "Visit")]), ]
    
    #multiple tier 2 columns
    QC13 <- try(if("Tier2b" %in% colnames(allFlags)) {
      
      subset(allFlags, Tier2b != input$t2bD & Tier2b != input$t2bND & Tier2b != "N/A" & Tier2b != "NA")
      
    })
    
    QC14 <- try(if("Tier2b" %in% colnames(allFlags)) {
      
      subset(allFlags, Tier2 == input$t2aD & is.na(Tier2b))
      
    })
    
    QC15 <- try(if("Tier2c" %in% colnames(allFlags)) {
      
      subset(allFlags, Tier2c != input$t2cD & Tier2c != input$t2cND & Tier2c != "N/A" & Tier2c != "NA")
      
    })
    
    QC16 <- try(if("Tier2c" %in% colnames(allFlags)) {
      
      subset(allFlags, Tier2 == input$t2aD & is.na(Tier2c))
      
    })
    
    QC17 <- try(if("Tier4b" %in% colnames(allFlags)) {
      
      subset(allFlags, Tier2 == input$t2aD & is.na(Tier4b))
      
    })
    
    QC18 <- try(if("Tier2b" %in% colnames(allFlags)) {
      
      subset(allFlags, Tier2 == input$t2aND & Tier2b == input$t2bD)
      
    })
    
    QC19 <- try(if("Tier2c" %in% colnames(allFlags)) {
      
      subset(allFlags, Tier2 == input$t2aND & Tier2c == input$t2cD)
      
    })
    
    QC20 <- try(if("Tier4b" %in% colnames(allFlags)) {
      
      subset(allFlags, Tier2 == input$t2aND & Tier4b == input$t4bD)
      
    })
    
    QC21 <- try(if("Tier4b" %in% colnames(allFlags)) {
      
      subset(allFlags, Tier4b != input$t4bD & Tier4b != input$t4bND & Tier4b != "N/A" & Tier4b != "NA")
      
    })
    
    QC22 <- try(if("Tier4c" %in% colnames(allFlags)) {
      
      subset(allFlags, Tier4c != input$t4cD & Tier4c != input$t4cND & Tier4c!= "N/A" & Tier4c != "NA")
      
    })
    
    QC23 <- try(if("Tier4c" %in% colnames(allFlags)) {
      
      subset(allFlags, Tier2b == input$t2bD & is.na(Tier4c))
      
    })
    
    QC24 <- try(if("Tier4c" %in% colnames(allFlags)) {
      
      subset(allFlags, Tier2b == input$t2bND & Tier4c == input$t4cD)
      
    })
    
    QC25 <- try(if("Tier4d" %in% colnames(allFlags)) {
      
      subset(allFlags, Tier4d != input$t4dD & Tier4d != input$t4dND & Tier4d != "N/A" & Tier4d != "NA")
      
    })
    
    QC26 <- try(if("Tier4d" %in% colnames(allFlags)) {
      
      subset(allFlags, Tier2c == input$t2cD & is.na(Tier4d))
      
    })
    
    QC27 <- try(if("Tier4d" %in% colnames(allFlags)) {
      
      subset(allFlags, Tier2c == input$t2cND & Tier4d == input$t4dD)
      
    })
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
    
    try(if(QC13$Premise == "exp") {
      QC13$Premise <- "T2b Discrepant Value"
    }, silent = TRUE)
    
    try(if(QC14$Premise == "exp") {
      QC14$Premise <- "T2(+) w/o Result in T2b"
    }, silent = TRUE)
    
    try(if(QC15$Premise == "exp") {
      QC15$Premise <- "T2c Discrepant Value"
    }, silent = TRUE)
    
    try(if(QC16$Premise == "exp") {
      QC16$Premise <- "T2(+) w/o Result in T2c"
    }, silent = TRUE)
    
    try(if(QC17$Premise == "exp") {
      QC17$Premise <- "T2(+) w/o Result in T4b"
    }, silent = TRUE)
    
    try(if(QC18$Premise == "exp") {
      QC18$Premise <- "T2(-) with T2b(+)"
    }, silent = TRUE)
    
    try(if(QC19$Premise == "exp") {
      QC19$Premise <- "T2(-) with T2c(+)"
    }, silent = TRUE)
    
    try(if(QC20$Premise == "exp") {
      QC20$Premise <- "T2(-) with T4b(+)"
    }, silent = TRUE)
    
    try(if(QC21$Premise == "exp") {
      QC21$Premise <- "T4b Discrepant Value"
    }, silent = TRUE)
    
    try(if(QC22$Premise == "exp") {
      QC22$Premise <- "T4c Discrepant Value"
    }, silent = TRUE)
    
    try(if(QC23$Premise == "exp") {
      QC23$Premise <- "T2b(+) w/o Result in T4c"
    }, silent = TRUE)
    
    try(if(QC24$Premise == "exp") {
      QC24$Premise <- "T2b(-) with T4c(+)"
    }, silent = TRUE)
    
    try(if(QC25$Premise == "exp") {
      QC25$Premise <- "T4d Discrepant Value"
    }, silent = TRUE)
    
    try(if(QC26$Premise == "exp") {
      QC26$Premise <- "T2c(+) w/o Result in T4d"
    }, silent = TRUE)
    
    try(if(QC27$Premise == "exp") {
      QC27$Premise <- "T2c(-) with T4d(+)"
    }, silent = TRUE)
    
    
    
    #combine all rows that have any of the errors above
    errorTable <- try(rbind(QC1, QC2, QC3, QC4, QC5, QC6, QC7, QC8, QC9, QC10, QC11, QC12, QC13, QC14,
                            QC15, QC16, QC17, QC18, QC19, QC20, QC21, QC22, QC23, QC24, QC25, QC26, QC27))
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
      
      return(originalData())
      
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
  
  
  
  #begin list of "Premises" frequency table
  output$premises <- renderTable({
    
    errorTable <- flagFunc()
    
    #frequency table
    countError <- as.data.frame(table(errorTable$Premise))
    
    if(nrow(countError) >= 1) {
      
      names(countError) <- c("Premise", "Count")
      countError <- countError[with(countError, order(-Count)), ]
      return(countError)
      
    } else if(nrow(countError) == 0) {
      
      noCountError <- data.frame("Premise" = ("No errors found"),
                                 "Count" = (0))
      return(noCountError)
    }
    
  })
  #end list of "Premises" frequency table
  
  
  
  #begin "Plot" tab
  output$plot <- renderPlot({
    
    #use subject pivot table to create frequency table of each unique titer AND drop titer value of zero or "-Inf" (AKA no maxTiter)
    countTiter <- as.data.frame(table(pivotTableFunc()$maxTiter))
    names(countTiter) <- c("Titer", "Count")
    countTiter <- countTiter[!(countTiter$Titer == 0 | countTiter$Titer == "-Inf"), ]
    
    #plot titer counts
    
    plot1 <- ggplot(countTiter, aes(x = Titer, y = Count)) + 
      geom_bar(stat = "identity", color = "#337ab7", size = 0.6, fill = "#18bc9c", alpha = 0.7) + 
      geom_text(aes(label = Count), vjust = -0.3, color = "#2c3e50", size = 4.5) + 
      ggtitle("Frequency of Highest Titer (Post-BL) per Subject*")
    
    plot1 + theme(
      axis.ticks.length = unit(10, "pt"),
      plot.title = element_text(color = "#2c3e50", size = 24, face = "bold"),
      axis.title.x = element_text(color = "#2c3e50", size = 20, face = "bold"),
      axis.title.y = element_text(color = "#2c3e50", size = 20, face = "bold"),
      axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 14, hjust = 1),
      axis.line = element_line(color = "#337ab7", size = 1, linetype = "solid"),
      panel.background = element_rect(fill = "#cccccc", color = "#cccccc"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )
    
  })
  
  
  
  #begin "sample size" output field
  output$sampleSize <- renderText({
    
    plotCount <- filter(pivotTableFunc(), maxTiter != 0, maxTiter != "-Inf")
    
    paste("n = ", nrow(plotCount), "    *may include subjects who 1) had titers after missing baseline and/or 2) are not treatment emergent", sep="")
    
  })
  #end "sample size" output field
  #end "Plot" tab
  
  
  
  
  #begin "Summary" tab
  #begin Tier table results
  output$summary1 <- DT::renderDataTable({
    
    statsData <- myData()
    
    tier1Row <- try(if("Tier1" %in% colnames(statsData)) {
      
      rowFunc <- function() {
        
        cat("T1 exists\n");
        
        # num of Tier 1 samples tested
        t1Tested <- nrow(subset(statsData, Tier1 == input$t1D | Tier1 == input$t1ND))
        
        # num of Tier 1 detected samples
        t1Pos <- nrow(subset(statsData, Tier1 == input$t1D))
        
        # Tier 1 positive rate
        t1PR <- round((t1Pos/t1Tested * 100), 2)
        
        #row for Tier 1
        t1Table<- data.frame("TierSubset" = paste0("Tier 1", sep = ""),
                             "SamplesTested" = (t1Tested),
                             "Detected" = (t1Pos),
                             "PositiveRate" = paste(t1PR, "%", sep = ""),
                             row.names = c(" "))
        
        return(t1Table)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        cat("T1 column does not exist\n");
        
        #dummy row with NAs
        t1Table <- data.frame("TierSubset" = ("NA"),
                              "SamplesTested" = ("NA"),
                              "Detected" = ("NA"),
                              "PositiveRate" = (0),
                              row.names = c("Tier 1"))
        
        return(t1Table)
      }
      
    })
    
    
    
    tier2aRow <- try(if("Tier2" %in% colnames(statsData)) {
      
      rowFunc <- function() {
        
        cat("T2 exists\n");
        
        # num of Tier 2 samples tested
        t2aTested <- nrow(subset(statsData, Tier2 == input$t2aD | Tier2 == input$t2aND))
        
        # num of Tier 2 detected samples
        t2aPos <- nrow(subset(statsData, Tier2 == input$t2aD))
        
        # Tier 2 positive rate
        t2aPR <- round((t2aPos/t2aTested * 100), 2)
        
        #row for Tier 2
        t2aTable<- data.frame("TierSubset" = paste0("Tier 2", sep = ""),
                              "SamplesTested" = (t2aTested),
                              "Detected" = (t2aPos),
                              "PositiveRate" = paste(t2aPR, "%", sep = ""),
                              row.names = c("  "))
        
        return(t2aTable)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        cat("T2 column does not exist\n");
        
        #dummy row with NAs
        t2aTable<- data.frame("TierSubset" = ("NA"),
                              "SamplesTested" = ("NA"),
                              "Detected" = ("NA"),
                              "PositiveRate" = (0),
                              row.names = c("Tier 2"))
        
        return(t2aTable)
      }
      
    })
    
    
    
    tier2bRow <- try(if("Tier2b" %in% colnames(statsData)) {
      
      rowFunc <- function() {
        
        cat("T2b exists\n");
        
        # num of Tier 2b samples tested
        t2bTested <- nrow(subset(statsData, Tier2b == input$t2bD | Tier2b == input$t2bND))
        
        # num of Tier 2b detected samples
        t2bPos <- nrow(subset(statsData, Tier2b == input$t2bD))
        
        # Tier 2b positive rate
        t2bPR <- round((t2bPos/t2bTested * 100), 2)
        
        #row for Tier 2b
        t2bTable<- data.frame("TierSubset" = paste0("Tier 2b", sep = ""),
                              "SamplesTested" = (t2bTested),
                              "Detected" = (t2bPos),
                              "PositiveRate" = paste(t2bPR, "%", sep = ""),
                              row.names = c("   "))
        
        return(t2bTable)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        cat("T2b column does not exist\n");
        
        #dummy row with NAs
        t2bTable<- data.frame("TierSubset" = ("NA"),
                              "SamplesTested" = ("NA"),
                              "Detected" = ("NA"),
                              "PositiveRate" = (0),
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
        
        # Tier 2c positive rate
        t2cPR <- round((t2cPos/t2cTested * 100), 2)
        
        #row for Tier 2c
        t2cTable<- data.frame("TierSubset" = paste0("Tier 2c", sep = ""),
                              "SamplesTested" = (t2cTested),
                              "Detected" = (t2cPos),
                              "PositiveRate" = paste(t2cPR, "%", sep = ""),
                              row.names = c("    "))
        
        return(t2cTable)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        cat("T2c column does not exist\n");
        
        #dummy row with NAs
        t2cTable<- data.frame("TierSubset" = ("NA"),
                              "SamplesTested" = ("NA"),
                              "Detected" = ("NA"),
                              "PositiveRate" = (0),
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
        
        # Tier 2d positive rate
        t2dPR <- round((t2dPos/t2dTested * 100), 2)
        
        #row for Tier 2d
        t2dTable<- data.frame("TierSubset" = paste0("Tier 2d", sep = ""),
                              "SamplesTested" = (t2dTested),
                              "Detected" = (t2dPos),
                              "PositiveRate" = paste(t2dPR, "%", sep = ""),
                              row.names = c("     "))
        
        return(t2dTable)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        cat("T2d column does not exist\n");
        
        #dummy row with NAs
        t2dTable<- data.frame("TierSubset" = ("NA"),
                              "SamplesTested" = ("NA"),
                              "Detected" = ("NA"),
                              "PositiveRate" = (0),
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
        
        # Tier 4 positive rate
        t4aPR <- round((t4aPos/t4aTested * 100), 2)
        
        #row for Tier 4
        t4aTable<- data.frame("TierSubset" = paste0("Tier 4", sep = ""),
                              "SamplesTested" = (t4aTested),
                              "Detected" = (t4aPos),
                              "PositiveRate" = paste(t4aPR, "%", sep = ""),
                              row.names = c("      "))
        
        return(t4aTable)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        cat("T4 column does not exist\n");
        
        #dummy row with NAs
        t4aTable<- data.frame("TierSubset" = ("NA"),
                              "SamplesTested" = ("NA"),
                              "Detected" = ("NA"),
                              "PositiveRate" = (0),
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
        
        # Tier 4b positive rate
        t4bPR <- round((t4bPos/t4bTested * 100), 2)
        
        #row for Tier 4b
        t4bTable<- data.frame("TierSubset" = paste0("Tier 4b", sep = ""),
                              "SamplesTested" = (t4bTested),
                              "Detected" = (t4bPos),
                              "PositiveRate" = paste(t4bPR, "%", sep = ""),
                              row.names = c("       "))
        
        return(t4bTable)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        cat("T4b column does not exist\n");
        
        #dummy row with NAs
        t4bTable<- data.frame("TierSubset" = ("NA"),
                              "SamplesTested" = ("NA"),
                              "Detected" = ("NA"),
                              "PositiveRate" = (0),
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
        
        # Tier 4c positive rate
        t4cPR <- round((t4cPos/t4cTested * 100), 2)
        
        #row for Tier 4c
        t4cTable<- data.frame("TierSubset" = paste0("Tier 4c", sep = ""),
                              "SamplesTested" = (t4cTested),
                              "Detected" = (t4cPos),
                              "PositiveRate" = paste(t4cPR, "%", sep = ""),
                              row.names = c("        "))
        
        return(t4cTable)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        cat("T4c column does not exist\n");
        
        #dummy row with NAs
        t4cTable<- data.frame("TierSubset" = ("NA"),
                              "SamplesTested" = ("NA"),
                              "Detected" = ("NA"),
                              "PositiveRate" = (0),
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
        
        # Tier 4d positive rate
        t4dPR <- round((t4dPos/t4dTested * 100), 2)
        
        #row for Tier 4d
        t4dTable<- data.frame("TierSubset" = paste0("Tier 4d", sep = ""),
                              "SamplesTested" = (t4dTested),
                              "Detected" = (t4dPos),
                              "PositiveRate" = paste(t4dPR, "%", sep = ""),
                              row.names = c("         "))
        
        return(t4dTable)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        cat("T4d column does not exist\n");
        
        #dummy row with NAs
        t4dTable<- data.frame("TierSubset" = ("NA"),
                              "SamplesTested" = ("NA"),
                              "Detected" = ("NA"),
                              "PositiveRate" = (0),
                              row.names = c("Tier 4d"))
        
        return(t4dTable)
      }
      
    })
    
    
    
    tier4eRow <- try(if("Tier4e" %in% colnames(statsData)) {
      
      rowFunc <- function() {
        
        cat("T4e exists\n");
        
        # num of Tier 4e samples tested
        t4eTested <- nrow(subset(statsData, Tier4d == input$t4eD | Tier4e == input$t4eND))
        
        # num of Tier 4e detected samples
        t4ePos <- nrow(subset(statsData, Tier4e == input$t4eD))
        
        # Tier 4e positive rate
        t4ePR <- round((t4ePos/t4eTested * 100), 2)
        
        #row for Tier 4e
        t4eTable<- data.frame("TierSubset" = paste0("Tier 4e", sep = ""),
                              "SamplesTested" = (t4eTested),
                              "Detected" = (t4ePos),
                              "PositiveRate" = paste(t4ePR, "%", sep = ""),
                              row.names = c("                               "))
        
        return(t4eTable)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        cat("T4e column does not exist\n");
        
        #dummy row with NAs
        t4eTable<- data.frame("TierSubset" = ("NA"),
                              "SamplesTested" = ("NA"),
                              "Detected" = ("NA"),
                              "PositiveRate" = (0),
                              row.names = c("Tier 4e"))
        
        return(t4eTable)
      }
      
    })
    
    
    
    tier4fRow <- try(if("Tier4f" %in% colnames(statsData)) {
      
      rowFunc <- function() {
        
        cat("T4f exists\n");
        
        # num of Tier 4f samples tested
        t4fTested <- nrow(subset(statsData, Tier4f == input$t4fD | Tier4f == input$t4fND))
        
        # num of Tier 4f detected samples
        t4fPos <- nrow(subset(statsData, Tier4f == input$t4fD))
        
        # Tier 4f positive rate
        t4fPR <- round((t4fPos/t4fTested * 100), 2)
        
        #row for Tier 4f
        t4fTable<- data.frame("TierSubset" = paste0("Tier 4f", sep = ""),
                              "SamplesTested" = (t4fTested),
                              "Detected" = (t4fPos),
                              "PositiveRate" = paste(t4fPR, "%", sep = ""),
                              row.names = c("                                "))
        
        return(t4fTable)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        cat("T4f column does not exist\n");
        
        #dummy row with NAs
        t4fTable<- data.frame("TierSubset" = ("NA"),
                              "SamplesTested" = ("NA"),
                              "Detected" = ("NA"),
                              "PositiveRate" = (0),
                              row.names = c("Tier 4f"))
        
        return(t4fTable)
      }
      
    })
    
    
    
    baseData <- baselineFunc()
    
    tier1RowBase <- try(if("Tier1" %in% colnames(baseData)) {
      
      rowFunc <- function() {
        
        # num of Tier 1 samples tested
        t1TestedBase <- nrow(subset(baseData, Tier1 == input$t1D | Tier1 == input$t1ND))
        
        # num of Tier 1 detected samples
        t1PosBase <- nrow(subset(baseData, Tier1 == input$t1D))
        
        # Tier 1 positive rate
        t1PRBase <- round((t1PosBase/t1TestedBase * 100), 2)
        
        #row for Tier 1
        t1TableBase <- data.frame("TierSubset" = paste("Tier 1", sep = ""),
                                  "SamplesTested" = (t1TestedBase),
                                  "Detected" = (t1PosBase),
                                  "PositiveRate" = paste(t1PRBase, "%", sep = ""),
                                  row.names = c("          "))
        
        return(t1TableBase)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        #dummy row with NAs
        t1TableBase <- data.frame("TierSubset" = ("NA"),
                                  "SamplesTested" = ("NA"),
                                  "Detected" = ("NA"),
                                  "PositiveRate" = (0),
                                  row.names = c("Tier 1 "))
        
        return(t1TableBase)
      }
      
    })
    
    
    
    tier2aRowBase <- try(if("Tier2" %in% colnames(baseData)) {
      
      rowFunc <- function() {
        
        # num of Tier 2 samples tested
        t2aTestedBase <- nrow(subset(baseData, Tier2 == input$t2aD | Tier2 == input$t2aND))
        
        # num of Tier 2 detected samples
        t2aPosBase <- nrow(subset(baseData, Tier2 == input$t2aD))
        
        # Tier 2 positive rate
        t2aPRBase <- round((t2aPosBase/t2aTestedBase * 100), 2)
        
        #row for Tier 2
        t2aTableBase<- data.frame("TierSubset" = paste("Tier 2", sep = ""),
                                  "SamplesTested" = (t2aTestedBase),
                                  "Detected" = (t2aPosBase),
                                  "PositiveRate" = paste(t2aPRBase, "%", sep = ""),
                                  row.names = c("           "))
        
        return(t2aTableBase)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        #dummy row with NAs
        t2aTableBase<- data.frame("TierSubset" = ("NA"),
                                  "SamplesTested" = ("NA"),
                                  "Detected" = ("NA"),
                                  "PositiveRate" = (0),
                                  row.names = c("Tier 2 "))
        
        return(t2aTableBase)
      }
      
    })
    
    
    
    tier2bRowBase <- try(if("Tier2b" %in% colnames(baseData)) {
      
      rowFunc <- function() {
        
        # num of Tier 2b samples tested
        t2bTestedBase <- nrow(subset(baseData, Tier2b == input$t2bD | Tier2b == input$t2bND))
        
        # num of Tier 2b detected samples
        t2bPosBase <- nrow(subset(baseData, Tier2b == input$t2bD))
        
        # Tier 2b positive rate
        t2bPRBase <- round((t2bPosBase/t2bTestedBase * 100), 2)
        
        #row for Tier 2b
        t2bTableBase<- data.frame("TierSubset" = paste0("Tier 2b", sep = ""),
                                  "SamplesTested" = (t2bTestedBase),
                                  "Detected" = (t2bPosBase),
                                  "PositiveRate" = paste(t2bPRBase, "%", sep = ""),
                                  row.names = c("            "))
        
        return(t2bTableBase)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        #dummy row with NAs
        t2bTableBase<- data.frame("TierSubset" = ("NA"),
                                  "SamplesTested" = ("NA"),
                                  "Detected" = ("NA"),
                                  "PositiveRate" = (0),
                                  row.names = c("Tier 2b "))
        
        return(t2bTableBase)
      }
      
    })
    
    
    
    tier2cRowBase <- try(if("Tier2c" %in% colnames(baseData)) {
      
      rowFunc <- function() {
        
        # num of Tier 2c samples tested
        t2cTestedBase <- nrow(subset(baseData, Tier2c == input$t2cD | Tier2c == input$t2cND))
        
        # num of Tier 2c detected samples
        t2cPosBase <- nrow(subset(baseData, Tier2c == input$t2cD))
        
        # Tier 2c positive rate
        t2cPRBase <- round((t2cPosBase/t2cTestedBase * 100), 2)
        
        #row for Tier 2c
        t2cTableBase<- data.frame("TierSubset" = paste0("Tier 2c", sep = ""),
                                  "SamplesTested" = (t2cTestedBase),
                                  "Detected" = (t2cPosBase),
                                  "PositiveRate" = paste(t2cPRBase, "%", sep = ""),
                                  row.names = c("             "))
        
        return(t2cTableBase)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        #dummy row with NAs
        t2cTableBase<- data.frame("TierSubset" = ("NA"),
                                  "SamplesTested" = ("NA"),
                                  "Detected" = ("NA"),
                                  "PositiveRate" = (0),
                                  row.names = c("Tier 2c "))
        
        return(t2cTableBase)
      }
      
    })
    
    
    
    tier2dRowBase <- try(if("Tier2d" %in% colnames(baseData)) {
      
      rowFunc <- function() {
        
        # num of Tier 2d samples tested
        t2dTestedBase <- nrow(subset(baseData, Tier2d == input$t2dD | Tier2d == input$t2dND))
        
        # num of Tier 2d detected samples
        t2dPosBase <- nrow(subset(baseData, Tier2d == input$t2dD))
        
        # Tier 2d positive rate
        t2dPRBase <- round((t2dPosBase/t2dTestedBase * 100), 2)
        
        #row for Tier 2d
        t2dTableBase<- data.frame("TierSubset" = paste0("Tier 2d", sep = ""),
                                  "SamplesTested" = (t2dTestedBase),
                                  "Detected" = (t2dPosBase),
                                  "PositiveRate" = paste(t2dPRBase, "%", sep = ""),
                                  row.names = c("              "))
        
        return(t2dTableBase)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        #dummy row with NAs
        t2dTableBase<- data.frame("TierSubset" = ("NA"),
                                  "SamplesTested" = ("NA"),
                                  "Detected" = ("NA"),
                                  "PositiveRate" = (0),
                                  row.names = c("Tier 2d "))
        
        return(t2dTableBase)
      }
      
    })
    
    
    
    tier4aRowBase <- try(if("Tier4" %in% colnames(baseData)) {
      
      rowFunc <- function() {
        
        # num of Tier 4 samples tested
        t4aTestedBase <- nrow(subset(baseData, Tier4 == input$t4aD | Tier4 == input$t4aND))
        
        # num of Tier 4 detected samples
        t4aPosBase <- nrow(subset(baseData, Tier4 == input$t4aD))
        
        # Tier 4 positive rate
        t4aPRBase <- round((t4aPosBase/t4aTestedBase * 100), 2)
        
        #row for Tier 4
        t4aTableBase<- data.frame("TierSubset" = paste0("Tier 4", sep = ""),
                                  "SamplesTested" = (t4aTestedBase),
                                  "Detected" = (t4aPosBase),
                                  "PositiveRate" = paste(t4aPRBase, "%", sep = ""),
                                  row.names = c("               "))
        
        return(t4aTableBase)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        #dummy row with NAs
        t4aTableBase<- data.frame("TierSubset" = ("NA"),
                                  "SamplesTested" = ("NA"),
                                  "Detected" = ("NA"),
                                  "PositiveRate" = (0),
                                  row.names = c("Tier 4 "))
        
        return(t4aTableBase)
      }
      
    })
    
    
    
    tier4bRowBase <- try(if("Tier4b" %in% colnames(baseData)) {
      
      rowFunc <- function() {
        
        # num of Tier 4b samples tested
        t4bTestedBase <- nrow(subset(baseData, Tier4b == input$t4bD | Tier4b == input$t4bND))
        
        # num of Tier 4b detected samples
        t4bPosBase <- nrow(subset(baseData, Tier4b == input$t4bD))
        
        # Tier 4b positive rate
        t4bPRBase <- round((t4bPosBase/t4bTestedBase * 100), 2)
        
        #row for Tier 4b
        t4bTableBase<- data.frame("TierSubset" = paste0("Tier 4b", sep = ""),
                                  "SamplesTested" = (t4bTestedBase),
                                  "Detected" = (t4bPosBase),
                                  "PositiveRate" = paste(t4bPRBase, "%", sep = ""),
                                  row.names = c("                "))
        
        return(t4bTableBase)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        #dummy row with NAs
        t4bTableBase<- data.frame("TierSubset" = ("NA"),
                                  "SamplesTested" = ("NA"),
                                  "Detected" = ("NA"),
                                  "PositiveRate" = (0),
                                  row.names = c("Tier 4b "))
        
        return(t4bTableBase)
      }
      
    })
    
    
    
    tier4cRowBase <- try(if("Tier4c" %in% colnames(baseData)) {
      
      rowFunc <- function() {
        
        # num of Tier 4c samples tested
        t4cTestedBase <- nrow(subset(baseData, Tier4c == input$t4cD | Tier4c == input$t4cND))
        
        # num of Tier 4c detected samples
        t4cPosBase <- nrow(subset(baseData, Tier4c == input$t4cD))
        
        # Tier 4c positive rate
        t4cPRBase <- round((t4cPosBase/t4cTestedBase * 100), 2)
        
        #row for Tier 4c
        t4cTableBase<- data.frame("TierSubset" = paste0("Tier 4c", sep = ""),
                                  "SamplesTested" = (t4cTestedBase),
                                  "Detected" = (t4cPosBase),
                                  "PositiveRate" = paste(t4cPRBase, "%", sep = ""),
                                  row.names = c("                 "))
        
        return(t4cTableBase)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        #dummy row with NAs
        t4cTableBase<- data.frame("TierSubset" = ("NA"),
                                  "SamplesTested" = ("NA"),
                                  "Detected" = ("NA"),
                                  "PositiveRate" = (0),
                                  row.names = c("Tier 4c "))
        
        return(t4cTableBase)
      }
      
    })
    
    
    
    tier4dRowBase <- try(if("Tier4d" %in% colnames(baseData)) {
      
      rowFunc <- function() {
        
        # num of Tier 4d samples tested
        t4dTestedBase <- nrow(subset(baseData, Tier4d == input$t4dD | Tier4d == input$t4dND))
        
        # num of Tier 4d detected samples
        t4dPosBase <- nrow(subset(baseData, Tier4d == input$t4dD))
        
        # Tier 4d positive rate
        t4dPRBase <- round((t4dPosBase/t4dTestedBase * 100), 2)
        
        #row for Tier 4d
        t4dTableBase<- data.frame("TierSubset" = paste0("Tier 4d", sep = ""),
                                  "SamplesTested" = (t4dTestedBase),
                                  "Detected" = (t4dPosBase),
                                  "PositiveRate" = paste(t4dPRBase, "%", sep = ""),
                                  row.names = c("                  "))
        
        return(t4dTableBase)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        #dummy row with NAs
        t4dTableBase<- data.frame("TierSubset" = ("NA"),
                                  "SamplesTested" = ("NA"),
                                  "Detected" = ("NA"),
                                  "PositiveRate" = (0),
                                  row.names = c("Tier 4d "))
        
        return(t4dTableBase)
      }
      
    })
    
    
    
    tier4eRowBase <- try(if("Tier4e" %in% colnames(baseData)) {
      
      rowFunc <- function() {
        
        # num of Tier 4e samples tested
        t4eTestedBase <- nrow(subset(baseData, Tier4e == input$t4eD | Tier4e == input$t4eND))
        
        # num of Tier 4e detected samples
        t4ePosBase <- nrow(subset(baseData, Tier4e == input$t4eD))
        
        # Tier 4e positive rate
        t4ePRBase <- round((t4ePosBase/t4eTestedBase * 100), 2)
        
        #row for Tier 4e
        t4eTableBase<- data.frame("TierSubset" = paste0("Tier 4e", sep = ""),
                                  "SamplesTested" = (t4eTestedBase),
                                  "Detected" = (t4ePosBase),
                                  "PositiveRate" = paste(t4ePRBase, "%", sep = ""),
                                  row.names = c("                                 "))
        
        return(t4eTableBase)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        #dummy row with NAs
        t4eTableBase<- data.frame("TierSubset" = ("NA"),
                                  "SamplesTested" = ("NA"),
                                  "Detected" = ("NA"),
                                  "PositiveRate" = (0),
                                  row.names = c("Tier 4e "))
        
        return(t4eTableBase)
      }
      
    })
    
    
    
    tier4fRowBase <- try(if("Tier4f" %in% colnames(baseData)) {
      
      rowFunc <- function() {
        
        # num of Tier 4f samples tested
        t4fTestedBase <- nrow(subset(baseData, Tier4f == input$t4fD | Tier4f == input$t4fND))
        
        # num of Tier 4f detected samples
        t4fPosBase <- nrow(subset(baseData, Tier4f == input$t4fD))
        
        # Tier 4f positive rate
        t4fPRBase <- round((t4fPosBase/t4fTestedBase * 100), 2)
        
        #row for Tier 4f
        t4fTableBase<- data.frame("TierSubset" = paste0("Tier 4f", sep = ""),
                                  "SamplesTested" = (t4fTestedBase),
                                  "Detected" = (t4fPosBase),
                                  "PositiveRate" = paste(t4fPRBase, "%", sep = ""),
                                  row.names = c("                                  "))
        
        return(t4fTableBase)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        #dummy row with NAs
        t4fTableBase<- data.frame("TierSubset" = ("NA"),
                                  "SamplesTested" = ("NA"),
                                  "Detected" = ("NA"),
                                  "PositiveRate" = (0),
                                  row.names = c("Tier 4f "))
        
        return(t4fTableBase)
      }
      
    })
    
    
    
    postData <- (postBaselineFunc())
    
    tier1RowPost <- try(if("Tier1" %in% colnames(postData)) {
      
      rowFunc <- function() {
        
        # num of Tier 1 samples tested
        t1TestedPost <- nrow(subset(postData, Tier1 == input$t1D | Tier1 == input$t1ND))
        
        # num of Tier 1 detected samples
        t1PosPost <- nrow(subset(postData, Tier1 == input$t1D))
        
        # Tier 1 positive rate
        t1PRPost <- round((t1PosPost/t1TestedPost * 100), 2)
        
        #row for Tier 1
        t1TablePost <- data.frame("TierSubset" = paste("Tier 1", sep = ""),
                                  "SamplesTested" = (t1TestedPost),
                                  "Detected" = (t1PosPost),
                                  "PositiveRate" = paste(t1PRPost, "%", sep = ""),
                                  row.names = c("                    "))
        
        return(t1TablePost)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        #dummy row with NAs
        t1TablePost <- data.frame("TierSubset" = ("NA"),
                                  "SamplesTested" = ("NA"),
                                  "Detected" = ("NA"),
                                  "PositiveRate" = (0),
                                  row.names = c("Tier 1 "))
        
        return(t1TablePost)
      }
      
    })
    
    
    
    tier2aRowPost <- try(if("Tier2" %in% colnames(postData)) {
      
      rowFunc <- function() {
        
        # num of Tier 2 samples tested
        t2aTestedPost <- nrow(subset(postData, Tier2 == input$t2aD | Tier2 == input$t2aND))
        
        # num of Tier 2 detected samples
        t2aPosPost <- nrow(subset(postData, Tier2 == input$t2aD))
        
        # Tier 2 positive rate
        t2aPRPost <- round((t2aPosPost/t2aTestedPost * 100), 2)
        
        #row for Tier 2
        t2aTablePost<- data.frame("TierSubset" = paste("Tier 2", sep = ""),
                                  "SamplesTested" = (t2aTestedPost),
                                  "Detected" = (t2aPosPost),
                                  "PositiveRate" = paste(t2aPRPost, "%", sep = ""),
                                  row.names = c("                     "))
        
        return(t2aTablePost)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        #dummy row with NAs
        t2aTablePost<- data.frame("TierSubset" = ("NA"),
                                  "SamplesTested" = ("NA"),
                                  "Detected" = ("NA"),
                                  "PositiveRate" = (0),
                                  row.names = c("Tier 2 "))
        
        return(t2aTablePost)
      }
      
    })
    
    
    
    tier2bRowPost <- try(if("Tier2b" %in% colnames(postData)) {
      
      rowFunc <- function() {
        
        # num of Tier 2b samples tested
        t2bTestedPost <- nrow(subset(postData, Tier2b == input$t2bD | Tier2b == input$t2bND))
        
        # num of Tier 2b detected samples
        t2bPosPost <- nrow(subset(postData, Tier2b == input$t2bD))
        
        # Tier 2b positive rate
        t2bPRPost <- round((t2bPosPost/t2bTestedPost * 100), 2)
        
        #row for Tier 2b
        t2bTablePost<- data.frame("TierSubset" = paste0("Tier 2b", sep = ""),
                                  "SamplesTested" = (t2bTestedPost),
                                  "Detected" = (t2bPosPost),
                                  "PositiveRate" = paste(t2bPRPost, "%", sep = ""),
                                  row.names = c("                      "))
        
        return(t2bTablePost)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        #dummy row with NAs
        t2bTablePost<- data.frame("TierSubset" = ("NA"),
                                  "SamplesTested" = ("NA"),
                                  "Detected" = ("NA"),
                                  "PositiveRate" = (0),
                                  row.names = c("Tier 2b "))
        
        return(t2bTablePost)
      }
      
    })
    
    
    
    tier2cRowPost <- try(if("Tier2c" %in% colnames(postData)) {
      
      rowFunc <- function() {
        
        # num of Tier 2c samples tested
        t2cTestedPost <- nrow(subset(postData, Tier2c == input$t2cD | Tier2c == input$t2cND))
        
        # num of Tier 2c detected samples
        t2cPosPost <- nrow(subset(postData, Tier2c == input$t2cD))
        
        # Tier 2c positive rate
        t2cPRPost <- round((t2cPosPost/t2cTestedPost * 100), 2)
        
        #row for Tier 2c
        t2cTablePost<- data.frame("TierSubset" = paste0("Tier 2c", sep = ""),
                                  "SamplesTested" = (t2cTestedPost),
                                  "Detected" = (t2cPosPost),
                                  "PositiveRate" = paste(t2cPRPost, "%", sep = ""),
                                  row.names = c("                       "))
        
        return(t2cTablePost)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        #dummy row with NAs
        t2cTablePost<- data.frame("TierSubset" = ("NA"),
                                  "SamplesTested" = ("NA"),
                                  "Detected" = ("NA"),
                                  "PositiveRate" = (0),
                                  row.names = c("Tier 2c "))
        
        return(t2cTablePost)
      }
      
    })
    
    
    
    tier2dRowPost <- try(if("Tier2d" %in% colnames(postData)) {
      
      rowFunc <- function() {
        
        # num of Tier 2d samples tested
        t2dTestedPost <- nrow(subset(postData, Tier2d == input$t2dD | Tier2d == input$t2dND))
        
        # num of Tier 2d detected samples
        t2dPosPost <- nrow(subset(postData, Tier2d == input$t2dD))
        
        # Tier 2d positive rate
        t2dPRPost <- round((t2dPosPost/t2dTestedPost * 100), 2)
        
        #row for Tier 2d
        t2dTablePost<- data.frame("TierSubset" = paste0("Tier 2d", sep = ""),
                                  "SamplesTested" = (t2dTestedPost),
                                  "Detected" = (t2dPosPost),
                                  "PositiveRate" = paste(t2dPRPost, "%", sep = ""),
                                  row.names = c("                        "))
        
        return(t2dTablePost)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        #dummy row with NAs
        t2dTablePost<- data.frame("TierSubset" = ("NA"),
                                  "SamplesTested" = ("NA"),
                                  "Detected" = ("NA"),
                                  "PositiveRate" = (0),
                                  row.names = c("Tier 2d "))
        
        return(t2dTablePost)
      }
      
    })
    
    
    
    tier4aRowPost <- try(if("Tier4" %in% colnames(postData)) {
      
      rowFunc <- function() {
        
        # num of Tier 4 samples tested
        t4aTestedPost <- nrow(subset(postData, Tier4 == input$t4aD | Tier4 == input$t4aND))
        
        # num of Tier 4 detected samples
        t4aPosPost <- nrow(subset(postData, Tier4 == input$t4aD))
        
        # Tier 4 positive rate
        t4aPRPost <- round((t4aPosPost/t4aTestedPost * 100), 2)
        
        #row for Tier 4
        t4aTablePost<- data.frame("TierSubset" = paste0("Tier 4", sep = ""),
                                  "SamplesTested" = (t4aTestedPost),
                                  "Detected" = (t4aPosPost),
                                  "PositiveRate" = paste(t4aPRPost, "%", sep = ""),
                                  row.names = c("                         "))
        
        return(t4aTablePost)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        #dummy row with NAs
        t4aTablePost<- data.frame("TierSubset" = ("NA"),
                                  "SamplesTested" = ("NA"),
                                  "Detected" = ("NA"),
                                  "PositiveRate" = (0),
                                  row.names = c("Tier 4 "))
        
        return(t4aTablePost)
      }
      
    })
    
    
    
    tier4bRowPost <- try(if("Tier4b" %in% colnames(postData)) {
      
      rowFunc <- function() {
        
        # num of Tier 4b samples tested
        t4bTestedPost <- nrow(subset(postData, Tier4b == input$t4bD | Tier4b == input$t4bND))
        
        # num of Tier 4b detected samples
        t4bPosPost <- nrow(subset(postData, Tier4b == input$t4bD))
        
        # Tier 4b positive rate
        t4bPRPost <- round((t4bPosPost/t4bTestedPost * 100), 2)
        
        #row for Tier 4b
        t4bTablePost<- data.frame("TierSubset" = paste0("Tier 4b", sep = ""),
                                  "SamplesTested" = (t4bTestedPost),
                                  "Detected" = (t4bPosPost),
                                  "PositiveRate" = paste(t4bPRPost, "%", sep = ""),
                                  row.names = c("                          "))
        
        return(t4bTablePost)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        #dummy row with NAs
        t4bTablePost<- data.frame("TierSubset" = ("NA"),
                                  "SamplesTested" = ("NA"),
                                  "Detected" = ("NA"),
                                  "PositiveRate" = (0),
                                  row.names = c("Tier 4b "))
        
        return(t4bTablePost)
      }
      
    })
    
    
    
    tier4cRowPost <- try(if("Tier4c" %in% colnames(postData)) {
      
      rowFunc <- function() {
        
        # num of Tier 4c samples tested
        t4cTestedPost <- nrow(subset(postData, Tier4c == input$t4cD | Tier4c == input$t4cND))
        
        # num of Tier 4c detected samples
        t4cPosPost <- nrow(subset(postData, Tier4c == input$t4cD))
        
        # Tier 4c positive rate
        t4cPRPost <- round((t4cPosPost/t4cTestedPost * 100), 2)
        
        #row for Tier 4c
        t4cTablePost<- data.frame("TierSubset" = paste0("Tier 4c", sep = ""),
                                  "SamplesTested" = (t4cTestedPost),
                                  "Detected" = (t4cPosPost),
                                  "PositiveRate" = paste(t4cPRPost, "%", sep = ""),
                                  row.names = c("                           "))
        
        return(t4cTablePost)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        #dummy row with NAs
        t4cTablePost<- data.frame("TierSubset" = ("NA"),
                                  "SamplesTested" = ("NA"),
                                  "Detected" = ("NA"),
                                  "PositiveRate" = (0),
                                  row.names = c("Tier 4c "))
        
        return(t4cTablePost)
      }
      
    })
    
    
    
    tier4dRowPost <- try(if("Tier4d" %in% colnames(postData)) {
      
      rowFunc <- function() {
        
        # num of Tier 4d samples tested
        t4dTestedPost <- nrow(subset(postData, Tier4d == input$t4dD | Tier4d == input$t4dND))
        
        # num of Tier 4d detected samples
        t4dPosPost <- nrow(subset(postData, Tier4d == input$t4dD))
        
        # Tier 4d positive rate
        t4dPRPost <- round((t4dPosPost/t4dTestedPost * 100), 2)
        
        #row for Tier 4d
        t4dTablePost<- data.frame("TierSubset" = paste0("Tier 4d", sep = ""),
                                  "SamplesTested" = (t4dTestedPost),
                                  "Detected" = (t4dPosPost),
                                  "PositiveRate" = paste(t4dPRPost, "%", sep = ""),
                                  row.names = c("                            "))
        
        return(t4dTablePost)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        #dummy row with NAs
        t4dTablePost<- data.frame("TierSubset" = ("NA"),
                                  "SamplesTested" = ("NA"),
                                  "Detected" = ("NA"),
                                  "PositiveRate" = (0),
                                  row.names = c("Tier 4d "))
        
        return(t4dTablePost)
      }
      
    })
    
    
    
    tier4eRowPost <- try(if("Tier4e" %in% colnames(postData)) {
      
      rowFunc <- function() {
        
        # num of Tier 4e samples tested
        t4eTestedPost <- nrow(subset(postData, Tier4e == input$t4eD | Tier4e == input$t4eND))
        
        # num of Tier 4e detected samples
        t4ePosPost <- nrow(subset(postData, Tier4e == input$t4eD))
        
        # Tier 4e positive rate
        t4ePRPost <- round((t4ePosPost/t4eTestedPost * 100), 2)
        
        #row for Tier 4e
        t4eTablePost<- data.frame("TierSubset" = paste0("Tier 4e", sep = ""),
                                  "SamplesTested" = (t4eTestedPost),
                                  "Detected" = (t4ePosPost),
                                  "PositiveRate" = paste(t4ePRPost, "%", sep = ""),
                                  row.names = c("                             "))
        
        return(t4eTablePost)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        #dummy row with NAs
        t4eTablePost<- data.frame("TierSubset" = ("NA"),
                                  "SamplesTested" = ("NA"),
                                  "Detected" = ("NA"),
                                  "PositiveRate" = (0),
                                  row.names = c("Tier 4e "))
        
        return(t4eTablePost)
      }
      
    })
    
    
    
    tier4fRowPost <- try(if("Tier4f" %in% colnames(postData)) {
      
      rowFunc <- function() {
        
        # num of Tier 4f samples tested
        t4fTestedPost <- nrow(subset(postData, Tier4f == input$t4fD | Tier4f == input$t4fND))
        
        # num of Tier 4f detected samples
        t4fPosPost <- nrow(subset(postData, Tier4f == input$t4fD))
        
        # Tier 4f positive rate
        t4fPRPost <- round((t4fPosPost/t4fTestedPost * 100), 2)
        
        #row for Tier 4f
        t4fTablePost<- data.frame("TierSubset" = paste0("Tier 4f", sep = ""),
                                  "SamplesTested" = (t4fTestedPost),
                                  "Detected" = (t4fPosPost),
                                  "PositiveRate" = paste(t4fPRPost, "%", sep = ""),
                                  row.names = c("                              "))
        
        return(t4fTablePost)
      }
      
    } else {
      
      noRowFunc <- function() {
        
        #dummy row with NAs
        t4fTablePost<- data.frame("TierSubset" = ("NA"),
                                  "SamplesTested" = ("NA"),
                                  "Detected" = ("NA"),
                                  "PositiveRate" = (0),
                                  row.names = c("Tier 4f "))
        
        return(t4fTablePost)
      }
      
    })
    
    
    
    #row for Total
    totalRow <- function() {
      
      #dummy row with NAs
      totalPlaceholder <- data.frame("TierSubset" = (" "),
                                     "SamplesTested" = (" "),
                                     "Detected" = (" "),
                                     "PositiveRate" = (" "),
                                     row.names = c("Total"))
      
      return(totalPlaceholder)
    }
    
    
    
    #row for Baseline
    BLRow <- function() {
      
      #dummy row with NAs
      BLPlaceholder <- data.frame("TierSubset" = (" "),
                                  "SamplesTested" = (" "),
                                  "Detected" = (" "),
                                  "PositiveRate" = (" "),
                                  row.names = c("Baseline"))
      
      return(BLPlaceholder)
    }
    
    #row for Baseline
    PostBLRow <- function() {
      
      #dummy row with NAs
      PostBLPlaceholder <- data.frame("TierSubset" = (" "),
                                      "SamplesTested" = (" "),
                                      "Detected" = (" "),
                                      "PositiveRate" = (" "),
                                      row.names = c("Post-Baseline"))
      
      return(PostBLPlaceholder)
    }
    
    
    
    #combine all rows for each Tier
    if("Tier1" %in% colnames(baseData)) {
      
      finalTierTable <- try(rbind(tier1Row(), tier2aRow(), tier2bRow(), tier2cRow(), tier2dRow(),
                                  tier4aRow(), tier4bRow(), tier4cRow(), tier4dRow(), tier4eRow(), tier4fRow(),
                                  BLRow(), tier1RowBase(), tier2aRowBase(), tier2bRowBase(), tier2cRowBase(), tier2dRowBase(),
                                  tier4aRowBase(), tier4bRowBase(), tier4cRowBase(), tier4dRowBase(), tier4eRowBase(), tier4fRowBase(),
                                  PostBLRow(), tier1RowPost(), tier2aRowPost(), tier2bRowPost(), tier2cRowPost(), tier2dRowPost(),
                                  tier4aRowPost(), tier4bRowPost(), tier4cRowPost(), tier4dRowPost(), tier4eRowPost(), tier4fRowPost()))
      
      #drop rows that do not have statistics (they do not appear in the dataset)  
      finalTierTable<- subset(finalTierTable, SamplesTested != "NA")
      rbind(totalRow(), finalTierTable)
      
    } else {
      
      altFinalTierTable <- try(rbind(tier2aRow(), tier2bRow(), tier2cRow(), tier2dRow(),
                                     tier4aRow(), tier4bRow(), tier4cRow(), tier4dRow(), tier4eRow(), tier4fRow(),
                                     BLRow(), tier2aRowBase(), tier2bRowBase(), tier2cRowBase(), tier2dRowBase(),
                                     tier4aRowBase(), tier4bRowBase(), tier4cRowBase(), tier4dRowBase(), tier4eRowBase(), tier4fRowBase(),
                                     PostBLRow(), tier2aRowPost(), tier2bRowPost(), tier2cRowPost(), tier2dRowPost(),
                                     tier4aRowPost(), tier4bRowPost(), tier4cRowPost(), tier4dRowPost(), tier4eRowPost(), tier4fRowPost()))
      
      #drop rows that do not have statistics (they do not appear in the dataset)
      altFinalTierTable <- subset(altFinalTierTable, SamplesTested != "NA")
      rbind(totalRow(), altFinalTierTable)
      
    }
    
    
    
  }, options = list(dom = 't', pageLength = 50, ordering = FALSE)
  )
  #end Tier table results
  
  
  
  #begin Treatment Emergent table
  output$summary2 <- DT::renderDataTable({
    
    # num of evaluable subjects in dataset
    numEvalSubjects <- nrow(titerPivot())
    
    # percent of subjects evaluable for TE ADA
    baseRate <- round((numEvalSubjects/nrow(pivotTableFunc()) * 100), 2)
    
    
    
    # num of subjects with positive baselines
    if(bpFunc()$Subject=="EMPTY") {
      
      numBLPosSubjects <- 0
      
    } else {
      
      numBLPosSubjects <- nrow(bpFunc())
      
    }
    
    # percent of subjects positive at baseline
    basePosRate <- round((numBLPosSubjects/numEvalSubjects * 100), 2)
    
    
    
    # num of treatment emergent subjects
    if(treatEmerFunc()$Subject=="EMPTY") {
      
      numTESubjects <- 0
      
    } else {
      
      numTESubjects <- nrow(treatEmerFunc())
      
    }
    
    # percent of subjects that are treatment emergent
    teRate <- round((numTESubjects/numEvalSubjects * 100), 2)
    
    
    
    # num of treatment induced subjects
    numTISubjects <- nrow(subset(treatEmerFunc(), Baseline == 0))
    
    # percent of subjects that are treatment induced
    tiRate <- round((numTISubjects/numEvalSubjects * 100), 2)
    
    
    
    # num of treatment boosted subjects
    numTBSubjects <- nrow(subset(treatEmerFunc(), Baseline != 0))
    
    # percent of subjects that are treatment boosted
    tbRate <- round((numTBSubjects/numEvalSubjects * 100), 2)
    
    
    
    # output table for Treatment Emergence results
    data.frame("Count" = c(numEvalSubjects, numBLPosSubjects, numTESubjects, numTISubjects, numTBSubjects),
               "Rate" = c(paste(baseRate, "%", sep = ""),
                          paste(basePosRate, "%", sep = ""),
                          paste(teRate, "%", sep = ""), 
                          paste(tiRate, "%", sep = ""), 
                          paste(tbRate, "%", sep = "")),
               row.names = c("Evaluable Subjects for Treatment Emergence", "Evaluable Subjects w/ Baseline Positive Result",
                             "Treatment Emergent Subjects", "Treatment-Induced", "Treatment-Boosted"))
    
    
    
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
    highBLTiter <- max(baselineFunc()$Tier3)
    
    # max titer of treatment emergence table
    highPostBLTiter <- max(postBaselineFunc()$Tier3)
    
    
    
    # output table for General Stats results
    data.frame("Sort" = c(numAllSubjects, numUnEvalSubjects, highBLTiter, highPostBLTiter),
               row.names = c("Total Unique Subjects", "Unevaluated Subjects", "Highest Baseline Titer", "Highest Post-Baseline Titer"))
    
    
    
  }, options = list(dom = 't')
  )
  #end General Stats table
  #end "Summary" tab
  
  
  
  #begin "Download All Tables" button
  output$downloadData <- downloadHandler(
    
    #define the file name
    
    filename = function() {
      paste(Sys.Date(), '-IAN-output.xlsx', sep="")
    },
    
    #dump all rows from tables into separate excel sheets
    content = function(file) {
      
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
  
  
  
  #begin "Help" tab
  #begin Download documentation button
  output$downloadGuide <- downloadHandler(
    
    filename <- function() {
      paste("IAN User Guide", "pdf", sep=".")
    },
    
    content <- function(file) {
      file.copy("www/IAN User Guide.pdf", file)
    },
    contentType = "pdf"
  )
  #end Download documentation button
  
  
  
  #begin help instructions
  output$help <- renderUI({
    
    HTML(
      
      '<div id = format>',
      
      '<h3>Data formatting and preprocessing</h3>',
      '<br />',
      
      '<p>Below is the precise phrasing for column headers. These columns are required.</p>',
      '<p><code>Subject | Visit | Tier2 | Tier3</code></p>',
      '<br />',
      '<p>Datasets with additional tier columns are not required, but must also have precise phrasing.</p>',
      '<p><code>Tier1 | Tier2b | Tier2c | Tier2d | Tier4 | Tier4b | Tier4c | Tier4d</code></p>',
      
      '</div>',
      
      '<div id = flags>',
      
      '<h3>List of all possible QC flags</h3>',
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
      
      '</div>',
      
      '<div id = summary>',
      
      '<h3>Flow of summary statistics</h3>',
      '<br />',
      
      '<ol>',
      
      '<li><b>SamplesTested</b> is sum of user-entered Detected and Not Detected values in each Tier</li>',
      '<li><b>Detected</b> is sum of user-entered Detected values in each Tier</li>',
      '<li><b>PositiveRate</b> is # of <b>Detected</b> / # of <b>SamplesTested</b> in each Tier</li>',
      '<li><b>Evaluable Subjects</b> are those that have a Baseline visit and at least 1 follow-up visit</li>',
      '<li><b>Unevaluated Subjects</b> are those that either:</li>',
      '<ul>',
      '<li>missed Baseline</li>',
      '<li>have Baseline visit without any follow-up visits</li>',
      '</ul>',
      '<li><b>Total Unique Subjects</b> is sum of <b>Evaluable</b> and <b>Unevaluated</b> subjects in the study</li>',
      '<li><b>Highest Titer</b> is the maximum Post-Baseline titer in the study</li>',
      
      '</ol>',
      
      '</div>',
      
      '<br />'
      
    )
    
  })
  #end help instructions
  #end "Help" tab
  
  
  
  
  
  
  
  
  
  
  #begin "Plot" tab
  
  #gets the unique Subject list once the original dataset has been loaded
  observeEvent(originalData(), {
    updateSelectInput(session, "subs", choices = distinct(originalData(), Subject))
  })
  
  
  
  #gets the unique variables once the original dataset has been loaded
  #Subject and Visit are prepopulated in the select list
  observeEvent(originalData(), {
    updateSelectInput(session, "vars", choices = names(originalData()), selected = c("Subject", "Visit"))
  })
  
  
  
  #begin "scale" input field
  output$scaleOut <- renderText({
    
    paste("Scale set at 1:", input$scaleIn, " transformation", sep="")
    
  })
  #end "scale" input field
  
  
  
  #create table withSubject, Visit, and additional columns selected by the user
  lineGraphCols <- function() {
    
    #arranges the table based on order user selects variables
    newLinesData <- originalData() %>% dplyr::select(!!!input$vars)
    return(newLinesData)
    
  }
  
  
  
  output$plot2 <- renderPlot({
    
    #takes in user input MRD value
    scaleInt <- input$scaleIn
    
    #subset graphical data by one subject
    currentSubject <- subset(lineGraphCols(), Subject == input$subs)
    
    #drop columns "Subject" and "Visit" from the currentSubject table
    excludedCols <- select(currentSubject, Subject, Visit)
    excludedNames <- c("Subject", "Visit")
    
    #make currentSubject table numeric (excluding Subject and Visit)
    currentSubject <- currentSubject %>% select(-one_of(excludedNames))
    currentSubject[, ] <- lapply(currentSubject[, ], as.numeric)
    
    #recombine columns "Subject" and "Visit" to the numeric table for this subject
    currentSubject <- cbind(excludedCols, currentSubject)
    
    #change first and second variable selections from input$vars to "Primary" and "Secondary" for graphing purposes
    colnames(currentSubject)[3] <- "Primary"
    colnames(currentSubject)[4] <- "Secondary"
    primLine <- as.factor(input$vars[3])
    secLine <- as.factor(input$vars[4])
    
    #function to retrieve the order of visits set in pivTableView()
    visitReorder <- function() {
      
      targetOrder <- colnames(pivTableView())
      return(targetOrder)
      
    }
    
    #rearrange subject visits according to the vector order of columns set in pivTableView()
    currentSubject$Visit <- factor(currentSubject$Visit, levels = visitReorder())
    dfSubject <- currentSubject[order(currentSubject$Visit),]
    print(dfSubject)
    
    
    
    #begin plot for dual y-axis time series data
    
    #set x-axis
    plot2 <- ggplot(dfSubject, aes(x = Visit, group = 1))
    
    #draw first line
    plot2 <- plot2 + geom_line(aes(y = Primary, colour = primLine), size = 1.2) +
      geom_point(aes(y = Primary, colour = primLine), size = 3.5, na.rm = TRUE)
    
    #draw second line
    plot2 <- plot2 + geom_line(aes(y = Secondary/scaleInt, colour = secLine), linetype = "dashed", size = 1.2) +
      geom_point(aes(y = Secondary/scaleInt, colour = secLine), size = 3.5, na.rm = TRUE)
    
    #styling, set secondary y-axis scale, adjust names of x-axis and primary y-axis
    #primary variable choice will be solid line, secondary variable choice will be dashed line
    #blue line is the variable that comes first in the alphabet, green with variable name that comes second to the first variable
    colorVec <- c("#337ab7", "#18bc9c")
    plot2 <- plot2 + scale_colour_manual(values = colorVec)
    plot2 <- plot2 + scale_y_continuous(sec.axis = sec_axis(~.*scaleInt, name = secLine))
    plot2 <- plot2 + labs(x = "Visit", y = primLine, colour = "")
    plot2Title <- paste0("Time Series Trend: ", primLine, " vs. ", secLine, sep = "")
    plot2 <- plot2 + ggtitle(plot2Title)
    
    plot2 + theme(
      plot.title = element_text(color = "#2c3e50", size = 24, face = "bold"),
      legend.text = element_text(size = 14),
      axis.ticks.length = unit(10, "pt"),
      axis.title.x = element_text(color = "#2c3e50", size = 20, face = "bold"),
      axis.title.y = element_text(color = "#2c3e50", size = 20, face = "bold"),
      axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 14),
      axis.line = element_line(color = "#337ab7", size = 1, linetype = "solid"),
      panel.background = element_rect(fill = "#cccccc", color = "#cccccc")
    )
    
    
    
  })
  
  
  
}
#end Shiny server function

shinyApp(ui,server)

