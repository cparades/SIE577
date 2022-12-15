################################
# Provider Aid Application
# SIE 577 Final Project
# Carlos Parades
# Adrian Martinez
################################

# Install package (only need to do this once, comment out after)
install.packages('dplyr')

library(shiny)

# Excel File
library(readr)
library(dplyr)
library(DT)

# Load Data
patientData = read_csv("Data\\patients.csv", show_col_types = FALSE)
encounterData = read_csv("Data\\encounters.csv", show_col_types = FALSE)
conditionData = read_csv("Data\\conditions.csv", show_col_types = FALSE)
medicationData = read_csv("Data\\medications.csv", show_col_types = FALSE)

# Patient List
patientList = patientData["Id"]
patientList["Patient_Names"] = paste(patientData$FIRST, patientData$LAST)
patientList = patientList[order(patientList$Patient_Names),]

# Encounter Types
distinctEncounters = distinct(encounterData,encounterData$ENCOUNTERCLASS)
distinctEncounters$ID = seq.int(nrow(distinctEncounters))
colnames(distinctEncounters)[1] = "ENCOUNTERCLASS"

# Define UI for application that draws
ui <- fluidPage(

    # Application title
    titlePanel(h1("Provider Aid", align = "center")),
    
    p("Provider Aid was designed for simplicity and usability. Instead of only providing one type of analysis to the user, 
      it provides four types of analysis that can be selected via a dropdown menu. Users also have the option to filter through the 
      raw data if they choose to. Each analysis type can be customized using the various input fields shown on the left-hand side of 
      the application, giving users more flexibility and power. The result of the analysis is displayed on the right-hand side in the 
      form of a table and/or plot. This takes away all of the manual synthesizing of data providers had to do before and gives it to 
      them from a few simple clicks."),
    
    br(),
    
    # Sidebar 
    sidebarLayout(
        sidebarPanel(
          
          # Allow the user to select an analysis to perform
          selectInput("functionType","Analysis Type:",
                      c("Raw Data" = "1",
                        "Returning Patients" = "2",
                        "Encounter Counts" = "3",
                        "Patient Event Sequence" = "4",
                        "Top 10 Medications" = "5")),
          
          # Patient Picker
          conditionalPanel(
            condition = "input.functionType == 4",
            
            selectInput("patientName","Patient:", patientList$Patient_Names)
          ),
          
          # Data Range Filter
          conditionalPanel(
            condition = "input.functionType == 5 | input.functionType == 4 | input.functionType == 3 | input.functionType == 2",
            
            dateRangeInput("datarange", "Data Range:",
                           start = "2010-01-01",
                           end = "2021-01-01",
                           min = "1900-01-01",
                           max = "2021-01-01",
                           format = "mm/dd/yyyy",
                           separator = " - ")
          ),
          
          # Plot Option
          conditionalPanel(
            condition = "input.functionType == 3 | input.functionType == 4 | input.functionType == 5",
            
            checkboxInput("plotOption","Display Plot", value = FALSE)
          ),
          
          # Display function text help
          hr(style = "border-top: 1px solid #000000;"),
          h4("Function Help:"),
          textOutput("functionText")
          
        ),

        # Show Results
        mainPanel(
          
          conditionalPanel(
            condition = "input.functionType == 1",
            
            tabsetPanel(
              id = "dataset",
              tabPanel("Patient Data",dataTableOutput("patientTable")),
              tabPanel("Encounter Data",dataTableOutput("encounterTable")),
              tabPanel("Condition Data",dataTableOutput("conditionTable")),
              tabPanel("Medication Data",dataTableOutput("medicationTable"))
            )
          ),
          conditionalPanel(
            condition =  "input.functionType == 2",
            
            DT::dataTableOutput("returnPatientTable")
          ),
          conditionalPanel(
            condition = "input.functionType == 3",
            
            DT::dataTableOutput("encounterCountTotalTable")
          ),
          conditionalPanel(
            condition = "input.functionType == 3 & input.plotOption",
            
            plotOutput("encounterCountPlot")
          ),
          conditionalPanel(
            condition = "input.functionType == 4",
            
            DT::dataTableOutput("patientEventTable")
          ),
          conditionalPanel(
            condition = "input.functionType == 4 & input.plotOption",
            
            plotOutput("patientEventPlot")
          ),
          conditionalPanel(
            condition = "input.functionType == 5",
            
            DT::dataTableOutput("topTenMeds")
          ),
          conditionalPanel(
            condition = "input.functionType == 5 & input.plotOption",
            
            plotOutput("topTenMedPlot")
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Display function description
  output$functionText <- renderText({
    functionText(input$functionType)
  })
  
  # Raw Data Table
  output$patientTable <- DT::renderDataTable({patientData})
  output$encounterTable <- DT::renderDataTable({encounterData})
  output$conditionTable <- DT::renderDataTable({conditionData})
  output$medicationTable <- DT::renderDataTable({medicationData})
  
  # Top 10 Medications (date Range)
  output$topTenMeds <- DT::renderDataTable({
    topTenMedicationTable(c(input$datarange[1], input$datarange[2]))
  })
  output$topTenMedPlot <- renderPlot({
    tenMedTable = topTenMedicationTable(c(input$datarange[1], input$datarange[2]))
    
    if(!is.na(tenMedTable$Code[1]))
    {
      barplot(tenMedTable$COUNT, main="Medication Totals", xlab = "Medication Codes",
              names.arg=tenMedTable$Code, col=tenMedTable$Code)
    }
    else
    {
      plot(1,type="n", xlab="", ylab = "", main = "NO DATA TO PLOT")
    }
    
  })
  
  # Patient Event Sequence
  output$patientEventTable <- DT::renderDataTable({
    patientEventSequenceTable(c(input$datarange[1], input$datarange[2]), input$patientName[1])
  })
  output$patientEventPlot <- renderPlot({
    patientTable = patientEventSequenceTable(c(input$datarange[1], input$datarange[2]), input$patientName[1])
    
    # Bug fix by looking at the first id value and making sure it is a value before plotting
    if(nrow(patientTable) > 0 & !is.na(patientTable$ID[1]))
    {
      plot(patientTable$START,patientTable$ID, ylim=c(0,7.5),
           col = patientTable$ID, cex = 1.5, pch=patientTable$ID + 14, 
           main = "Patient Encounter Timeline", xlab="Date", ylab = "",yaxt="n")
      legend("top", ncol = 3, legend = distinctEncounters$ENCOUNTERCLASS, 
             col=distinctEncounters$ID, pch = distinctEncounters$ID + 14)
    }
    else
    {
      plot(1,type="n", xlab="", ylab = "", main = "NO DATA TO PLOT")
    }
  })
  
  # Encounter Count
  output$encounterCountTotalTable <- DT::renderDataTable({
    encounterCountTable(c(input$datarange[1], input$datarange[2]))
  })
  output$encounterCountPlot <- renderPlot({
    encounterTableforPlot = encounterCountTable(c(input$datarange[1], input$datarange[2]))
    encounterTableforPlot = left_join(encounterTableforPlot,distinctEncounters, by = "ENCOUNTERCLASS")
    
    barplot(encounterTableforPlot$COUNT, main="Encounter Totals", xlab = "Encounter Types",
            names.arg=encounterTableforPlot$ENCOUNTERCLASS, col=encounterTableforPlot$ID)
  })
  
  # Returning Patient
  output$returnPatientTable <- DT::renderDataTable({
    returningPatientTable(c(input$datarange[1], input$datarange[2]))
  })
}

################################
# Analysis Functions
################################

# Function Description Text
functionText <- function(data) {
  
  resultText= ""
  
  if(data[1] == 1) # Raw Data
  {
    resultText = "Display the raw data to the user. Tabs are created for each different table used.
    Users can search search for entries, sort data based on columns, and pick how many entries they want to show per page."
  }
  else if(data[1] == 2) # Returning Patient
  {
    resultText = "Software will return a table of patients that received care for the same condition during different encounters. The table will show
     the condition the patient was treated for as well as the dates they were seen. Users can narrow or expand the date range using the date selector input."
  }
  else if(data[1] == 3) # Encounter Counts
  {
    resultText = "Display a table that shows the total number of encounters inside the date range specified. Encounters include ambulatory,
     emergency, inpatient, outpatient, wellness, and urgent care visits. Users can narrow or expand the date range using the date selector input."
  }
  else if(data[1] == 4) # Patient Event Sequence
  {
    resultText = "Display a sequence of events that shows encounters a patient had. Encounters include ambulatory, emergency, inpatient, outpatient,
     wellness, and urgent care visits. Users can select a patient, select a date range, and select whether they want a plot to be included underneath
     the table of results."
  }
  else if(data[1] == 5) # Top 10 Medications
  {
    resultText = "Display the top 10 medications amongst all patients inside the date range specified. A table of results will be produced which
    displays the number of times a medication was used. Users can narrow or expand the date range using the date selector input."
  }
  
  return(resultText)
}

# Top 10 Medication Calculation
topTenMedicationTable <- function(data) {
  # Get distinct medication data only
  distinctMedData = distinct(medicationData,medicationData$CODE,medicationData$DESCRIPTION)
  colnames(distinctMedData)[1] = "Code"
  
  # Filter data by date
  filteredMedData = medicationData[medicationData$START > as.POSIXct(data[1]) & 
                                     medicationData$START < as.POSIXct(data[2]),]
  
  # Count and sort Data
  countMeds = count(filteredMedData, filteredMedData$CODE)
  orderMeds = countMeds[order(-countMeds$n),]
  colnames(orderMeds)[1] = "Code"
  
  # Create Final Table
  top10 = left_join(orderMeds,distinctMedData, by ='Code') [0:10,]
  colnames(top10)[3] = "Medication"
  
  # Rename column to count
  colnames(top10)[2] = "COUNT"
  
  return (top10)
}

# Patient Event Sequence
patientEventSequenceTable <- function(dates,name) {
  # Filter Encounter Data by date and order
  filteredEncounterData = encounterData[encounterData$START> as.POSIXct(dates[1]) & 
                                          encounterData$START < as.POSIXct(dates[2]), 
                                        c("PATIENT","START","ENCOUNTERCLASS")]
  filteredEncounterData = filteredEncounterData[order(filteredEncounterData$START),]
  
  # 00185faa-2760-4218-9bf5-db301acf8274
  patientEncounter = filteredEncounterData[filteredEncounterData$PATIENT == 
                                             patientList[patientList$Patient_Names == name,c("Id")]$Id,]
  
  # Add values to encounter to plot
  patientEncounter = left_join(patientEncounter,distinctEncounters, by = "ENCOUNTERCLASS")
  
  # Remove Patient ID Column
  patientEncounter = subset(patientEncounter, select = -c(PATIENT))
  
  # Return final table
  return (patientEncounter)
}

# Encounter Count
encounterCountTable <- function(dates) {
  # Filter Encounter Data by date and order
  encounterDataSubset = encounterData[encounterData$START> as.POSIXct(dates[1]) & encounterData$START < as.POSIXct(dates[2]),
                                      c("START","ENCOUNTERCLASS","CODE")]
  encounterDataSubset = encounterDataSubset[order(encounterDataSubset$START),]
  
  # Get data for all types of encounters
  ambulatoryData = encounterDataSubset[encounterDataSubset$ENCOUNTERCLASS == "ambulatory",]
  wellnessData = encounterDataSubset[encounterDataSubset$ENCOUNTERCLASS == "wellness",]
  outpatientData = encounterDataSubset[encounterDataSubset$ENCOUNTERCLASS == "outpatient",]
  urgentcareData = encounterDataSubset[encounterDataSubset$ENCOUNTERCLASS == "urgentcare",]
  emergencyData = encounterDataSubset[encounterDataSubset$ENCOUNTERCLASS == "emergency",]
  inpatientData = encounterDataSubset[encounterDataSubset$ENCOUNTERCLASS == "inpatient",]
  
  # Create a totals table
  totalsTable = data.frame(ENCOUNTERCLASS = c("ambulatory","wellness","outpatient","urgentcare","emergency","inpatient"),
                           COUNT = c(nrow(ambulatoryData),nrow(wellnessData),nrow(outpatientData),nrow(urgentcareData),
                                     nrow(emergencyData),nrow(inpatientData)))
  totalsTable = totalsTable[order(-totalsTable$COUNT),]
  
  # Return final table
  return (totalsTable)
}

# Returning Patient
returningPatientTable <- function(data) {
  # Filter Condition Data by date
  filteredConditionData = conditionData[conditionData$START> as.Date(data[1]) & conditionData$START < as.Date(data[2]),]
  
  # Distinct Conditions
  distinctConditions = distinct(filteredConditionData,filteredConditionData$CODE)
  
  # Create Result Table
  resultTable = data.frame(matrix(ncol=5,nrow=0))
  colnames(resultTable) = c("PATIENT","n","DATES","CODE","DESCRIPTION")
  
  if (nrow(distinctConditions) > 0)
  {
    # Loop Through Distinct conditions
    for (i in 1:nrow(distinctConditions))
    {
      condition = as.numeric(distinctConditions[i,1])
      conditionDesctiption = filteredConditionData[filteredConditionData$CODE == condition,c("DESCRIPTION")] [1,]
      
      # Filter Data to one condition
      tempFilteredConditionData = filteredConditionData[filteredConditionData$CODE == condition,]
      
      # Count how many times each patient came in for condition
      tempPatientCount = count(tempFilteredConditionData,tempFilteredConditionData$PATIENT)
      
      # Remove patients that were only seen once for the condition
      tempPatientCount = tempPatientCount[tempPatientCount$n > 1,]
      colnames(tempPatientCount)[1] = "PATIENT"
      
      # Next condition if no patients were seen more than once for the condition
      if (nrow(tempPatientCount) < 1)
      {
        next 
      }
      
      # Add dates and condition to table
      dateList = list(c("DummyValue"))
      for (k in 1:nrow(tempPatientCount))
      {
        patient = as.character(tempPatientCount[k,1])
        dateList = append(dateList,c(tempFilteredConditionData[tempFilteredConditionData$PATIENT == patient,c("START")]))
      }
      dateList[[1]] = NULL
      
      tempPatientCount["DATES"] = data.frame(I(dateList))
      tempPatientCount["CODE"] = rep(condition,nrow(tempPatientCount))
      tempPatientCount["DESCRIPTION"] = conditionDesctiption
      
      resultTable = rbind(resultTable,tempPatientCount)
    }
  }
  
  # Replace patient ID with name
  colnames(resultTable)[1] = "Id"
  resultTable = left_join(resultTable,patientList, by = "Id")
  
  # Rename column to count
  colnames(resultTable)[2] = "COUNT"
  
  # Order Columns for display
  resultTable <- resultTable[,c(6,2,3,4,5)]
  colnames(resultTable)[1] = "PATIENT"
  
  # Return final table
  return (resultTable)
}

# Run the application 
shinyApp(ui = ui, server = server)
