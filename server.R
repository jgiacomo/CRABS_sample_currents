
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

source("getBatchCurrents.R")

library(shiny)
library(dplyr)

shinyServer(function(input, output) {

  # Get the files from the UI
  runlog <- eventReactive(input$runlogF,{
    input$runlogF$datapath
  })
  analysis <- eventReactive(input$analysisF,{
    input$analysisF$datapath
  })
  
  # Get the summarized results from the sourced function
  df <- reactive({
    validate(need(input$runlogF, message="Select a runlog."))
    validate(need(input$analysisF, message="Select an analysis file."))
    
    getBatchCurrents(runlog(), analysis())
  })
  
  # Apply filters and units according to user selections
  results <- reactive({
    # Filter the ion
    if(input$currentType=="12C"){
      tempdf <- df() %>% select(Pos, Sample.Name, he12C)
    } else if(input$currentType=="13C"){
      tempdf <- df() %>% select(Pos, Sample.Name, he13C)
    } else {
      tempdf <- df()
    }
    
    # Get the multiplication factor to convert the units
    if(input$currentUnits=="nA"){
      unitFactor <- 1e9
    } else if(input$currentUnits=="uA"){
      unitFactor <- 1e6
    } else {
      unitFactor <- 1
    }
    
    # Apply the multiplication factor to any numeric, but not integer, columns
    sigdig <- as.numeric(input$sigfigs)
    tempdf %>% mutate_if(function(col) is.numeric(col) && !is.integer(col),
                         # funs(signif(. * unitFactor, sigdig)))
                         funs(
                           if(unitFactor==1){
                             signif(.*unitFactor, sigdig)
                           } else {
                             round(.*unitFactor, sigdig)
                           }
                         ))
  })
  
  
  # Output the table of currents
  output$currentTable <- renderDataTable(results())
  
  # Output the csv file for download
  output$dlCurrents <- downloadHandler(filename="batch_currents.csv",
                                       content=function(file){
                                         write.csv(results(), file,
                                                   row.names=FALSE)
                                       },
                                       contentType="text/csv")

})
