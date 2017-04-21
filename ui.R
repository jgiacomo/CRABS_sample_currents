
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel(p(img(src="Accium_Full_Logo.png", height = 75, width = 197,
                   style="vertical-align:bottom;margin:0px 50px 0px 0px"),
               "Get sample currents from CRABS analysis")),
  
  # Sidebar to select the runlog file, the CRABS analysis file, which ion you
  # want currents for, and the units of current.
  sidebarLayout(
    sidebarPanel(
      
      fileInput('runlogF', 'Select the runlog file'),
      fileInput('analysisF', 'Select the CRABS analysis file',
                accept=c(".ana")),
      radioButtons("currentType", "Select which current you want",
                   choices=c("12C", "13C", "both"), selected=c("13C")),
      radioButtons("currentUnits","Select the units",
                   choiceNames=list("nA", HTML("&mu;A"), "A"),
                   choiceValues=list("nA","uA","A")),
      selectInput("sigfigs", HTML("Select the number of decimal places to show (nA, &mu;A) or significant figures (A)"),
                  choices=c(0,1,2,3,4,5), selected=1)
      
    ),

    # Show a table of the selected sample currents with a download button.
    mainPanel(
      downloadButton('dlCurrents', "Download currents (csv file)"),
      dataTableOutput("currentTable")
    )
  )
))
