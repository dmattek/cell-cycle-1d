library(shiny)
library(shinyjs) #http://deanattali.com/shinyjs/

shinyUI(
  fluidPage(useShinyjs(),
            # Include shinyjs
            
            # Application title
            title = "Timecourse Inspector",
            
            sidebarLayout(
              sidebarPanel(
                #Selector for file upload
                fileInput(
                  'inFileLoadNuc',
                  'Select data file (e.g. tCoursesSelected.csv) and press "Load Data"',
                  accept = c('text/csv', 'text/comma-separated-values,text/plain')
                ),
                actionButton("inButLoadNuc", "Load Data"),
                actionButton("butReset", "Reset file input"),
                actionButton('inDataGen1', 'Generate artificial dataset'),
                
                tags$hr(),
                checkboxInput('chBtrackUni', 'Track Label unique across entire dataset', FALSE),
                uiOutput('varSelSite'),
                uiOutput('varSelTrackLabel'),
                uiOutput('varSelGroup'),
                #uiOutput('varSelTime'),
                uiOutput('varSelMeas1'),
                radioButtons(
                  'inSelMath',
                  'Math operation 1st and 2nd meas.:',
                  c(
                    'None' = '',
                    'Divide' = " / ",
                    'Sum' = " + ",
                    'Multiply' = " * ",
                    'Subtract' = ' - ',
                    '1 / X' = '1 / '
                  )
                ),
                uiOutput('varSelMeas2'),
                
                tags$hr(),
                checkboxInput('chBmeasTrim', 'Trim measurement', FALSE),
                uiOutput('uiSlMeasTrim'),
                
                downloadButton('downloadDataClean', 'Download mod\'d data')
              ),
              
              mainPanel(
                tabsetPanel(
                tabPanel(
                  'Manual Thresholds',
                  tabHistPlotUI('myTabHistPlot')
                ),
                tabPanel(
                  'Fit G1G2S0 model',
                  tabFitModelUI('myTabFitModel')
                )
              )
              )
            )
  )
)