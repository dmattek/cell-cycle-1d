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
                  accept = c('text/csv', 
                             'text/comma-separated-values,text/plain',
                             'application/gzip', 
                             'application/bz2')
                ),
                actionButton("inButLoadNuc", "Load Data"),
                actionButton("butReset", "Reset file input"),
                actionButton('inDataGen1', 'Generate artificial dataset'),
                
                tags$hr(),
                checkboxInput('chBcellID', 
                              'Dataset contains cell IDs', 
                              FALSE),
                
                uiOutput('uiChBtrackUni'),
                uiOutput('uiChBcellIDunique'),
                uiOutput('varSelSite'),
                uiOutput('varSelTrackLabel'),

                checkboxInput('chBgroup', 'Dataset contains grouping column (e.g. treatment, condition)', FALSE),                
                uiOutput('varSelGroup'),
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
                    '1 / X' = '1 / ',
                    'log10' = 'log10'
                  )
                ),
                uiOutput('varSelMeas2'),
                
                tags$hr(),
                checkboxInput('chBmeasTrim', 'Trim measurement', FALSE),
                uiOutput('uiSlMeasTrim')
              ),
              
              mainPanel(
                tabsetPanel(
                tabPanel(
                  'Manual',
                  tabHistPlotUI('myTabHistPlot')
                ),
                tabPanel(
                  'Model',
                  tabFitModelUI('myTabFitModel')
                )
              )
              )
            )
  )
)