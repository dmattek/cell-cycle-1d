# Analyzes HOECHST-stained single-cell data
# Fits cell cycle models
#
# Data input - csv with following columns:
# 1. unique cell ids OR cell ids and a grouping column, where cell ids are unique within that group, e.g. FOV
# 2. total hoechst per nucleus
# 3. grouping column, e.g. well or a condition
#
# The entire dataset is a population snapshot at a particular time point

library(shiny)
library(shinyjs) #http://deanattali.com/shinyjs/
library(data.table)

# increase file upload limit
options(shiny.maxRequestSize = 120 * 1024 ^ 2)

shinyServer(function(input, output, session) {
  useShinyjs()
  
  # This is only set at session start
  # we use this as a way to determine which input was
  # clicked in the dataInBoth reactive
  counter <- reactiveValues(
    # The value of inDataGen1,2 actionButton is the number of times they were pressed
    dataGen1     = isolate(input$inDataGen1),
    dataLoadNuc  = isolate(input$inButLoadNuc)
    #dataLoadStim = isolate(input$inButLoadStim)
  )
  
  ####
  ## UI for side panel
  
  # FILE LOAD
  # This button will reset the inFileLoad
  observeEvent(input$inButReset, {
    reset("inFileLoadNuc")  # reset is a shinyjs function
    #reset("inButLoadStim")  # reset is a shinyjs function
  })
  
  # generate random dataset 1
  dataGen1 <- eventReactive(input$inDataGen1, {
    cat("dataGen1\n")
    
    return(userDataGen_cellCycle())
  })
  
  # load main data file
  dataLoadNuc <- eventReactive(input$inButLoadNuc, {
    cat("dataLoadNuc\n")
    locFilePath = input$inFileLoadNuc$datapath
    
    counter$dataLoadNuc <- input$inButLoadNuc - 1
    
    if (is.null(locFilePath) || locFilePath == '')
      return(NULL)
    else {
      return(fread(locFilePath))
    }
  })
  
  # This button will reset the inFileLoad
  observeEvent(input$butReset, {
    reset("inFileLoadNuc")  # reset is a shinyjs function
    #    reset("inFileStimLoad")  # reset is a shinyjs function
    
  })
  
  
  # UI FOR COLUMN SELECTION

  # observer: sets chBcellID true if column names contain cellID
  # here we check the rpesence of 'ObjectNumber' string in column names
  observe({
    locCols = getDataNucCols()
    updateCheckboxInput(session, "chBcellID", value = sum(locCols %like% 'ObjectNumber') > 0)
  })

  # observer: sets chBcellID true if column names contain grouping (e.g. treatment)  
  # here we check the rpesence of 'Treat' string in column names
  observe({
    locCols = getDataNucCols()
    updateCheckboxInput(session, "chBgroup", value = sum(locCols %like% 'Treat') > 0)
  })
  
  output$uiChBcellIDunique = renderUI({
    if(input$chBcellID) {
      checkboxInput('chBtrackUni', 'Cell IDs unique across entire dataset', TRUE)
    }
  })
  
    output$varSelTrackLabel = renderUI({
    cat(file = stderr(), 'UI varSelTrackLabel\n')
      
      if(input$chBcellID) {
        locCols = getDataNucCols()
        locColSel = locCols[locCols %like% 'ObjectNumber'][1] # index 1 at the end in case more matches; select 1st
        
        selectInput(
          'inSelTrackLabel',
          'Select Track Label (e.g. objNuclei_ObjectNumber):',
          locCols,
          width = '100%',
          selected = locColSel
        )
        
      }
  })
  

    # This is to select FOV
    # The value is used to create dataset-wide unique cell ids
    # Cell ids should be uniwue within group selected here
    output$varSelSite = renderUI({
      cat(file = stderr(), 'UI varSelSite\n')
      
      if (input$chBcellID) {
        if (!input$chBtrackUni) {
          locCols = getDataNucCols()
          locColSel = locCols[locCols %like% 'Site' | locCols %like% 'Series' ][1] # index 1 at the end in case more matches; select 1st
          
          selectInput(
            'inSelSite',
            'Select FOV (e.g. Metadata_Site or Metadata_Series):',
            locCols,
            width = '100%',
            selected = locColSel
          )
        }
        
      }
    })
    
    
    
  # This is main field to select plot facet grouping
  # It's typically a column with the entire experimental description,
  # e.g. in Yannick's case it's Stim_All_Ch or Stim_All_S.
  # In Coralie's case it's a combination of 3 columns called Stimulation_...
  output$varSelGroup = renderUI({
    cat(file = stderr(), 'UI varSelGroup\n')
    
    if (input$chBgroup) {
      
      locCols = getDataNucCols()
      
      if (!is.null(locCols)) {
        locColSel = locCols[locCols %like% 'Treat']
        if (length(locColSel) == 0)
          locColSel = locCols[locCols %like% 'Conc'][1] # index 1 at the end in case more matches; select 1st
        else if (length(locColSel) > 1) {
          locColSel = locColSel[1]
        }
        #    cat('UI varSelGroup::locColSel ', locColSel, '\n')
        selectInput(
          'inSelGroup',
          'Select one or more facet groupings (e.g. Metadata_Well, Metadata_TreatConc):',
          locCols,
          width = '100%',
          selected = locColSel,
          multiple = TRUE
        )
      }
    }
  })
  
  output$varSelMeas1 = renderUI({
    cat(file = stderr(), 'UI varSelMeas1\n')
    locCols = getDataNucCols()
    
    if (!is.null(locCols)) {
      locColSel = locCols[locCols %like% 'IntegratedIntensity'][1] # index 1 at the end in case more matches; select 1st
      
      selectInput(
        'inSelMeas1',
        'Select 1st measurement:',
        locCols,
        width = '100%',
        selected = locColSel
      )
    }
  })
  
  
  output$varSelMeas2 = renderUI({
    cat(file = stderr(), 'UI varSelMeas2\n')
    locCols = getDataNucCols()
    
    if (!is.null(locCols) &&
        !(input$inSelMath %in% c('', '1 / '))) {
      locColSel = locCols[locCols %like% 'objNuc_Intensity_MeanIntensity_imErkCor.*'][1] # index 1 at the end in case more matches; select 1st
      
      selectInput(
        'inSelMeas2',
        'Select 2nd measurement',
        locCols,
        width = '100%',
        selected = locColSel
      )
    }
  })
  
  
  # UI for trimming measurement
  output$uiSlMeasTrim = renderUI({
    cat(file = stderr(), 'UI uiSlMeasTrim\n')
    
    if (input$chBmeasTrim) {
      loc.dt  = data4histPlot()
      
      if(is.null(loc.dt))
        return(NULL)
      
      locMeasMin = floor(min(loc.dt$y))
      locMeasMax = ceiling(max(loc.dt$y))
      locMeasQ3 = round(quantile(loc.dt$y, 0.9)) # set initial upper limit to 0.9 quantile
      
      sliderInput(
        'slMeasTrim',
        label = 'Range of values to include',
        step = 0.1,
        min = locMeasMin,
        max = locMeasMax,
        value = c(locMeasMin, locMeasQ3)
      )
      
    }
  })
  
  ####
  ## data processing
  
  dataInBoth <- reactive({
    # Without direct references to inDataGen1,2 and inFileLoad, inDataGen2
    #    does not trigger running this reactive once inDataGen1 is used.
    # This is one of the more nuanced areas of reactive programming in shiny
    #    due to the if else logic, it isn't fetched once inDataGen1 is available
    # The morale is use direct retrieval of inputs to guarantee they are available
    #    for if else logic checks!
    
    locInGen1 = input$inDataGen1
    locInLoadNuc = input$inButLoadNuc
    #locInLoadStim = input$inButLoadStim
    
    cat(
      "dataInBoth\ninGen1: ",
      locInGen1,
      "   prev=",
      isolate(counter$dataGen1),
      "\ninDataNuc: ",
      locInLoadNuc,
      "   prev=",
      isolate(counter$dataLoadNuc),
      # "\ninDataStim: ",
      # locInLoadStim,
      # "   prev=",
      # isolate(counter$dataLoadStim),
      "\n"
    )
    
    # isolate the checks of counter reactiveValues
    # as we set the values in this same reactive
    if (locInGen1 != isolate(counter$dataGen1)) {
      cat("dataInBoth if inDataGen1\n")
      dm = dataGen1()
      # no need to isolate updating the counter reactive values!
      counter$dataGen1 <- locInGen1
    } else if (locInLoadNuc != isolate(counter$dataLoadNuc)) {
      cat("dataInBoth if inDataLoadNuc\n")
      dm = dataLoadNuc()
      # no need to isolate updating the counter reactive values!
      counter$dataLoadNuc <- locInLoadNuc
    } else {
      cat("dataInBoth else\n")
      dm = NULL
    }
    return(dm)
  })
  
  # return column names of the main dt
  getDataNucCols <- reactive({
    cat(file = stderr(), 'getDataNucCols: in\n')
    loc.dt = dataInBoth()
    
    if (is.null(loc.dt))
      return(NULL)
    else
      return(colnames(loc.dt))
  })
  
  
  #############
  ## NEXT: modify here to account for UI choices
  ## In the simplest case one should be able to load a single-column dataset with measurement only
  ## What's passed further to other modules is a dt with 3 columns: cellid, group, measurement
  ## The 1st two columns should be created artificially if no choices are indicated in UI
  
  
  # return dt with an added column with dataset-wide unique object label
  # Typically, object lables are unique within 1 a single FOV
  dataMod <- reactive({
    cat(file = stderr(), 'dataMod\n')
    loc.dt = dataInBoth()
    
    if (is.null(loc.dt))
      return(NULL)
    
    if (!input$chBtrackUni) {
      loc.types = lapply(loc.dt, class)

      # Make sure that UI fields that are checked further down aren't empty
      if(!(input$inSelTrackLabel == "") & !(input$inSelSite == "")) {
        if (loc.types[[input$inSelTrackLabel]] %in% c('numeric', 'integer') &
            loc.types[[input$inSelSite]] %in% c('numeric', 'integer'))
        {
          loc.dt[, trackObjectsLabelUni := paste(sprintf("%03d", get(input$inSelSite)),
                                                 sprintf("%04d", get(input$inSelTrackLabel)),
                                                 sep = "_")]
        } else {
          if (loc.types[[input$inSelTrackLabel]] %in% c('numeric', 'integer')) {
            loc.dt[, trackObjectsLabelUni := paste(sprintf("%s", get(input$inSelSite)),
                                                   sprintf("%04d", get(input$inSelTrackLabel)),
                                                   sep = "_")] 
          } else {
            if (loc.types[[input$inSelSite]] %in% c('numeric', 'integer')) {
              loc.dt[, trackObjectsLabelUni := paste(sprintf("%03d", get(input$inSelSite)),
                                                     sprintf("%s", get(input$inSelTrackLabel)),
                                                     sep = "_")]
            } else {
              loc.dt[, trackObjectsLabelUni := paste(sprintf("%s", get(input$inSelSite)),
                                                     sprintf("%s", get(input$inSelTrackLabel)),
                                                     sep = "_")]
            }
          }
        }
      }
    } else
      loc.dt[, trackObjectsLabelUni := get(input$inSelTrackLabel)]
    

    return(loc.dt)
  })
  
  
  # prepares a data table with 3 columns based on fields with column selection
  # y - measurement value to analyse
  # id - dataset-wide unique cell id
  # group - grouping for facetting
  # This table DOES NOT affect any mods such as data trimming or outlier removal
  data4histPlot <- reactive({
    cat(file = stderr(), 'data4histPlot\n')
    
    loc.dt = dataMod()
    if (is.null(loc.dt))
      return(NULL)
    
    if (input$inSelMath == '')
      loc.s.y = input$inSelMeas1
    else if (input$inSelMath == '1 / ')
      loc.s.y = paste0(input$inSelMath, input$inSelMeas1)
    else
      loc.s.y = paste0(input$inSelMeas1, input$inSelMath, input$inSelMeas2)
    
    # create expression for parsing
    # creates a merged column based on other columns from input
    # used for grouping of plot facets
    if (length(input$inSelGroup) == 0)
      return(NULL)
    loc.s.gr = sprintf("paste(%s, sep=';')",
                       paste(input$inSelGroup, sep = '', collapse = ','))
    
    loc.out = loc.dt[, .(
      y = eval(parse(text = loc.s.y)),
      id = trackObjectsLabelUni,
      group = eval(parse(text = loc.s.gr))
    )]
    
    # Reorder levels according to the output of unique.
    # Once reordered, ggplot will plot correctly.
    # This is to avoid having levels ordered as: '10 um', '100 um', '20 um'...
    loc.out$group = factor(loc.out$group, unique(loc.out$group))
    
    return(loc.out)
  })
  
  # prepares final data table for analysis
  # accounts for outlier removal and data trimming
  # columns y, id, and group kept
  data4histPlotMod <- reactive({
    cat(file = stderr(), 'data4histPlotMod\n')
    
    loc.dt = data4histPlot()
    if (is.null(loc.dt))
      return(NULL)
    
    if (input$chBmeasTrim)
      loc.dt = loc.dt[y >= input$slMeasTrim[1] & y <= input$slMeasTrim[2]]
    
    return(loc.dt)
    
  })
  
  
  
  callModule(tabHistPlot, 'myTabHistPlot', data4histPlotMod)
  callModule(tabFitModel, 'myTabFitModel', data4histPlotMod)
  
})