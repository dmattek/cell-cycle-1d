require(ggplot2)
require(plotly)
require(rlist) # provides list.clean f-n
require(DT) # for rendering table


#####
## UI
####

tabFitModelUI =  function(id, label = "Histogram") {
  ns <- NS(id)
  
  tagList(
    h4(
      "Fit G1G2S0 model"
    ),
    fluidRow(
      column(
        4,
        sliderInput(
          ns('slNhistBins'),
          'Number of histogram bins',
          value = 50,
          min = 1,
          max = 100,
          step = 1
        ),
        uiOutput(ns('uiG1loc'))
      ),
      column(
        4,
        numericInput(
          ns('inG2mult'),
          'G2 multiplier',
          value = 2,
          min = 0.1,
          step = 0.1
        ),
        numericInput(
          ns('inG12cv'),
          'G1 and G2 CV',
          value = 0.1,
          min = 0.01,
          step = 0.01
        )
      )
    ),
    tabsetPanel(
      tabPanel(
        'Histogram',
        fluidRow(
          column(
            4,
            checkboxInput(ns('plotInt'), 
                          'Interactive Plot?',
                          value = FALSE),
            actionButton(ns('butGo'), 'Plot!')
          ),
          column(
            4,
            numericInput(
              ns('inPlotHeight'),
              'Display plot height [px]',
              value = 1000,
              min = 100,
              step = 100)
          ),
          column(
            4,
            numericInput(
              ns('inPlotFacetNcol'),
              '#Columns:',
              value = 2,
              min = 1,
              width = '100px',
              step = 1
            ))
        ),
        uiOutput(ns('uiPlot')),
        downPlotUI(ns('downPlotHist'), "Download PDF")
      ),
      tabPanel(
        'Parameters',
        checkboxInput(ns('chbTabStats'), 'Show stats of fitted model', FALSE),
        uiOutput(ns('uiTabCCperc')),
        uiOutput(ns('uiTabStats'))
      ),
      tabPanel(
        'CC Phase %',
        plotCCpercUI(ns('plotCCpercG1G2S0'), 'Plot CC Percentages')
      )
    )
  )
}

############
## server ##
############

# in.data - data table with 3 columns:
# y - measurement value to analyse
# id - dataset-wide unique cell id
# group - grouping for facetting
tabFitModel = function(input, output, session, in.data) {
  
  
  # Set location of G1 peak
  # The default value is set automatically based on 30% quantile
  output$uiG1loc = renderUI({
    
    ns <- session$ns
    
    loc.dt = in.data()
    if (is.null(loc.dt))
      return(NULL)
    
    locG1loc = round(quantile(loc.dt$y, 0.3))
    locG1min = floor(quantile(loc.dt$y, 0.05))
    locG1max = ceiling(quantile(loc.dt$y, 0.95))
    
    sliderInput(
      ns('slG1loc'),
      'Set location of G1 peak',
      value = locG1loc,
      min = locG1min,
      max = locG1max,
      step = 0.1
    )
    
  })
  
  # Fit model to data
  # Takes data in.data - input param of the module - from data4histPlotMod()
  # Calculates historgram by 'group'
  # Applies model fitting to every histogram group
  # Returns dt with fitted params by group in long format.
  # Columns: Estimate, Std. Error, t value, Pr(>|t|), coeff,  group
  fitModel <- reactive({
    cat(file=stderr(), "tabFitModel: fitModel\n")
    
    loc.dt = in.data()
    
    if (is.null(loc.dt)) {
      return(NULL)
    }
    
    # split dt by group column to fit separately every facet histogram
    loc.l = split(loc.dt, by = 'group')
    
    # apply histogram function to every group
    loc.l.hist = lapply(loc.l, function(x) calcDensHist(x, input$slNhistBins))
    
    # apply fitting function
    loc.l.res = lapply(loc.l.hist, function(x) fit.modelG12S0(x, list(G1 = 1, 
                                                                      x1 = input$slG1loc, 
                                                                      s1 = input$inG12cv * input$slG1loc, 
                                                                      G2 = .5, 
                                                                      d = input$inG2mult, 
                                                                      s2 = input$inG12cv * input$slG1loc * input$inG2mult, 
                                                                      S = 1)))
    
    # remove NULL list elements (from failed fitting)
    loc.l.res = list.clean(loc.l.res, fun = is.null, recursive = FALSE)
    
    if (is.null(loc.l.res))
      return(NULL)
    
    # convert the list with fitting params to data.table
    loc.dt.res = as.data.table(do.call(rbind, loc.l.res))
    
    return(loc.dt.res)        
  })
  
  ####
  ## Display tables with fitted results
  
  # full results of all fitted parameters with stats
  output$uiTabStats = renderUI({
    cat(file = stderr(), 'UI uiTabStats\n')
    ns <- session$ns
    
    if(input$chbTabStats) {
      DT::dataTableOutput(ns('outTabStats'))
    }
  })
  
  
  output$outTabStats = DT::renderDataTable(server = FALSE, {
    cat(file = stderr(), 'tabFitModel: outTabStats\n')
    loc.dt = fitModel()
    
    if (is.null(loc.dt))
      return(NULL)
    
    datatable(loc.dt, 
              rownames = FALSE,
              extensions = 'Buttons', 
              options = list(
                dom = 'Bfrtip',
                buttons = list('copy', 
                               'print', 
                               list(extend = 'collection',
                                    buttons = list(list(extend='csv',
                                                        filename = 'hitStats'),
                                                   list(extend='excel',
                                                        filename = 'hitStats'),
                                                   list(extend='pdf',
                                                        filename= 'hitStats')),
                                    text = 'Download')))) %>% formatRound(1:4, 3)
  })
  
  # percentages of cells in selected cell cycle phases
  output$uiTabCCperc = renderUI({
    cat(file = stderr(), 'UI uiTabCCperc\n')
    ns <- session$ns
    
    DT::dataTableOutput(ns('outTabCCperc'))
  })
  
  getData4CCperc = reactive({
    cat(file = stderr(), 'tabFitModel: getData4CCperc\n')
    
    loc.dt = fitModel()
    
    if (is.null(loc.dt))
      return(NULL)
    
    # c('A1', 'A2', 'sa') selects names of fitted params that correspond
    # to contributions of model terms to cell-cycle phases, e.g.
    # A1 -> G1, A2 -> G2, sa -> S
    return(calcCCperc(loc.dt, 
                      inParams = c('G1', 'G2', 'S'), 
                      inMeas = 'Estimate', 
                      inGroup = 'group'))
  })
  
  getData4CCpercPlot = reactive({
    cat(file = stderr(), 'tabFitModel: getData4CCpercPlot\n')
    
    loc.dt = getData4CCperc()
    
    if (is.null(loc.dt))
      return(NULL)
    
    return(melt(loc.dt, id.vars = 'group', variable.name = 'cc', value.name = 'count'))
  })
  
  output$outTabCCperc = DT::renderDataTable(server = FALSE, {
    cat(file = stderr(), 'tabFitModel: outTabCCperc\n')
    
    loc.dt = getData4CCperc()
    
    if (is.null(loc.dt))
      return(NULL)
    
    datatable(loc.dt, 
              rownames = FALSE,
              extensions = 'Buttons', 
              options = list(
                dom = 'Bfrtip',
                buttons = list('copy', 
                               'print', 
                               list(extend = 'collection',
                                    buttons = list(list(extend='csv',
                                                        filename = 'hitStats'),
                                                   list(extend='excel',
                                                        filename = 'hitStats'),
                                                   list(extend='pdf',
                                                        filename= 'hitStats')),
                                    text = 'Download')))) %>% formatRound(2:4, 2)
  })
  

  ####
  ## Plot of histogram with fitted model
  ## Used for static, interactive, and download
  plotHist <- function() {
    cat(file=stderr(), "tabFitModel: plotHist\n")
    
    loc.dt = in.data()
    loc.dt.model = fitModel()
    
    cat("plotHist on to plot\n\n")
    if (is.null(loc.dt) | is.null(loc.dt.model)) {
      cat(file=stderr(), 'plotHist: dt is NULL\n')
      return(NULL)
    }
    
    cat(file=stderr(), 'plotHist: dt not NULL\n')
    
    # setting min and max x-arg for plotting fitted histogram
    loc.y.min = floor(min(loc.dt$y))
    loc.y.max = ceiling(max(loc.dt$y))
    
    # calculate fitted histogram of the entire model
    # calcFittedHist f-n accepts different expressions (2nd param) to act on existing columns of dt given in the 1st param
    loc.dt.fit.all = calcFittedHist(loc.dt.model, 
                                    f.modelG12S0(x, list(A1 = G1, x1 = x1, d = d, s1 = s1, 
                                                         A2 = G2, s2 = s2, 
                                                         sa = S)),
                                    in.group.col = 'group',
                                    in.params.col = 'coeff',
                                    in.meas.col = 'Estimate',
                                    x.min = loc.y.min, 
                                    x.max = loc.y.max)
    
    # calculate fit for G1 peak
    loc.dt.fit.g1 = calcFittedHist(loc.dt.model, 
                                   modelG.norm(x, A1 = G1, x1 = x1, s1 = s1), 
                                   in.group.col = 'group', 
                                   in.params.col = 'coeff',
                                   in.meas.col = 'Estimate',
                                   x.min = loc.y.min, 
                                   x.max = loc.y.max)
    
    # calculate fit for G2 peak
    loc.dt.fit.g2 = calcFittedHist(loc.dt.model, 
                                   modelG.norm(x, A1 = G2, x1 = x1 * d, s1 = s2), 
                                   in.group.col = 'group', 
                                   in.params.col = 'coeff',
                                   in.meas.col = 'Estimate',
                                   x.min = loc.y.min, 
                                   x.max = loc.y.max)
    
    # calculate fit for S-phase
    loc.dt.fit.s  = calcFittedHist(loc.dt.model, 
                                   modelS.lin(x, a = S, x1 = x1, d = d, z1 = 1, z2 = 1), 
                                   in.group.col = 'group', 
                                   in.params.col = 'coeff',
                                   in.meas.col = 'Estimate',
                                   x.min = loc.y.min, 
                                   x.max = loc.y.max)
    
    p.out = ggplot(loc.dt, aes(x = y, y = ..density..)) +
      geom_histogram(bins = input$slNhistBins)
    
    if(!is.null(loc.dt.fit.all))
      p.out = p.out +
      geom_line(data = loc.dt.fit.all, aes(x = x, y = y), color = 'red') +
      geom_line(data = loc.dt.fit.g1,  aes(x = x, y = y), color = 'orange') +
      geom_line(data = loc.dt.fit.g2,  aes(x = x, y = y), color = 'orange') +
      geom_line(data = loc.dt.fit.s,   aes(x = x, y = y), color = 'green')
    
    p.out = p.out +
      facet_wrap(as.formula(paste("~", 'group')), ncol = input$inPlotFacetNcol) +
      myGgplotTheme
    
    return(p.out)
  }
  
  
  # display static plot
  output$outPlot <- renderPlot({
    locBut = input$butGo
    
    if (locBut == 0) {
      cat(file=stderr(), 'plotHist: Go button not pressed\n')
      
      return(NULL)
    }
    
    plotHist()
  })
  
  # display interactive plot
  output$outPlotInt <- renderPlotly({
    # This is required to avoid 
    # "Warning: Error in <Anonymous>: cannot open file 'Rplots.pdf'"
    # When running on a server. Based on:
    # https://github.com/ropensci/plotly/issues/494
    
    locBut = input$butGo
    if (locBut == 0) {
      cat(file=stderr(), 'plotHistInt Go button not pressed\n')
      return(NULL)
    }
    
    if (names(dev.cur()) != "null device") dev.off()
    pdf(NULL)
    
    return( plotly_build(plotHist()))
    
  })
  
  # Choose to display static or interactive plot
  output$uiPlot <- renderUI({
    ns <- session$ns
    if (input$plotInt)
      tagList(plotlyOutput(ns("outPlotInt"), height = paste0(input$inPlotHeight, "px")))
    else
      tagList(plotOutput(ns('outPlot'), height = paste0(input$inPlotHeight, "px")))
  })
  

  
  ####
  # Download pdf of histogram
  callModule(downPlot, "downPlotHist", "histogram_fittedModel.pdf", plotHist, TRUE)
  
  ## Plot barplot with cell cycle fractions
  callModule(plotCCperc, 'plotCCpercG1G2S0', 
             getData4CCpercPlot, in.fname = 'barplot_fittedModel.pdf')
}
