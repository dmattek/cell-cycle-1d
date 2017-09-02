require(ggplot2)
require(plotly)

########
## UI ##
########

# names of cc phases to assign based on manual threshold definition
def.vs.cc = c('sub-G1', 'G1', 'S', 'G2/M', 'mult')

tabHistPlotUI =  function(id, label = "Histogram") {
  ns <- NS(id)
  
  tagList(
    h4(
      "Histogram of entire dataset"
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
          ns('inG1cv'),
          'G1 CV',
          value = 0.1,
          min = 0.01,
          step = 0.01
        ),
        numericInput(
          ns('inG2cv'),
          'G2 CV',
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
        uiOutput(ns('uiTabCCperc'))
      ),
      tabPanel(
        'CC Phase %',
        plotCCpercUI(ns('plotCCpercManual'), 'Plot CC Percentages')
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

tabHistPlot = function(input, output, session, in.data) {
  
  
  # Boxplot - download pdf
  callModule(downPlot, "downPlot", 'histplot.pdf', plotHist, TRUE)
  
  output$uiG1loc = renderUI({
    
    ns <- session$ns
    
    loc.dt = in.data()
    if (is.null(loc.dt))
      return(NULL)
    
    locG1loc = round(quantile(loc.dt$y, 0.3))
    locG1min = round(quantile(loc.dt$y, 0.05))
    locG1max = round(quantile(loc.dt$y, 0.95))
    
    sliderInput(
      ns('slG1loc'),
      'Set location of G1 peak',
      value = locG1loc,
      min = locG1min,
      max = locG1max,
      step = 0.1
    )
    
  })
  
  # Calculate count of cells within histogram thresholds set in UI
  # The output is a dt ready for display with plotCCperc module:
  #    group     cc ncells
  # 1:     1     G1    949
  # 2:     1   G2/M    718
  # 3:     1      S    270
  # 4:     1 sub-G1     63
  # 5:     2   G2/M    698
  # 6:     2     G1    946
  getData4CCpercPlot = reactive({
    cat(file = stderr(), 'tabFitModel: getData4CCperc\n')
    
    loc.dt = in.data()
    
    if (is.null(loc.dt))
      return(NULL)
    
    # assign cell cycle phase to each cell based on thresholds set in UI
    # assigns sub-G1, G1, S, G2/M, multiploid (stored in def.vs.cc string vector)
    
    loc.thr.min = floor(min(loc.dt[, y]))
    loc.thr.max = ceiling(max(loc.dt[, y]))
    
    loc.thr.g1.low = input$slG1loc - input$inG1cv * input$slG1loc
    loc.thr.g1.upp = input$slG1loc + input$inG1cv * input$slG1loc
    
    loc.thr.g2.low = input$slG1loc * input$inG2mult - input$inG2cv * input$slG1loc * input$inG2mult
    loc.thr.g2.upp = input$slG1loc * input$inG2mult + input$inG2cv * input$slG1loc * input$inG2mult
    
    loc.dt[, cc := cut(y, 
                       c(loc.thr.min, 
                         loc.thr.g1.low, 
                         loc.thr.g1.upp, 
                         loc.thr.g2.low, 
                         loc.thr.g2.upp, 
                         loc.thr.max), 
                       include.lowest = TRUE, 
                       ordered_result = TRUE,
                       labels = def.vs.cc)]
    
    # count cells per cc phase for every group
    loc.dt.aggr = loc.dt[, .(count = .N), by = .(group, cc)]
    
    return(loc.dt.aggr)
  })
  
  # Display cc percentage in a tab
  # Takes plot-ready data prepared in getData4CCpercPlot
  # and transforms it into wide format:
  #    group sub-G1    G1     S  G2/M
  # 1:     1   0.75 51.10 10.65 37.50
  # 2:     2   0.55 51.55  9.80 38.10
  #
  # Note: tabFitModel module does it other way round:
  # data for tab display is in wide format from fitting and then 
  # converted to long format in getData4CCpercPlot for plotting.
  getData4CCperc = reactive({
    cat(file = stderr(), 'tabFitModel: getData4CCpercPlot\n')
    
    loc.dt = getData4CCpercPlot()
    
    if (is.null(loc.dt))
      return(NULL)
    
    return(calcCCperc(loc.dt, 
                      inParams = def.vs.cc,
                      inParamsCol = 'cc',
                      inMeas = 'count', 
                      inGroup = 'group')
    )
  })
  
  output$uiTabCCperc = renderUI({
    cat(file = stderr(), 'UI uiTabCCperc\n')
    ns <- session$ns
    
    DT::dataTableOutput(ns('outTabCCperc'))
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
                                    text = 'Download')))) %>% formatRound(2:6, 2)
  })
  
  
  plotHist <- function() {
    cat(file=stderr(), "plotHist\n")
    
    # isolate because calculations & plotting take a while
    # re-plotting done upon button press
    loc.dt = in.data()
    
    cat("plotHist on to plot\n\n")
    if (is.null(loc.dt)) {
      cat(file=stderr(), 'plotHist: dt is NULL\n')
      return(NULL)
    }
    
    cat(file=stderr(), 'plotHist: dt not NULL\n')
    
    p.out = ggplot(loc.dt, aes(x = y, y = ..density..)) +
      geom_histogram(bins = input$slNhistBins) +
      geom_vline(xintercept = input$slG1loc, color = 'red') +
      geom_vline(xintercept = input$slG1loc - input$inG1cv * input$slG1loc, color = 'red', linetype = 'dashed') +
      geom_vline(xintercept = input$slG1loc + input$inG1cv * input$slG1loc, color = 'red', linetype = 'dashed') +
      geom_vline(xintercept = input$slG1loc * input$inG2mult, color = 'orange') +
      geom_vline(xintercept = input$slG1loc * input$inG2mult - input$inG2cv * input$slG1loc * input$inG2mult, color = 'orange', linetype = 'dashed') +
      geom_vline(xintercept = input$slG1loc * input$inG2mult + input$inG2cv * input$slG1loc * input$inG2mult, color = 'orange', linetype = 'dashed') +
      facet_wrap(as.formula(paste("~", 'group')), ncol = input$inPlotFacetNcol)
    
    return(p.out)
  }
  
  # display plot
  output$outPlot <- renderPlot({
    locBut = input$butGo
    
    if (locBut == 0) {
      cat(file=stderr(), 'plotHist: Go button not pressed\n')
      
      return(NULL)
    }
    
    plotHist()
  })
  
  
  
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
  
  # download pdf
  callModule(downPlot, "downPlotHist", "histogram_manualThreshold.pdf", plotHist, TRUE)
  
  # Hierarchical - choose to display regular or interactive plot
  output$uiPlot <- renderUI({
    ns <- session$ns
    if (input$plotInt)
      tagList(plotlyOutput(ns("outPlotInt"), height = paste0(input$inPlotHeight, "px")))
    else
      tagList(plotOutput(ns('outPlot'), height = paste0(input$inPlotHeight, "px")))
  })
  
  ####
  ## Plot barplot with cell cycle fractions
  callModule(plotCCperc, 'plotCCpercManual', 
             getData4CCpercPlot, in.fname = 'barplot_manualThreshold.pdf')
  
}
