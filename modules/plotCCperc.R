require(ggplot2)
require(plotly)
require(scales) # for percentages on y-axis

########
## UI ##
########

plotCCpercUI =  function(id, label = "Plot CC perc") {
  ns <- NS(id)

  tagList(
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
      )
    ),
    uiOutput(ns('uiPlot')),
    downPlotUI(ns('downPlotCCperc'), "Download PDF")
  )
}

############
## server ##
############

# Stacked barplot normalized to 100%
# Accepts in.data with 3 columns (actual column names given in in.cols)
# x - categories to show on the x-axis
# y - count 
# g - groups for colouring stacked bars
plotCCperc = function(input, output, session, 
                      in.data, 
                      in.cols = list(x = 'group', y = 'count', g = 'cc'),
                      in.fname = 'barplot.pdf') {
  ####
  ## Bar plot
  plotBar <- function() {
    cat(file=stderr(), "tabFitModel: plotCCperc\n")
    
    loc.dt = in.data()

    cat("plotHist on to plot\n\n")
    if (is.null(loc.dt)) {
      cat(file=stderr(), 'plotBar: dt is NULL\n')
      return(NULL)
    }
    
    cat(file=stderr(), 'plotBar: dt not NULL\n')
    
    p.out = ggplot(loc.dt, aes_string(x = in.cols[['x']], y = in.cols[['y']])) +
      geom_bar(aes_string(fill = in.cols[['g']]), stat = 'identity', position = 'fill')
    
    p.out = p.out + scale_fill_discrete(name = "CC Phase")

    p.out = p.out + 
      scale_y_continuous(labels = percent) +
      ylab("") +  
      xlab("") +  
      myGgplotTheme +
      theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))

    return(p.out)
  }
  
  
  # display plot
  output$outPlot <- renderPlot({
    locBut = input$butGo
    
    if (locBut == 0) {
      cat(file=stderr(), 'plotBar: Go button not pressed\n')
      
      return(NULL)
    }
    
    plotBar()
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
    
    return( plotly_build(plotBar()))
    
  })
  
  # download pdf
  callModule(downPlot, "downPlotCCperc", in.fname, plotBar, TRUE)
  
  # Choose to display regular or interactive plot
  output$uiPlot <- renderUI({
    ns <- session$ns
    if (input$plotInt)
      tagList(plotlyOutput(ns("outPlotInt"), height = paste0(input$inPlotHeight, "px")))
    else
      tagList(plotOutput(ns('outPlot'), height = paste0(input$inPlotHeight, "px")))
  })
  
}