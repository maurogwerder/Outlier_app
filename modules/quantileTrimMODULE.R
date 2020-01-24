#
# Time Course Inspector: Shiny app for plotting time series data
# Author: Maciej Dobrzynski
#
# This is a module of a Shiny web application.
# Outlier identification, selection
helpText.quanTrim = c(numOutliersPerc = 'Percentage of data points to remove from pooled data from all time points.',
                         chBtrajInter = "Linearly interpolate gaps created after removing outlier time points. This option will also interpolate pre-existing NAs and missing time points.",
                         rbOutliersType = 'Choose whether to remove outliers from the top, bottom, or both ends of the pooled data distribution.',
                         slOutliersGapLen = paste0("Time series with gaps longer than the threshold will be removed entirely. ",
                                                   "Shorter gaps can be interpolated or can remain in time series.")
)
# UI-remove-outliers ----
QuanTrimInput <- function(id, label = "modSelOutliers") {
  ns <- NS(id)
  
  shinyjs::useShinyjs()
  
  fluidRow(
    box(title = "Menu",
      fluidRow(
        column(2, 
               numericInput(ns('numOutliersPerc'),
                            label = '% of data',
                            min = 0,
                            max = 100,
                            value = 0, 
                            step = 0.05, width = '100px'),
               bsTooltip(ns('numOutliersPerc'), helpText.quanTrim[["numOutliersPerc"]], placement = "top", trigger = "hover", options = NULL),
               
               checkboxInput(ns('chBtrajInter'), 'Interpolate gaps', value = F),
               bsTooltip(ns('chBtrajInter'), helpText.quanTrim[["chBtrajInter"]], placement = "top", trigger = "hover", options = NULL),
               uiOutput(ns('varSelTimeFreq'))
        ),
        column(2, 
               radioButtons(ns('rbOutliersType'), 
                            label = 'From', 
                            choices = c('top' = 'top', 'top & bottom' = 'mid', 'bottom' = 'bot')),
               bsTooltip(ns('rbOutliersType'), helpText.quanTrim[["rbOutliersType"]], placement = "top", trigger = "hover", options = NULL)
        ),
        column(3,
               sliderInput(ns('slOutliersGapLen'),
                           label = 'Max allowed gap duration',
                           min = 0,
                           max = 10,
                           value = 0, 
                           step = 1),
               bsTooltip(ns('slOutliersGapLen'), helpText.quanTrim[["slOutliersGapLen"]], placement = "top", trigger = "hover", options = NULL)
        ),
        column(3,
               downloadButton(ns('downOutlierCSV'), label = 'CSV with outlier IDs'),
               htmlOutput(ns("txtOutliersPerc"))
        )
        ),
      width=12),
    
    box(uiOutput(ns('uiDistPlot')), width=12))
  
  
}

# Server-remove-outliers ----
QuanTrim <- function(input, output, session, in.data) {
  
  ns = session$ns
  
  # reactive counter to hold number of tracks before and after outlier removal
  nCellsCounter <- reactiveValues(
    nCellsOrig = 0,
    nCellsAfter = 0,
    nOutlierTpts = 0
  )
  
  # reactive vector with cell ids
  vOut = reactiveValues(
    id = NULL
  )
  

  
  # Provide interval between 2 time points (for interpolation of NAs and missing time points)
  output$varSelTimeFreq = renderUI({
    
    cat(file = stdout(), 'server:varSelTimeFreq\n')
    
    if (input$chBtrajInter & input$slOutliersGapLen > 0) {
      numericInput(
        ns('inSelTimeFreq'),
        'Interval between 2 time points',
        min = 0,
        step = 1,
        width = '100%',
        value = 1
      )
    }
  })
  
  # Display number of tracks and outliers  
  output$txtOutliersPerc <- renderText({ 
    cat(file = stdout(), 'modSelOutliers: txtOutliersPerc\n')
    
    sprintf('<b>%d total track(s)<br>%d removed track(s)<br>%d removed point(s)</b><br>', 
            nCellsCounter$nCellsOrig, 
            nCellsCounter$nCellsOrig - nCellsCounter$nCellsAfter,
            nCellsCounter$nOutlierTpts)
  })
  
  # button for downloading CSV with ids of removed tracks
  output$downOutlierCSV <- downloadHandler(
    filename = "FCSVOUTLIERS.csv",
    content = function(file) {
      loc.dt = vOut$id
      
      if (is.null(loc.dt))
        return(NULL)
      else
        write.csv(unique(loc.dt[, ID, with = F]), file, row.names = FALSE, quote = F)
    }
  )
  
  # Plot of value distribution
  output$uiDistPlot <- renderUI({
    ns <- session$ns
    
      
    locDT = in.data()
    
    validate(
      need(locDT != "", "Please select a data set")
    )
    
    output$densPlot = renderPlot({
      
      # main density plot
      locP = ggplot(locDT, aes(x = MEAS)) +
        geom_density()
      
      # Shade regions of the density plot according to
      # value set in input$numOutliersPerc.
      
      # extract data from density plot
      locDTtmp = as.data.table(ggplot_build(locP)$data[[1]])
      
      # shade region on the right
      if (input$rbOutliersType == 'top') {
        cat("input$rbOutliersType == 'top'\n")
        # find position of the right boundary
        
        locQuantR = quantile(locDT[, MEAS], 
                             1 - input$numOutliersPerc * 0.01, 
                             na.rm = T, 
                             type = 3)
        
        cat("locQuantR: ", locQuantR, "\n")
        
        # select only those points of the density plot right to the right boundary
        locDTtmpSub = locDTtmp[x > locQuantR]

        
        # add shaded RIGHT region to the plot
        if (nrow(locDTtmpSub) > 0 )
          locP = locP + 
          geom_area(data = locDTtmpSub, aes(x=x, y=y), fill="red") +
          geom_vline(xintercept = locQuantR, linetype = 'dashed', color = 'red')
      } else 
        # shade region on the left
        if (input$rbOutliersType == 'bot') {
          
          # find position of the right boundary
          locQuantL = quantile(locDT[, MEAS], 
                               input$numOutliersPerc * 0.01, 
                               na.rm = T, 
                               type = 3)
          
          # select only those points of the density plot left to the left boundary
          locDTtmpSub = locDTtmp[x < locQuantL]
          
          # add shaded LEFT region to the plot
          if (nrow(locDTtmpSub) > 0 )
            locP = locP + 
            geom_area(data = locDTtmpSub, aes(x=x, y=y), fill="red") +
            geom_vline(xintercept = locQuantL, linetype = 'dashed', color = 'red')
          
        } else 
          # shade region on the left
          if (input$rbOutliersType == 'mid') {
            
            # find position of the right boundary
            locQuantR = quantile(locDT[, MEAS], 
                                 1 - input$numOutliersPerc * 0.005, 
                                 na.rm = T, 
                                 type = 3)
            
            # find position of the left boundary
            locQuantL = quantile(locDT[, MEAS], 
                                 input$numOutliersPerc * 0.005, 
                                 na.rm = T, 
                                 type = 3)
            
            # select only those points of the density plot left or right of the boundaries
            locDTtmpSubL = locDTtmp[x < locQuantL]
            locDTtmpSubR = locDTtmp[x > locQuantR]
            
            # add shaded LEFT region to the plot
            if (nrow(locDTtmpSubL) > 0 )
              locP = locP + 
              geom_area(data = locDTtmpSubL, aes(x=x, y=y), fill="red") +
              geom_vline(xintercept = locQuantL, linetype = 'dashed', color = 'red')
            
            
            if (nrow(locDTtmpSubR) > 0 )
              locP = locP + 
              geom_area(data = locDTtmpSubR, aes(x=x, y=y), fill="red") +
              geom_vline(xintercept = locQuantR, linetype = 'dashed', color = 'red')
          }
      
      locP = locP +
        xlab('Measurement value') +
        LOCggplotTheme(in.font.base = 16, 
                       in.font.axis.text = 16, 
                       in.font.axis.title = 16, 
                       in.font.strip = 20, 
                       in.font.legend = 16)
      
      return(locP)
      
    })
  
  plotOutput(ns('densPlot'))
  })
  
  # Switch on the option to interpolate gaps only if "Max allowed gap duration" (slOutliersGapLen > 0)
  observe({
    shinyjs::toggleState("chBtrajInter", input$slOutliersGapLen > 0)
  })
  
  # Identify outliers
  # Outlier values are identified based on a distribution of pooled data from MEAS column.
  # There's an option to identify outliers at the left, right, or both ends of the distribution.
  # Time points with outlier measurements are removed entirely.
  # Depending on the length of a gap created by outlier removal, entire trajectories can be removed.
  # The resulting gaps can be interpolated.
  dtReturn = reactive({ 
    cat(file = stdout(), 'modSelOutliers:dtReturn\n')
    
    loc.out = in.data()
    
    if (is.null(loc.out)) {
      return(NULL)
    }
    
    # store the number of trajectories before prunning
    nCellsCounter$nCellsOrig = length(loc.out[, unique(ID)])
    
    # Remove outliers if the field with percentage of data to remove is greater than 0
    if (input$numOutliersPerc > 0) {
      
      # scale all measurement points      
      loc.out[, y.sc := scale(MEAS)]  
      # Identify outlier time points
      # In the UI, user selects the percentage of data to remove from the bottom and/or top of the pooled distribution
      # loc.outpts is a datatable only with outlier time points
      # warning: quantile type = 3: SAS definition: nearest even order statistic.
      switch(input$rbOutliersType,
             'top' = {loc.outpts = loc.out[ y.sc > quantile(y.sc, 1 - input$numOutliersPerc * 0.010, na.rm = T, type = 3)]},
             'mid' = {loc.outpts = loc.out[ y.sc < quantile(y.sc,     input$numOutliersPerc * 0.005, na.rm = T, type = 3) | 
                                              y.sc > quantile(y.sc, 1 - input$numOutliersPerc * 0.005, na.rm = T, type = 3)]},
             'bot' = {loc.outpts = loc.out[ y.sc < quantile(y.sc,     input$numOutliersPerc * 0.010, na.rm = T, type = 3)]}
      )
      
      cat(file = stdout(), '\nmodSelOutliers:dtReturn: Outlier points:\n')
      print(loc.outpts[, ID])
      
      
      # remove tracks if gaps longer than a threshold; interpolate if UI set
      # to do that, we need to calculate the ap length
      if (input$slOutliersGapLen > 0) {
        # remove tracks with gaps equal to or longer than the value set in slOutliersGapLen
        # shorter gaps are interpolated linearly
        
        loc.out = LOCremoveOutTracks(loc.out, loc.outpts, ID, input$slOutliersGapLen, T)
        
        # interpolate gaps due to outliers
        if (input$chBtrajInter) {
          # make sure that time interval is set correctly
          if (input$inSelTimeFreq > 0) {
            
            if( (COLPOSX %in% names(loc.out)) & (COLPOSY %in% names(loc.out)) )
              s.cols = c(MEAS, COLPOSX, COLPOSY)
            else
              s.cols = c(MEAS)
            
            loc.out = LOCinterpolate(loc.out, COLGR, ID, COLRT, s.cols, input$inSelTimeFreq, T)
            
          } # end: if (input$inSelTimeFreq > 0) 
        } # if (input$chBtrajInter)
      } else {
        # remove all tracks regardless of gap length (input$slOutliersGapLen == 0)
        
        loc.out = loc.out[!ID %in% unique(loc.outpts[, ID])]
      } # end if (input$slOutliersGapLen > 0)
      
      # clean
      loc.out[, y.sc := NULL]
      
      # store a vector of outlier timepoints with the corresponding IDs
      vOut$id = loc.outpts
      nCellsCounter$nOutlierTpts = length(loc.outpts[, ID])
      cat(sprintf("%d outlier tpts\n", nCellsCounter$nOutlierTpts))
    } else {
      # no outlier removal
      # !(input$numOutliersPerc > 0)
      loc.outpts = NULL
      vOut = NULL
      
      nCellsCounter$nOutlierTpts = 0
    } # end: if (input$numOutliersPerc > 0)
    
    # count number of trajectories after removing outlier tracks
    nCellsCounter$nCellsAfter = length(loc.out[, unique(ID)])
    # count number of outlier points
    
    # return cleaned dt
    if (nrow(loc.out) < 1)
      return(NULL) else
        return(loc.out)
    
  })
  
  return(dtReturn)
}

