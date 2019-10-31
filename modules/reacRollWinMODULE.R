#'
#' Outlier Detection App: Application to identify outliers in time-series data.
#' Author: Mauro Gwerder
#' 
#' The Module "RollWin" will apply a "Rolling window"-Algorithm to the dataset and allows 
#' single or complete interpolation of all detected outliers. This algorithm is intended 
#' for single-point outliers that can be corrected instead of just removing whole trajectories.
#' 

# UI of the module 
RollWinInput <- function(id, label = "RollWin"){
  
  ns <- NS(id)
  
  fluidRow(
    # plots all trajectories divided by FOV. All trajectories carrying detected outliers will be 
    # coloured red for visualization
    box(title = "Overview",
        selectInput(ns("sel.plotvis"), "select visualization method", 
                    choices = c("line plot","heatmap"), selected = "line plot"),
        plotOutput(ns("plot.overview")),
        actionButton(ns("b.detect"), "detect outliers"),
        downloadButton(ns('downPlot'), 'Download Overview Plot'),
        width = 8),
    
    # Carries all options for the rolling window algorithm. Link.print will return a .csv file
    # with all removed trajectories. Does not work right now.
    box(title = "Controls",
      numericInput(ns("num.TF"), "tolerance factor:", 1.5, min = 0.2, max = 3, step = 0.1),
      numericInput(ns("num.window"), "window size:", 13, min = 7, max = 30, step = 1),
      selectInput(ns("sel.rollmethod"), "select rolling window method",
                  choices = c("median", "IQR"), 
                  selected = "IQR"),
      downloadButton(ns("downRemove"), "print removed trajectories as .csv"),
      width = 4),
    
    # Plots selected trajectory for single interpolation and verification.
    box(title = "Selected Trajectory", 
        plotOutput(ns("plot.single"), height = 250),
        actionButton(ns("b.prevtraj"), "previous trajectory"),
        actionButton(ns("b.nxtraj"),"next trajectory"),
        actionButton(ns("b.interp"), "interpolate"),
        width = 8),
    
    # Sets option for interpolation and Removement of trajectories that are unsuited for interpolation
    box(title = "Interpolation & Removing Options",
        selectInput(ns("sel.interp.method"), "select interpolation method",
                    choices = c("linear", "spline", "stine", "mean", "median", "mode", "Kalman",
                                "Last Observation Carried Forward", "Next Observation Carried Backward", 
                                "Simple Moving Average", "Linear Weighted Moving Average",
                                "Exponential Weighted Moving Average", "Seasonally Decomposed Missing Value Imputation",
                                "random sample"),
                    selected = "linear"),
        actionButton(ns("b.interp.all"), "Interpolate all"),
        br(),
        br(),
        numericInput(ns("num.remove.which"), "Tolerated subsequent outliers:", 1, min = 0, max = 6, step = 1),
        actionButton(ns("b.remove"), "remove all with more subsequent outliers"),
        width = 4)
  )
  
}


# Server of the module
RollWin <- function(input, output, session, in.data){
  
  ns <- session$ns
  
  # Reactive values: All reactive values are used to keep track if the buttons were pressed.
  #                  Resets after effect of their respective if-statement is done
  Rval <- reactiveValues(detect = isolate(input$b.detect),
                         interp = isolate(input$b.interp),
                         remove = isolate(input$b.remove),
                         interpAll = isolate(input$b.interp.all),
                         prevTraj = isolate(input$b.prevtraj),
                         nxTraj = isolate(input$b.nxtraj),
                         count = 0)
  
  # Conversion of the universal column names into a list. Needed for the "heatmap.outliers" function
  l.cols <- list() 
  l.cols$id <- "ID"
  l.cols$time <- "TIME"
  l.cols$meas <- "MEAS"
  l.cols$fov <- "FOV"
  
  
  # Keeps track of which trajectory is currently selected using "input$b.prevtraj" & "input$b.nxtraj"
  trajCount <- reactive({ # works at the moment but would probably be nicer if it wouldn't rely on the raw values of the action buttons
    
    ns <- session$ns
    cat("trajCount\n")
    InPrevTraj <- input$b.prevtraj
    InNxtTraj <- input$b.nxtraj
    InCount <- Rval$count
    
    if (InNxtTraj != isolate(Rval$nxTraj)) {
      
      InCount <- InCount + 1
      Rval$nxTraj <- InNxtTraj
      
    } else if (InPrevTraj != isolate(Rval$prevTraj)) {
      
      InCount <- InCount - 1
      Rval$prevTraj <- InPrevTraj
      
    }
    
    cat("current count is", InCount, "\n")
    if (InCount < 0)
      InCount <- 0
    
    Rval$count <- InCount
    return(InCount)
  })
  
  # adds columns to the datatable: - OUTL.NA: Will add a clipped vector as a column where every 
  #                                           outlier is replaced by NA.
  #                                - GROUP.TRAJ: Marks every trajectory carrying an outlier.
  #                                - OUTL.LOC: Will add a vector as a column where every outlier 
  #                                            is marked with a "1". This is needed to count how many
  #                                            subsequent outliers there are in a trajectory.
  #                                - REMOVE: Marks all trajectory that should be removed for 
  #                                          interpolation. This is done via the function rollwin_index()
  #                                          which uses the OUTL.LOC vector as an input.
  outlDetection <- reactive({
    
    ns <- session$ns
    
    cat("outlDetection\n")
    dm <- in.data()
    InTF <- input$num.TF
    InWinSize <- input$num.window
    InDetect <- input$b.detect
    InMethod <- input$sel.rollmethod
    InRemoveWhich <- input$num.remove.which
    
    if (is.null(dm))
      return(NULL)
    
    # checks whether "input$b.detect" was pressed. Rollwin_loop() will be applied only if this is given.
    if (InDetect != isolate(Rval$detect)) {
      cat("outlDetection if InDetect\n")
      
      dm[, OUTL.NA := rollwin_loop(MEAS, win_length = InWinSize, thresh_val = InTF, method = InMethod, return_loc = F), ID]
      dm[, GROUP.TRAJ := anyNA(OUTL.NA), ID]
      dm[, OUTL.LOC := abs(rollwin_loop(MEAS, win_length = InWinSize, thresh_val = InTF, method = InMethod, return_loc = T)), ID]
      dm[, REMOVE := rollwin_index(OUTL.LOC, thresh_val = InRemoveWhich), ID]
      
      Rval$detect <- InDetect
      
    } else {
      
      #Initialises columns that are needed for plotting.
      cat("outlDetection else\n")
      dm[, GROUP.TRAJ := c(rep(T, nrow(dm)))]
      dm[, OUTL.NA := MEAS]
    }
    
    
    
    return(dm)
  })
  
  # removes all trajectories that are unsuited for interpolation. Suitability is determined by how
  # many subsequent outliers there are in a trajectory, which can be adjusted with input$b.remove.
  removeAll <- reactive({
    ns <- session$ns
    
    cat("removeAll\n")
    dm <- outlDetection()
    InRemoveAll <- input$b.remove
    
    
    if(is.null(dm))
      return(NULL)
    # checks whether "input$b.remove" was pressed. Data will only be removed if this is given.
    if(InRemoveAll != isolate(Rval$remove)) {
    
      dm <- dm[REMOVE == T]
      Rval$remove <- InRemoveAll
      
    }
    
    return(dm)
  })
  
  # Selects all trajectories that were removed by the rollwin_index function. The IDs of these trajectories
  # will be the output of the .csv download file
  removeDownload <- reactive({
    ns <- session$ns
    
    cat("removeDownload\n")
    dm.in <- outlDetection()
    
    if(is.null(dm.in))
      return(NULL)
    
    dm.out <- as.vector(dm.in[REMOVE == F, unique(ID)])
    cat("dm.out: ", dm.out, "\n")
      
    return(dm.out)
  })
  
  # DownloadHandler for all removed trajectories
  output$downRemove <- downloadHandler(
    filename = "removed_Trajectories.csv",
    content = function(file) {
      write.csv(x = removeDownload(), file = file, row.names = FALSE)
    })
  
  # Reactive that checks the clicking of "input$b.interp" which allows the interpolation of
  # detected outliers in currently selected single trajectories. 
  InterpSingle <- reactive({
    ns <- session$ns
    
    dm <- removeAll()
    InTrajCount <- trajCount()
    InInterpCount <- input$b.interp
    InSelInterp <- input$sel.interp.method
    
    if (InInterpCount != isolate(Rval$interp)) {
      
      IDnames <- as.vector(dm[GROUP.TRAJ == T, unique(ID)])
      print(IDnames)
      dm[ID == IDnames[InTrajCount], MEAS := interpolComplete(OUTL.NA, method = InSelInterp)]
      
      Rval$interp <- InInterpCount
    }
    
    return(dm)
    
  })
  
  # Checks if "input$b.interp.all" was pressed. This allows the interpolation of all detected outliers
  # at once. Note that for interpolation unsuited trajectories should be removed first.
  interpAll <- reactive ({
    
    cat("interpAll\n")
    dm <- InterpSingle()
    InInterpAll <- input$b.interp.all
    InSelInterp <- input$sel.interp.method
    
    if(is.null(dm))
      return(NULL)
    
    if(InInterpAll != isolate(Rval$interpAll)){
      cat("InInterpAll: ", InInterpAll, "\n")
      dm <- dm[, MEAS := interpolComplete(OUTL.NA, method = InSelInterp)]
      Rval$interpAll <- InInterpAll
    }
    
    return(dm)
    
  })
  
  # creates a vector containing all IDs which is then used for single trajectory plotting. 
  # Outlier-containing trajectories will be displayed first, with all other trajectories 
  # following. 
  # Sidenote: Inside these two groups,the trajectories are sorted by ID. This will cause 
  # a different order than in the hierClust method.
  trajVec <- reactive({
    
    ns <- session$ns
    cat("trajVec\n")
    dm.in <- interpAll()
    
    if (is.null(dm.in))
      return(NULL)
    
    dm.out <- c(dm.in[GROUP.TRAJ == T, unique(ID)], dm.in[GROUP.TRAJ == F, unique(ID)])
    
    return(dm.out)
  })
  
  # With trajVec(), which carries all IDs, and trajCount(), which carries the information
  # about the current selection, the trajectory to be plotted in "output$plot.single"
  # will be selected.
  singleTrajData <- reactive({
    
    ns <- session$ns
    cat("singleTrajData\n")
    dm.in <- interpAll()
    InTrajVec <- trajVec()
    InTrajCount <- trajCount()
    
    if (is.null(dm.in))
      return(NULL)
    
    # Initialising the column "GROUP.SINGLE"
    dm.in[, GROUP.SINGLE := c(rep(F, nrow(dm.in)))]
    dm.in[is.na(OUTL.NA), GROUP.SINGLE := T]
    dm.out <- dm.in[ID %in% InTrajVec[InTrajCount]]
    
    return(dm.out)
  })
  
  # Function instead of reactive as per:
  # http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  plotOverview <- function() {
    ns <- session$ns
    
    cat("plot.overview\n")
    dm <- interpAll()
    
    InPlotVis <- input$sel.plotvis
    
    if (is.null(dm))
      return(NULL)
    
    if(InPlotVis == "line plot") {
      
      dm.out <- ggplot(dm, aes(x = TIME, y = MEAS, group = ID, color = GROUP.TRAJ)) +
        geom_line(alpha = 0.5, size = 0.4) +
        scale_color_manual(values = c("#000000", "#fc0505"), name = "Detected Outlier", labels = c("No", "Yes")) +
        scale_x_continuous(name = "Time") +
        scale_y_continuous(name = "Measurement") +
        facet_wrap(as.formula(paste0("~", "FOV"))) +
        theme_bw()
      
    } else if(InPlotVis == "heatmap") {
      
      # gives high values to detected outlier points, so that they will be significantly different to undetected values for heatmap plotting
      dm[, HEAT.MEAS := MEAS] 
      binningMax <- dm[,max(OUTL.NA, na.rm = T)]
      dm[GROUP.SINGLE == T, HEAT.MEAS := binningMax + 0.1]
      l.cols.heat <- l.cols
      l.cols.heat$meas <- "HEAT.MEAS"
      binningMax <- dm[,max(OUTL.NA, na.rm = T)] #creates specific threshold to separate outliers from normal data
      markedheat <- c(heat.colors(30, alpha = 1), "#2E64FE") # creates custom color palette to color outliers blue
      
      dm.out <- heatmapforVisual(dm, trim.pos = 0, breaks.in = 30, break.thresh = binningMax, col_in = markedheat, break.man = T, in.list = l.cols.heat)
    }
    return(dm.out)
  }
  
  output$downPlot <- downloadHandler(
    filename = "downloadPlot.pdf",
    content = function(file) {
      pdf(file,
          width = 8,
          height = 5.5)
      print(plotOverview())
      dev.off()
    })
  
  # Function instead of reactive as per:
  # http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  plotSingle <- function(){
    
    ns <- session$ns
    cat("plot.single\n")
    dm.in <- singleTrajData()
    
    if (is.null(dm.in))
      return(NULL)
    
    dm.out <- ggplot(dm.in, aes(x = TIME, y = MEAS, group = ID )) +
      ggtitle(paste("trajectory", dm.in[, unique(ID)])) +
      theme_bw() +
      scale_color_manual(values = c("#000000", "#fc0505"), name = "Detected Outlier", labels = c("No", "Yes")) +
      scale_x_continuous(name = "Time") +
      scale_y_continuous(name = "Measurement") +
      geom_line() +
      geom_point(aes(color = GROUP.SINGLE)) 
      
    
    return(dm.out)
  }
  
  output$plot.overview <- renderPlot({
    
    plotOverview()
    
  })
  
  output$plot.single <- renderPlot({
    
    plotSingle()
    
  })
  
  #callModule(downPlot, "downPlot", in.plot = plotOverview)
}

