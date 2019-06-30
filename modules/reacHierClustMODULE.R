
#'
#' Outlier Detection App: Application to identify outliers in time-series data.
#' Author: Mauro Gwerder
#' 
#' The Module "HierClust" is clustering the data hierarchically and creates a
#' heatmap (using "heatmap.2" from the package "gplots") for visualization.
#' Detected outlier-containing trajectories can be verified and kicked out one
#' by one by the researcher.
#'


HierClusterInput <- function(id,label = "HierClust"){
  # gives all labels into a namespace (can I say it like that?)
  ns <- NS(id)
  
  fluidRow(
    
    # plots the heatmap for an overview of the clustering
    box(title = "Heatmap", 
        plotOutput(ns("plot.heat"), height = "600px"),
        # If there was a mistake with clustering or removing, this will reset the loaded dataset.
        actionButton(ns("b.reset"),"Reset Data"),
        #downPlotInput(ns("downPlot")),
        downloadButton(ns('downPlot'), 'Download Overview Plot'),
        width = 7),
    
    # Selectable options for the function "heatmap.2"
    box(title = "Options for heatmap", selectInput(ns("sel.plot"), "heatmap or dendrogram?", 
                                                   choices = c("heatmap","dendrogram"), selected = "heatmap"), # select plot output
        selectInput(ns("sel.hclust"), "select clustering method",
                    choices = c("single", "ward.D2", "complete", "average", "median"), 
                    selected = "single"), 
        selectInput(ns("sel.dist"), "select distance method", 
                    choices = c("euclidean", "maximum", "manhattan", "canberra" ,"binary", "minkowski"),
                    selected = "euclidean"), 
        selectInput(ns("sel.col"), "select colour scheme", 
                    # need to still change this to maciej's "l.col.pal.dend" palette options
                    choices = c("Greens", "Spectral", "RdYlGn", "BrBG", "Greys"), selected = "Spectral"),
        br(),
        
        #returns a .csv file with all the trajectories that were removed
        downloadButton(ns("downOutl"), "print removed trajectories as .csv"),
        width = 4),
    # Plots single trajectories for verification.
    box(title = "Trajectories", plotOutput(ns("plot.traj"), height = 250),
        actionButton(ns("b.prevtraj"), "previous trajectory"),
        actionButton(ns("b.nxtraj"),"next trajectory"),
        actionButton(ns("b.remove.left"), "remove trajectories (left)"),
        actionButton(ns("b.remove.right"),"remove trajectories (right)"),
        width = 7)
  )
}


HierCluster <- function(input, output, session, in.data) {
  
  ns <- session$ns
  
  # Reactive values: "Rval$trajStatus" stores a vector of flags for keeping track of which 
  #                  trajectories were already removed.
  #                  All reactive values with "counter" in their name are used to keep track
  #                  of which trajectory should be displayed for verification.
  Rval <- reactiveValues(trajStatus = NULL,
                         countRemoveL = isolate(input$b.remove.left),
                         countRemoveR = isolate(input$b.remove.right),
                         countPrev = isolate(input$b.prevtraj),
                         countNxt = isolate(input$b.nxtraj),
                         countReset = isolate(input$b.reset),
                         count = 0,
                         cutvalue = NULL)
  
  # Conversion of the universal column names into a list. Needed for the "heatmap.outliers" function
  l.cols <- list() 
  l.cols$id <- 'ID'
  l.cols$time <- 'TIME'
  l.cols$meas <- 'MEAS'
  l.cols$fov <- 'FOV'
  
  # This reactive environment keeps track of which trajectories are selected for verification in "plot.traj".
  # It also resets this selection whenever the reset button or the remove button are pressed.
  # Inside the if-statements for "reset" and "remove", there are some deliberate side effects.
  # I decided to do it this way because these side-effects are changing a reactive value (trajStatus) that cannot
  # be returned using a reactive. This is an alternative to observeEvent() and is combined with the selection-
  # reset.
  branchCount <- reactive({
    ns <- session$ns
    
    cat("branchCount\n")
    InPrevTraj <- input$b.prevtraj
    InNxtTraj <- input$b.nxtraj
    InRemoveL <- input$b.remove.left
    InRemoveR <- input$b.remove.right
    InResetTraj <- input$b.reset
    InTrajStatus <- Rval$trajStatus
    InCount <- Rval$count
    # checks if and which button was pressed most recently and affects the object "count" respectively.
    if (InNxtTraj != isolate(Rval$countNxt)) {
      
      Rval$count <- InCount + 1
      cat("branchCount if InNxtTraj\n")
      Rval$countNxt <- InNxtTraj
      
    } else if (InPrevTraj != isolate(Rval$countPrev)) {
      
      Rval$count <- InCount - 1
      cat("branchCount if InPrevTraj\n")
      Rval$countPrev <- InPrevTraj
      
    } else if (InRemoveL != isolate(Rval$countRemoveL)) {
      
      Rval$count <- 1
      cat("branchCount if InRemoveL\n")
      
      # converts selected trajectories (marked inside the vector with a "1") to removed 
      # trajectories (marked with a "2")
      InTrajStatus[which(InTrajStatus %in% 1)] <- 2
      Rval$trajStatus <- InTrajStatus
      Rval$countRemoveL <- InRemoveL
      
    } else if (InRemoveR != isolate(Rval$countRemoveR)) {
      
      Rval$count <- 1
      cat("branchCount if InRemoveR\n")
      
      # converts selected trajectories (marked inside the vector with a "0") to removed 
      # trajectories (marked with a "2")
      InTrajStatus[which(InTrajStatus %in% 0)] <- 2
      Rval$trajStatus <- InTrajStatus
      Rval$countRemoveR <- InRemoveR
      
    } else if (InResetTraj != isolate(Rval$countReset)) {
      
      Rval$count <- 1
      cat("branchCount if InResetTraj\n")
      Rval$countReset <- InResetTraj
      
      # causes the vector with information about removed and selected trajectories to reset.
      Rval$trajStatus <- rep(0, length(Rval$trajStatus))
    }
    
    OutCount <- Rval$count
    
    # because negative counts do not make sense for trajectory selection, these values will be 
    # converted to "0".
    if (OutCount < 0)
      OutCount <- 0
    
    return(OutCount)
  })
  
  IDnames <- reactive({
    ns <- session$ns
    
    cat("calling IDnames\n")
    dm.in <- in.data()
    InTrajStatus <- Rval$trajStatus
    
    if(is.null(dm.in))
      return(NULL)
    l.id <- list()
    l.id$complete <- as.vector(dm.in[, unique(ID)])
    l.id$active <- l.id$complete[which(InTrajStatus %in% c(0,1))]
    l.id$outl <- l.id$complete[which(InTrajStatus %in% 1)]
    l.id$update <- l.id$complete[which(InTrajStatus %in% 0)]
    l.id$remove <- l.id$complete[which(InTrajStatus %in% 2)]
    
    return(l.id)
  })
  
  
  # This reactive will return IDs of selected trajectories. The selection will be determined by trimming the 
  # dendrogram step by step. The reactive "branchCount" will determine on which branching the tree will be 
  # trimmed. 
  hierCut <- reactive({
    ns <- session$ns
    
    cat("calling hierCut\n")
    dm.in <- in.data()
    InBranchCount <- branchCount()
    InIDnames <- IDnames()
    InHclustSel <- input$sel.hclust
    InDistSel <- input$sel.dist
    InTrajStatus <- Rval$trajStatus
    InPlotSel <- input$sel.plot
    InColourSel <- input$sel.col
    
    if (is.null(dm.in)) {
      return(NULL)
    }
    
    # As a side effect, this if-statement checks whether the tree isn't cut at all and if any data was removed.
    # If this is the case, the vector stored in "Rval$trajStatus" can be initialised.
    if(InBranchCount == 0 && is.null(InTrajStatus[which(InTrajStatus %in% 2)])){ 
      cat("checkCut\n")
      InTrajStatus <- rep(0, length(dm.in[, unique(ID)]))
      Rval$trajStatus <- InTrajStatus
      return(dm.in)
    } 
    
    # Only IDs that weren't removed (So trajectories that have either the flag "0" or "1" in "Rval$trajStatus")
    # can be returned.
    
    currentValues <- which(InTrajStatus %in% c(0, 1)) 
    dm.cut <- dm.in[ID %in% InIDnames$active] 
    
    # heatmap.outl() is run twice, once to find the current values that are present in the smaller group after cutting the dendrogram,
    # and once to get the corresponding heatmap.
    Rval$cutvalue <- heatmap.outl(dm.cut, 
                           plot = F, 
                           trim.pos = InBranchCount, 
                           in.list = l.cols, 
                           dist.method = InDistSel, 
                           hclust.method = InHclustSel)
    
    dm.out <- heatmap.outl(dm.cut,
                           plot = InPlotSel, 
                           trim.pos = InBranchCount, 
                           in.list = l.cols,
                           dist.method = InDistSel, 
                           hclust.method = InHclustSel,
                           col_in = InColourSel)
    
    return(dm.out)
  })
  
  
  
  # Removed Data will be excluded in this reactive.
  groupedData <- reactive({
    ns <- session$ns
    
    cat("updating groupvector\n")
    dm.in <- in.data()
    cutreeNames <- Rval$cutvalue
    InIDnames <- IDnames()
    InTrajStatus <- Rval$trajStatus
    
    if (is.null(dm.in))
      return(NULL)
    
    # This reactive will only return something if Rval$trajStatus was already initialised in "hierCut()"
    if (is.null(InTrajStatus))
      return(NULL)
    cutreeValues <- which(InIDnames$complete %in% cutreeNames)
    oldValues <- which(InTrajStatus %in% c(0, 1))
    newValues <- oldValues[which(oldValues %in% cutreeValues)]
    emptyValues <- oldValues[ - which(oldValues %in% cutreeValues)] 
    
    # as a side effect, "Rval$trajStatus" will be overwritten with the updated selection
    InTrajStatus[newValues] <- 1
    InTrajStatus[emptyValues] <- 0
    Rval$trajStatus <- InTrajStatus
    
    dm.ID <- InIDnames$active[which(InTrajStatus %in% c(0, 1))]
    dm.out <- dm.in[ID %in% dm.ID]
    
    return(dm.out)
  })
  
  IDnamesRemove <- reactive({
    ns <- session$ns
    
    cat("IDnamesRemove\n")
    InIDnames <- IDnames()
    InTrajStatus <- Rval$trajStatus
    
    if(is.null(InIDnames))
      return(NULL)
    
    selGroup <- InTrajStatus[InTrajStatus %in% 2]
    selGroupRemove <- which(selGroup %in% 2)
    IDnamesRemove <- InIDnames$remove
    
    return(IDnamesRemove)
  })
  
  output$downOutl <- downloadHandler(
    filename = "Removed_Trajectories.csv",
    content = function(file) {
      write.csv(x = IDnamesRemove(), file = file, row.names = FALSE)
    })
  
  # plots a clustered heatmap with a coloured dendrogram which gives information about the selected
  # trajectories.
  # Function instead of reactive as per:
  # http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  plotHierHeat <- function() {
    ns <- session$ns
    
    cat("hierHeatMap\n")

    InHeatmap <- hierCut()
    
    if (is.null(InHeatmap))
      return(NULL)
    
    
    dm.out <- InHeatmap 
    
    return(dm.out)
  }
    
  output$downPlot <- downloadHandler(
    filename = "downloadPlot.pdf",
    content = function(file){
      pdf(file,
          width = 8.5,
          height = 7)
      print(plotHierHeat())
      dev.off()
    })
  
  # Returns two plots (using gridExtra): plot1 will return the selected trajectories for verification.
  # The second plot will return all the remaining plots (not sure if needed). They are divided using
  # their flags stored in "Rval$trajStatus" as a vector.
  plotHierTraj <- reactive({
    ns <- session$ns
    
    cat("hierPlot\n")
    dm.in <- groupedData()
    InIDlist <- IDnames()
    InTrajStatus <- Rval$trajStatus
    heatmap.traj <- as.vector(Rval$cutvalue)
    
    if (is.null(dm.in))
      return(NULL)
    
    # Excludes removed trajectories from the vector, such that the vector and the datatable have the same length.
    if(is.null(InIDlist$update))
      return(NULL)
    
    dm.outliers <- dm.in[ID %in% InIDlist$outl]
    dm.update <- dm.in[ID %in% InIDlist$update]
    
    # plots selected trajectory
    plot1 <- ggplot(dm.outliers, aes(x = TIME, y = MEAS, group = ID)) + 
      ggtitle(paste0("trajectory: ", paste(heatmap.traj, collapse = ", "))) +
      geom_line() 
    # plots all the remaining trajectories
    plot2 <- ggplot(dm.update, aes(x = TIME, y = MEAS, group = ID)) + 
      ggtitle("remaining trajectories") +
      geom_line(alpha = 0.5) 
    # arranges plots side to side
    dm.out <- grid.arrange(plot1, plot2, ncol = 2)
    
    return(dm.out)
  })
  
  output$plot.heat <- renderPlot({
    
    plotHierHeat()
    
  })
  
  output$plot.traj <- renderPlot({
    
    plotHierTraj()
    
  })
  
  #callModule(downPlot, "downPlot", in.plot = plotHierHeat)
}

