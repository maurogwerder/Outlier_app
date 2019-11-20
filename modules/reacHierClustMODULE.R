
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
        actionButton(ns("b.reset"), "Reset Data"),
        downloadButton(ns("downPlot"), "Download Overview Plot"),
        width = 8),
    
    # Selectable options for the function "heatmap.2"
    box(title = "Options for heatmap", selectInput(ns("sel.plot"), "heatmap or dendrogram?", 
                                                   choices = c("heatmap","dendrogram"), selected = "heatmap"), # select plot output
        selectInput(ns("sel.hclust"), "select clustering method",
                    choices = c("single", "ward.D2", "complete", "average"), 
                    selected = "single"), 
        selectInput(ns("sel.dist"), "select distance method", 
                    choices = c("euclidean", "maximum", "manhattan", "canberra" ,"binary", "minkowski"),
                    selected = "euclidean"), 
        selectInput(ns("sel.col"), "select colour scheme", 
                    choices = c("Greens", "Spectral", "RdYlGn", "BrBG", "Greys"), selected = "Spectral"),
        br(),
        actionButton(ns("b.update"), "Update plots"),
        checkboxInput(ns("check.update"), "live plot updating?", value = T),
        height = 700,
        width = 4),
    # Plots single trajectories for verification.
    box(title = "active Trajectory",
        plotOutput(ns("plot.1"), width = "100%"),
        actionButton(ns("b.prevtraj"), "Previous"),
        actionButton(ns("b.nxtraj"),"Next"),
        actionButton(ns("b.remove"), "Remove"),
        actionButton(ns("b.skip"), "Skip "),
        width = 4
        ),
    
    box(title = "other Trajectories",
        plotOutput(ns("plot.2"), width = "100%"),
        width = 4,
        br()
        )
    
  )
}


HierCluster <- function(input, output, session, in.data) {
  
  ns <- session$ns
  
  # Reactive values: "Rval$trajStatus" stores a vector of flags for keeping track of which 
  #                  trajectories were already removed.
  #                  All reactive values with "counter" in their name are used to keep track
  #                  of which trajectory should be displayed for verification.
  Rval <- reactiveValues(trajStatus = NULL,
                         countSkip = isolate(input$b.skip),
                         countRemove = isolate(input$b.remove),
                         countPrev = isolate(input$b.prevtraj),
                         countNxt = isolate(input$b.nxtraj),
                         countReset = isolate(input$b.reset),
                         count = 0)
  
  # Conversion of the universal column names into a list. Needed for the "heatmap.outliers" function
  l.cols <- list() 
  l.cols$id <- "ID"
  l.cols$time <- "TIME"
  l.cols$meas <- "MEAS"
  l.cols$fov <- "FOV"
  
  
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
    InSkip <- input$b.skip
    InRemove <- input$b.remove
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
      
    } else if (InRemove != isolate(Rval$countRemove)) {
      
      Rval$count <- 1
      cat("branchCount if InRemove\n")
      
      # converts selected trajectories (marked inside the vector with a "1") to removed 
      # trajectories (marked with a "2"); If-statement guarantees that the selected trajectory
      # will be removed rather than the remaining ones (bug-fixing)
      if(length(InTrajStatus[InTrajStatus == 1]) < length(InTrajStatus[InTrajStatus == 0])) {
        InTrajStatus[which(InTrajStatus %in% 1)] <- 2
      } else {
        InTrajStatus[which(InTrajStatus %in% 0)] <- 2
      }
      Rval$trajStatus <- InTrajStatus
      Rval$countRemove <- InRemove
      
      
    } else if (InSkip != isolate(Rval$countSkip)) {
      
      Rval$count <- 1
      cat("branchCount if InSkip\n")
      
      # converts selected trajectories (marked inside the vector with a "1") to skipped
      # trajectories (marked with a "3"); If-statement guarantees that the selected trajectory
      # will be skipped rather than the remaining ones (bug-fixing)
      if(length(InTrajStatus[InTrajStatus == 1]) < length(InTrajStatus[InTrajStatus == 0])) {
        InTrajStatus[which(InTrajStatus %in% 1)] <- 3
      } else {
        InTrajStatus[which(InTrajStatus %in% 0)] <- 3
      }
      Rval$trajStatus <- InTrajStatus
      Rval$countSkip <- InSkip
      
    } else if (InResetTraj != isolate(Rval$countReset)) {
      
      Rval$count <- 1
      cat("branchCount if InResetTraj\n")
      Rval$countReset <- InResetTraj
    }
    
    OutCount <- Rval$count
    
    # because negative counts do not make sense for trajectory selection, these values will be 
    # converted to "0".
    if (OutCount < 0)
      OutCount <- 0
    
    return(OutCount)
  })
  
  
  allIDnames <- reactive({
    ns <- session$ns
    
    
    cat("calling allIDnames")
    
    dm.in <- in.data()
    
    
    if(is.null(dm.in))
      return(NULL)
    
    allIDs <- as.vector(dm.in[, unique(ID)])
    return(allIDs)
  })
  
  
  activeData <- reactive({
    ns <- session$ns
    
    cat("calling activeData\n")
    
    dm.in <- in.data()
    allIDs <- allIDnames()
    InTrajStatus <- Rval$trajStatus
    
    if(is.null(dm.in))
      return(NULL)
    
    dm.out <- dm.in[ID %in% allIDs[which(InTrajStatus %in% c(0, 1))]]
    
    return(dm.out)
  })
  
  
  notSelData <- reactive({
    ns <- session$ns
    
    cat("calling notSelData\n")
    
    dm.in <- activeData()
    allIDs <- allIDnames()
    InTrajStatus <- Rval$trajStatus
    
    if(is.null(dm.in))
      return(NULL)
    
    # if statement to guarantee for selected trajectories to appear on plot1 rather than plot2 which shows remaining trajectories.
    if( length(InTrajStatus %in% 0) > length(InTrajStatus %in% 1)) {
      
      dm.out <- dm.in[ID %in% allIDs[which(InTrajStatus %in% 1)]]
      
    } else {

      dm.out <- dm.in[ID %in% allIDs[which(InTrajStatus %in% 0)]]
      
    }
    
    return(dm.out)
    
  })
  
  
  selData <- reactive({
    ns <- session$ns
    
    cat("calling selData\n")
    
    dm.in <- activeData()
    allIDs <- allIDnames()
    InTrajStatus <- Rval$trajStatus
    
    if(is.null(dm.in))
      return(NULL)
    
    # if statement to guarantee for selected trajectories to appear on plot1 rather than plot2 which shows remaining trajectories.
    if( length(InTrajStatus %in% 0) > length(InTrajStatus %in% 1)) {
      
      dm.out <- dm.in[ID %in% allIDs[which(InTrajStatus %in% 0)]]
      
    } else {
      
      dm.out <- dm.in[ID %in% allIDs[which(InTrajStatus %in% 1)]]
      
    }
    
    return(dm.out)
    
  })
  
  
  removedIDs <- reactive({
    ns <- session$ns
    
    cat("calling removedIDs\n")
    
    dm.in <- in.data()
    allIDs <- allIDnames()
    InTrajStatus <- Rval$trajStatus
    
    if(is.null(dm.in))
      return(NULL)
    
    id.out <- allIDs[which(InTrajStatus %in% 2)]
    
    return(id.out)
  })

  
  hierMatrix <- reactive({
    ns <- session$ns
    
    cat("updating hierMatrix\n")
    
    dm.in <- activeData()
    
    if (is.null(dm.in))
      return(NULL)
    
    heat.matrix <- heatmap.matrix(dm.in, in.list = l.cols)
    
    return(heat.matrix)
  })
  
  # calculates distance matrix
  hierDist <- reactive({
    ns <- session$ns
    
    cat("updating hierDist\n")
    
    InMatrix <- hierMatrix()
    InDistSel <- input$sel.dist
    
    if (is.null(InMatrix))
      return(NULL)
    
    cat("Calculating distance matrix\n")
    heat.dist <- dist(InMatrix, method = InDistSel)
    
    return(heat.dist)
  })
  
  # calculates the clustering and returns the height where the tree is cut into 
  # two clusters
  hierClust <- reactive({
    ns <- session$ns
    
    cat("updating hierClust\n")
    
    InDist <- hierDist()
    InHclustSel <- input$sel.hclust
    
    if (is.null(InDist))
      return(NULL)
    
    cat("Calculating dendrogram\n")
    hc.rows <-  hclust(InDist, method = InHclustSel) #stores information about branching heights
    
    return(hc.rows)
    
  })
  
  
  # Calculates dendrogram
  hierDend <- reactive({
    ns <- session$ns
    
    cat("updating hierDend\n")
    
    InClust <- hierClust()
    
    
    if (is.null(InClust))
      return(NULL)
    
    dend <- as.dendrogram(InClust)

    
    return(dend)
  })
  
  
  # Returns the IDs of all trajectories that were cut off the tree
  cutValues <- reactive({
    
    ns <- session$ns
    
    cat("updating cutValues\n")
    
    InClust <- hierClust()
    InBranchCount <- branchCount()
    allIDs <- allIDnames()
    InTrajStatus <- Rval$trajStatus
    
    # As a side effect, this if-statement checks whether the tree isn't cut at all and if any data was removed.
    # If this is the case, the vector stored in "Rval$trajStatus" can be initialised.
    if(InBranchCount == 0 && is.null(InTrajStatus[which(InTrajStatus %in% 2)])){ 
      cat("checkCut\n")
      InTrajStatus <- rep(0, length(allIDs))
      Rval$trajStatus <- InTrajStatus
    }
    
    # sets the length of the vector "Rval$trajStatus" according to the amount of the number of trajectories. 
    # This is needed when another dataset with more trajectories is loaded during the same session.
    if (length(InTrajStatus) != length(allIDs))
      Rval$trajStatus <- rep(0, length(allIDs))
    
    if (is.null(InClust))
      return(NULL)
    
    cat("Get height\n")
    hc.height <- rev(InClust$height)[InBranchCount + 1] # here we get the height output of the first branching event
    print(hc.height)
    
    cat("Calculate tree cut\n")
    ct.loc <- cutree(InClust, h = hc.height) # contains information about grouping after treecutting
    
    cutvalue <- names(which(ct.loc > 1))
    
    cutreeValues <- which(allIDs %in% cutvalue)
    oldValues <- which(InTrajStatus %in% c(0,1))
    newValues <- oldValues[which(oldValues %in% cutreeValues)]
    emptyValues <- oldValues[ - which(oldValues %in% cutreeValues)] 
    
    # as a side effect, "Rval$trajStatus" will be overwritten with the updated selection
    #InTrajStatus [oldValues] <- 0
    InTrajStatus[newValues] <- 1
    InTrajStatus[emptyValues] <- 0
    Rval$trajStatus <- InTrajStatus
    
    return(InTrajStatus)
  })
  
  
  # plots a clustered heatmap with a coloured dendrogram which gives information about the selected
  # trajectories.
  # Function instead of reactive as per:
  # http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  plotHierHeat <- function() {
    ns <- session$ns
    
    checkUpdate <- input$check.update
    
    # Construct that decides between updating the plot directly or simply with a dummy-button
    if(checkUpdate) {
      
      InDend <- hierDend()
      InMatrix <- hierMatrix()
      InPlotSel <- input$sel.plot
      InColourSel <- input$sel.col
      
    } else {
      
      Update <- input$b.update  # Dummy-button to update the heatmap only when button is pressed
      InDend <- isolate(hierDend())
      InMatrix <- isolate(hierMatrix())
      InPlotSel <- isolate(input$sel.plot)
      InColourSel <- isolate(input$sel.col)
      
    }
    
    if (is.null(InDend))
      return(NULL)
    
    if (is.null(InMatrix))
      return(NULL)
    
    palette.loc <- colorRampPalette(brewer.pal(9, rev(InColourSel)))(n = 99) #color palette. package: RColorBrewer
    
    if (InPlotSel == "heatmap"){
      dm.out <- heatmap.2(InMatrix,
                        Rowv = InDend,
                        Colv = FALSE,
                        dendrogram = "row",
                        trace = "none",
                        col = palette.loc
      )
    } else {
      dm.out <- plot(InDend)
    }
    
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
  
  
  # Plots the currently selected trajectory
  plot1 <- reactive({
    ns <- session$ns
    
    cat("Plot1: \n")
    InSelData <- selData()
    dummy <- cutValues()
    
    if (is.null(InSelData))
      return(NULL)
    
 
    plot1 <- ggplot(InSelData, aes(x = TIME, y = MEAS, group = ID)) + 
      ggtitle(paste0("trajectory: ", paste(InSelData[, unique(ID)], collapse = ", "))) +
      theme_bw() +
      scale_x_continuous(name = "Time") +
      scale_y_continuous(name = "Measurement") +
      geom_line() 
    
    return(plot1)
  })
  
  
  # Plots all the remaining trajectories (optional updating)
  plot2 <- reactive({
    ns <- session$ns
    
    cat("Plot2\n")
    
    # Construct that decides between updating the plot directly or simply with a dummy-button
    checkUpdate <- input$check.update
    if (checkUpdate) {
      InnotSelData <- notSelData()
    } else {
      dummy <- input$b.update # Dummy-button to update the plot only when button is pressed
      InnotSelData <- isolate(notSelData())
    }
    
    if (is.null(InnotSelData))
      return(NULL)

    plot2 <- ggplot(InnotSelData, aes(x = TIME, y = MEAS, group = ID)) + 
      ggtitle("remaining trajectories") +
      theme_bw() +
      scale_x_continuous(name = "Time") +
      scale_y_continuous(name = "Measurement") +
      geom_line(alpha = 0.4) 
    
    return(plot2)
  })
  
  
  output$plot.heat <- renderPlot({
    
    plotHierHeat()
    
  })
  
  
  output$plot.1 <- renderPlot({
    
    plot1()
    
  })
  
  output$plot.2 <- renderPlot({
    
    plot2()
    
  })
   return(removedIDs)
}

