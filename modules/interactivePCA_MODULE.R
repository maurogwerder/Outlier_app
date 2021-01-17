#'
#' Outlier Detection App: Application to identify outliers in time-series data.
#' Author: Mauro Gwerder
#' 
#' The module 'interactivePCA' provides an interactive PCA, where interesting 
#' data-points can be examined by hovering and selected by clicking. Feature
#' extraction is handled by checkboxes. Downloading data removed using this
#' module is currently not possible. More options for feature extraction, 
#' interactive boxplots and selection of multiple points at once will be added
#' in the future.

library(tsfeaturex)
library(ggfortify)
library(plotly)

InterPcaInput <- function(id, label = "InterPca") {
  
  ns <- NS(id)
  
  fluidRow(
    box(title = "Features", width = 12,
        checkboxGroupInput(ns("chg.features"), label = "Select features: ", inline = T,
                           choiceNames = c("mean", "median", "min", "max"),
                           choiceValues = c("f.mean", "f.median", "f.min", "f.max")
                           )
        ),
    tabBox(tabPanel(title = "PCA",
                    actionButton(ns("b.pca"), "Plot!"),
                    splitLayout(plotlyOutput(ns("ply.pca")),
                    plotOutput(ns("plot.hover")), cellWidths = c("70%", "30%")),
                    textOutput(ns("txt.removedIDs"))
                    ),
           # interactive boxplot will be added in the future
           #tabPanel(title = "Boxplot",
                    #actionButton(ns("b.boxplot"), "Plot!"),
                    #plotlyOutput(ns("ply.boxplot"))),
           width = 12)
  )
}


InterPca <- function(input, output, session, in.data) {
  
  ns <- session$ns
  
  Rval <- reactiveValues(removedIDs = c())  # stores all as outliers selected IDs in a vector
  
  
  # Reactive that returns a data.table of extracted features
  feat_extract <- reactive({
    cat("call: feat_extract")
    ns <- session$ns 
    
    dt <- in.data()
    chgFeatures <- input$chg.features
    
  validate(
     need(dt != "" && chgFeatures != "", "Please select features and a data set")
     )
   
  # Edited function from package 'tsfeaturex' (Nelson Roque).
  # Extracts all selected features as a list
  # Edit enables vector for 'custom_feature_list'-argument
  out.list <- extract_features_edit(df=dt,
                                    group_var="ID",
                                    value_var="MEAS",
                                    features="custom",
                                    custom_feature_list = chgFeatures,
                                    return_timing = F)
    
  out.dt <- as.data.table(features_to_df_edit(out.list,
                                         data.format = "wide",
                                         group_var = "id"))
  
  # Column PACKAGE_VERSION not needed for PCA
  return(out.dt[, PACKAGE_VERSION := NULL])
  })
  
  
  # returns a data.table with the information of pca$x
  pca_calc <- eventReactive(input$b.pca, {
    ns <- session$ns
    
    feats <- feat_extract()
    dt <- in.data()
    chgFeatures <- input$chg.features
    
    
    # Manually constructing a data.table for PCA, as it increases flexibility
    # for plotting with ggplot & plotly.
    pca_list <- prcomp(feats[,!"ID", with=F])
    pca_dt <- as.data.table(pca_list$x)
    pca_dt[, ID := feats[, ID]]
    pca_dt <- merge(pca_dt, unique(dt[, .(ID, FOV)]))
    return(pca_dt)
  })
  
  
  
  plot.out <- reactive({
    
    ns <- session$ns
    
    pca_dt <- pca_calc()
    chgFeatures <- input$chg.features
    
    validate(
      need(length(chgFeatures) > 1, "Please select two or more features")
    )
   
    # http://huboqiang.cn/2016/03/03/RscatterPlotPCA
    # plotting PCA without autoplot
    # Autoplot takes away a lot of features (like giving arbitrarily many args to aes()),
    # which makes it more difficult to include features like custom tooltips.
    ggplot.pca <- ggplot(pca_dt, aes(x=PC1, y=PC2, ID=ID, #color = FOV,
                                     text = sprintf("ID: %s<br>Group: %s", ID, FOV))) +
                          geom_point() +
                          theme_bw()
    
    plotly.pca <- ggplotly(ggplot.pca, source = "ply.pca", tooltip = "text")
                        
    
    return(plotly.pca)
  })
  
  
  # the plotly-function 'event_data' is triggered by the event specified and returns
  # a data.frame with information about x- and y-coordinates, group and ID.
  hover_data <- reactive({
    
    event_data("plotly_hover", source="ply.pca")
  })
  
  
  # Hovering over a data-point triggers the plotting of the whole trajectory
  # correspondent to that data-point.
  plot_hover <- reactive({
    
    ns <- session$ns
    cat("plot_select: \n")
    
    dt <- in.data()
    rowNum <- hover_data()$pointNumber # number of row -1 (starts from 0)
    pca_dt <- pca_calc()
    
    validate(
      need(dt != "", "Please generate Data")
    )
    
    validate(
      need(rowNum != "", "Please hover over a datapoint")
    )
    
    # rowNum needs to be adjusted as the output of hover_data()$pointNumber starts with 0
    pca_ID <- pca_dt[rowNum + 1, ID]
    
    plot.out <- ggplot(data = dt[ID == pca_ID], aes(x = TIME, y = MEAS)) + 
      geom_line(stat = "identity") +
      ggtitle(paste0("ID: ", dt[ID == pca_ID, unique(ID)])) +
      theme_bw()
    
    return(plot.out)
  })
  
  
  # the plotly-function 'event_data' is triggered by the event specified and returns
  # a data.frame with information about x- and y-coordinates, group and ID.
  click_data <- reactive({
    
    event_data("plotly_click", source="ply.pca")
  })
  
  
  # Clicking on a data-point will add the correspondent ID to the reactive value
  # 'Rval$removedIDs'.
  plot_click <- reactive({
    ns <- session$ns
    
    dt <- in.data()
    rowNum <- click_data()$pointNumber # number of row -1 (starts from 0)
    pca_dt <- pca_calc()
    removedIDs <- isolate(Rval$removedIDs)
    
    pca_ID <- pca_dt[rowNum + 1, ID]
    
    validate(
      need(rowNum != "", "click on datapoints to select them as outliers")
    )
    
    removedIDs[length(removedIDs) + 1] <- pca_ID
    
    Rval$removedIDs <- removedIDs
    
    return(removedIDs)
  })

  
  output$ply.pca <- renderPlotly({
    plot.out()
  })
  
  
  output$plot.hover <- renderPlot({
    plot_hover()
  })
  
  
  output$txt.removedIDs <- renderText({
    paste0("Trajectories selected as outliers: ", paste(plot_click(), collapse = ", "))
  })
  
}