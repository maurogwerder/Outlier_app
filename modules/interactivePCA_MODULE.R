library(tsfeaturex)
library(ggfortify)
library(plotly)
library(shinyBS)

helpText.interPCA <- c(chgFeatures = "Select at least two features to construct a PCA",
                       linkPCA = paste0("Hover over datapoint to show its trajectory, click to select a point for subsequent removal.",
                                      "Click again to unselect. If unselect doesn't work, double click to clear memory.")
                       )

InterPcaInput <- function(id, label = "InterPca") {
  
  ns <- NS(id)
  
  shinyjs::useShinyjs()
  
  fluidRow(
    box(title = "Features", width = 12,
        checkboxGroupInput(ns("chgFeatures"), label = "Select features: ", inline = T,
                           choiceNames = c("mean", "median", "min", "max"),
                           choiceValues = c("f.mean", "f.median", "f.min", "f.max")
                           ),
        bsTooltip(ns("chgFeatures"), helpText.interPCA[["chgFeatures"]], placement = "top", trigger = "hover"),
        ),
    
    tabBox(tabPanel(title = "PCA",
                    actionButton(ns("b.pca"), "Plot!"),
                    splitLayout(plotlyOutput(ns("ply.pca")),
                    plotOutput(ns("plot.hover")), cellWidths = c("70%", "30%")),
                    textOutput(ns("txt.removedIDs")),
                    br(),
                    actionLink(ns("linkPCA"), "usage instructions")
                    ),
           #tabPanel(title = "Boxplot",
                    #actionButton(ns("b.boxplot"), "Plot!"),
                    #plotlyOutput(ns("ply.boxplot"))),
           width = 12)
  )
}


InterPca <- function(input, output, session, in.data) {
  
  ns <- session$ns
  
  Rval <- reactiveValues(removedIDs = c())  # stores all as outliers selected IDs in a vector
  
  feat_extract <- reactive({
    
    ns <- session$ns 
    
    dt <- in.data()
    chgFeatures <- input$chgFeatures
    
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
  
  
  pca_calc <- eventReactive(input$b.pca, {
    ns <- session$ns
    
    feats <- feat_extract()
    dt <- in.data()
    
    
    # Manually constructing a data.table for PCA, as it increases flexibility
    # for plotting with ggplot & plotly.
    pca_list <- prcomp(feats[,!"ID", with=F])
    
    # Calculating variance explained
    eigs <- pca_list$sdev^2
    varEx <- eigs / sum(eigs)
    
    pca_dt <- as.data.table(pca_list$x)
    pca_dt[, ID := feats[, ID]]
    pca_dt <- merge(pca_dt, unique(dt[, .(ID, FOV)]))
    return(list(pca_dt = pca_dt, varEx = varEx))
  })
  
  plot.out <- reactive({
    
    ns <- session$ns
    
    pca_dt <- pca_calc()$pca_dt
    varEx <- pca_calc()$varEx
    chgFeatures <- input$chgFeatures
    
    validate(
      need(length(chgFeatures) > 1, "Please select two or more features")
    )
    # http://huboqiang.cn/2016/03/03/RscatterPlotPCA
    # plotting PCA without autoplot
    # Autoplot takes away a lot of features (like giving arbitrarily many args to aes()),
    # which makes it more difficult to include features like custom tooltips.
    ggplot.pca <- ggplot(pca_dt, aes(x=PC1, y=PC2, ID=ID, color = FOV,
                                     text = sprintf("ID: %s<br>Group: %s", ID, FOV))) +
                          geom_point() +
                          theme_bw() + 
                          labs(x = paste0("PC1 (", round(varEx[1], 2), "%)"),
                               y = paste0("PC2 (", round(varEx[2], 2), "%)"))
    
    plotly.pca <- ggplotly(ggplot.pca, source = "ply.pca", tooltip = "text")
                        
    
    return(plotly.pca)
  })
  
  
  hover_data <- reactive({
    
    event_data("plotly_hover", source="ply.pca")
  })
  
  # Hovering over a data-point triggers the plotting of the whole trajectory
  # correspondent to that data-point.
  plot_hover <- reactive({
    
    ns <- session$ns
    
    dt <- in.data()
    groupNum <- hover_data()$curveNumber
    pointNum <- hover_data()$pointNumber
    rowNum <- as.numeric(paste0(groupNum, pointNum)) + 1 # number of row -1 (starts from 0)
    
    pca_dt <- pca_calc()$pca_dt
    
    validate(
      need(dt != "", "Please generate Data")
    )
    
    validate(
      need(rowNum != "", "Please hover over a datapoint")
    )
    pca_IDs <- pca_dt[FOV == unique(FOV)[groupNum + 1], ID]
    pca_ID <- pca_IDs[pointNum + 1]
    
    plot.out <- ggplot(data = dt[ID == pca_ID], aes(x = TIME, y = MEAS)) + 
      geom_line(stat = "identity") +
      ggtitle(paste0("ID: ", dt[ID == pca_ID, unique(ID)])) +
      theme_bw()
    
    return(plot.out)
  })
  
  
  click_data <- reactive({
    event_data("plotly_click", source="ply.pca")
  })
  
  # Clicking on a data-point will add the correspondent ID to the reactive value
  # 'Rval$removedIDs'.
  plot_click <- reactive({
    ns <- session$ns
    
    dt <- in.data()
    groupNum <- click_data()$curveNumber
    pointNum <- click_data()$pointNumber
    rowNum <- as.numeric(paste0(groupNum, pointNum)) + 1 # number of row -1 (starts from 0)
    
    pca_dt <- pca_calc()$pca_dt
    removedIDs <- isolate(Rval$removedIDs)
    
    pca_IDs <- pca_dt[FOV %in% unique(FOV)[groupNum + 1], ID]
    pca_ID <- pca_IDs[pointNum + 1]
    
    # If there were already some datapoints removed, the validate 
    if(length(removedIDs) == 0){
      validate(
        need(rowNum != "", "click on datapoints to select them as outliers")
      )
    }
    
    # enables to unselect a datapoint by clicking on it again
    # issue: quickly selectand unselect the same datapoint is not possible,
    # because 'plotly_click' does not register a click that is already in 
    # memory. Double click will clear memory and it will work again.
    if(length(pca_ID) != 0){
      if(length(removedIDs) != 0 && pca_ID %in% removedIDs){
        removedIDs <- removedIDs[!removedIDs %in% pca_ID]
      } else {
        removedIDs[length(removedIDs) + 1] <- pca_ID
      }
      
    }
    
    
    
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
  
  addPopover(session, 
             id = ns("linkPCA"),
             title = "Instructions",
             placement = "top",
             content = helpText.interPCA[["linkPCA"]],
             trigger = "click")
  
  return(plot_click)
}