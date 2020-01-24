library(shinydashboard)
#library(shinyjs) # http://deanattali.com/shinyjs/

ui <- dashboardPage( # starts shiny in dashboard
  
  dashboardHeader(
    
    # Application title
    title ="Outlier Detection",
    titleWidth = 400
    
    
  ),
  dashboardSidebar(
    width = 400,
    
    # dashboard-equivalent to "tabs" in normal shiny
    sidebarMenu(
      
      # Every item stands for one tab. "Rolling Window" and "isolation Tree" will show outputs of the
      # correspondent modules, whereas "Generate synthetic Data" & "Load Data" represent dropdown-menus that load
      # or generate datasets.
      
      menuItem("Generate synthetic Data", tabName = "synDataOpt", icon = icon("random"), # tab for synthetic Data options
               sliderInput("slider.syn", "amount of outliers", 0, 30, 1, width = "100%"),
               checkboxInput("check.super", "add innovative outliers"),
               actionButton("b.syndata", "generate synthetic data"),
               br()
               ),
      
      menuItem("Load Data", tabName = "ownDataOpt", icon = icon("file"),
               fileInput("file.name", "file name:",
                         accept = c("text/csv", 
                                    "text/comma-separated-values,text/plain",
                                    "application/gzip", 
                                    "application/bz2"),
                         width = "100%"
               ),
               # extracted columns from the loaded dataset will be displayed for selection
               uiOutput("uiOut.ID"),
               uiOutput("uiOut.time"),
               uiOutput("uiOut.meas1"),
               uiOutput("uiOut.meas2"),
               uiOutput("uiOut.ops"),
               uiOutput("uiOut.FOV"),
               actionButton("b.loaddata", "load File"),
               br()
      ),
      menuItem("Downloads", tabName = "downloads", icon = icon("download"),
               selectInput("s.downMod", "Data from which Module?",
                           choices = c("Rolling Window",
                                       "Hierarchical Clustering",
                                       "Time Series"),
                           selected = "Rolling Window"),
               
               selectInput("s.download", "select download",
                           choices = c("full dataset without outliers",
                                       "shortened dataset without outliers",
                                       "Outlier IDs"), 
                           selected = "full dataset without outliers",
                           width = "100%"), 
               
               downloadButton("b.download", "Download now!"),
               br())
    ),
    
    menuItem("quantile Trimming", tabName = "quantrim", icon = icon("cut")),
    
    menuItem("Rolling Window", tabName = "rollwindow", icon = icon("redo-alt")),
    
    menuItem("Isolation Tree", tabName = "isotree", icon = icon("tree"))
  ),
  
  dashboardBody(
    
    
    # Includes the usage of ShinyJS - not used in the app for now
    useShinyjs(),
    tabItems(
      
      # references to the modules "RollWin", "IsoTrim" & "QuanTrim"
      tabItem(tabName = "quantrim",
              QuanTrimInput("QuanTrim")
      ),
      tabItem(tabName = "rollwindow",
              RollWinInput("RollWin")
      ),
      tabItem(tabName = "isotree",
              IsoTreeInput("IsoTree")
      )
    )
  )
)
