library(shinydashboard)
#library(shinyjs) # http://deanattali.com/shinyjs/

ui <- dashboardPage( # starts shiny in dashboard
  
  dashboardHeader(
    
    # Application title
    title ="Outlier Detection"
    
    
  ),
  dashboardSidebar(
    width = 400,
    
    # dashboard-equivalent to "tabs" in normal shiny
    sidebarMenu(
      
      # Every item stands for one tab. "Rolling Window" and "hierarchical clustering" will show outputs of the
      # correspondent modules, whereas "Generate synthetic Data" & "Load Data" represent dropdown-menus that load
      # or generate datasets.
      menuItem("Rolling Window", tabName = "rollwindow", icon = icon("windows")),
      
      menuItem("hierarchical clustering", tabName = "hiercluster", icon = icon("tree")),
      
      menuItem("Generate synthetic Data", tabName = "synDataOpt", icon = icon("random"), # tab for synthetic Data options
               sliderInput("slider.syn", "amount of outliers", 0, 30, 1, width = "100%"),
               checkboxInput("check.super", "add innovative outliers"),
               actionButton("b.syndata", "generate synthetic data"),
               br()
               ),
      
      menuItem("Load Data", tabName = "ownDataOpt", icon = icon("file"),
               fileInput("file.name", "file name:",
                         accept = c("text/csv", "text/comma-separated-values,text/plain"),
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
               selectInput("s.download", "select download",
                           choices = c("full dataset without outliers",
                                       "shortened dataset without outliers",
                                       "Outlier IDs"), 
                           selected = "full dataset without outliers",
                           width = "100%"), 
               selectInput("s.downMod", "Data from which Module?",
                           choices = c("Rolling Window",
                                       "Hierarchical Clustering"),
                           selected = "Rolling Window"),
               downloadButton("b.download", "Download now!"),
               br())
    )
  ),
  dashboardBody(
    
    # Includes the usage of ShinyJS - not used in the app for now
    #useShinyjs(),
    tabItems(
      
      # references to the modules "RollWin" & "HierCluster"
      tabItem(tabName = "rollwindow",
              RollWinInput("RollWin")
      ),
      tabItem(tabName = "hiercluster",
              HierClusterInput("HierCluster")
      )
    )
  )
)
