ui <- dashboardPage( # starts shiny in dashboard
  
  dashboardHeader(
    
    # Application title
    title ="Outlier Detection" 
    
  ),
  dashboardSidebar(
    
    # dashboard-equivalent to "tabs" in normal shiny
    sidebarMenu(
      
      # Every item stands for one tab. "Rolling Window" and "hierarchical clustering" will show outputs of the
      # correspondent modules, whereas "Generate synthetic Data" & "Load Data" represent dropdown-menus that load
      # or generate datasets.
      menuItem("Rolling Window", tabName = "rollwindow", icon = icon("windows")),
      
      menuItem("hierarchical clustering", tabName = "hiercluster", icon = icon("tree")),
      
      menuItem("Generate synthetic Data", tabName = "synDataOpt", icon = icon("random"), # tab for synthetic Data options
               sliderInput("slider.syn", "amount of outliers", 0, 30, 1),
               checkboxInput("check.super", "add innovative outliers"),
               actionButton("b.syndata", "generate synthetic data"),
               br()),
      
      menuItem("Load Data", tabName = " ownDataOpt", icon = icon("file"),
               fileInput("file.name", "file name:",
                         accept = c('text/csv', 'text/comma-separated-values,text/plain')
               ),
               # extracted columns from the loaded dataset will be displayed for selection
               uiOutput("uiOut.ID"),
               uiOutput("uiOut.time"),
               uiOutput("uiOut.meas"),
               uiOutput("uiOut.FOV"),
               actionButton("b.loaddata", "load File"),
               br())
    )
  ),
  dashboardBody(
    
    # Includes the usage of ShinyJS - not used in the app for now
    useShinyjs(),
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
