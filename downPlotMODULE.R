downPlotInput <- function(id, label = "DownPlot") {
  
  ns <- NS(id)
  
  fluidRow(
    
    downloadButton(ns('downPlot'), 'Download Overview Plot')
    
  )
}

downPlot <- function(input, output, session, in.plot) {
  ns <- session$ns
  
  output$downPlot <- downloadHandler(
    filename = "downloadPlot.png",
    content = function(file) {
      png(file)
      print(in.plot())
      dev.off()
    })
  
}

