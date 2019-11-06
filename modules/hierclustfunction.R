
require(gplots)
require(RColorBrewer)
require(dendextend)


heatmap.matrix <- function(data,
                           in.list
                           ) {
  l.cols <- in.list
  
  if(nrow(data) == 0)
    return(NULL)
  
  # creating a data.table of wide format
  data.wide <- dcast(data, 
                     reformulate(response = l.cols$id, termlabels = l.cols$time),
                     value.var = l.cols$meas)
  
  # storing rownames for later
  loc.rownames <- data.wide[[l.cols$id]]
  
  # omitting first column containing rownames
  mat.loc <- as.matrix(data.wide[, -1])
  
  # assigning row names to matrix
  rownames(mat.loc) <- loc.rownames
  
  # Check for missing time points
  # Missing rows in the long format give rise to NAs during dcast
  # Here, we are not checking for explicit NAs in COLY column
  if ((sum(is.na(data[[l.cols$meas]])) == 0) & (sum(is.na(data.wide)) > 0))
    cat(file = stderr(), "Missing rows. Consider interpolation.\n")
  
  return(mat.loc)
}


heatmapforVisual <- function(data, # should be DT
                         dist.method = "euclidian", # method of distance measurement
                         hclust.method = "single",  # method of clustering
                         col_in = "heat.colors", # color palette for colorRampPalette()
                         plot = "heatmap", # if a heatmap of only the dendrogram should be plotted
                         breaks.in = 16, # how many breaks 
                         in.list,
                         break.man = F, # T if breaks should be assigned manually. used for fitting breaks to custom color palette
                         break.thresh, # 
                         trim.pos = 1 # input that determines at which branching event the tree is cut, starting with the first event at 1
) {
  
  l.cols <- in.list
  data.loc <- data #converts data to local environment
  
  # creates local matrix: rows = id numbers, cols = timepoints
  mat.loc <- acast(data.loc, data.loc[[l.cols$id]] ~ data.loc[[l.cols$time]], value.var = l.cols$meas) 
  
  distfun.loc <- function(x) dist(x, method = dist.method) # function for changing the distance-measurement method
  hclustfun.loc <- function(x) hclust(x, method = hclust.method) #function for changing the hclustering method
  
  if(break.man){ 
    
    # Fits breaks to normal data such that a difference between outliers and normal data can be made
    breaks.out <- c(max(mat.loc[which(mat.loc < break.thresh + 0.1)]) * (seq(0, 1, 1 / breaks.in)), break.thresh + 0.2)
    
  } else {
    
    breaks.out <- breaks.in
    
  }
  
  hc.rows <-  hclust(dist(mat.loc, method = dist.method), method = hclust.method) #stores information about branching heights
  hc.height <- rev(hc.rows$height)[trim.pos + 1] # here we get the height output of the first branching event
  ct.loc <- cutree(hclust(dist(mat.loc, method = dist.method), method = hclust.method), h = hc.height) # contains information about grouping after treecutting
  ct.uniq <- length(unique(ct.loc)) # to see how many groups there are
  dend <- as.dendrogram(hclust(dist(mat.loc, method = dist.method), method = hclust.method), h = hc.height) #dendrogram is needed to customize the colours
  d1 <- color_branches(dend, k = ct.uniq, col = c(rep(2,(ct.uniq-1)),1)) # ct.uniq can be used to differ between outlier groups.
  col_labels <- get_leaves_branches_col(d1)
  col_labels <- col_labels[order(order.dendrogram(d1))] # label ordering for heatmap.2 function
  
  if (plot == "heatmap") {
    
    # plots heatmap
    heatmap.2(mat.loc,
              Rowv = TRUE,
              Colv = FALSE,
              dendrogram = "row",
              trace = "none",
              distfun = distfun.loc,
              hclustfun = hclustfun.loc,
              col = col_in,
              breaks = breaks.out,
              colRow = col_labels
    )
    
  } else {
    
    if (plot == "dendrogram") {
      
      # plots only the dendrogram
      plot(d1)
      
    }
  }
  
  return (names(which(ct.loc > 1))) #returns position of detected trajectories
  
}

