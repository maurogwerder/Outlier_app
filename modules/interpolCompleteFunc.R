
require(imputeTS)

interpolComplete <- function(data, method = "linear")  {
  if (method == "linear") {
    interpData <- na.interpolation(data, option = "linear")
  }
  if (method == "spline") {
    interpData <- na.interpolation(data, option = "spline")
  }
  if (method == "stine") {
    interpData <- na.interpolation(data, option = "stine")
  }
  if (method == "mean") {
    interpData <- na.mean(data, option = "mean")
  }
  if (method == "median") {
    interpData <- na.mean(data, option = "median")
  }
  if (method == "mode") {
    interpData <- na.mean(data, option = "mode")
  }
  if (method == "Kalman") {
    interpData <- na.kalman(data)
  }
  if (method == "Last Observation Carried Forward") {
    interpData <- na.locf(data, option = "locf")
  }
  if (method == "Next Observation Carried Backward") {
    interpData <- na.locf(data, option = "nocb")
  }
  if (method == "Simple Moving Average") {
    interpData <- na.ma(data, weighting = "simple")
  }
  if (method == "Linear Weighted Moving Average") {
    interpData <- na.ma(data, weighting = "linear")
  }
  if (method == "Exponential Weighted Moving Average") {
    interpData <- na.ma(data, weighting = "exponential")
  }
  if (method == "Seasonally Decomposed Missing Value Imputation") {
    interpData <- na.seadec(data)
  }
  if (method == "random sample") {
    interpData <- na.random(data)
  }
  return(interpData)
}


LOCggplotTheme = function(in.font.base = 12,
                          in.font.axis.text = 12,
                          in.font.axis.title = 12,
                          in.font.strip = 14,
                          in.font.legend = 12) {
  loc.theme =
    theme_bw(base_size = in.font.base) +
    theme(
      panel.spacing = unit(1, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "black", size = 0.25),
      axis.text = element_text(size = in.font.axis.text),
      axis.title = element_text(size = in.font.axis.title),
      strip.text = element_text(size = in.font.strip, face = "bold"),
      strip.background = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(size = in.font.legend),
      legend.key.height = unit(1, "lines"),
      legend.key.width = unit(2, "lines")
    )
}


#' Interpolate missing rows in time series
#'
#' @param inDT Data.table in long format with time series
#' @param inColGr Name of the grouipng column
#' @param inColID Name of the column with unique time series IDs
#' @param inColT Name of the column with time
#' @param inColY Name of the column(s) with variables to interpolate
#' @param inTfreq Interval between two time points
#' @param inDeb Debugging, extended output
#'
#' @return Data.table with interpolated missing time points
#' @export
#'
#' @examples
LOCinterpolate = function(inDT, inColGr, inColID, inColT, inColY, inTfreq = 1, inDeb = F) {
  
  if(is.null(inDT))
    return(NULL)
  else
    loc.out = inDT
  
  # Stretch time series by every time series' min/max time
  # Gaps filled with NA's
  setkeyv(loc.out, c(inColGr, inColID, inColT))
  loc.out = loc.out[setkeyv(loc.out[, 
                                    .(seq(min(get(inColT), na.rm = T), 
                                          max(get(inColT), na.rm = T), 
                                          inTfreq)), 
                                    by = c(inColGr, inColID)], c(inColGr, inColID, 'V1'))]
  
  # x-check: print all rows with NA's
  if (inDeb) {
    cat(file = stdout(), '\nLOCinterpolate: Rows with NAs to interpolate:\n')
    print(loc.out[rowSums(is.na(loc.out)) > 0, ])
  }
  
  # Apparently the loop is faster than lapply+SDcols
  for(col in inColY) {
    if(inDeb)
      cat(file = stdout(), sprintf("Interpolating NAs in column: %s\n", col))
    
    # Interpolated columns should be of type numeric (float)
    # This is to ensure that interpolated columns are of porper type.
    data.table::set(loc.out, j = col, value = as.numeric(loc.out[[col]]))
    
    loc.out[, (col) := na_interpolation(get(col)), by = c(inColID)]        
  }
  
  return(loc.out)
}


#' Remove outlier time points and/or tracks depdending on maximum permissible gap length due to outliers
#'
#' @param inDT Data.table in long format with main dataset
#' @param inDTout Data.table in long format with rows of inDT that include outlier time points
#' @param inColID Name of the column with unique time series IDs
#' @param inGapLen Length of the maximum allowed gap. Tracks with gaps longer than threshold will be removed. Shorter gaps will be interpolated
#' @param inDeb Debugging, extended output
#'
#' @return Data.table with time points and/or time series removed
#' @export
#'
#' @examples
LOCremoveOutTracks = function(inDT, inDTout, inColID, inGapLen = 0, inDeb = F) {
  
  if(is.null(inDT))
    return(NULL)
  else
    loc.out = inDT
  
  # add index column per trajecory
  loc.out[, myColIdx := 1:.N, by = c(inColID)]
  
  # remove single outlier points (anti-join)
  # From: https://stackoverflow.com/a/46333620/1898713
  loc.out = loc.out[!inDTout, on = names(inDTout)]
  
  # calculate diff on index column to see the length of gaps due to removed points
  # the value of that column corresponds to the gap length (hence the "-1")
  loc.out[, 
          myColIdxDiff := c(1, diff(myColIdx)) - 1, 
          by = c(inColID)]
  
  # get track ids where the max gap is longer than the threshold
  loc.idgaps = loc.out[, 
                       max(myColIdxDiff), 
                       by = c(inColID)][V1 > inGapLen, get(inColID)]
  
  if (inDeb) {
    cat(file = stdout(), sprintf('\nLOCremoveTracks: Track IDs with max gap >= %d:\n', inGapLen))
    if (length(loc.idgaps) > 0)
      print(loc.idgaps) else
        cat("none\n")
  }
  
  # remove outlier tracks with gaps longer than the value set in slOutliersGapLen
  if (length(loc.idgaps) > 0)
    loc.out = loc.out[!(get(inColID) %in% unique(loc.idgaps))]
  
  # clean
  loc.out[, `:=`(myColIdx = NULL, myColIdxDiff = NULL)]
  
  return(loc.out)
}
