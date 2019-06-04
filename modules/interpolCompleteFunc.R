
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
