
require(data.table)
require(ggplot2)
require(imputeTS)


#' Generate synthetic CellProfiler output with single cell time series
#'
#'
#'
#' @param in.ntpts Number of time points (default 60)
#' @param in.ntracks Number of tracks per FOV (default 10)
#' @param in.nfov Number of FOV (default 6)
#' @param in.nwells Number of wells (default 1)
#' @param in.addna Number of NAs to add randomly in the data (default NULL)
#'
#' @return Data table with the follwoing columns: Metadata_Site, Metadata_Well, Metadata_RealTime, objCyto_Intensity_MeanIntensity_imErkCor (normal distributed),
#' objNuc_Intensity_MeanIntensity_imErkCor (normal distributed), objNuc_Location_X and objNuc_Location_Y (uniform ditributed), TrackLabel
#' @export
#' @import data.table

LOCgenTraj <- function(in.ntpts = 60, in.ntracks = 10, in.nfov = 6, in.nwells = 1, in.addna = NULL, in.addout = NULL) {
  
  x.rand.1 = c(rnorm(in.ntpts * in.ntracks * in.nfov * 1/3, 0.5, 0.1), rnorm(in.ntpts * in.ntracks * in.nfov * 1/3,   1, 0.2), rnorm(in.ntpts * in.ntracks * in.nfov * 1/3,  2, 0.5))
  x.rand.2 = c(rnorm(in.ntpts * in.ntracks * in.nfov * 1/3, 0.25, 0.1), rnorm(in.ntpts * in.ntracks * in.nfov * 1/3, 0.5, 0.2),  rnorm(in.ntpts * in.ntracks * in.nfov * 1/3, 1, 0.2))
  
  # add NA's for testing
  if (!is.null(in.addna)) {
    locTabLen = length(x.rand.1)
    x.rand.1[round(runif(in.addna) * locTabLen)] = NA
    x.rand.2[round(runif(in.addna) * locTabLen)] = NA
  }
  
  # add outliers for testing
  if (!is.null(in.addout)) {
    cat(file = stderr(), 'adding outliers\n')
    locTabLen = length(x.rand.1)
    x.rand.1[round(runif(in.addout) * locTabLen)] = 10
    x.rand.2[round(runif(in.addout) * locTabLen)] = 10
  }
  
  x.arg = rep(seq(1, in.ntpts), in.ntracks * in.nfov)
  
  dt.nuc = data.table(Metadata_Well = rep(LETTERS[1:in.nwells], each = in.ntpts * in.nfov * in.ntracks / in.nwells),
                      Metadata_Site = rep(1:in.nfov, each = in.ntpts * in.ntracks),
                      Metadata_RealTime = x.arg,
                      objCyto_Intensity_MeanIntensity_imErkCor = x.rand.1,
                      objNuc_Intensity_MeanIntensity_imErkCor  = x.rand.2,
                      objNuc_Location_X = runif(in.ntpts * in.ntracks * in.nfov, min = 0, max = 1),
                      objNuc_Location_Y = runif(in.ntpts * in.ntracks * in.nfov, min = 0, max = 1),
                      TrackLabel = rep(1:(in.ntracks*in.nfov), each = in.ntpts))
  return(dt.nuc)
  
}


rollwin_loop <- function(vec_in,
                         win_length,
                         thresh_val = 1.5,
                         return_loc = F,
                         method = "median",
                         reverse = T){
  #' detects values which lie outside the defined threshold range via a sliding window approach
  #' 
  #' Args:
  #'  vec_in: input vector
  #'  win_length: defines the window length by the nuber of included values
  #'  thresh_val: value that is used to threshold range
  #'  return_loc: TRUE or FALSE statement that determines if only a clipped output vector or also
  #'              a vector with information about the location of the outlier is returned.
  #'  reverse: TRUE or FALSE statement that determines if window should be rolled in both directions
  #'           of the ts-data

  # transforming global vector into local vector
  vec_loc <- vec_in
  
  # assign index of last point of vector (minus window length)
  endvec <- length(vec_loc) - win_length
  
  # initialise location output vector
  vec_out <- rep(0, length(vec_loc))
  
  # loop over vector
  for (win_start in seq(1, endvec)) {
    
    # index of value to be checked as outlier
    ref_point <- win_start + win_length
    
    # value of possible outlier
    next_val <- vec_loc[ref_point]
    
    # last point of the window
    win_end <- ref_point - 1
    
    # definition of window width --- change win vec
    win_vec <- vec_loc[win_start : win_end]
    
    # calc threshold of window
    if(method == "median") {
      
      upper_range <- median(win_vec, na.rm = T) + median(win_vec, na.rm = T) * thresh_val
      lower_range <- median(win_vec, na.rm = T) - median(win_vec, na.rm = T) * thresh_val
      
    } else {
      
      upper_range <- quantile(win_vec, 0.75, na.rm = T) + median(win_vec, na.rm = T) * thresh_val
      lower_range <- quantile(win_vec, 0.25, na.rm = T) - median(win_vec, na.rm = T) * thresh_val
      
    }
    
    # check if value 'ref_point' is an outlier
    if (next_val > upper_range) {
      
      vec_loc[ref_point] <- NA # assign identified outlier as NA
      vec_out[ref_point] <- vec_out[ref_point] + 1 # update location output vector
      
    } else {
      if (next_val < lower_range) {
        
        vec_loc[ref_point] <- NA # assign identified outlier as NA
        vec_out[ref_point] <- vec_out[ref_point] - 1 # update location output vector
        
      }
    }
  }
  # adds reverse vectors and combines them with the forward vectors
  if (reverse) {
    
    # calls function 'rollwin_loop_rev' for reverse window 
    backwards <- rollwin_loop_rev(vec_in, win_length, thresh_val = 1.5, return_loc = T, method = method)
    clipped_both <- list(clipped_f = vec_loc,clipped_b = backwards$clipped)
    clipped_both_red <- Reduce('&',clipped_both)
    outl_loc_both <- vec_out + backwards$outl_loc
    
    # returns the clipped vector with or without the location output vector
    if (return_loc) {
      
      return(outl_loc = outl_loc_both)
      
    } else {
      
      return(list(clipped = vec_in[clipped_both_red]))
      
    }
  } else {
    
    # returns the clipped vector with or without the location output vector
    if (return_loc) {
      
      return(vec_out)
      
    } else {
      
      return(vec_loc)
      
    }
  }
}



rollwin_loop_rev <- function(vec_in = vec_in, win_length = win_length , thresh_val = thresh_val, return_loc = return_loc, method) {
  #' detects values which lie outside the defined threshold range via a sliding window approach
  #' 
  #' Args:
  #'  vec_in: input vector
  #'  win_length: defines the window length by the nuber of included values
  #'  thresh_val: value that is used to threshold range
  #'  return_loc: TRUE or FALSE statement that determines if only a clipped output vector or also
  #'              a vector with information about the location of the outlier is returned.
  #'  
  
  # transforming global vector into local vector
  vec_loc <- rev(vec_in)
  
  # assign index of last point of vector (minus window length)
  
  endvec <- length(vec_loc) - win_length
  # initialise result vector
  
  vec_out <- rep(0, length(vec_loc))
  
  # loop over vector
  for (win_start in seq(1, endvec)) {
    
    # index of value to be checked as outlier
    ref_point <- win_start + win_length
    
    # value of possible outlier
    next_val <- vec_loc[ref_point]
    
    # last point of the window
    win_end <- ref_point - 1
    
    # definition of window width --- change win vec
    win_vec <- vec_loc[win_start : win_end]
    
    # calc threshold of window
    if (method == "median") {
      
      upper_range <- median(win_vec, na.rm = T) + median(win_vec, na.rm = T) * thresh_val
      lower_range <- median(win_vec, na.rm = T) - median(win_vec, na.rm = T) * thresh_val
    
    } else {
      
      upper_range <- quantile(win_vec, 0.75, na.rm = T) + median(win_vec, na.rm = T) * thresh_val
      lower_range <- quantile(win_vec, 0.25, na.rm = T) - median(win_vec, na.rm = T) * thresh_val
      
    }
    # check if point is an outlier
    if (next_val > upper_range) {
      
      vec_loc[ref_point] <- NA
      vec_out[ref_point] <- vec_out[ref_point] + 1
      
    } else { 
      
      if (next_val < lower_range) {
        
        vec_loc[ref_point] <- NA
        vec_out[ref_point] <- vec_out[ref_point] - 1
        
      }
    }
   
  }
  # returns the clipped vector with or without the location output vector
  if (return_loc) {
    
    return(list(clipped = rev(vec_loc), outl_loc = rev(vec_out)))
    
  } else {
    
    return(rev(vec_loc))
    
  }
  
}

rollwin_index <- function(vec_in, thresh_val = 2){
  #' detects values which lie outside the defined threshold range via a sliding window approach
  #' 
  #' Args:
  #'  vec_in: input vector
  #'  thresh_val: how many consecutive NA's are allowed
  
  # transforming global vector into local vector
  vec_loc <- vec_in
  
  # assign index of last point of vector (minus window length)
  endvec <- length(vec_loc) - thresh_val
  
  # initialise location output vector
  vec_out <- rep(0, length(vec_loc))
  
  # loop over vector
  for (win_start in seq(1, endvec, 1)) {
    
    # last point of the window
    win_end <- (win_start + thresh_val) 
    
    # definition of window width --- change win vec
    win_vec <- vec_loc[win_start : win_end]
    
    # calc window sum
    vec_out[win_start] <- sum(win_vec)
    
  }
  if(max(vec_out) > thresh_val){
    
    return(F)
    
  }else{
    
    return(T)
    
  }
}

