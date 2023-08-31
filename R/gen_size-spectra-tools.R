# This utilities are for processing data in size bins

#' Calculate the numeric particle spectra
#' 
#' Calculate numeric particle spectra 
#' 
#' @param df a size.count.df or data frame with size_class column
#' @param vol_L the volumn in L, will be converted to cm3
#' @param needs_format if TRUE, will bin_format
#' 
#' @export
numeric_spectra <- function(df, 
                            vol_L, 
                            needs_format = FALSE) {

  if(!is.etx_class(df, 'size.count.df')) {
    if(!'size_class' %in% names(df)) {
      stop('must provide size.count.df class')
    } else {
      warning('irregular class provided, guessing columns!')
    }
  }
  
  #format if needed
  if(needs_format) {
    df <- bin_format(df, 'size_class')
  }
  
  
  df$size_width <- df$max_size_class - df$min_size_class
  df$n_s <- (df$count / (vol_L) / df$size_width)
  
  
  assign_etx_class(df, c('size.count.df','n_s.df'))
  return(df)
}

#' Count he number of observations in each size bin
#' 
#' @import dplyr
#' 
#' @param df a dataframe with a size_class column
#' @export
count_size_classes <- function(df) {
  size_class <- NULL
  rdf <- df |> 
    group_by(size_class) |> 
    summarize(count = n()) |> 
    assign_etx_class('size.count.df')
  return(rdf)
}

#' Make size bins in a cut style vector
#' 
#' Runs for a single bin group
#' 
#' @param size_vect the vector to assign classes for
#' @param method log or width
#' @param width if width, need to specify the width
#' @param base if log method, the base of the log
#' @param min_size minimum size
#' @param max_size maximum size
#' 
#' @export
assign_spectra_bins <- function(size_vect, 
                                method,
                                width = NULL,
                                base = 2, 
                                min_size = NULL,
                                max_size = NULL) {
  
  # create size_limits if needed
  if(is.null(min_size)) {
    min_size <- min(size_vect)
  }
  
  if(is.null(max_size)) {
    max_size <- max(size_vect)
  }
  
  # format lower bin limit for cut to work well
  # This is a little messy but preserves the bin_format feature later
  # If I were to rebuild, bin_format should work with cut include.lowest
  # but we're too deep now
  min_size_magnitude <- log10(min_size) |> 
    floor()
  
  min_size <- min_size - 10**(min_size_magnitude - 4)
  
  if (method == 'log') {
    size_bin_limits <- create_log_size_classes(min_size = min_size,
                                               max_size = max_size,
                                               base = base)
  } else if (method == 'width') {
    if(is.null(width)) {
      stop('width parameter must be specified')
    }
    
    size_bin_limits <- create_width_size_classes(min_size = min_size,
                                                 max_size = max_size,
                                                 width = width)
  } else {
    stop('MUST SELECT log or width method')
  }
  
  class_vect <- cut(size_vect, size_bin_limits)
  return(class_vect)
}


#' Generate size limits based on a log scale 
#' 
#' Useful interior function for NBSS or particle size spectrum calcs
#' 
#' @param min_size the minimum size class
#' @param max_size the maximum size class
#' @param base base of logarithm for bin width. Default 2
create_log_size_classes <- function(min_size,
                                    max_size,
                                    base = 2) {
  
  n_max <- log(max_size/min_size, base = base) |> 
    ceiling()
  
  exp_vect <- 0:n_max
  
  return(min_size * base^exp_vect)
}

#' Generate size limits based on a fixed width
#' 
#' Useful interior function for NBSS or particle size spectrum calcs
#' 
#' @param min_size the minimum size class
#' @param max_size the maximum size class
#' @param width the width of the approach -- needs to be selected
create_width_size_classes <- function(min_size,
                                      max_size,
                                      width) {
  
  breaks <- seq(min_size, max_size, width)
  if(max(breaks) < max_size) {
    breaks <- c(breaks, max(breaks) + width)
  }
  
  return(breaks)
}
