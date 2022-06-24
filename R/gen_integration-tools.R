#' Linear interpolation between sets of points
#'
#' @param x typically this should be depth
#' @param y typically this should be n/m3 or mg/m3
#' @param min_x expands the first y value to this value default at 0
#' @param max_x expands the last y value to this value
#' 
#' @importFrom stats approxfun
#' 
#' @export
#' @author Alex Barth
lin_interp <- function(x,y,min_x = 0,max_x){
  
  if(min_x > max_x){
    stop('min_x must be smaller than max x')
  } else if (any(x < min_x)) {
    stop('min_x must be the smallest value in range of x')
  } else if (any(x > max_x)) {
    stop('max_x must be the largest value in the range of x')
  } else {
    
    #expand
    x_expand <- c(min_x,x,max_x)
    y_expand <- c(y[1],y,y[length(y)])
    
    xy_line <- approxfun(x_expand, y = y_expand)
    
    
  }
}

#' Trapezoidal integration
#'
#' This exists in a much more comprehensive way in dan kelley's oce
#' but i hate using too many packages
#' @param x typically this should be depth
#' @param y typically this should be n/m3 or mg/m3
#' @param min_x expands the first y value to this value
#' @param max_x expands the last y value to this value
#' @param ... arguements to pass to \code{integrate} : most helpful to increase subdivisons or rel.tol
#' 
#' @importFrom stats integrate
#' 
#' @export
#' @author Alex Barth
trapz_integarate <- function(x,y,min_x,max_x,...) {
  line <- lin_interp(x,y,min_x,max_x)
  auc <- integrate(line,min_x,max_x,...)
  return(auc)
}


#' Match df to integrate
#' 
#' Interior function will match up a data frame for integration
#' 
#' @param df the input data frame
#' @param ... arguments to pass to integrate
integration_matcher <- function(df,...) {
  out <- trapz_integarate(x = df$mp, y = df$conc_m3,
                          min_x = min(df$min_d), max_x = max(df$max_d),...)
  return(out)
}


#' Integrate all taxa in a group
#' 
#' Apply trapz integrate to a list of data frames
#' 
#' @param df input data frame
#' @param need_format logical input to determine if needed to format
#' @param ... arguements to pass to integrate()
#' 
#' @export
integrate_all <- function(df, need_format = F, ...){
  if(need_format) {
    info_cols <- get_bin_limtis(df$db)
    df$min_d <- info_cols$min_d
    df$max_d <- info_cols$max_d
    df$mp <- info_cols$mp
  }
  bin_list <- split(df, f = df$taxa)
  
  intg_list <- lapply(bin_list, integration_matcher, ...)
  class(intg_list) <- c('etx_integration_list', 'list')
  return(intg_list)
}


#' Switch integration list to a dataframe
#' 
#' This takes an etx_depth_integration_list and will return a dataframe
#' 
#' @param intg_list the integration list
#' 
#' @importFrom tidyr pivot_longer
#' @importFrom tibble tibble
#' 
#' @export
#' @author Alex Barth
intg_to_tib <- function(intg_list) {
  stopifnot(is.etx_class(intg_list, 'etx_integration_list'))
  # set up the dataframe
  ret_tib <- tibble(taxa = names(intg_list),
                 intg = rep(NA, length(names(intg_list))))
  
  ret_tib$intg <- sapply(intg_list, `[[`, 'value') # get the value from integration
  return(ret_tib)
}
