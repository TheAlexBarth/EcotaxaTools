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

