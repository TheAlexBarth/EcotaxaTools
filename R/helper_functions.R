#' add_range
#'
#' copied from helper box
#' 
#' @param number center number
#' @param width range on both sided
#' 
#' @export
add_range <- function(number,width){
  return(c(number - width, number+width))
}

#' nearest
#' 
#' A function to assign a numeric bin between a range
#'
#' @param number the number to check
#' @param vector a numeric vector to match values to
#' 
#' @examples
#' nearest(74.2,seq(0,100,5))
#' 
#' @export
nearest <- function(number,vector){
  index <- which.min(abs(vector - number))
  return(vector[index])
}
