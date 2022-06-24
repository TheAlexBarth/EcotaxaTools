#' Check classes of objects
#' 
#' These are a suite of functions which are to check the class of objects
#' Mainly these are for allowing for functions to accept different types
#' 
#' @param input the input of object to class check
#' @param classCheck the class to check against
is.etx_class <- function(input, classCheck) {
  if(any(class(input) == classCheck)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Assign an etx class
#' 
#' Primarily for looping in lapply
#' 
#' @param input the input of object to assign a class
#' @param add_class the class to assign (can be a character vector)
#' 
#' @export
#' @author Alex Barth
assign_etx_class <- function(input, add_class) {
  output <- structure(input,
                      class = c(class(input),add_class))
  return(output)
}