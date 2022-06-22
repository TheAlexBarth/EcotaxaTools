#' Check if input is ecopart_obj
#' 
#' @param input the input
#' 
#' @export
is.ecopart_obj <- function(input) {
  
  if(any(class(input) == 'ecopart_obj')){
    return(T)
  } else {
    return(F)
  }
}
