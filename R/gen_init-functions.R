#' instatiate an ecopart_obj
#' 
#' Sometimes you might need to instatiate new objects. If you just make a list,
#' it can be tedious to resurrect the class structure for the new object.
#' You can use this to instatiate an empty object
#' 
#' export
ecopart_obj <- function(){
  
  init_obj <- structure(list(),
                        class = c('list', 'ecopart_obj'))
                        
  init_obj$par_files <- structure(list(),
                                  class = c('list','par_list'))
  
  init_obj$zoo_files <- structure(list(),
                                  class = c('list','zoo_list'))
  
  init_obj$meta <- data.frame()
  
  return(init_obj)
}
