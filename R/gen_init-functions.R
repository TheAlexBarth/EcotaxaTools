#' instatiate an ecopart_obj
#' 
#' Sometimes you might need to instatiate new objects. If you just make a list,
#' it can be tedious to resurrect the class structure for the new object.
#' You can use this to instatiate an empty object
#' 
#' 
#' @export
ecopart_obj <- function(){
  
  if(!(is.null(obj)) &!(is.null(c(par_files,zoo_files,meta)))){
    warning('An object and additional files were provided. 
            Files will be overwritten')
  }
  
  
  # if it is all NULL
  if(is.null(obj, par_files, zoo_files, meta)) {
    init_obj <- structure(list(),
                          class = c('list', 'ecopart_obj'))
                          
    init_obj$par_files <- structure(list(),
                                    class = c('list','par_list'))
    
    init_obj$zoo_files <- structure(list(),
                                    class = c('list','zoo_list'))
    
    init_obj$meta <- data.frame()
    
    return(init_obj)
  } 
}

#' Set classes
#' 
#' Will set the class as an ecopart object
#' 
#' @param obj the object
#' @param par_files the par files
#' @param zoo_files the zoo_files
#' @param meta the meta files
#' 
#' @export
as_ecopart_obj <- function(obj = NULL,
                           par_files = NULL,
                           zoo_files = NULL,
                           meta = NULL){
  if(!(is.null(obj))){
    stopifnot(names(obj) %in% c('par_files','zoo_files','meta'))
    par_files <- obj$par_files
    zoo_files <- obj$zoo_files
    meta <- obj$meta
  }
  
  par_files <- lapply(par_files, as_par_df)
  zoo_files <- lapply(zoo_files, as_zoo_df)
  
  ret_obj <- structure(
    list(
      par_files = structure(par_files, class = c('list','par_list')),
      zoo_files = structure(zoo_files, class = c('list', 'zoo_list')),
      meta = meta
    ),
    class = c('list', 'ecopart_obj')
  )
  return(ret_obj)
}

#' Set class for zoo_df
#' 
#' @param df the data frame
#' @export
as_zoo_df <- function(df) {
  class(df) <- c(class(df), 'zoo_df')
  return(df)
}

#' set class for par_pdf
#' 
#' @param df the data frame
#' @export
as_par_df <- function(df) {
  class(df) <- c(class(df), 'par_df')
  return(df)
}