#' Merge casts into one file
#' 
#' This function takes in a list of casts and will combine them based on a list
#' of possible names to combine
#' 
#' @param cast_list either a list of dataframes or an ecopart_obj
#' @param name_map a list with cast names to combine - names of list items will be new names
#' 
#' @export
#' @author Alex Barth
merge_casts <- function(cast_list, name_map) {
  
  #if it is an ecopart_obj, will need to pool both zoo_files & par_files
  if(is.etx_class(cast_list, 'ecopart_obj')) {
    robj <- vector(mode = 'list', 3)
    names(robj) = names(cast_list)
    
    robj$zoo_files <- structure(lapply(name_map, pool_casts, cast_list$zoo_files),
                                class = c('list','zoo_list'))
    robj$par_files <- structure(lapply(name_map, pool_casts, cast_list$par_files),
                                class = c('list','par_list'))
    robj$meta <- cast_list$meta
    
    return(structure(robj,
                     class = 'ecopart_obj'))
  }
  
  
  #if it is a list of cast dataframes:
  rlist <- lapply(name_map, pool_casts, cast_list)
  return(rlist)
}

#' Inside cast pooling
#' 
#' 
#' This function will take either zoo files and/or par files and put them into 
#' Similar bins based on their names
#' 
#' @param cast_names the names of the casts which should be pooled
#' @param cast_list the list of all cast dataframe
#' 
#' @author Alex Barth
pool_casts <- function(cast_names, cast_list) {
  rdf <- do.call(rbind, cast_list[names(cast_list) %in% cast_names])
  return(rdf)
}
