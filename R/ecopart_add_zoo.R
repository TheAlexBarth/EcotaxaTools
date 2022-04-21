#' add_col
#' 
#' This function adds a column to the .tsv data frame
#' 
#' @param df a tsv ecotaxa dataframe
#' @param column the colum to add
#' @param col_name the name to assign new column
#'
#' @author Alex Barth
add_col <- function(df, column, col_name) {
  rdf <- df
  rdf[[col_name]] <- column
  return(rdf)
}

#' Add column to all zooplankton files
#' 
#' This function will add a column to all zooplankton files in either an ecopart-object 
#' or just a list of ecotaxa_tsvs
#' 
#' @param zoo_list an ecopart object list or list of ecotaxa-tsv (zoo_files)
#' @param func function to apply (must take df and return vector) see ellps_vol
#' @param col_name name to assign to new column
#' @param ... to pass to \code{lapply()} if needed
#' 
#' @export
#' 
#' @author Alex Barth
add_zoo <- function(zoo_list, func, col_name, ...) {
  
  #check if it is an ecopart_obj
  if(all(names(zoo_list) %in% c('par_files', 'zoo_files', 'meta'))) {
    robj <- zoo_list
    zoo_list <- zoo_list$zoo_files
    ret_obj <- TRUE
  } else {
    ret_obj <- FALSE
  }
  
  temp_col <- lapply(zoo_list, func, ...) #create the new column
  
  #loop to add the new column
  for(i in 1:length(zoo_list)) {
    zoo_list[[i]] <- add_col(zoo_list[[i]], temp_col[[i]],
                                 col_name = col_name)
  }
  
  if(ret_obj) {
    robj$zoo_files <- zoo_list
    return(robj)
  } else {
    return(zoo_list)
  }
}

#' Modify a list of ecotaxa tsv dfs
#' 
#' Apply a modifying function to a list of dataframes or ecopart object
#' 
#' @param zoo_list a list of ecotaxa tsvs or an ecopart-object
#' @param func the modifier function to apply
#' @param ... additional arguments to pass to function
#' 
#' @export
#' @author Alex Barth
mod_zoo <- function(zoo_list, func, ...) {
  
  #check if it is an ecopart_obj
  #check if it is an ecopart_obj
  if(all(names(zoo_list) %in% c('par_files', 'zoo_files', 'meta'))) {
    robj <- zoo_list
    zoo_list <- zoo_list$zoo_files
    ret_obj <- TRUE
  } else {
    ret_obj <- FALSE
  }
  
  r_list <- lapply(zoo_list, func, ...)
  
  if(ret_obj) {
    robj$zoo_files <- r_list
    return(robj)
  } else {
    return(r_list)
  }
}
