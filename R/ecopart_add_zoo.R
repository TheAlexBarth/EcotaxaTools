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
#' This function will add a column to all zooplankton files in an ecopart_object
#' It returns a new ecopart object
#' 
#' @param ecopart_obj an ecopart object list
#' @param func function to apply (must take df and return vector) see ellps_vol
#' @param col_name name to assign to new column
#' @param ... to pass to \code{lapply()} if needed
#' 
#' @export
#' 
#' @author Alex Barth
add_zoo <- function(ecopart_obj, func, col_name, ...) {
  temp_col <- lapply(ecopart_obj$zoo_files, func, ...)
  robj <- ecopart_obj
  for(i in 1:length(ecopart_obj$zoo_files)) {
    robj$zoo_files[[i]] <- add_col(ecopart_obj$zoo_files[[i]], temp_col[[i]],
                                 col_name = col_name)
  }
  return(robj)
}
