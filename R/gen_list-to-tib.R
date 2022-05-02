#' Make a tibble from a list - with names as a new colume
#' 
#' This function just calls rbind on a whole list but will first
#' make a new column in the dataframe, corresponding to the names of the list
#' 
#' @param list the input list - should have all elements be a df
#' @param new_col_name the column name to correspond to list names
#' 
#' @export
#' @author Alex Barth
list_to_tib <- function(list, new_col_name = 'group') {
  r_list <- list
  for(i in 1:length(r_list)){
    r_list[[i]][[new_col_name]] <- names(r_list)[i]
  }
  rdf <- do.call(rbind, r_list)
  row.names(rdf) <- NULL
  return(as_tibble(rdf))
}