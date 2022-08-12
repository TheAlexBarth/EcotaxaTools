#' Get all taxa
#' 
#' retrieve the names of all taxa in the ecopart_obj
#' 
#' @param ecopart_obj an ecopart object
#' 
#' @example man/examples/all_taxa.R
#' 
#' @export
all_taxa <- function(ecopart_obj) {
  all_rows <- ecopart_obj$zoo_files |> list_to_tib('cast')
  all_taxa <- all_rows$name
  return(all_taxa)
}
