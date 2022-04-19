#' Get the percent contribution of taxa
#'
#' This function is for the inner bits of rel_taxa
#' 
#' @importFrom tibble as_tibble
#' @param df an ecotaxa .tsv style dataframe
#' 
#' @author Alex Barth
rel_abundance <- function(df) {
  taxa <- df[[get_col_name(df, 'taxo_name')]]
  rdf <- as_tibble(table(taxa) / nrow(df))
  names(rdf) <- c('taxa','rel_abundance')
  return(rdf)
}

#' Get the percent contribution of taxa
#' 
#' This will take either a list or a df and get the relative abundace
#' 
#' @param ecodf a list, data frame, or ecopart_obj
#' 
#' @export
#' @author Alex Barth
rel_taxa <- function(ecodf) {
  
  #if it is a list or ecopart_obj
  if(any(class(ecodf) == 'list')) {
    #if it's an ecopart object, extract just the zoo_files
    if(all(names(ecodf) %in% c('par_files', 'zoo_files', 'meta'))) {
      ecodf <- ecodf$zoo_files
    }
    
    rlist <- lapply(ecodf, rel_abundance)
    return(rlist)
  }
  
  rdf <- rel_abundance(ecodf)
  return(rdf)
  
}
