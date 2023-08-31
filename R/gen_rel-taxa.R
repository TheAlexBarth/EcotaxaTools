#' Get the percent contribution of taxa
#' 
#' This will take either a list or a df and get the relative abundance
#' 
#' @param ecodf a list, data frame, or ecopart_obj
#' 
#' @example man/examples/rel_taxa.R
#' 
#' @export
#' @author Alex Barth
rel_taxa <- function(ecodf) {
    
  if(is.etx_class(ecodf, 'ecopart_obj')) {
    ecodf <- ecodf$zoo_files
    rlist <- lapply(ecodf, rel_abundance)
    class(rlist) <- c(class(rlist), 'rel_list')
    return(rlist)
  } else if(is.etx_class(ecodf, 'zoo_list')) {
    rlist <- lapply(ecodf, rel_abundance)
    class(rlist) <- c(class(rlist), 'rel_list')
    return(rlist)
  } else if(is.etx_class(ecodf, 'zoo_df')) {
    rdf <- rel_abundance(ecodf)
  } else {
    rdf <- rel_abundance(ecodf)
  }
  rdf <- rel_abundance(ecodf)
  return(rdf)
  
}

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
  class(rdf) <- c(class(rdf),'rel_df')
  return(rdf)
}
