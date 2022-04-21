#' Find a match to list
#' 
#' Inside piece to broad_names
#' 
#' @param taxo_hier taxonomoy hierarchy
#' @param poss_match list of possible matches
name_match <- function(taxo_hier, poss_match) {
  matches <- which(taxo_hier %in% poss_match)
  if(length(matches) == 0) {
    stop(paste0("From name_match: No possible match for ",taxo_hier))
  }
  
  ret_match <- taxo_hier[min(matches)]
  return(ret_match)
}

#' Assign names to a possible list
#' 
#' @param df an ecotaxa tsv-style dataframe
#' @param new_names a vector of character names to move towards default
#' @param suppress_print set to true to stop messages of matches
#' 
#' @export
#' 
#' @author Alex Barth
names_to <- function(df, new_names, suppress_print = F) {
  taxo_hier <- get_col_name(df, 'taxo_hierarchy')
  
  cur_taxo <- df[[taxo_hier]]
  split_names <- strsplit(cur_taxo, ">")
  split_names <- lapply(split_names, rev)
  
  change_name <- rep(NA, length(split_names))
  
  for(i in 1:length(change_name)) {
    change_name[i] <- name_match(split_names[[i]], new_names)  
  }
  
  if(suppress_print != T) {
    taxo_name <- get_col_name(df, 'taxo_name')
    swaps <- paste0(df[[taxo_name]], " --> ", change_name)
    unique_swaps <- unique(swaps)
    cat('Old Names --> New Names')
    cat('\n')
    for(i in 1:length(unique_swaps)) {
      cat('\n')
      cat(unique_swaps[i])
    }
  }
  return(change_name)
}

#' Drop certain names from the ecotaxa dataframe
#' 
#' This is a modifier which will remove rows which contain matching function values
#' 
#' @param df an ecotaxa-tsv style data frame
#' @param drop_names character vector of which names to drop
#' 
#' @export
#' 
#' @author Alex Barth
names_drop <- function(df, drop_names) {
  name_col <- get_col_name(df, 'taxo_name')
  drop_rows <- which(df[[name_col]] %in% drop_names)
  rdf <- df[-drop_rows,]
  return(rdf)
}