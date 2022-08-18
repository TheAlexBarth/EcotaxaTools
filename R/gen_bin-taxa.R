#' Bin observations by taxa according to some common factor
#' This version allows for summing over some factor like dry mass or volume
#' 
#' It will sum for each individual in a depth bin by taxa
#' 
#' @importFrom tidyr pivot_wider
#' 
#' @param df a data frame should have a taxo_name, name, or object_annotation_category column
#' @param depth_breaks the limits of what bins should be made
#' @param zooscan if the df is a zooscan export set to true, default false
#' @param cat_col a category other than taxa to bin by
#' @param func_col a single character vector of the value to apply function to; esd, biomass, drymass, etc
#' @param func the function to apply
#' @param max_d the maximum possible depth for a bin
#' @param force_bins set to true if you want to ensure observations for all 0's
#'  
#' @export
#' @author Alex Barth
bin_taxa <- function(df,
                     depth_breaks,
                     zooscan = F,
                     cat_col = NULL,
                     func_col = 'taxo_name',
                     func = length,
                     max_d = NULL,
                     force_bins = F) {
  
  if(is.null(cat_col)){
    cat_col <- get_col_name(df, 'taxo_name')
  }
  
  #get indexing names
  if(func_col == 'taxo_name') {
    func_col <- get_col_name(df,'taxo_name')
  }

  #establish points to break on
  if(zooscan == T) {
    depth_bins <- df$sample_id #this needs to be improved in the future
  } else {
    depth_col <- get_col_name(df, 'depth_offset')
    
    if(is.null(max_d)) {
      max_d <- max(df[[depth_col]])
    }
    
    depth_bins <- cut(df[[depth_col]], 
                      breaks = check_custom(depth_breaks, max_d))
  }
  
  unique_bins <- unique(depth_bins)
  
  agg_df <- aggregate(df[[func_col]], by = list(db = depth_bins,
                                                taxa = df[[cat_col]]),
                      FUN = func)
  
  agg_df[is.na(agg_df)] <- 0
  
  
  
  if(zooscan == F) {
    agg_df <- order_bins(agg_df)
  }
  
  if(force_bins == T) {
    rdf <- force_bins_switch(agg_df)
    
    # annoying fix to taxa name issue
    if(!(is.null(cat_col))) {
      names(rdf)[which(names(rdf) == 'taxa')] <- 'group'
    }
    
    return(rdf)
  }
  
  # annoying fix to taxa name issue
  if(!(is.null(cat_col))) {
    names(agg_df)[which(names(agg_df) == 'taxa')] <- 'group'
  }
  return(agg_df)
}

#' Inside function for force bins option
#' 
#' @param tdf the split dataframe
#' @param taxa_names all possible taxa names
add_count_zeros <- function(tdf, taxa_names){
  if(all(taxa_names %in% tdf$taxa)) {
    return(tdf)
  } else {
    add_taxa <- taxa_names[which(!(taxa_names %in% tdf$taxa))]
    rdf <- data.frame(taxa = c(tdf$taxa, add_taxa),
                      x = c(tdf$x, rep(0,length(add_taxa))))
    return(rdf)
  }
}

#' Force bins will add zeros to bins that don't have any observations
#' 
#' This function should be used with consideration. It is necessary for the UVP
#' because zoo files are blind as to where the UVP did actually collect an image
#' 
#' @param counts the count vector
force_bins_switch <- function(counts) {
  count_bins <- split(counts[,2:3], f=counts$db)
  adj_counts <- lapply(count_bins, add_count_zeros, unique(counts$taxa))
  ret_counts <- list_to_tib(adj_counts, 'db')
  ret_counts <- order_bins(ret_counts[,c(3,1,2)])
  return(ret_counts)
}