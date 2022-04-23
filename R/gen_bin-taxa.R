#' bin_taxa() - a modern rendition of bin_by_df
#' This version allows for summing over some factor like dry mass or volume
#' 
#' It will sum for each individual in a depth bin by taxa
#' 
#' @importFrom tidyr pivot_wider
#' 
#' @param df a data frame should have a taxo_name, name, or object_annotation_category column
#' @param depth_breaks the limits of what bins should be made
#' @param zooscan if the df is a zooscan export set to true, default false
#' @param func_col a single character vector of the value to apply function to; esd, biomass, drymass, etc
#' @param func the function to apply
#' 
#' @export
#' @author Alex Barth
bin_taxa <- function(df,depth_breaks,zooscan = F,
                    func_col = 'taxo_name',func = length) {
  
  cat_col <- get_col_name(df, 'taxo_name')
  
  #get indexing names
  if(func_col == 'taxo_name') {
    func_col <- get_col_name(df,'taxo_name')
  }

  #establish points to break on
  if(zooscan == T) {
    depth_bins <- df$sample_id #this needs to be improved in the future
  } else {
    depth_col <- get_col_name(df, 'depth_offset')

    depth_bins <- cut(df[[depth_col]], 
                      breaks = check_custom(depth_breaks, max(df[[depth_col]])))
  }
  
  unique_bins <- unique(depth_bins)
  
  agg_df <- aggregate(df[[func_col]], by = list(db = depth_bins,
                                                taxa = df[[cat_col]]),
                      FUN = func)
  
  agg_df[is.na(agg_df)] <- 0
  if(zooscan == F) {
    agg_df <- order_bins(agg_df)
  }
  return(agg_df)
}

