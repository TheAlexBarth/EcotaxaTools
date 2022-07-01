#' Ecopart Volume Binning
#' 
#' This function calculates the volume for each 1-m depth bin 
#' 
#' @param df a data frame to enter
#' @param depth_breaks limits for depth bins
#' 
#' @importFrom stats aggregate
#' 
#' @export
ecopart_vol_bin <- function(df,depth_breaks) {
  
  #check that df is correct format
  if(!identical(names(df), c("depth", "vol_sampled"))) {
    stop('Input data frame must be in two-column layout with depth, vol_sampled')
  }
  
  break_vect <- check_custom(depth_breaks, max(df$depth))
  
  bins <- cut(df$depth, breaks = break_vect)
  
  out_df <- aggregate(df$vol_sampled,by = list(db = bins),FUN = sum)
  names(out_df) <- c('db','vol_sampled')
  return(out_df)
}