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

#' Get Volumes from Par files
#' 
#' This takes in the par_files from an ecopart object
#' then returns the volume sampled in 1m depth bins
#' 
#' @param ecopart_obj a list from ecopart_import()
#'
#' @export
#' 
#' @seealso [uvp_par_conc()]
#' 
#' @author Alex Barth
get_ecopart_vol <- function(ecopart_obj) {
  
  par_files = ecopart_obj$par_files
  par_meta = ecopart_obj$meta
  ecopar_vol <- vector(mode = "list",length(par_files)) #storage
  for(i in 1:length(par_files)){
    ecopar_vol[[i]] <- unique(par_files[[i]][,c(1,2)]) #get the depth and imgcount
    ecopar_vol[[i]][,2] <- ecopar_vol[[i]][,2] * par_meta$acq_volimage #multiple by image size
    names(ecopar_vol[[i]])[2] <- "vol_sampled" #name it
  }
  names(ecopar_vol) <- names(par_files)
  
  return(ecopar_vol)
}