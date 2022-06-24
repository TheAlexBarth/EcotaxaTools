#' Get Volumes from Par files
#' 
#' This takes in the par_files from an ecopart object
#' then returns the volume sampled in 1m depth bins
#' 
#' @param ecopart_obj a list from ecopart_import()
#'
#' @export
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



#' Calculate UVP Concentration from Ecopart Object
#' 
#' This function can take in an ecopart object (from ecopart_import)
#' And for a given cast name, it will return the concentration.
#' Relies on default formatting and will return count concentration by default
#' However, you can use func_col and func arguements to pass to bin_taxa 
#' if trying to get biomass/biovolume concentration.
#' 
#' @param ecopart_obj an ecopart object list
#' @param cast_name name of the cast to get concentration of
#' @param depth_breaks a vector of depth break ranges
#' @param ... pass to \code{bin_taxa()} for func and func_col
#' 
#' @importFrom tibble as_tibble
#' 
#' @export
#' 
#' @author Alex Barth
uvp_conc <- function(ecopart_obj, cast_name, depth_breaks, ...) {
  
  #safety check for cast names
  if(!(is.etx_class(ecopart_obj, 'ecopart_obj'))){
    stop('Must enter an ecopart_obj')
  }
  
  if(!(cast_name %in% names(ecopart_obj$zoo_list) |
       !(cast_name %in% names(ecopart_obj$par_list)))) {
    stop('Error in Cast Name')
  }
  
  # calculate abundance of all zooplankton groups
  counts <- bin_taxa(ecopart_obj$zoo_files[[cast_name]],
                     depth_breaks = depth_breaks, force_bins = T, ...)
  
  # get matching volume bins
  vol_df <- as_tibble(ecopart_vol_bin(get_ecopart_vol(ecopart_obj)[[cast_name]],
                                      depth_breaks = depth_breaks))
  
  # calulate concentration
  temp_merge <- merge(counts, vol_df)
  temp_merge$conc_m3 <- (temp_merge$x / temp_merge$vol_sampled) * 1000
  
  rdf <- add_zeros(temp_merge, 'conc_m3')
  class(rdf) <- c('data.frame', 'etx_conc_obj')
  return(rdf)
}

