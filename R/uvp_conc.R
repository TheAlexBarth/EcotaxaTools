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
  if(!(cast_name %in% names(ecopart_obj$zoo_list) |
       !(cast_name %in% names(ecopart_obj$par_list)))) {
    stop('Error in Cast Name')
  }
  
  # calculate abundance of all zooplankton groups
  counts <- bin_taxa(ecopart_obj$zoo_files[[cast_name]],
                     depth_breaks = depth_breaks, ...)
  
  # get matching volume bins
  vol_df <- as_tibble(ecopart_vol_bin(get_ecopart_vol(ecopart_obj)[[cast_name]],
                                      depth_breaks = depth_breaks))
  
  # calulate concentration
  temp_merge <- merge(counts, vol_df)
  temp_merge$conc_m3 <- (temp_merge$x / temp_merge$vol_sampled) * 1000
  
  rdf <- add_zeros(temp_merge, 'conc_m3')
  return(rdf)
}



#' #' Tool to add similar uvp_casts based on some bin
#' #' 
#' #' See also \code{sum_casts_conc()}
#' #' 
#' #' @param cast_names character vector of cast names in the list values
#' #' @param zoo_list an ecopart zooplankton list
#' #' @param vol_list an ecopart volume list
#' #' @param custom_range custom depth bin limits
#' #' @param ... extra arguments to pass to \code{bin_taxa()}
#' #' 
#' #' @export
#' #' @author Alex barth
#' sum_casts <- function(cast_names, zoo_list, vol_list,custom_range,...) {
#'   ## Safety checks
#'   require(EcotaxaTools) #load in package
#'   
#'   if(!all(cast_names %in% names(zoo_list))){stop('Error in cast names')}
#'   
#'   
#'   zoo_df <- do.call(rbind,zoo_list[which(names(zoo_list) %in% cast_names)])
#'   vol_df <- do.call(rbind,vol_list[which(names(vol_list) %in% cast_names)])
#'   
#'   counts <- bin_taxa(zoo_df, custom_range,...)
#'   vol_sum <- ecopart_vol_bin(vol_df, method = "Custom",
#'                              custom_range = custom_range)
#'   
#'   
#'   #adjust for fully empty count rows
#'   if(nrow(counts) != nrow(vol_sum)) {
#'     if(is.null(custom_range)) {
#'       stop('All methods asides from custom depreciated')
#'     }
#'     counts <- add_zero_rows(counts,custom_range) #add zero to count rows
#'     if(length(vol_sum$db) < length(counts$db)) {
#'       vol_sum <- add_zero_rows(vol_sum,custom_range)
#'     }
#'     
#'     if(!identical(counts$db,vol_sum$db)) {
#'       stop('depth bins do not match, something wrong this functions')
#'     }
#'   }
#'   return(list(counts = counts,
#'               vol = vol_sum))
#' }
#' 
#' #' Calculation of concentraiton from summed casts
#' #' 
#' #' see also \code{sum-casts}
#' #'
#' #' @param cast_names character vector of cast names in the list values
#' #' @param zoo_list an ecopart zooplankton list
#' #' @param vol_list an ecopart volume list
#' #' @param custom_range custom depth bin limits
#' #' @param ... other arguments to pass to \code{bin_taxa()}
#' #' 
#' #' @export
#' #' @author Alex Barth
#' sum_casts_conc <- function(cast_names,zoo_list,vol_list,custom_range, ...) {
#'   out_list <- sum_casts(cast_names,zoo_list,vol_list,custom_range,...)
#'   raw_counts <- out_list$counts[,which(names(out_list$counts) != 'db')]
#'   out_df <- (raw_counts / out_list$vol$vol_sampled) * 1000
#'   out_df$db <- out_list$vol$db
#'   return(out_df)
#' }

