

#' Calculate UVP Concentration
#' 
#' This script takes the UVP data and calculates densities
#' it requries output from raw ecopart export for both taxa and volume
#' These files are automatically imported by the ecopart_import
#' 
#' Eventually I will need to update this function since i don't want looping inside functions
#' 
#' @param cast_names a character vector of cast names to loop over
#' @param zoo_list list of ecopart zooplankton data
#' @param vol_list list of volume ecopart data
#' @param custom_range range of vectors to break into bins
#' @param ... arguments to pass to \code{bin_taxa}
#' 
#' @importFrom tidyr as_tibble
#' 
#' @export
#' 
#' @author Alex Barth
uvp_conc <- function(cast_names, zoo_list, vol_list,
                     custom_range,...) {
  
  if(!all(cast_names %in% names(zoo_list))){stop('Error in cast names')}
  
  conc_list <- vector('list',length(cast_names)) #set up shell
  names(conc_list) <- cast_names # name the list
  
  for(i in 1:length(cast_names)) {
    
    counts <- bin_taxa(zoo_list[[cast_names[i]]],custom_range = custom_range,...)
    #get volumes into bins
    vol_df <-  as_tibble(ecopart_vol_bin(vol_list[[cast_names[i]]],
                                         custom_range = custom_range))
    
    #adjust for fully empty count rows
    if(nrow(counts) != nrow(vol_df)) {
      if(is.null(custom_range)) {
        stop('All methods asides from custom depreciated')
      }
      counts <- add_zero_rows(counts,custom_range) #add zero to count rows
      if(length(vol_df$db) < length(counts$db)) {
        vol_df <- add_zero_rows(vol_df,custom_range)
      }
      
      if(!identical(counts$db,vol_df$db)) {
        stop('depth bins do not match, something wrong this functions')
      }
    }
    
    conc_list[[cast_names[i]]] <- (counts[,-which(names(counts) == 'db')] / vol_df$vol_sampled) * 1000 #concentration converted to cubic M
    conc_list[[cast_names[i]]]$db <- vol_df$db #add depth bins
  }
  
  return(conc_list)
  
}

#' Tool to add similar uvp_casts based on some bin
#' 
#' See also \code{sum_casts_conc()}
#' 
#' @param cast_names character vector of cast names in the list values
#' @param zoo_list an ecopart zooplankton list
#' @param vol_list an ecopart volume list
#' @param custom_range custom depth bin limits
#' @param ... extra arguments to pass to \code{bin_taxa()}
#' 
#' @export
#' @author Alex barth
sum_casts <- function(cast_names, zoo_list, vol_list,custom_range,...) {
  ## Safety checks
  require(EcotaxaTools) #load in package
  
  if(!all(cast_names %in% names(zoo_list))){stop('Error in cast names')}
  
  
  zoo_df <- do.call(rbind,zoo_list[which(names(zoo_list) %in% cast_names)])
  vol_df <- do.call(rbind,vol_list[which(names(vol_list) %in% cast_names)])
  
  counts <- bin_taxa(zoo_df, custom_range,...)
  vol_sum <- ecopart_vol_bin(vol_df, method = "Custom",
                             custom_range = custom_range)
  
  
  #adjust for fully empty count rows
  if(nrow(counts) != nrow(vol_sum)) {
    if(is.null(custom_range)) {
      stop('All methods asides from custom depreciated')
    }
    counts <- add_zero_rows(counts,custom_range) #add zero to count rows
    if(length(vol_sum$db) < length(counts$db)) {
      vol_sum <- add_zero_rows(vol_sum,custom_range)
    }
    
    if(!identical(counts$db,vol_sum$db)) {
      stop('depth bins do not match, something wrong this functions')
    }
  }
  return(list(counts = counts,
              vol = vol_sum))
}

#' Calculation of concentraiton from summed casts
#' 
#' see also \code{sum-casts}
#'
#' @param cast_names character vector of cast names in the list values
#' @param zoo_list an ecopart zooplankton list
#' @param vol_list an ecopart volume list
#' @param custom_range custom depth bin limits
#' @param ... other arguments to pass to \code{bin_taxa()}
#' 
#' @export
#' @author Alex Barth
sum_casts_conc <- function(cast_names,zoo_list,vol_list,custom_range, ...) {
  out_list <- sum_casts(cast_names,zoo_list,vol_list,custom_range,...)
  raw_counts <- out_list$counts[,which(names(out_list$counts) != 'db')]
  out_df <- (raw_counts / out_list$vol$vol_sampled) * 1000
  out_df$db <- out_list$vol$db
  return(out_df)
}

