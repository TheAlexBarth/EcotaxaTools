#' Inside function to assign casts for lapply
#' 
#' @inheritParams uvp_conc
cast_assign <- function(cast_name, ecopart_obj, ...) {
  conc_output <- ecopart_obj |> 
    uvp_conc(cast_name, max_d = max(ecopart_obj$par_files[[cast_name]]$depth),...) |> 
    suppressWarnings()
  
  return(conc_output)
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
#' @param ... pass to [bin_taxa()] for func and func_col
#' 
#' @importFrom tibble as_tibble
#' 
#' @export
#' 
#' @author Alex Barth
uvp_conc <- function(ecopart_obj, cast_name, depth_breaks, ...) {
  warning(
    "You are using uvp_conc directly. It is suggested to use the new option
    uvp_zoo_conc() for better performance and options. This is a limited interior
    which is only still available for my old code to still run."
  )
  
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
  
  rdf <- data.frame(data.frame(db = temp_merge$db,
                               group = temp_merge$group,
                               conc_m3 = temp_merge$conc_m3))
  class(rdf) <- c('data.frame', 'etx_conc_obj')
  return(rdf)
}


#' Calculate UVP Zooplankton Concentration from an Ecopart Object
#' 
#' This function takes in an ecopart object and will calculate the
#' concentration for zooplankton data. This is a big wrapper function to
#' build and add functionality to [uvp_conc()].
#' 
#' @param ecopart_obj an ecopart object list
#' @param cast_name the name (or names of a cast)
#' @param breaks a vector to break on
#' @param ... pass into [bin_taxa()]
#' 
#' @export
uvp_zoo_conc <- function(ecopart_obj,
                         cast_name = NULL,
                         breaks,
                         ...) {
  
  if(!(is.etx_class(ecopart_obj, 'ecopart_obj'))) {
    'Must provide an ecopart_obj class object'
  }
  
  
  # go for the full cast list
  if(is.null(cast_name)) {
    cast_name <- names(ecopart_obj$zoo_files)
  }
  
  if(length(cast_name) > 1) {
    ret_list <- cast_name |> lapply(cast_assign,
                                    ecopart_obj,
                                    depth_breaks = breaks,
                                    ...)
                                    
    names(ret_list) <- cast_name
    
    class(ret_list) <- c('list','zoo_conc_list')
    return(ret_list)
  } else {
    ret_df <- uvp_conc(ecopart_obj = ecopart_obj,
                       cast_name = cast_name,
                       depth_breaks = breaks,
                       ...)
    return(ret_df)
  }
}
