#' Add particle zeros
#' 
#' Just an inside function I wrote at 2am in the middle of the pacific
#' 
#' @param full_seq the full sequence
#' @param par_sum a par_sum object from calc_par_conc
add_par_zeros <- function(par_sum, full_seq) {
  missing_d <- full_seq[which(!(full_seq %in% par_sum$depth))]
  add_par_rows <- data.frame(depth = missing_d,
                             par_conc = rep(0, length(missing_d)))
  ret_par_sum <- rbind(par_sum, add_par_rows)
  sort_depth <- order(ret_par_sum$depth)
  ret_par_sum <- ret_par_sum[sort_depth,]
  return(ret_par_sum)
}

#' Force 0's to the particle concentrations
#' 
#' @param par_sum a par_conc_df
force_par_zeros <- function(par_sum) {
  max_d <- max(par_sum$depth)
  min_d <- min(par_sum$depth)
  full_seq <- c(min_d:max_d)
  # if not split into esd bins
  if(is.null(par_sum$esd_bin)) {
    if(all(full_seq %in% par_sum$depth)) {
      return(par_sum)
    } else {
      par_sum <- par_sum |> add_par_zeros(full_seq = full_seq)
      return(par_sum)
    }
  } else {
    esd_bin <- par_sum$esd_bin
    par_sum_corr <- par_sum[,c(2,3)] |> 
      split(f = esd_bin) |> 
      lapply(add_par_zeros, full_seq = full_seq) |> 
      list_to_tib(new_col_name = 'esd_bin')
    return(par_sum_corr)
  }
}


#' inner particle concentration calculator
#' 
#' Get the particle volumetric concentraiton from a par file
#' 
#' @param par a par_df or ecopart_obj
#' @param min_esd minimum esd IN MM
#' @param max_esd maximum esd IN MM
#' @param pixel_mm pixel conversion
#' @param img_vol image volume size
calc_par_conc <- function(par, min_esd, max_esd, pixel_mm, img_vol) {
  
  # safety warning
  if(min_esd > 10) {
    warning('the minimum ESD was set fairly large. Are you sure?')
  }
  
  # Vol_sampled
  volume_sampled <- par$imgcount * img_vol
  
  # convert esd range to vol
  min_vol <- (min_esd/2)^3 * (4/3) * pi
  max_vol <- (max_esd/2)^3 * (4/3) * pi
  
  # calculate the volume of each particle size
  par_vol_classes <- calc_sph_vol(par$area, pixel_mm)
  
  #trim by size limits
  keep_sizes <- which(par_vol_classes > min_vol & par_vol_classes < max_vol)
  par <- par[keep_sizes,]
  par_vol_classes <- par_vol_classes[keep_sizes]
  
  #multiple by number of observations
  par_vol_tot <- par_vol_classes * par$nbr
  #calculate concentration
  par_conc <- par_vol_tot / volume_sampled #this is mmcu per L
  
  par_sum <- aggregate(list(par_conc = par_conc), 
                       by = list(depth = par$depth),
                       FUN = sum) |> force_par_zeros()
  class(par_sum) <- c(class(par_sum), 'par_conc_df')
  return(par_sum)
}

#' Inside calculation for particle counts by bin
#' 
#' Calculate based on some bin size the particle concentration
#' 
#' @param par a par df
#' @param bin_limits ESD limits for particle bin sizes in MM
#' @param pixel_mm pixel conversion
#' @param img_vol image volume size
calc_par_conc_bin <- function(par,
                              bin_limits,
                              pixel_mm,
                              img_vol) {
  
  # Vol_sampled
  volume_sampled <- par$imgcount * img_vol
  
  #calculate spherical volume of size classes
  par_vol_classes <- calc_sph_vol(par$area, pixel_mm)
  
  #get classes by ESD
  par_esd_classes <- (par_vol_classes/((4/3)*pi))^(1/3)
  
  #assign bin limits
  par$bin_limits <- cut(par_esd_classes, bin_limits)
  
  #remove NA's from par_df
  drop_rows <- which(is.na(par$bin_limits))
  par <- par[-drop_rows,]
  par_vol_classes <- par_vol_classes[-drop_rows]
  volume_sampled <- volume_sampled[-drop_rows]
  
  #calc total volume and conc
  par_vol_tot <- par_vol_classes * par$nbr
  par_conc <- par_vol_tot / volume_sampled #mmcu per L
  par_sum <- aggregate(list(par_conc = par_conc), 
                       by = list(esd_bin = par$bin_limits,
                                 depth = par$depth),
                       FUN = sum) |> force_par_zeros()
  class(par_sum) <- c(class(par_sum), 'par_conc_df')
  return(par_sum)
}


#' Calculate particle concentration
#' 
#' caculate the particle concentration for a given size class
#' The bulk of this code lives in \code{calc_par_conc} and this is a wrapper
#' 
#' @inheritParams calc_par_conc
#' @param bin_limits optional for returning size-binned particle concentration
#' @export
uvp_par_conc <- function(par, 
                         min_esd = 0, 
                         max_esd = 100, 
                         pixel_mm = NULL, 
                         img_vol = NULL,
                         bin_limits = NULL) {
  if(is.etx_class(par,'ecopart_obj')) {
    
    if(length(unique(par$meta$default_instrumsn)) > 1){
      warning('There are multiple sn\'s. Pixel size and img_vol will be wrong.
              Consider split_ecopart_obj by instrument serial number.')
    }
    
    pixel_mm <- unique(par$meta$acq_pixel)[1]
    img_vol <- unique(par$meta$acq_volimage)[1]
    
    #if no bin_limits are provided, use total method
    if(is.null(bin_limits)) {
      par_sum_list <- lapply(par$par_files,
                             calc_par_conc,
                             min_esd = min_esd,
                             max_esd = max_esd,
                             pixel_mm = pixel_mm,
                             img_vol = img_vol)
    } else {
      par_sum_list <- lapply(par$par_files,
                             calc_par_conc_bin,
                             bin_limits = bin_limits,
                             pixel_mm = pixel_mm,
                             img_vol = img_vol)
    }
    names(par_sum_list) <- names(par$par_files)
    class(par_sum_list) <- c('list', 'par_conc_list')
    return(par_sum_list)
    
  } else if (is.etx_class(par, 'par_list')) {
    
    if(is.null(pixel_mm) | is.null(img_vol)){
      stop('Must provide either ecopart_obj or 
           manually select pixel size and img vol')
    }
    
    #if no bin_limits provided, use total method
    if(is.null(bin_limits)) {
      par_sum_list <- lapply(par,
                             calc_par_conc,
                             min_esd = min_esd,
                             max_esd = max_esd,
                             pixel_mm = pixel_mm,
                             img_vol = img_vol)
    } else {
      par_sum_list <- lapply(par,
                             calc_par_conc_bin,
                             bin_limits = bin_limits,
                             pixel_mm = pixel_mm,
                             img_vol = img_vol)
    }    
    names(par_sum_list) <- names(par)
    class(par_sum_list) <- c('list', 'par_conc_list')
    return(par_sum_list)
  
  } else if(is.etx_class(par, 'par_df')) {
    
    if(is.null(pixel_mm) | is.null(img_vol)){
      stop('Must provide either ecopart_obj or 
           manually select pixel size and img vol')
    }
    #if no bin_limits provided use total method
    if(is.null(bin_limits)) {
      par_sum_df <- par |> calc_par_conc(min_esd = min_esd,
                           max_esd = max_esd,
                           pixel_mm = pixel_mm,
                           img_vol = img_vol)
    } else {
      par_sum_df <- par |> calc_par_conc_bin(bin_limits = bin_limits,
                                             pixel_mm = pixel_mm,
                                             img_vol = img_vol)
    }
    return(par_sum_df)
  }
}
