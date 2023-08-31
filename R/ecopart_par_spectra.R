#' Number Size Spectra by bins
#' 
#' Cacluate the numerical size spectrum by depth bins
#' 
#' @param ecopart_obj the ecopart_obj
#' @param depth_breaks the depth breaks
#' @param pixel_mm pixel conversion
#' @param img_vol image to volume
#' @param method the method to by by
#' @param ... arguments to pass to `assign_spectra_bins`
#' 
#' @export
uvp_par_spectra <- function(ecopart_obj,
                            depth_breaks,
                            pixel_mm = NULL,
                            img_vol = NULL,
                            ...) {
  if(is.etx_class(ecopart_obj, 'ecopart_obj')) {
    par_list <- ecopart_obj$par_files
    pixel_mm <- ecopart_obj$meta$acq_pixel |> unique()
    img_vol <- ecopart_obj$meta$acq_volimage |> unique()
    
    if(length(pixel_mm) > 1 | length(img_vol) > 1) {
      stop('multple conversions provided -- 
           check meta, split project if needed')
    }
    
    ret_list <- par_list |> 
      lapply(par_df_spectra, depth_breaks,
             pixel_mm, img_vol, ...)
    
    return(ret_list)
    
  } else if (is.etx_class(ecopart_obj, 'par_list')) {
    
    ret_list <- ecopart_obj |> 
      lapply(par_df_spectra, depth_breaks,
             pixel_mm, img_vol, ...)
    return(ret_list)
  } else if (is.etx_class(ecopart_obj, 'par_df')) {
    ret_df <- ecopart_obj |> 
      par_df_spectra(depth_breaks, pixel_mm, img_vol, ...)
    return(ret_df)
  } else {
    stop('invalid format for ecopart_obj')
  }
}




#' Calculate numeric spectra in depth bins of par_df
#' 
#' THis is an interior function for a single iteration
#'  
#' @param par_df the original par_df
#' @param depth_breaks the depth breaks
#' @param pixel_mm pixel conversion
#' @param img_vol image to volume
#' @param ... arguments to pass to `assign_spectra_bins`
par_df_spectra <- function(par_df, 
                           depth_breaks,
                           pixel_mm,
                           img_vol,
                           ...) {
  
  
  par_by_db <- uvp_split_par(par_df = par_df,
                             depth_breaks = depth_breaks)
  
  ns_list <- par_by_db |> 
    lapply(calc_par_db_spectra,
           pixel_mm = pixel_mm,
           img_vol = img_vol,
           ...)
  
  rdf <- ns_list |> 
    list_to_tib('db') |> 
    bin_format()
  
  return(rdf)
}


#' Split par_calc
#' 
#' Mainly for lapply to par_by_db
#' 
#' @param single_db par files split by db
#' @param pixel_mm the pixel conversion
#' @param img_vol the image-to-vol conversion
#' @param method log or width
#' @param width if width, need to specify the width
#' @param base if log method, the base of the log
#' @param min_size minimum size
#' @param max_size maximum size
#' 
calc_par_db_spectra <- function(single_db, 
                                pixel_mm,
                                img_vol,
                                method,
                                width = NULL,
                                base = NULL,
                                min_size = NULL,
                                max_size = NULL) {
  
  imgcount <- NULL
  
  single_db$esd <- area_to_esd(single_db$area, pixel_mm)
  
  single_db$size_class <- single_db$esd |> 
    assign_spectra_bins(method = method,
                        width = width,
                        base = base,
                        min_size = min_size,
                        max_size = max_size)
  
  total_images <- single_db[,c(1,2)] |> 
    unique() |> 
    summarize(images = sum(imgcount)) |> 
    as.numeric()
  
  vol_L <- total_images * img_vol
  
  db_num <- single_db |> 
    count_size_classes() |> 
    numeric_spectra(vol_L, needs_format = T)
  
  return(db_num)
}



#' Split par file based on depth bins
#' 
#' @param par_df the particle file
#' @param depth_breaks a vector of depth breaks
uvp_split_par <- function(par_df, depth_breaks) {
  
  if(!is.etx_class(par_df, 'par_df')) {
    stop('irregular format entered - need par_df')
  }
  
  depth_breaks <- check_custom(depth_breaks, max(par_df$depth))
  par_df$db <- cut(par_df$depth, depth_breaks)
  
  par_by_db <- par_df |> split(f = par_df$db, drop = T)
  
  return(par_by_db)
}

