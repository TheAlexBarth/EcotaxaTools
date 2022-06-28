#' Calculate the ellipsoid volume of a vignette
#' 
#' Should give the raw input with pixels for the size values
#' 
#' @param pixel_mm pixel to mm conversion
#' @param major the major axis, can be a vector
#' @param minor the minor axis, can be a vector
#'
#' @export
calc_ellps_vol  <-  function(major, minor, pixel_mm) {
  major_mm <- major * pixel_mm;
  minor_mm <- minor * pixel_mm;
  vol_mmcu <- (4/3) * pi * (minor_mm / 2)^2 * (major_mm / 2);
  
  return(vol_mmcu)
}

#' Calculate Spherical Volume of vignette
#'
#' @inheritParams calc_ellps_vol
#' @param area the area in pixels for a vignette
#' @export
calc_sph_vol <- function(area, pixel_mm) {
  area_mmsq <- area * pixel_mm^2 
  vol_mmcu <- (sqrt(area_mmsq)/pi)^3 * (4/3) * pi
  
  return(vol_mmcu)
}

#' Get major and minor axis from zoo_df
#' 
#' @param zoo_df the zoo_df
get_zoo_df_features <- function(zoo_df) {
  stopifnot(!is.etx_class(zoo_df, 'zoo_df'), 'Must provide zoo_df object')
  
  major <- zoo_df$major
  minor <- zoo_df$minor
  area <- zoo_df$area
  return(major = major,
         minor = minor,
         area = area)
}

#' Get vignette biovolume
#' 
#' This function takes a df and will direct it 
#' 
#' @param input a zoo_df. use add_zoo for ecopart_objs
#' @param shape assume a sphere or ellipsoid
#' @param pixel_mm option to user specify pixel size in mm
#' 
#' @export
biovolume <- function(input, shape, pixel_mm) {
  
  if(is.etx_class(input, 'ecopart_obj') | is.etx_class(input, 'zoo_list')) {
    stop('To add biovolume to an ecopart object or zoo_list use add_zoo()')
  } else if (!is.etx_class(input, 'zoo_df')) {
    stop('Only zoo_df format accepted')
  }
  
  features <- get_zoo_df_features(input)
  
  if(shape == 'ellipsoid') {
    vol_mmcu <- calc_ellps_vol(features$major,
                               features$minor,
                               pixel_mm)
    return(vol_mmcu)
  } else if(shape == 'sphere'){
    vol_mmcu <- calc_sph_vol(features$area,
                             pixel_mm)
    return(vol_mmcu)
  } else {
    stop('No valid shape provided')
  }
  
}
