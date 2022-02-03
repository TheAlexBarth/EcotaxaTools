#' ellps_vol - calculate volume of an ellipsoid shape
#' 
#' This function takes a data frame of ecotaxa output and returns a vector of ellipsoid shape
#' 
#' Must have column headers formatted. This assumes you are using the pixels input
#' it will return it in mm. If you converted to mm already - this doesn't work
#' unless you renamed converted columns
#' 
#' @param df - the data frame to feed it
#' 
#' @export
ellps_vol  <-  function(df) {
  major_mm <- df$major * df$pixel_mm;
  minor_mm <- df$minor * df$pixel_mm;
  vol_mmcu <- (4/3) * pi * (minor_mm / 2)^2 * (major_mm / 2);
  
  return(vol_mmcu);
}

#' sph_vol - calculate volume of a spherical object
#' 
#' This function calculates the volume of an object if it were a sphere
#' Input an ecotaxa df and get back a vector of spherical volums
#' 
#' @param df - an ecotaxa dataframe
#' 
#' @export
sph_vol <- function(df) {
  area_mmsq <- df$area * df$pixel_mm^2; #get area
  vol <- sqrt(area_mmsq)^3 * (4/3) * pi;
  
  return(vol);
}