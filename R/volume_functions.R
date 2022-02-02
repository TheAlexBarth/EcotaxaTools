#' ellps_vol - calculate volume of an ellipsoid shape
#' 
#' This function takes a data frame of ecotaxa output and returns a vector of ellipsoid shape
#' 
#' Must have column headers formatted. This assumes you are using the pixels input
#' it will return it in mm. If you converted to mm already - this doesn't work
#' unless you renamed converted columns
#' 
#' @param df - the data frame to feed it
ellps_vol  <-  function(df) {
  major_mm <- df$major * df$pixel_mm;
  minor_mm <- df$minor * df$pixel_mm;
  vol_mm <- (4/3) * pi * (minor_mm / 2)^2 * (major_mm / 2);
  
  return(vol_mm);
}

#' spherical_vol - calculate volume of a spherical object