#' Get all for some column
#' 
#' @param zoo an ecopart object or zoo_list
#' @param col_name a column name in the zoo_dfs
#' @param pixel_conv if using a size metric, should you convert the pixel size?
#' 
#' @example man/examples/get_all.R
#' @export
get_all <- function(zoo, col_name, pixel_conv = F){
  if(is.etx_class(zoo,'ecopart_obj')){
    all_rows <- zoo$zoo_files |> list_to_tib('cast')
    if(pixel_conv) {
      pix_mm <- unique(zoo$meta$acq_pixel)
      if(length(pix_mm) > 1){
        stop("There are multiple pixel conversion in the meta file. Mass querying not available.")
      }
    }
  } else {
    all_rows <- zoo |> list_to_tib('cast')
    if(pixel_conv){
      stop("For pixel conversion must provide an ecopart_obj.")
    }
  }
  
  all_out <- all_rows[[col_name]]
  
  if(pixel_conv) {
    all_out <- all_out * pix_mm
  }
  
  return(all_out)
}