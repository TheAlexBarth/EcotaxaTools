#' format_header - a function to format headers
#' 
#' Takes the ecotaxa output and formats certain headers
#' 
#' @param df a data frame to read the headers
#' @export
format_header <- function(df) {
  names(df)[which(names(df) == "object_id")] <- "obj_id";
  names(df) <- sub("object_","",names(df));
  
  #specific cases
  names(df)[which(names(df) %in% c("annotation_category","name"))] <- "taxo_name";
  names(df)[which(names(df) %in% c("process_particle_pixel_size_mm",
                                   "process_pixel"))] <- "pixel_mm";
  
  return(df)
}


#' read_etx - a function to read tsv from ecotaxa
#' 
#' Honestly, tired of making sure to use ecotaxar package so I copied this one
#' 
#' @param path the file to open
#' @param format_headers - if true, it will remove object_. This is required for code compatibiility
#' 
#' @rdname read_etx
#' @export
read_etx <- function(path, format_headers = T) {
  df <- read.table(path,header = T,as.is = T,
                   fileEncoding="UTF-8-BOM")
  if(format_headers == T) {
    df <- format_header(df);
  }
  return(df);
}
