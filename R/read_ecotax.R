#' read_etx - a function to read tsv from ecotaxa
#' 
#' Honestly, tired of making sure to use ecotaxar package so I copied this one
#' 
#' @param path the file to open
#' 
#' @rdname read_etx
#' @export
read_etx <- function(path){
  df <- read.table(path,header = T,as.is = T,
                   fileEncoding="UTF-8-BOM")
  return(df)
}
