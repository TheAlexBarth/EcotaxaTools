#' vol_sampled - get the volume sampled from a UVP cast
#' 
#' @importFrom utils read.table
#' 
#' @param cast_name - the name of the cast
#' @param path - where the dat and meta files are located
#' @param method "Equal Space", "Custom", "Equal Number" "Zooscan"
#' @param custom_range vector of ranges
#' @param equal_step create equal steps of this size
#' @param equal_number create this number of ranges
#' 
#' @export
vol_sampled <- function(cast_name, path, method, custom_range, equal_step = 10,
                        equal_number = 100){
  
  #read in first images
  initial_dat <- read.table(paste(path,cast_name,"_datfile.txt",sep = ""),
                            header = F, sep = ";")
  meta_dat <- read.table(paste(path,cast_name,"_meta.txt",sep = ""),
                         header = T, sep = ";")
  
  #trim to first image
  first_img <- meta_dat$firstimage
  trim_dat <- initial_dat[initial_dat[,1] >= first_img,]
  trim_dat <- as.numeric(trim_dat[,3])/10 #convert centibar to decibar
  
  # get depth bin
  break_vect <- switch(method, 
                       "Equal Space" = seq(0,max(trim_dat[,3]),equal_step),
                       "Equal Number" = equal_number,
                       "Custom" = custom_range,
                       stop("Invalid Method, needs to be Equal Space, Equal Number,
                            or Custom"))
  
  #count the number of images per depth bin
  rdf <- as.data.frame(table((cut(trim_dat,breaks = break_vect))))
  rdf[,3] <- rdf[,2] * 1.1
  
  names(rdf) <- c("depth_bin","num_img","vol_sampled")
  return(rdf)
}