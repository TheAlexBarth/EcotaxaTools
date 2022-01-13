#' vol_sampled - get the volume sampled from a UVP cast
#' 
#' @importFrom utils read.table
#' 
#' @param cast_name - the name of the cast
#' @param path - where the UVP project are located
#' @param method "Equal Space", "Custom", "Equal Number" "Zooscan"
#' @param custom_range vector of ranges
#' @param equal_step create equal steps of this size
#' @param equal_number create this number of ranges
#' @param depth_as_num logical to ask if depth should be printed as number
#' 
#' @export
vol_sampled <- function(cast_name, path, method, custom_range, equal_step = 10,
                        equal_number = 100, depth_as_num = F){
  dir_path <- paste(path,"work",cast_name,sep = "/") #set up path to the work folder
  dat_name <- paste(cast_name,"datfile.txt",sep = "_")
  meta_name <- paste(cast_name,"meta.txt",sep = "_")
  #read in first images
  initial_dat <- read.table(paste(dir_path,dat_name,sep = "/"),
                            header = F, sep = ";")
  meta_dat <- read.table(paste(dir_path,meta_name,sep = "/"),
                         header = T, sep = ";")
  
  #trim to first image
  first_img <- meta_dat$firstimage
  trim_dat <- initial_dat[initial_dat[,1] >= first_img,]
  trim_dat[,3] <- as.numeric(trim_dat[,3])/10 + 1.2 #convert to msw and sensor offset
  
  # get depth bin
  break_vect <- switch(method, 
                       "Equal Space" = seq(0,max(trim_dat[,3]),equal_step),
                       "Equal Number" = equal_number,
                       "Custom" = custom_range,
                       stop("Invalid Method, needs to be Equal Space, Equal Number,
                            or Custom"))
  
  #count the number of images per depth bin
  rdf <- as.data.frame(table((cut(trim_dat[,3],breaks = break_vect))))
  rdf[,3] <- rdf[,2] * meta_dat$volimage
  
  #pull lower depth if that is better
  if(depth_as_num == T){
    rdf[,1] <- regmatches(rdf[,1],regexpr("(?<=,).*(?=])",rdf[,1],perl = T))
    rdf[,1] <- as.numeric(rdf[,1])
    names(rdf) <- c("depth","num_img","vol_sampled")
  } else {
    names(rdf) <- c("depth_bin","num_img","vol_sampled")
  }
  return(rdf)
}