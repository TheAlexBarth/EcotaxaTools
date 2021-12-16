#' bin_by - a function to make easy cuts by default 
#' 
#' This fuction will return a vector for which depth bins for each observation
#' 
#' @param df the vector to include to be cut. Should be an ecotaxa tsv export
#' @param method "Equal Space", "Custom", "Equal Number" "Zooscan"
#' @param custom_range vector of ranges
#' @param equal_step create equal steps of this size
#' @param equal_number create this number of ranges
#'
#' @export 
bin_by <- function(df, method, custom_range = NULL, equal_step = 10,
                   equal_number = 100){

  if(method == "Zooscan"){
    return(df$object_depth_max)
  } else {
    break_vect <- switch(method, "Custom" = custom_range, 
                         "Equal Space" = equal_step,
                         "Equal Number" = equal_number,
                         stop("Invalid Method argument. Choose 'Equal Space',
                         'Custom','Equal Number','Zooscan'"))
    return(cut(df$object_depth_max, breaks = break_vect))
  }
}

#' bin_by_df - returns a data frame for counts by bin
#' 
#' will count number of observations for each bin then return a count dataframe
#' 
#' @param df the vector to include to be cut. Should be an ecotaxa tsv export
#' @param method "Equal Space", "Custom", "Equal Number" "Zooscan"
#' @param custom_range vector of ranges
#' @param equal_step create equal steps of this size
#' @param equal_number create this number of ranges
#' @param secondary a secondary characteristic (like esd) to split these on
#' @param secondary_breaks the range at which to break the secondary var
#' @param return_list a T/F to get back a list of secondary breaks
#' 
#' @export
bin_by_df <- function(df, method, custom_range, equal_step = 10,
                      equal_number = 100, secondary = NULL, 
                      secondary_breaks = NULL, return_list = F){
  df = as.data.frame(df) #why do tibbles exist?!?
  df$object_annotation_category = as.factor(df$object_annotation_category)
  
  depth_bins <- bin_by(df, method = method,custom_range, equal_step = 10,
                         equal_number = 100)
  unique_bins <- unique(depth_bins)
    
  if(length(secondary) == 0){
    calc_list <- vector(mode = "list",length = length(unique_bins))
    for(i in 1:length(unique_bins)){
       cat_table <- table(df$object_annotation_category[depth_bins 
                                                       == unique_bins[i]])
       calc_list[[i]] <- as.data.frame(cat_table)
    }
    rdf <- as.data.frame(matrix(nrow = length(unique_bins),
                                ncol = length(unique(df$object_annotation_category))))
    row.names(rdf) <- unique_bins
    names(rdf) <- sort(unique(df$object_annotation_category))
    for(r in 1:nrow(rdf)){
       rdf[r,] <- calc_list[[r]][,2]
    }
  } else if(length(secondary) > 0){
    if(class(secondary) == "numeric"){
      sec_levels <- as.character((cut(df[,which(names(df)==secondary)],
                                   secondary_breaks)))
    } else if(class(secondary) == "character"){
      sec_levels <- df[,which(names(df)==secondary)]
    } else{
      stop("Error in secondary class - needs to be numeric or character")
    }
    
    macro_list <- vector(mode = "list", length = length(unique(sec_levels)))
    calc_list <- vector(mode = "list",length = length(unique(sec_levels)))
    rdl <- vector(mode = "list", length = length(unique(sec_levels)))
    
    for(l in 1:length(unique(sec_levels))){
      macro_list[[l]] <- df[which(sec_levels == unique(sec_levels)[l]),]
      calc_list[[l]] <-  vector(mode = "list", length = length(unique_bins))
      
      #get sub-lists of data
      for(i in 1:length(unique_bins)){
        cat_table <- table(macro_list[[l]]$object_annotation_category[depth_bins 
                                                                      == unique_bins[i]])
        calc_list[[l]][[i]] <- as.data.frame(cat_table)
      }
      #count up sublists
      rdl[[l]] <- as.data.frame(matrix(nrow = length(unique_bins),
                                       ncol = length(unique(df$object_annotation_category))))
      row.names(rdl[[l]]) <- unique_bins
      names(rdl[[l]]) <- sort(unique(df$object_annotation_category))
      for(r in 1:nrow(rdl[[l]])){
        rdl[[l]][r,] <- calc_list[[l]][[r]][,2] #fill out dataframe
      }
      rdl[[l]]$bin <- unique(sec_levels)[l]
    }
    if(return_list == T){
      return(rdl)
    } else {
      rdf <- do.call("rbind",rdl)
    }
  } else {
    stop("Unidentified error - issue with secondary value")
  }
    return(rdf)
}


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
vol_sampled <- function(cast_name, path, method, custom_range, equal_step = 10,
                        equal_number = 100){
  
  #read in first images
  initial_dat <- read.table(paste(path,cast_name,"_datfile.txt",sep = ""),
                                 header = F, sep = ";")
  meta_dat <- read.table(paste(path,cast_name,"_meta.txt",sep = ""),
                         header = T, sep = ";")
  
  #trim to first image
  first_img <- meta_data$firstimage
  trim_dat <- initial_dat[intial_dat[,1] >= first_img,]
  trim_dat <- as.numeric(trim_dat[,3])/10 #convert centibar to decibar
  
  # get depth bin
  break_vect <- switch(method, 
                       "Equal Space" = seq(0,max(trim_dat[,3]),equal_step),
                       "Equal Number" = equal_number,
                       "Custom" = custom_range,
                       stop("Invalid Method, needs to be Equal Space, Equal Number,
                            or Custom"))
  
  #count the number of images per depth bin
  rdf <- as.data.frame(table((cut(trim_dat[,3],breaks = break_vect))))
  rdf[,3] <- rdf[,2] * 1.1
  
  names(rdf) <- c("depth_bin","num_img","vol_sampled")
  return(rdf)
}
  
  
  