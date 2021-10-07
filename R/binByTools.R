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
                   equal_number = 100,){
   if(!(method %in% c("Equal Space","Custom","Equal Number","Zooscan"))){
     stop("Invalid Method argument. Choose 'Equal Space','Custom',
          'Equal Number','Zooscan'")
   } else if(method == "Custom") {
     break_vect <- custom_range
   } else if(method == "Equal Space"){
     break_vect <- equal_step
   } else if(method == "Equal Number"){
     break_vect <- equal_number
   } else if(method == "Zooscan"){
     return(df$object_depth_max)
   }
  return(cut(df$object_depth_max, breaks = break_vect))
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
#' 
bin_by_df <- function(df method, custom_range, equal_step = 10,
                      equal_number = 100, secondary = NULL, 
                      secondary_breaks = NULL, return_list = F){
  #zooscan method
  if(method == "Zooscan"){
    depth_bin <- unique(bin_by(df, method = method))
    
    if(length(secondary) == 0){
     calc_list <- vector(mode = "list",length = length(depth_bin))
     for(l in 1:length(depth_bin)){
       cat_table = table(df$object_annotation_category[df$object])
       calc_list[[i]] = as.data.frame(table(df$object_annotation_category[]))
     }
    }
  }
}