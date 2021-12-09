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