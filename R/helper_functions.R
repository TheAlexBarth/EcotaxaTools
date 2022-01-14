#' add_range
#'
#' copied from helper box
#' 
#' @param number center number
#' @param width range on both sided
#' 
#' @export
add_range <- function(number,width){
  return(c(number - width, number+width))
}

#' nearest
#' 
#' A function to assign a numeric bin between a range
#'
#' @param number the number to check
#' @param vector a numeric vector to match values to
#' 
#' @examples
#' nearest(74.2,seq(0,100,5))
#' 
#' @export
nearest <- function(number,vector){
  index <- which.min(abs(vector - number))
  return(vector[index])
}

#' get_col_name
#' 
#' A function to choose which column to use
#' This is a hacky fix to the issue that column names switch 
#' 
#' @param df the data frame, exported from ecopart or ecotaxa
#' @param goal_name the name of column
#' @param possible possible column names
get_col_name <- function(df, goal_name = NULL,possible = NULL){
  if(is.null(possible)){
    possible <- swtich(goal_name,
                       "object_annotation_category" = c("object_annotation_category",
                                                        "name"),
                       "depth_offset" = c("depth_including_offset",
                                          "depth_offset"),
                       stop("goal_name not in pre-selected options - see source code"))
  }
  if(all(!(names(df) %in% possible))){stop("no column name found in possible set")}
  index <- which(names(df) %in% possible)
  if(length(index) != 1){stop("More than one column name in possible set")}
  return(index)
}



