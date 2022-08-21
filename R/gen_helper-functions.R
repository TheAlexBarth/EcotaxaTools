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
#' nearest(74.2,seq(0,100,5)) #returns 75
#' 
#' @export
nearest <- function(number,vector){
  index <- which.min(abs(vector - number))
  return(vector[index])
}

#' formatting plugs into timeOfDay
#'
#' @param time a vector
getTime <- function(time) {
  return(as.POSIXct(strftime(time,format = "%H:%M:%S", tz = "UTC"),
                    format = "%H:%M:%S", tz = "UTC"))
}

#' Calculate the time of day as day, twilight, buffer
#' 
#' @param time a character vector of times (formatted to be from parmeta)
#' @param sunrise in UVP timezone (typically UTC)
#' @param sunset sunset
#' @param buffer range around sunset/sunrise to assign as twilight in hours
#' @param tz timezone
#' 
#' @examples 
#' timeOfDay("14:00:00", "06:00:00", "18:00:00", buffer = .5) #Should return day
#' 
#' @export
#' @author Alex Barth
timeOfDay <- function(time,sunrise,sunset,buffer, tz = "UTC") {
  adjTime <- getTime(time)
  hour_buffer <- buffer * 60 * 60
  dawnStart <- as.POSIXct(sunrise,format = '%H:%M:%S',tz = tz) - hour_buffer
  dawnEnd <- as.POSIXct(sunrise,format = '%H:%M:%S',tz = tz) + hour_buffer
  duskStart <- as.POSIXct(sunset, format = '%H:%M:%S',tz = tz) -hour_buffer
  duskEnd <- as.POSIXct(sunset, format = '%H:%M:%S',tz = tz) +hour_buffer
  
  if( adjTime > dawnEnd & adjTime < duskStart) {
    return('day')
  } else if ((adjTime > dawnStart & adjTime < dawnEnd) |
             (adjTime > duskStart & adjTime < duskEnd)) {
    return('twilight')
  } else {
    return('night')
  }
}

#' get_col_name
#' 
#' A function to choose which column to use
#' This is a hacky fix to the issue that column names switch for different instruments
#' 
#' @param df the data frame, exported from ecopart or ecotaxa
#' @param goal_name 'taxo_name', 'depth_offset', or 'taxo_hierarchy
#' @param possible possible column names
#' 
#' @export
get_col_name <- function(df, goal_name = NULL,possible = NULL){
  if(is.null(possible)){
    possible <- switch(goal_name,
                       "taxo_name" = c("object_annotation_category",
                                       "name", "taxo_name"),
                       "depth_offset" = c("depth_including_offset",
                                          "depth_offset",
                                          'depth'),
                       'taxo_hierarchy'= c('taxo_hierarchy',
                                           'object_taxo_hierarchy'),
                       stop("goal_name not in pre-selected options - see source code"))
  }
  if(all(!(names(df) %in% possible))){stop("no column name found in possible set")}
  index <- which(names(df) %in% possible)
  if(length(index) != 1){stop("More than one column name in possible set")}
  return(index)
}

#' Any In
#' 
#' Simply a wrapper for %in% to check if there are any
#' 
#' @param checkVect the input vector
#' @param refVect the reference vector
#' 
#' @param examples
#' any_in(c(0,4,3,1), c(1,7,6,8)) #returns TRUE
#' any_in(c(0,4,3), c(1,7,8,6)) #return FALSE
#' 
#' @export
any_in <- function(checkVect, refVect){
  if(any(checkVect %in% refVect)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
