#' multiManager - a tool to re-label vignettes with multiple individuals
#' 
#' This function is a tool which facilitates quick renaming of vignettes 
#' with multiple organisms
#' It will select all multiple-object vignettes then request user input for the 
#' names of the new file
#' For each ID entered, it will create a new line with that estimate
#' Note that there will 
#' 
#' @param path location of file to choose, if not entered, a box will open to select file
#' @param morphoInclude if true, will create a column of boolean values. True indicates that row will be included in morpho-measurements. New rows from multimanager default to F.
#' @param openImage if true, will find and open images. Requires file connection
#' @param imagePath if open image is true, a path to the image folder must be provided

multiManager = function(path, morphoInclude = T, openImage = T, 
                        imagePath = NULL){
  if(length(imagePath) == 0 & openImage == T){
    stop("imagePath not provided")
  }
  ogDf = ecoImport(path,trim = F) #open originalDf
  
  multiDf

}
