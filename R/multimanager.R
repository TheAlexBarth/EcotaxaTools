#' find_multiple - locate all multiple cateogories
#' 
#' Helper function to identify all types of multiple classes in 
#' object_annotation_category
#' 
#'
#' 
#' @param df a data frame with object_annotation_category
#' 
#' @noRd
find_multiple <- function(df){
  taxa_names <- unique(df$object_annotation_category)
  if(length(taxa_names) == 0){
    stop("No taxa names provided")
  } else {
    mults <- grep("^multiple<",taxa_names)
  }
  if(length(mults) == 0){
    stop("No multiples found")
  } else{
    return(taxa_names[mults])
  }
}

#' get_selection - helper function for user input
#' 
#' This calls for user input then asks which multiples to include
#' then it provides the index for all multiples of interest
#' 
#' @importFrom utils menu
#'  
#' @param df The input dataframe
#' 
#' @noRd
get_selection <-  function(df){
  idMult <- find_multiple(df)
  user_choice <- menu(c(idMult,"All"), title = cat("Found these multipe labels.","Select one?",
                             sep = "\n"))
  if(user_choice > length(idMult)){
    idMult = idMult
    
  } else {
    idMult = idMult[user_choice]
    
  }
  index = which(df$object_annotation_category %in% idMult)
  return(index)
}

#' multiManager - a tool to re-label vignettes with multiple individuals
#' 
#' This function is a tool which facilitates quick renaming of vignettes 
#' with multiple organisms
#' It will select all multiple-object vignettes then request user input for the 
#' names of the new file
#' For each ID entered, it will create a new line with that estimate
#' Note that there will 
#' 
#' @importFrom ecotaxar read_ecotaxa_tsv
#' 
#' @param path location of file to choose, if not entered, a box will open to select file
#' @param morpho_include if true, will create a column of boolean values. True indicates that row will be included in morpho-measurements. New rows from multimanager default to F.
#' @param open_image if true, will find and open images. Requires file connection
#' @param image_path if open image is true, a path to the image folder must be provided
#' 
#' @export
multiManager = function(path, morpho_include = T, open_image = T,
                        image_path = NULL){
  #possible errors
  if(length(image_path) == 0 & open_image == T){
    stop("imagePath not provided")
  }
  ogDf <- read_ecotaxa_tsv(path,trim = F) #open originalDf
  outDf <- ogDf # replicate output df
  
  if(morpho_include == T){
    outDf$morpho_include = rep(T, nrow(outDf))
  }
  
  

}


