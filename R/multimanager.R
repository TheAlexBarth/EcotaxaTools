#' findMultiple - locate all multiple cateogories
#' 
#' Helper function to identify all types of multiple classes in 
#' object_annotation_category
#' 
#' @param df a data frame with object_annotation_category
#' 
findMultiple = function(df){
  taxaNames = unique(df$object_annotation_category)
  if(length(taxaNames) == 0){
    stop("No taxa names provided")
  } else {
    mults = grep("^multiple<",taxaNames)
  }
  if(length(mults) == 0){
    stop("No multiples found")
  } else{
    return(taxaNames[mults])
  }
}

#' getMultiIndex - helper function for user input
#' 
#' This calls for user input then asks which multiples to include
#' then it provides the index for all multiples of interest
#' 
getSelection = function(df){
  idMult = findMultiple(df)
  userChoice = menu(c(idMult,"All"), title = cat("Found these multipe labels.","Select one?",
                             sep = "\n"))
  if(userChoice > length(idMult)){
    idMult = idMult
    
  } else {
    idMult = idMult[userChoice]
    
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
#' @importFrom ecotaxar -read_ecotaxa_tsv
#' 
#' @param path location of file to choose, if not entered, a box will open to select file
#' @param morphoInclude if true, will create a column of boolean values. True indicates that row will be included in morpho-measurements. New rows from multimanager default to F.
#' @param openImage if true, will find and open images. Requires file connection
#' @param imagePath if open image is true, a path to the image folder must be provided
#' @param removeMultiple if true, this will delete the multiple< lines
#' 

multiManager = function(path, morphoInclude = T, openImage = T,
                        imagePath = NULL){
  #possible errors
  if(length(imagePath) == 0 & openImage == T){
    stop("imagePath not provided")
  }
  ogDf = read_ecotaxa_tsv(path,trim = F) #open originalDf
  outDf = ogDf # replicate output df
  
  if(morphoInclude == T){
    outDf$morphoInclude = rep(T, nrow(outDf))
  }
  
  

}


