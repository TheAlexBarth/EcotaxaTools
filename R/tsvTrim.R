#' Call this function to edit vectorSelect
#' 
#' BE CAREFUL. this will edit the package to your local version of the package
#' This makes your script less sharable but easiy if you are doing repeated
#' work in house. If you reinstall package it will restore to the default
addCustomTrim = function(){
  trace(vectorSelect, edit = T)
}

#' vectorSelect - background mechanism for storing trim factors
#' 
#' @param trimMode set to "Minimal" or "Morpho" unless local selections added
vectorSelect = function(trimMode){
  
  necessary = c("object_id","object_lat","object_lon",
                "object_date","object_time","object_depth_max",
                "object_annotation_category")
  
  if(trimMode == "Minimal"){
    output = necessary
  } else if (trimMode == "Morpho"){
    output = c(necessary,"object_major","object_minor",
               "object_area", "object_feret","object_perim.","object_circ.",
               "object_elongation","object_thickr", 'object_ver.sym', "object_major",
               'object_minor', "object_feret", "object_esd")
  }
  return(output)
}

#' tsvTrim - an automated trimming tool for ecotaxa tsv's
#' 
#' This function has a built-in list of columns to trim tsv files from ecotaxa
#' I retired my ecoImport funtion after discovering ecotaxar's much better
#' read_ecotaxa_tsv funcitons
#' 
#' 
#' @param df the tsv file from ecoImport
#' @param trimMode Which preset trim set "Minimal", "Morpho", or "Custom"
#' @param customVector if "Custom" trimMode provide a vector of column names
#'
tsvTrim = function(df, trimMode = "Minimal", customVector = NULL){
  trimVect = vectorSelect(trimMode)
  tdf = df[,which(colnames(df) %in% trimVect)]
}
