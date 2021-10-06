#' Call this function to edit vectorSelect
#' 
#' BE CAREFUL. this will edit the package to your local version of the package
#' This makes your script less sharable but easiy if you are doing repeated
#' work in house. If you reinstall package it will restore to the default
#' 
#' @export
addCustomTrim = function(){
  trace(vectorSelect, edit = T)
}

#' vectorSelect - background mechanism for storing trim factors
#' 
#' @param trimMode set to "Minimal" or "Morpho" unless local selections added
vectorSelect = function(trimMode){
  
  necessary = c("object_id","object_lat","object_lon",
                "object_date","object_time","object_depth_min",
                "object_depth_max","object_annotation_category")
  
  if(trimMode == "Minimal"){
    output = necessary
  } else if (trimMode == "Morpho"){
    output = c(necessary,"object_area","object_mean","object_stddev",
               "object_mode","object_min","object_max","object_perim.",
               "object_major","object_minor","object_circ.","object_feret",
               "object_intden","object_median","object_skew","objet_kurt",
               "object_%area","object_area_exc","object_fractal",
               "object_skelarea","object_slope","object_histcum1",
               "object_histcum2","object_histcum3","object_symetrieh",
               "object_symetriev","object_symetriehc","objet_symetrievc",
               "object_convperim","object_convarea","object_fcons",
               "object_thickr","object_esd","object_elongation","object_range",
               "object_cv","object_sr", "object_perimferet","object_perimmajor",
               "process_pixel")
  } else if(trimMode == "SampleInfo"){
    output = c(necessary, "sample_zmax","sample_zmin","sample_tot_vol")
  } else {
    stop("No valid trimMode provided, Choose Minimal,Morpho,SampleInfo")
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
#' @export
#'
tsvTrim = function(df, trimMode = "Minimal", customVector = NULL){
  if(trimMode == "Custom" & length(customVector) == 0){
    stop("No custom vector provided")
  } else if(trimMode == "Custom" & length(customVector) != 0){
    trimVect = customVector
  } else{
    trimVect = vectorSelect(trimMode)
  } 
  
  df$process_pixel = df[,which(names(df)%in%c("process_particle_pixel_size_mm",
                                            "process_pixel"))][1]
  tdf = df[,which(colnames(df) %in% trimVect)]
  return(tdf)
}
