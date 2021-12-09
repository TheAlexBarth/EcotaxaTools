#' Call this function to edit vectorSelect
#' 
#' BE CAREFUL. this will edit the package to your local version of the package
#' This makes your script less sharable but easiy if you are doing repeated
#' work in house. If you reinstall package it will restore to the default
#' 
#' @export
add_custom_trim <- function(){
  trace(vector_select, edit = T)
}

#' vector_select - background mechanism for storing trim factors
#' 
#' @param trim_mode set to "Minimal" or "Morpho" unless local selections added
vector_select <- function(trim_mode){
  
  necessary <- c("object_id","object_lat","object_lon",
                "object_date","object_time","object_depth_min",
                "object_depth_max","object_annotation_category")
  
  if(trim_mode == "Minimal"){
    output <- necessary
  } else if (trim_mode == "Morpho"){
    output <-  c(necessary,"object_area","object_mean","object_stddev",
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
  } else if(trim_mode == "SampleInfo"){
    output <-  c(necessary, "sample_zmax","sample_zmin","sample_tot_vol")
  } else {
    stop("No valid trimMode provided, Choose Minimal,Morpho,SampleInfo")
  }
  return(output)
}

#' tsv_trim - an automated trimming tool for ecotaxa tsv's
#' 
#' This function has a built-in list of columns to trim tsv files from ecotaxa
#' I retired my ecoImport funtion after discovering ecotaxar's much better
#' read_ecotaxa_tsv funcitons
#' 
#' 
#' @param df the tsv file from ecoImport
#' @param trim_mode set to "Minimal", "Morpho","sample_info", "Custom"
#' @param custom_vector if "Custom" trim_mode provide a vector of column names
#' 
#' @export
#'
tsv_trim <-  function(df, trim_mode = "Minimal", custom_vector = NULL){
  if(trim_mode == "Custom" & length(custom_vector) == 0){
    stop("No custom vector provided")
  } else if(trim_mode == "Custom" & length(custom_vector) != 0){
    trim_vect <- custom_vector
  } else{
    trim_vect <- vector_select(trim_mode)
  } 
  
  df$process_pixel <- df[,which(names(df)%in%c("process_particle_pixel_size_mm",
                                            "process_pixel"))][1]
  tdf <- df[,which(colnames(df) %in% trim_vect)]
  return(tdf)
}
