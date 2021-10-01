#' ecoImport - quick way to read tsv files from ecotaxa
#' 
#' 
#' @param fileName path to the tsv export file from ecotaxa
#' @param trim whether or not to trim to certain columns only, default F
#' @param customTrim names of columns to keep in a trim
#' 
ecoImport = function(fileName, trim = F,customTrim = NULL){
  df = read.table(fileName, header = T, as.is = T)
  if(trim == T){
    tdf = df #set temporary df for multiple subsetting
    if(length(customTrim) == 0){ 
      trimVect = c("object_id","object_lat","object_lon",
                    "object_date","object_time","object_depth_max",
                    "object_annotation_category",
                    "object_area","object_min","object_max",
                    "object_x","object_y","object_xm","object_ym","object_bx",
                    'object_by', "object_major",'object_minor',
                    "object_feret", "object_esd",
                    "object_ystart","object_xstart")
    } else {
      trimVect = customTrim
    }
    
    df = tdf[,which(colnames(df) %in% trimVect)]

  }
  names(df)[1] = "object_id" #dirty patch for annoying read-ins
  df$object_idNum = sapply(strsplit(df$object_id, split = "_(?=[^_]+$)",
                                    perl = T), 
                           "[[",2)
  df$object_id = sapply(strsplit(df$object_id, split = "_(?=[^_]+$)",
                                 perl = T), 
                        "[[",1)
  df$object_annotation_category = as.factor(df$object_annotation_category)
  df$process_pixel = df[,which(names(df) %in% c("process_pixel",
                                                "process_particle_pixel_size_mm"))]
  
  return(df)
}
