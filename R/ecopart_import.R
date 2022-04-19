#' reads in files from a folder of ecopart raw export
#' 
#' This function requires a path to a folder with the raw output from ecopart
#' and is primarily designed for UVP export objects. It will return a list of lists
#' each list holds tibbles for each cast in a project for: particle, plankton, volume, meta
#' 
#' @importFrom readr read_tsv cols
#' 
#' @param dat_path the path in your compute to the file
#' 
#' @export
#' 
#' @author Alex Barth
ecopart_import <- function(dat_path){
  dct <- dir(dat_path) #get directory list
  par_fnames <- dct[grep("_PAR_",dct)] #get names of par files
  zoo_fnames <- dct[grep("_ZOO_",dct)] #get names of the zooplankton files
  par_meta <- read_tsv(paste(dat_path,dct[grep("_metadata_",dct)],
                             sep = "/"), 
                       col_types = cols())
  
  
  #set up storage for list-read-ins
  par_files <- vector(mode = "list",length = length(par_fnames))
  zoo_files <- vector(mode = "list",length = length(zoo_fnames))
  names(par_files) <- par_fnames
  names(zoo_files) <- zoo_fnames
  for(i in 1:length(par_files)){
    par_files[[i]] <- read_tsv(paste(dat_path,par_fnames[i],
                                     sep = "/"),
                               col_types = cols())
    zoo_files[[i]] <- read_tsv(paste(dat_path,zoo_fnames[i],
                                     sep = "/"),
                               col_types = cols())
    zoo_files[[i]]$pixel_mm <- 0.088;
  }
  
  #format to have cast names
  names(par_files) <- regmatches(names(par_files),
                                 regexpr("(?<=[0-9]_).*?(?=_PAR)",
                                         names(par_files),
                                         perl = T))
  names(zoo_files) <- regmatches(names(zoo_files),
                                 regexpr("(?<=[0-9]_).*?(?=_ZOO)",
                                         names(zoo_files),
                                         perl = T))
  return(list(par_files = par_files,
              zoo_files = zoo_files,
              meta = par_meta))
}
