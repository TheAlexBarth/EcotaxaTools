#' reads in files from a folder of ecopart raw export
#' 
#' This function requires a path to a folder with the raw output from ecopart
#' and is primarily designed for UVP export objects. It will return a list of lists
#' each list holds tibbles for each cast in a project for: particle, plankton, and volume
#' 
#' @importFrom readr read_tsv
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
  par_meta <- invisible(read_tsv(paste(dat_path,dct[grep("_metadata_",dct)],
                                       sep = "/")))
  
  
  #set up storage for list-read-ins
  par_files <- vector(mode = "list",length = length(par_fnames))
  zoo_files <- vector(mode = "list",length = length(zoo_fnames))
  names(par_files) <- par_fnames
  names(zoo_files) <- zoo_fnames
  for(i in 1:length(par_files)){
    par_files[[i]] <- invisible(read_tsv(paste(dat_path,par_fnames[i],
                                               sep = "/")))
    zoo_files[[i]] <- invisible(read_tsv(paste(dat_path,zoo_fnames[i],
                                     sep = "/")))
    zoo_files[[i]]$pixel_mm <- 0.088;
  }
  ecopar_vol <- vector(mode = "list",length(par_files)) #storage
  for(i in 1:length(par_files)){
    ecopar_vol[[i]] <- unique(par_files[[i]][,c(1,2)]) #get the depth and imgcount
    ecopar_vol[[i]][,2] <- ecopar_vol[[i]][,2] * par_meta$acq_volimage #multiple by image size
    names(ecopar_vol[[i]])[2] <- "vol_sampled" #name it
  }
  names(ecopar_vol) <- names(par_files)
  
  #format to have cast names
  names(ecopar_vol) <- regmatches(names(ecopar_vol),
                                  regexpr("(?<=_).*?(?=_)",names(ecopar_vol),
                                          perl = T))
  names(par_files) <- regmatches(names(par_files),
                                 regexpr("(?<=_).*?(?=_)",names(par_files),
                                         perl = T))
  names(zoo_files) <- regmatches(names(zoo_files),
                                 regexpr("(?<=_).*?(?=_)",names(zoo_files),
                                         perl = T))
  return(list(vol_files = ecopar_vol,
              par_files = par_files,
              zoo_files = zoo_files))
}
