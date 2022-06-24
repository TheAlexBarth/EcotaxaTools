#' Warning message for unmatched PAR and ZOO files
#' 
#' This is a warning message and provides indices to remove PAR or ZOO files
#' that don't have matches
#' 
#' @param par_fnames the par names
#' @param zoo_fnames the zoo names
ecopart_file_check <- function(par_fnames, zoo_fnames){
  
  #extract profileID's
  par_names <- regmatches(par_fnames, 
                          regexpr("(?<=[0-9]_).*?(?=_PAR)",
                                  par_fnames, 
                                  perl = T))
  zoo_names <- regmatches(zoo_fnames, 
                          regexpr("(?<=[0-9]_).*?(?=_ZOO)",
                                  zoo_fnames, 
                                  perl = T))
  
  par_excess <- which(!(par_names %in% zoo_names)) #excess parnames
  zoo_excess <- which(!(zoo_names %in% par_names)) #excess zoonames
  
  if(length(zoo_excess) == 0 & length(par_excess)==0) {
    if(length(par_names) != length(zoo_names)){
      error("There is something deeply wrong with 
            the file names checker function")
    }
    keep_zoo = c(1:length(zoo_names))
    keep_par = c(1:length(par_names))
  } else if(length(par_excess) == 0) {
    keep_zoo = c(1:length(zoo_names))[-zoo_excess]
    keep_par = 1:length(par_names)
    warning(paste0('There were no par files for: ', 
                   paste(zoo_names[zoo_excess], collapse = " ")))
  } else if(length(zoo_excess) ==0) {
    keep_zoo = 1:length(zoo_names)
    keep_par = c(1:length(par_names))[-par_excess]
    warning(paste0('There were no zoo files for: ', 
                   paste(par_names[par_excess], collapse = " ")))
  } else {
    keep_zoo = c(1:length(zoo_names))[-zoo_excess]
    keep_par = c(1:length(par_names))[-par_excess]
    warning(paste0('There were no par files for: ', 
                   paste(zoo_names[zoo_excess], collapse = " ")))
    warning(paste0('There were no zoo files for: ', 
                   paste(par_names[par_excess], collapse = " ")))
  }
  return(list(keep_zoo = keep_zoo,
              keep_par = keep_par))
}


#' Read in files from a folder of ecopart raw export
#' 
#' This function requires a path to a folder with the raw output from ecopart
#' and is primarily designed for UVP export objects. It will return a list of lists
#' each list holds tibbles for each cast in a project for: particle, plankton, volume, meta
#' 
#' @importFrom readr read_tsv cols
#' 
#' @param dat_path the path in your compute to the file
#' @param trim_to_zoo option to trim profiles for those where zoops are available
#' 
#' @export
#' 
#' @author Alex Barth
ecopart_import <- function(dat_path, trim_to_zoo = F){
  dct <- dir(dat_path) #get directory list
  par_fnames <- dct[grep("_PAR_",dct)] #get names of par files
  zoo_fnames <- dct[grep("_ZOO_",dct)] #get names of the zooplankton files
  par_meta <- read_tsv(paste(dat_path,dct[grep("_metadata_",dct)],
                             sep = "/"), 
                       col_types = cols())
  
  #if interested in zoofiles, drop the excess par files
  if(trim_to_zoo) {
    drop_list <- ecopart_file_check(par_fnames = par_fnames,
                                    zoo_fnames = zoo_fnames)
    
    par_fnames <- par_fnames[drop_list$keep_par]
    zoo_fnames <- zoo_fnames[drop_list$keep_zoo]
  }
  
  #set up storage for list-read-ins
  par_files <- vector(mode = "list",length = length(par_fnames))
  zoo_files <- vector(mode = "list",length = length(zoo_fnames))
  names(par_files) <- par_fnames
  names(zoo_files) <- zoo_fnames
  
  #load in the files
  for(i in 1:length(par_files)){
    par_files[[i]] <- read_tsv(paste(dat_path,par_fnames[i],
                                     sep = "/"),
                               col_types = cols())
    class(par_files[[i]]) <- c(class(par_files[[i]]), 'par_df')
  }
  for(i in 1:length(zoo_files)){
    zoo_files[[i]] <- read_tsv(paste(dat_path, zoo_fnames[i],
                                     sep = '/'),
                               col_types = cols())
    class(zoo_files[[i]]) <- c(class(zoo_files[[i]]), 'zoo_df')
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
  
  # trim meta data to only have profiles which match
  # This step should only matter if you have trim_to_zoo true
  match_profid <- which(par_meta$profileid %in% names(zoo_files) |
                          par_meta$profileid %in% names(par_files))
  par_meta <- par_meta[match_profid,]
  
  if(any(names(par_files) != names(zoo_files))) {
    warning("The par_files and zoo_files don\'t exactly match")
  }
  #construct return structure
  ret_ecpt <- structure(list(par_files = structure(par_files,
                                                   class = c('list', 'par_list')),
                             zoo_files = structure(zoo_files,
                                                   class = c('list', 'zoo_list')),
                             meta = par_meta),
                        class = c('list','ecopart_obj'))
    
  return(ret_ecpt)
}
