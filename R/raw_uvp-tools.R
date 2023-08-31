#' Read in raw dat files
#' 
#' Sometimes it is useful to read in the dat files from
#' a UVP project. Note, that this does not include the functionality
#' to read directly from the raw folder. It is not advisable to read
#' directly from the raw file. However, if you want to evaluate the firstImage
#' detection, then you'll need to do that.
#' 
#' Also be aware the depth does not include offset
#' 
#' @param proj_path the path to the project root folder
#' @param cast_names names of casts (in work folder). If null will read all
#' @param trim_sensor remove excess sensor/summary data
#' 
#' @export
raw_dat_import <- function(proj_path, cast_names = NULL,
                           trim_sensor = T) {
  
  # check for null cast names
  if(is.null(cast_names)) {
    cast_names <- dir(paste0(proj_path,'/work/'))
  }
  
  if(!all(cast_names %in% dir(paste0(proj_path, '/work/')))) {
    stop('NOT ALL CASTS ARE IN THE WORK FOLDER -- CHECK YOUR FILES')
  }
  
  if(length(cast_names) == 1 ) {
    ret_obj <- read_raw_datfile(proj_path,
                                cast_name = cast_names,
                                trim_sensor = trim_sensor)
  } else {
    ret_obj <- cast_names |> 
      lapply(read_raw_datfile, proj_path = proj_path, trim_sensor = trim_sensor)
    names(ret_obj) <- cast_names
  }
  return(ret_obj)
}

#' Read raw dat
#' 
#' Interior for single raw dat file
#' 
#' @param proj_path the path to the project root folder
#' @param cast_name the cast to read
#' @param trim_sensor trim all sensor data
read_raw_datfile <- function(cast_name, proj_path, trim_sensor = TRUE) {
  dat_filename <- paste0(proj_path, '/work/', cast_name) |> 
    dir() |> 
    grep(pattern = '\\.dat',value = TRUE,)
  
  dat_file <- read.table(paste0(proj_path, '/work/',
                                cast_name, '/', dat_filename), 
                         sep = ";")
  
  names(dat_file)[c(1,2,3)] <- c("img_index",'img','pressure')
  
  #format depth
  dat_file$pressure <- dat_file$pressure / 10
  
  # Clean up date
  dat_file$img <- gsub("\t","", dat_file$img) |> 
    gsub(pattern = '_',replacement = ".",)
  
  dat_file$timestamp <- as.POSIXct(dat_file$img, 
                                   format = '%Y%m%d%H%M%OS',
                                   tz = 'UTC')
  if(trim_sensor) {
    dat_file <- dat_file[,c(1:3,ncol(dat_file))]
  }
  return(dat_file)
}

#' Read in raw bru files
#' 
#' @param proj_path the path to the project root folder
#' @param cast_names names of casts (in work folder). If null will read all
#' 
#' @export
raw_bru_import <- function(proj_path, cast_names = NULL) {
  # check for null cast names
  if(is.null(cast_names)) {
    cast_names <- dir(paste0(proj_path,'/work/'))
  }
  
  if(!all(cast_names %in% dir(paste0(proj_path, '/work/')))) {
    stop('NOT ALL CASTS ARE IN THE WORK FOLDER -- CHECK YOUR FILES')
  }
  
  if(length(cast_names) == 1 ) {
    ret_obj <- read_raw_bru(proj_path,
                            cast_name = cast_names)
  } else {
    ret_obj <- cast_names |> 
      lapply(read_raw_bru, proj_path = proj_path)
    names(ret_obj) <- cast_names
  }
  return(ret_obj)
}

#' Interior bru reader
#' 
#' @param proj_path the path to the project root folder
#' @param cast_name the cast to read
read_raw_bru <- function(cast_name, proj_path) {
  bru_filename <- paste0(proj_path, '/work/', cast_name,'/',cast_name,'.bru')
  
  bru_file <- read.table(bru_filename, sep = ';')
  
  names(bru_file) <- c('img_index','blob','area_px','meangrey','xcen','ycen')
  return(bru_file)
}


#' Plot Descent
#' 
#' @import ggplot2
#' 
#' @param datdf a dat raw dataframe
#' 
#' @export
descent_plot <- function(datdf) {
  timestamp <- pressure <- NULL
  plot <- ggplot(datdf) +
    geom_line(aes(x = timestamp, y = pressure + 1.2)) +
    scale_y_reverse()+
    labs(x = 'TimeStamp', y = 'Depth w/o offset')
  theme_bw()
  return(plot)
}




#' Merge raw_dat and raw_bru files
#' 
#' @param dat dat files
#' @param bru bru files
#' 
#' @export
raw_merge_brudat <- function(bru, dat) {
  if(is.list(bru) & is.list(dat)) {
    if(all(names(bru) != names(dat))) {
      stop('bru and dat lists do not match')
    } else {
      cast_names <- names(bru)
      ret_obj <- cast_names |> 
        lapply(function(x) bru_dat(bru[[x]], dat[[x]]))
      names(ret_obj) <- cast_names
    }
    
  } else {
    ret_obj <- bru_dat(bru, dat)
  }
  return(ret_obj)
}

#' Single merger function
#' 
#' @importFrom dplyr left_join
#' @param bru raw bru
#' @param dat raw dat
bru_dat <- function(bru, dat) {
  ret <- bru |> 
    left_join(dat)
  return(ret)
}