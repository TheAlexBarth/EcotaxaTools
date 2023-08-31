#' Plot a single particle diagram -- total concentration
#' 
#' @import ggplot2
#' 
#' @param par_conc_df a particle conc df
#' @param ... arguments to pass to \code{geom_smooth} can set gam or loess, span, etc
single_par_plot <- function(par_conc_df, ...) {
  depth <- par_conc <- NULL
  outplot <- ggplot(par_conc_df) +
    geom_point(aes(x = depth, y = par_conc), alpha = 0.25)+
    geom_smooth(aes(x = depth, y = par_conc), size = 1,
                color = 'black', ...)+
    scale_x_reverse()+
    coord_flip()+
    labs(x = "Depth [m]", y = 'Particle Concentration [per L]')+
    theme_bw()
  
  return(outplot)
}

#' Plot a size-binned particle plot
#' 
#' @import ggplot2
#' @inheritParams single_par_plot
bin_par_plot <- function(par_conc_df, ...) {
  depth <- par_conc <- esd_bin <-  NULL
  outplot <- ggplot(par_conc_df) +
    geom_point(aes(x = depth, y = par_conc, col = esd_bin), alpha = 0.25)+
    geom_smooth(aes(x = depth, y = par_conc, col = esd_bin),
                size = 1, ...)+
    scale_x_reverse()+
    coord_flip()+
    labs(x = "Depth [m]", y = 'Particle Concentration [per L]',
         col = '')+
    theme_bw()
  
  return(outplot)
}

#' Plot a particle diagram
#' 
#' @import ggplot2
#' @importFrom grDevices dev.new
#' 
#' @inheritParams uvp_par_conc
#' @param open_windows set to true will open plots for each profile
#' @export
particle_plot <- function(par,
                          min_esd = 0,
                          max_esd = 100,
                          open_windows = TRUE,
                          bin_limits = NULL,
                          pixel_mm = NULL,
                          img_vol = NULL) {
  
  # check for type of object
  if(!is.data.frame(par)) {#if looping is needed
    
    #get file types
    if (is.etx_class(par, 'ecopart_obj') | is.etx_class(par, 'par_list')) {
      
      # need to get list of par conc
      par_conc_list <- uvp_par_conc(par,
                                    min_esd = min_esd,
                                    max_esd = max_esd,
                                    bin_limits = bin_limits,
                                    pixel_mm = pixel_mm,
                                    img_vol = img_vol)
      
    } else if(is.etx_class(par, 'par_conc_list')){
      par_conc_list <- par
    } else {
      stop('Check input of par. needs to follow ecopart framework!')
    }
    
    # run plotting loop
    for(i in 1:length(par_conc_list)) {
      
      if(open_windows) {
        dev.new()
      }
      
      if(is.null(par_conc_list[[i]][['esd_bin']])) {
        print(single_par_plot(par_conc_list[[i]]))
      } else {
        print(bin_par_plot(par_conc_list[[i]]))
      }
    }
  
  # if not a list, print a single plot
  } else {
    
    #need to make a plot
    if(is.etx_class(par, 'par_df')) {
      par_df <- uvp_par_conc(par,
                             min_esd = min_esd,
                             max_esd = max_esd,
                             bin_limits = bin_limits,
                             pixel_mm = pixel_mm,
                             img_vol = img_vol)
    } else if(is.etx_class(par, 'par_conc_df')) {
      par_df <- par
    } else {
      stop("Need to use ecotaxatools framework. Provide correct object")
    }
    
    if(is.null(par_df[['esd_bin']])) {
      print(single_par_plot(par_df))
    } else {
      print(bin_par_plot(par_df))
    }
  }
  return("Enjoy the plots!")
}
