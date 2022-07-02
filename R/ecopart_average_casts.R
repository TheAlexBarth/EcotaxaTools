
#' Inside averaging
#' 
#' Takes a zoo_conc_list object and averages it
#' @param zoo_conc_list a zoo conc list
#' 
#' @importFrom stats sd
avg_casts <- function(zoo_conc_list) {
  conc_df <- do.call(rbind, zoo_conc_list)
  
  # get the name of the column
  cat_col <- names(conc_df)[which(!(names(conc_df) %in% c('db','conc_m3')))]
  
  if(length(cat_col) > 1) {
    stop("The provided zoo_conc_list has too many columns in the data frames.")
  }
  
  mean_df <- aggregate(list(mean = conc_df$conc_m3), 
                       by = list(db = conc_df$db,
                                 group = conc_df[[cat_col]]),
                       FUN = mean)
  
  sd_df <- aggregate(list(sd = conc_df$conc_m3), 
                     by = list(db = conc_df$db,
                               group = conc_df[[cat_col]]),
                     FUN = sd, na.rm = T)
  
  return(structure(merge(mean_df, sd_df), class = c('data.frame', 'etx_conc_obj')))
}

#' Average uvp concentrations across similar casts
#' 
#' This function will average concentrations of similar uvp casts.
#' It must have an input of zoo_conc_list. This is output from [uvp_zoo_conc()]
#' 
#' @param zoo_conc_list a list of zooplankton concentrations
#' @param name_map a list whose names correspond to cast grouping. If left Null, wil avearge all casts
#' 
#' @export
average_casts <- function(zoo_conc_list, name_map = NULL) {
  
  if(is.null(name_map)) {
    name_map <- list(all = names(zoo_conc_list))
  }
  
  if(length(name_map) == 1) {
    ret_df <- avg_casts(zoo_conc_list)
    return(ret_df)
  } else {
    ret_list <- vector('list',length(name_map))
    names(ret_list) <- names(name_map)
    for(i in 1:length(name_map)) {
      index <- which(names(zoo_conc_list) %in% name_map[[i]])
      ret_list[[i]] <- zoo_conc_list[index] |> avg_casts()
    }
    return(ret_list)
  }
}