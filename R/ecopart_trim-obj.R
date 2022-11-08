#' Trim an ecopart object by depth
#' 
#' This is a quick function to filter an ecopart object by depth
#' it runs on both trim_zoo and trim_par
#' 
#' @param ecopart_obj an ecopart_obj
#' @param depth the depth limit
#' @param operator 'less','greater','lessequal', or 'greaterequal'
#' 
#' @export
trim_ecopart_depth <- function(ecopart_obj,
                               depth,
                               operator = 'less') {
  
  ecopart_obj$par_files <- trim_par(ecopart_obj$par_files,
                                    trim_cat = 'depth',
                                    trim_amt = depth,
                                    operator = operator)
  ecopart_obj$zoo_files <- trim_zoo(ecopart_obj$zoo_files,
                                    trim_cat = 'depth_including_offset',
                                    trim_amt = depth,
                                    operator = operator)
  return(ecopart_obj)
}

#' Trim zoo_list
#' 
#' A simple function to trim zoo_list. see also trim_par
#' 
#' @param zoo_list a zoo_list
#' @param trim_cat the category to trim by
#' @param trim_amt the amount to trim by 
#' @param operator either 'less','greater','lessequal', or 'greaterequal'
#' 
#' @export
trim_zoo <- function(zoo_list,
                     trim_cat,
                     trim_amt,
                     operator) {
  
  # check operator
  possible <- c('less','greater','lessequal','greaterequal')
  
  if(!(operator %in% possible)) {
    stop(paste0('Invalid operator. Check help ?trim_zoo'))
  }
  
  #check file entry
  if(is.etx_class(zoo_list, 'ecopart_obj')) {
    
    new_zoo_list <- trim_zoo_inner(zoo_list$zoo_files,
                                   trim_cat,
                                   trim_amt,
                                   operator)
    ret_obj <- zoo_list
    ret_obj$zoo_files <- new_zoo_list
    return(ret_obj)
    
  } else if(is.etx_class(zoo_list, 'zoo_list')) {
    
    return(trim_zoo_inner(zoo_list, trim_cat,
                          trim_amt, operator))
  
  } else {
    stop('Must provide a zoo_list or ecopart_obj item')
  }
  
}


#' Trim par_list
#' 
#' A simple function to trim par list based on some criteria
#' 
#' @param zoo_list a zoo_list
#' @param trim_cat the category to trim by
#' @param trim_amt the amount to trim by 
#' @param operator either 'less','greater','lessequal', or 'greaterequal'
#' 
#' @export
trim_par <- function(par_list,
                     trim_cat,
                     trim_amt,
                     operator) {
  
  # check operator
  possible <- c('less','greater','lessequal','greaterequal')
  
  if(!(operator %in% possible)) {
    stop(paste0('Invalid operator. Check help ?trim_par'))
  }
  
  #check file entry
  if(is.etx_class(par_list, 'ecopart_obj')) {
    
    new_par_list <- trim_par_inner(par_list$par_files,
                                   trim_cat,
                                   trim_amt,
                                   operator)
    ret_obj <- par_list
    ret_obj$par_files <- new_par_list
    return(ret_obj)
    
  } else if(is.etx_class(par_list, 'par_list')) {
    
    return(trim_zoo_inner(par_list, trim_cat,
                          trim_amt, operator))
    
  } else {
    stop('Must provide a par_list or ecopart_obj item')
  }
  
}

#' Inside zoo_list trimmer
#' A simple function to trim par list based on some criteria
#' syntax is similar to lapply and users must define a trim function
#' 
#' @param zoo_list a zoo_list
#' @param trim_cat the category to trim by
#' @param trim_amt the amount to trim by 
#' @param operator either 'less','greater','lessequal', or 'greaterequal'
trim_zoo_inner <- function(zoo_list,
                     trim_cat,
                     trim_amt,
                     operator) {
  
  # check par_file name match
  if(!(trim_cat %in% names(zoo_list[[1]]))) {
    stop('Invalid trim_cat. Check names(zoo_list[[1]])')
  }
  
  
  
  #run it
  switch(operator,
         'less' = return(structure(lapply(zoo_list,
                                          function(x) 
                                            x[which(x[[trim_cat]] < trim_amt),]),
                                   class = c('list', 'zoo_list'))),
         'greater' = return(structure(lapply(zoo_list,
                                             function(x) 
                                               x[which(x[[trim_cat]] > trim_amt),]),
                                      class = c('list', 'zoo_list'))),
         'lessequal' =  return(structure(lapply(zoo_list,
                                                function(x) 
                                                  x[which(x[[trim_cat]] <= trim_amt),]),
                                         class = c('list', 'zoo_list'))),
         'greaterequal' =  return(structure(lapply(zoo_list,
                                                   function(x) 
                                                     x[which(x[[trim_cat]] >= trim_amt),]),
                                            class = c('list', 'zoo_list'))),
         stop('Some Error Occurred. This should be impossible :('))
  
}




#' Inside par trimmer
#' @param par_list either a par list
#' @param trim_cat the category to trim by
#' @param trim_amt the amount to trim by 
#' @param operator either 'less','greater','lessequal', or 'greaterequal'
trim_par_inner <- function(par_list,
                     trim_cat,
                     trim_amt,
                     operator) {
  
  
  # check par_file name match
  if(!(trim_cat %in% names(par_list[[1]]))) {
    stop('Invalid trim_cat. Check names(par_list[[1]])')
  }
  
  
  #run it
  switch(operator,
         'less' = return(structure(lapply(par_list,
                                          function(x) 
                                            x[which(x[[trim_cat]] < trim_amt),]),
                                   class = c('list', 'par_list'))),
         'greater' = return(structure(lapply(par_list,
                                             function(x) 
                                               x[which(x[[trim_cat]] > trim_amt),]),
                                      class = c('list', 'par_list'))),
         'lessequal' =  return(structure(lapply(par_list,
                                                function(x) 
                                                  x[which(x[[trim_cat]] <= trim_amt),]),
                                         class = c('list', 'par_list'))),
         'greaterequal' =  return(structure(lapply(par_list,
                                                   function(x) 
                                                     x[which(x[[trim_cat]] >= trim_amt),]),
                                            class = c('list', 'par_list'))),
         stop('Some Error Occurred. This should be impossible :('))
  
}
