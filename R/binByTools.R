#' bin_by - a function to make easy cuts by default 
#' 
#' This fuction will return a vector for which depth bins for each observation
#' 
#' @param df the vector to include to be cut. Should be an ecotaxa tsv export
#' @param method "Equal Space", "Custom", "Equal Number" "Zooscan"
#' @param custom_range vector of ranges
#' @param equal_step create equal steps of this size, it will be centered (d = 10 includes 5-15)
#' @param equal_number create this number of ranges
#'
#' @export 
bin_by <- function(df, method, custom_range = NULL, equal_step = 10,
                   equal_number = 100){

  if(method == "Zooscan"){
    return(df$sample_id) #return column name for depth breaks
  } else if (method %in% c("Equal Number","Custom")){
    depthcol <- get_col_name(df,"depth_offset") #get the column for UVP
    break_vect <- switch(method, "Custom" = custom_range, 
                         "Equal Number" = equal_number)
    return(cut(df[,depthcol], breaks = break_vect)) #cut into character vector
  } else if (method == "Equal Space"){
    depthcol <- get_col_name(df,"depth_offset") #get the column for UVP
    bins <- seq(0,max(df[,depthcol])+equal_step,equal_step)#set up bin sequence
    set_bins <- unlist(lapply(df[,depthcol],nearest,bins)) #choose nearest bin
    return(set_bins)
  } else {
    stop("Error: Invalid method arguement. Choose from 'Zooscan','Equal Number',
         'Custom', or 'Equal Space'.")
  }
}

#' bin_by_df - returns a data frame for counts by bin
#' 
#' will count number of observations for each bin then return a count dataframe
#' 
#' @param df the vector to include to be cut. Should be an ecotaxa tsv export
#' @param method "Equal Space", "Custom", "Equal Number" "Zooscan"
#' @param custom_range vector of ranges
#' @param equal_step create equal steps of this size
#' @param equal_number create this number of ranges
#' @param secondary a secondary characteristic (like esd) to split these on
#' @param secondary_breaks the range at which to break the secondary var
#' @param return_list a T/F to get back a list of secondary breaks
#' 
#' @export
bin_by_df <- function(df, method, custom_range, equal_step = 10,
                      equal_number = 100, secondary = NULL, 
                      secondary_breaks = NULL, return_list = F){
  
  df = as.data.frame(df) #why do tibbles exist?!?
  cat_col <- get_col_name(df,"taxo_name")
  df[,cat_col] = as.factor(df[,cat_col]) #set to factor so not lost in split
  
  depth_bins <- bin_by(df, method = method,custom_range, equal_step = equal_step,
                         equal_number = equal_number) #get depth bins
  unique_bins <- unique(depth_bins) #unique entries
    
  if(length(secondary) == 0){
    calc_list <- vector(mode = "list",length = length(unique_bins)) #list for dfs
    for(i in 1:length(unique_bins)){
       cat_table <- table(df[,cat_col][depth_bins == unique_bins[i]]) #table of counts
       calc_list[[i]] <- as.data.frame(cat_table) #save to list
    }
    #need special approach for Equal Step due to different nature of bin_by
    if(method != "Equal Step"){
      rdf <- as.data.frame(matrix(nrow = length(unique_bins),
                                  ncol = length(unique(df[,cat_col])))) #set-data frame
      row.names(rdf) <- unique_bins #row-names as depth bins
      names(rdf) <- sort(unique(df[,cat_col])) #columns as unique taxa
      for(r in 1:nrow(rdf)){
         rdf[r,] <- calc_list[[r]][,2] #assign to depthrow (possible to speed up with lapply)
      }
    } else if(method == "Equal Step"){
      depthcol <- get_col_name(df,"depth_offset") #get the column for UVP
      bin_length <- seq(0,max(df[,depthcol])+equal_step,equal_step)#set up bin sequence
      
      rdf <- as.data.frame(matrix(nrow = length(bin_length),
                                  ncol = length(unique(df[,cat_col])))) #rdf is a lot bigger
      row.names(rdf) <- bin_length
      names(rdf) <- sort(unique(df[,cat_col]))
      for(i in 1:length(unique_bins)){
        rdex <- which(bin_length == unique_bins[i])
        rdf[rdex,] <- calc_list[[i]][,2]
      }
      rdf[is.na(rdf)] <- 0 #alter all empty rows to 0
    }
  } else if(length(secondary) > 0){
    if(class(secondary) == "numeric"){
      sec_levels <- as.character((cut(df[,which(names(df)==secondary)],
                                   secondary_breaks))) #make a secondary cut
    } else if(class(secondary) == "character"){
      sec_levels <- df[,which(names(df)==secondary)] #by categories
    } else{
      stop("Error in secondary class - needs to be numeric or character")
    }
    
    macro_list <- vector(mode = "list", length = length(unique(sec_levels)))
    calc_list <- vector(mode = "list",length = length(unique(sec_levels)))
    rdl <- vector(mode = "list", length = length(unique(sec_levels)))
    
    for(l in 1:length(unique(sec_levels))){
      macro_list[[l]] <- df[which(sec_levels == unique(sec_levels)[l]),]
      calc_list[[l]] <-  vector(mode = "list", length = length(unique_bins))
      
      #get sub-lists of data
      for(i in 1:length(unique_bins)){
        cat_table <- table(macro_list[[l]]$taxo_name[depth_bins 
                                                                      == unique_bins[i]])
        calc_list[[l]][[i]] <- as.data.frame(cat_table)
      }
      #count up sublists
      # needs special condition if using equal step
      if(method != "Equal Step"){
        rdl[[l]] <- as.data.frame(matrix(nrow = length(unique_bins),
                                         ncol = length(unique(df[,cat_col]))))
        row.names(rdl[[l]]) <- unique_bins
        names(rdl[[l]]) <- sort(unique(df[,cat_col]))
        for(r in 1:nrow(rdl[[l]])){
          rdl[[l]][r,] <- calc_list[[l]][[r]][,2] #fill out dataframe
        }
        rdl[[l]]$bin <- unique(sec_levels)[l]
      } else if(method == "Equal Step"){
        depthcol <- get_col_name(df,"depth_offset") #get the column for UVP
        bin_length <- seq(0,max(df[,depthcol])+equal_step,equal_step)#set up bin sequence
        
        rdl[[l]] <- as.data.frame(matrix(nrow = length(bin_length),
                                    ncol = length(unique(df[,cat_col])))) #rdf is a lot bigger
        row.names(rdl[[l]]) <- bin_length
        names(rdl[[l]]) <- sort(unique(df[,cat_col]))
        for(i in 1:length(unique_bins)){
          rdex <- which(bin_length == unique_bins[i])
          rdl[[l]][rdex,] <- calc_list[[i]][,2]
        }
        rdl[[l]][is.na(rdl[[l]])] <- 0 #alter all empty rows to 0
      }
    }
    if(return_list == T){
      return(rdl)
    } else {
      rdf <- do.call("rbind",rdl)
    }
  } else {
    stop("Unidentified error - issue with secondary value")
  }
    return(rdf)
}

#' ecopart_vol_bin - get binned volumes from ecopart volume output
#' 
#' @param df a data frame to enter
#' @param method "Equal Space", "Custom", "Equal Number"
#' 
#' @param custom_range vector of ranges
#' @param equal_step create equal steps of this size
#' @param equal_number create this number of ranges
#' 
#' @importFrom stats aggregate
#' 
#' @export
ecopart_vol_bin <- function(df,method,custom_range = NULL,
                            equal_step = 10, equal_number = 100) {
  
  #check that df is correct format
  if(!identical(names(df), c("depth", "vol_sampled"))) {
    stop('Input data frame must be in two-column layout with depth, vol_sampled')
  }
  
  #check methods arguement
  if(!method %in% c("Equal Number",'Equal Space',"Custom")) {
    stop('Method argument must be either "Equal Number","Equal Space", or "Custom"')
  }
  
  #run for custom method and equal number
  if(method %in% c('Custom','Equal Number')) {
    
    #check custom range looks good
    if(!all(sapply(custom_range, is.numeric))) {
      stop('Custom method requires an input break vector of numeric type')
    }
    
    break_vect <- switch(method,
                         'Custom' = custom_range,
                         'Equal Number' = equal_number)
    bins <- cut(df$depth, breaks = break_vect)
    
  } else if(method == 'Equal Space') {
    bin_range <- seq(0,max(df$depth),equal_step) # make a range 0-max df depth by bin size
    bins <- sapply(df$depth, nearest, bin_range) # make bins
  }
  
  out_df <- aggregate(df$vol_sampled,by = list(depth_bin = bins),FUN = sum)
  names(out_df) <- c('depth_bin','vol_sampled')
  return(out_df)
}
