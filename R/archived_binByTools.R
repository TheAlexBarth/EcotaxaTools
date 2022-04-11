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
    break_vect <- switch(method, "Custom" = check_custom(custom_range,
                                                         max(df[[depthcol]])),
                         "Equal Number" = equal_number)
    return(cut(df[[depthcol]], breaks = break_vect)) #cut into character vector
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
#' 
#' @export
bin_by_df <- function(df, method, custom_range, equal_step = 10,
                      equal_number = 100){
  warning('bin_by_df has been replaced by bin_taxa()')
  df = as.data.frame(df) #why do tibbles exist?!?
  cat_col <- get_col_name(df,"taxo_name")
  df[,cat_col] = as.factor(df[,cat_col]) #set to factor so not lost in split
  
  depth_bins <- bin_by(df, method = method,custom_range, equal_step = equal_step,
                         equal_number = equal_number) #get depth bins
  unique_bins <- unique(depth_bins) #unique entries
    
  calc_list <- vector(mode = "list",length = length(unique_bins)) #list for dfs
  for(i in 1:length(unique_bins)){
     cat_table <- table(df[,cat_col][depth_bins == unique_bins[i]]) #table of counts
     calc_list[[i]] <- as.data.frame(cat_table) #save to list
  }
  #need special approach for Equal Space due to different nature of bin_by
  if(method != "Equal Space"){
    rdf <- as.data.frame(matrix(nrow = length(unique_bins),
                                ncol = length(unique(df[,cat_col])))) #set-data frame
    big_bins <- unique_bins
    names(rdf) <- sort(unique(df[,cat_col])) #columns as unique taxa
    for(r in 1:nrow(rdf)){
       rdf[r,] <- calc_list[[r]][,2] #assign to depthrow (possible to speed up with lapply)
    }
    
  } else if(method == "Equal Space"){
    depthcol <- get_col_name(df,"depth_offset") #get the column for UVP
    bin_length <- seq(0,max(df[,depthcol])+equal_step,equal_step)#set up bin sequence
    
    rdf <- as.data.frame(matrix(nrow = length(bin_length),
                                ncol = length(unique(df[,cat_col])))) #rdf is a lot bigger
    
    big_bins <- bin_length
    names(rdf) <- sort(unique(df[,cat_col]))
    for(i in 1:length(unique_bins)){
      rdex <- which(bin_length == unique_bins[i])
      rdf[rdex,] <- calc_list[[i]][,2]
    }
    rdf[is.na(rdf)] <- 0 #alter all empty rows to 0
  }
  rdf$db <- big_bins
  
  if(method != "Zooscan"){rdf <- order_bins(rdf)}
  
  return(rdf)
}
