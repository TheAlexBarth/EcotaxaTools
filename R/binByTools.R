#' check_custom - a function to check the custom range input
#' 
#' @param custom_range user defined range
#' @param max_d max depth from df
check_custom <- function(custom_range, max_d) {
  if(max(custom_range) < max_d){
    custom_range <- c(custom_range,max_d)
  }
  return(custom_range)
}

#' order_bins - a function to order bins correctly
#' 
#' @param rdf - the rdf right before return
order_bins <- function(rdf) {
  if(is.numeric(rdf$db)) {
    row_order <- order(rdf$db)
  } else {
    num_bins <- as.numeric(gsub('\\((.*)\\,.*','\\1',rdf$db))
    row_order <- order(num_bins)
  }
  return(rdf[row_order,])
}


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

#' ecopart_vol_bin - get binned volumes from ecopart volume output
#' 
#' @param df a data frame to enter
#' @param method "Equal Space", "Custom", "Equal Number"
#' @param custom_range limits for depth bins
#' 
#' @importFrom stats aggregate
#' 
#' @export
ecopart_vol_bin <- function(df,method,custom_range) {
  
  #check that df is correct format
  if(!identical(names(df), c("depth", "vol_sampled"))) {
    stop('Input data frame must be in two-column layout with depth, vol_sampled')
  }
    
    break_vect <- check_custom(custom_range, max(df$depth))
    
    bins <- cut(df$depth, breaks = break_vect)
  
  out_df <- aggregate(df$vol_sampled,by = list(db = bins),FUN = sum)
  names(out_df) <- c('db','vol_sampled')
  return(out_df)
}

#' bin_taxa() - a modern rendition of bin_by_df
#' This version allows for summing over some factor like dry mass or volume
#' 
#' It will sum for each individual in a depth bin by taxa
#' 
#' @importFrom tidyr pivot_wider
#' 
#' @param df a data frame should have a taxo_name, name, or object_annotation_category column
#' @param custom_range the limits of what bins should be made
#' @param zooscan if the df is a zooscan export set to true, default false
#' @param apply_func bin_taxa will apply function to a specified column if set to true
#' @param func_col a single character vector of the value to apply function to; esd, biomass, drymass, etc
#' @param func the function to apply
#' 
#' @export
#' @author Alex Barth
bin_taxa <- function(df,custom_range,zooscan = F,
                     apply_func = F, func_col,func) {
  cat_col <- get_col_name(df,'taxo_name')
  
  #this is old because bin_by requires method
  # Later this could be updated, to only call bin_by for zooscan == F
  if(zooscan == F) {
    method <-  "Custom"
  } else if(zooscan == T) {
    method <- 'Zooscan'
  }
  
  depth_bins <- bin_by(df,method = method, custom_range = custom_range)
  unique_bins <- unique(depth_bins)
  
  #assign function column and function
  if(apply_func == F) {
    func_col <- cat_col
    func <- length
  }
  
  agg_df <- aggregate(df[[func_col]], by = list(db = depth_bins,
                                                taxa = df[[cat_col]]),
                      FUN = func)
  rdf <- pivot_wider(agg_df, names_from = taxa, values_from = x)
  rdf[is.na(rdf)] <- 0
  if(zooscan == F) {
    rdf <- order_bins(rdf)
  }
  return(rdf)
}

