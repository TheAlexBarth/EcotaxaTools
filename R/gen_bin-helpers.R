#' bin_format_check
#' returns true if all character of the vector match the '(#,#]' format
#'
#' @param input input from higher function
#' @author Alex Barth
bin_format_check <- function(input) {
  if(all(grepl('^\\((.*)\\,(.*)\\]',input,perl = T))) {
    return(TRUE)
  } else if (! is.character(input)) {
    stop('input must be a character, check if it was left as a factor')
  } else{
    return(FALSE)
  }
}

#' add_zeros
#' 
#' This function adds 0's for the depth bins which don't have any present taxa
#' Will return a new data frame
#' 
#' @param df a data frame - should be in tidy format from bin_taxa
#' @param col_name a column to add 0s for
#' @param cat_col the category column
#' 
#' @export
#' @author Alex Barth
add_zeros <- function(df, col_name) {
  
  taxa_names <- unique(df$taxa) #save all taxa names
  
  #set up return df
  rdf <- data.frame(db = df$db,
                    taxa = df$taxa,
                    ret_col = df[[col_name]])
  
  for(i in 1:length(unique(df$db))) {
    tdf <- df[df$db == unique(df$db)[i],]
    add_taxa <- taxa_names[which(!(taxa_names %in% tdf$taxa))]
    if(length(add_taxa) == 0) {
      next()
    }
    #make new data frame
    ndf <- data.frame(db = rep(unique(df$db)[i], length(add_taxa)),
                      taxa = add_taxa,
                      ret_col = rep(0, length(add_taxa)))
    rdf <- rbind(rdf,ndf) #add to rdf
  }
  
  rdf <- rdf[order(rdf$db),]
  names(rdf)[3] <- col_name
  return(rdf)
}

#' get_bin_limits
#' This function pulls bin limits from the categorys made by the cut() function
#'
#' @param input the vector of characters - should be a db from bin_taxa typically
#' 
#' @export
#' @author Alex Barth
get_bin_limtis <- function(input) {
  ###
  # Initial conditions check
  ###
  input <- as.character(input)
  if(!bin_format_check(input)) {
    stop('input vector is not the correct format -- requires (###,###] format')
  }
  
  min_depth <- as.numeric(gsub('\\((.*)\\,.*','\\1',input))
  max_depth <- as.numeric(gsub('\\(.*\\,(.*)\\]','\\1',input))
  
  mp <- (min_depth + max_depth) / 2 #calc mid_point
  
  ret_list <- list(min_d = min_depth,max_d = max_depth,
                   mp = mp)
  return(ret_list)
}

#' Format Bins
#' 
#' Takes a concentration object and will format the bins
#' to have min_d, max_d, and mp (mid_point)
#' 
#' @param df a dataframe which is an etx_conc_obj instance
#' 
#' @export
#' @author Alex Barth
bin_format <- function(df) {
  info_cols <- get_bin_limtis(df$db)
  rdf <- df
  rdf$min_d <- info_cols$min_d
  rdf$max_d <- info_cols$max_d
  rdf$mp <- info_cols$mp
  return(rdf)
}

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