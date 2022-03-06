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

#' add_zero_rows
#' for bin_by, if there are no plankton in a bin it leaves it empty,
#' This fills those rows
#'
#' @importFrom tibble is_tibble
#' @param df a bin_by_df product
#' @param ref_range the reference range of data
add_zero_rows <- function(df,ref_range) {

  db <- as.character(df$db)
  if(!is_tibble(df)) { stop('FUCK - add zero rows in bin_formatting folder requires input df to be a tibble')}
  # a supery hacky fix to accomadate vol_df
  # eventually this will all be moved into package
  # and ecopart_vol_bin needs to have db in the last column
  if(identical(df$db, df[,1][[1]])) {
    df <- df[,c(2:ncol(df),1)]
  }
  
  ref_bins <- as.character(cut(ref_range,ref_range))[-1] #get reference bins
  empty_bins <- ref_bins[which(!(ref_bins %in% db))] #which are empty
  
  #safety check
  if(identical(empty_bins, as.character(NULL))) {
    stop('No mis-matched bins but got fed to add_zeros?? Code is fucked')
  }
  
  #establish new df
  empty_rows <- matrix(0,ncol = ncol(df) - 1,
                       nrow = length(empty_bins))
  add_df <- cbind.data.frame(empty_rows,empty_bins)
  names(add_df) <- names(df)
  
  out_df <- rbind.data.frame(df,add_df)
  
  #order out_df by ref_range
  out_df <- out_df[match(out_df$db,ref_bins),]
  return(out_df)
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
