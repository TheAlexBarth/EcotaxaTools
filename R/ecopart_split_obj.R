#' Split an ecopart object based on some meta_data
#' 
#' @param ecopart_obj ecopart object to split
#' @param split_by character corresponding to a metadata column
#' 
#' @example man/examples/split_ecopart_obj.R
#' 
#' @export
split_ecopart_obj <- function(ecopart_obj, split_by) {
  ret_list <- vector('list', length(unique(ecopart_obj$meta[[split_by]])))
  names(ret_list) <- unique(ecopart_obj$meta[[split_by]])
  meta_list <- split(ecopart_obj$meta, f= ecopart_obj$meta[[split_by]])
  for(i in 1:length(meta_list)) {
    temp_profiles <- meta_list[[i]]$profileid
    ret_list[[i]] <- structure(
      list(
        par = structure(
          ecopart_obj$par_files[which(names(ecopart_obj$par_files) %in% temp_profiles)],
          class = c('list','par_list')
          ),
        zoo = structure(
          ecopart_obj$zoo_files[which(names(ecopart_obj$zoo_files) %in% temp_profiles)],
          class = c('list','zoo_list')
          ),
        meta = meta_list[[i]]
      ),
      class = c('list', 'ecopart_obj')
    )
  }
  return(ret_list)
}
