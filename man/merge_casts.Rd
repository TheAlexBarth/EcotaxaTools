% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ecopart_merge_casts.R
\name{merge_casts}
\alias{merge_casts}
\title{Merge casts into one file}
\usage{
merge_casts(cast_list, name_map)
}
\arguments{
\item{cast_list}{either a list of dataframes or an ecopart_obj}

\item{name_map}{a list with cast names to combine - names of list items will be new names}
}
\description{
This function takes in a list of casts and will combine them based on a list
of possible names to combine
}
\examples{
#merging casts based on station:
name_map = list(
  'gf' = ecopart_example$meta$profileid[which(ecopart_example$meta$stationid == 'gf')],
  'other' = ecopart_example$meta$profileid[which(ecopart_example$meta$stationid != 'gf')]
)

merged_obj <- ecopart_example |> merge_casts(name_map = name_map)
# now there's only two aggregate casts:
names(merged_obj$par_files)
names(merged_obj$zoo_files)
}
\author{
Alex Barth
}
