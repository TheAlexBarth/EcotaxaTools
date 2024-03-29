% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ecopart_average_casts.R
\name{average_casts}
\alias{average_casts}
\title{Average uvp concentrations across similar casts}
\usage{
average_casts(zoo_conc_list, name_map = NULL)
}
\arguments{
\item{zoo_conc_list}{a list of zooplankton concentrations}

\item{name_map}{a list whose names correspond to cast grouping. If left Null, wil avearge all casts}
}
\description{
This function will average concentrations of similar uvp casts.
It must have an input of zoo_conc_list. This is output from \code{\link[=uvp_zoo_conc]{uvp_zoo_conc()}}
}
