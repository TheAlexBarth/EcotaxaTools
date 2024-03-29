% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ecopart_par_spectra.R
\name{calc_par_db_spectra}
\alias{calc_par_db_spectra}
\title{Split par_calc}
\usage{
calc_par_db_spectra(
  single_db,
  pixel_mm,
  img_vol,
  method,
  width = NULL,
  base = NULL,
  min_size = NULL,
  max_size = NULL
)
}
\arguments{
\item{single_db}{par files split by db}

\item{pixel_mm}{the pixel conversion}

\item{img_vol}{the image-to-vol conversion}

\item{method}{log or width}

\item{width}{if width, need to specify the width}

\item{base}{if log method, the base of the log}

\item{min_size}{minimum size}

\item{max_size}{maximum size}
}
\description{
Mainly for lapply to par_by_db
}
