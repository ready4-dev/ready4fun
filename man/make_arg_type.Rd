% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_arg_type}
\alias{make_arg_type}
\title{Make argument type}
\usage{
make_arg_type(
  fn_args_chr,
  object_type_lup = NULL,
  abbreviations_lup = NULL,
  dv_ds_nm_1L_chr = "ready4-dev/ready4",
  dv_url_pfx_1L_chr = deprecated(),
  fn,
  key_1L_chr = deprecated(),
  server_1L_chr = deprecated()
)
}
\arguments{
\item{fn_args_chr}{Function arguments (a character vector)}

\item{object_type_lup}{Object type (a lookup table), Default: NULL}

\item{abbreviations_lup}{Abbreviations (a lookup table), Default: NULL}

\item{dv_ds_nm_1L_chr}{Dataverse dataset name (a character vector of length one), Default: 'ready4-dev/ready4'}

\item{dv_url_pfx_1L_chr}{Dataverse url prefix (a character vector of length one), Default: deprecated()}

\item{fn}{Function (a function)}

\item{key_1L_chr}{Key (a character vector of length one), Default: deprecated()}

\item{server_1L_chr}{Server (a character vector of length one), Default: deprecated()}
}
\value{
Argument description (a character vector)
}
\description{
make_arg_type() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make argument type. The function returns Argument description (a character vector).
}
\keyword{internal}
