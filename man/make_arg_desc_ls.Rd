% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_arg_desc_ls}
\alias{make_arg_desc_ls}
\title{Make argument description list}
\usage{
make_arg_desc_ls(
  fn_nms_chr,
  fns_env_ls,
  abbreviations_lup = NULL,
  dv_ds_nm_1L_chr = "ready4-dev/ready4",
  dv_url_pfx_1L_chr = deprecated(),
  key_1L_chr = deprecated(),
  object_type_lup = NULL,
  server_1L_chr = deprecated()
)
}
\arguments{
\item{fn_nms_chr}{Function names (a character vector)}

\item{fns_env_ls}{Functions (a list of environments)}

\item{abbreviations_lup}{Abbreviations (a lookup table), Default: NULL}

\item{dv_ds_nm_1L_chr}{Dataverse dataset name (a character vector of length one), Default: 'ready4-dev/ready4'}

\item{dv_url_pfx_1L_chr}{Dataverse url prefix (a character vector of length one), Default: deprecated()}

\item{key_1L_chr}{Key (a character vector of length one), Default: deprecated()}

\item{object_type_lup}{Object type (a lookup table), Default: NULL}

\item{server_1L_chr}{Server (a character vector of length one), Default: deprecated()}
}
\value{
Argument description (a list)
}
\description{
make_arg_desc_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make argument description list. The function returns Argument description (a list).
}
\keyword{internal}
