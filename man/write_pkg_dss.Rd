% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_write.R
\name{write_pkg_dss}
\alias{write_pkg_dss}
\title{Write package datasets}
\usage{
write_pkg_dss(
  pkg_setup_ls,
  args_ls_ls = NULL,
  details_ls = NULL,
  inc_all_mthds_1L_lgl = T,
  paths_ls = make_fn_nms(),
  R_dir_1L_chr = "R",
  undocumented_fns_dir_chr = make_undmtd_fns_dir_chr(drop_empty_1L_lgl = T),
  dv_url_pfx_1L_chr = character(0),
  key_1L_chr = NULL,
  server_1L_chr = Sys.getenv("DATAVERSE_SERVER"),
  dev_pkg_nm_1L_chr = deprecated(),
  dv_ds_nm_1L_chr = deprecated(),
  inc_pkg_meta_data_1L_lgl = deprecated(),
  pkg_ds_ls_ls = deprecated(),
  pkg_url_1L_chr = deprecated()
)
}
\arguments{
\item{pkg_setup_ls}{Package setup (a list)}

\item{args_ls_ls}{Arguments (a list of lists), Default: NULL}

\item{details_ls}{Details (a list), Default: NULL}

\item{inc_all_mthds_1L_lgl}{Include all methods (a logical vector of length one), Default: T}

\item{paths_ls}{Paths (a list), Default: make_fn_nms()}

\item{R_dir_1L_chr}{R directory (a character vector of length one), Default: 'R'}

\item{undocumented_fns_dir_chr}{Undocumented functions directory (a character vector), Default: make_undmtd_fns_dir_chr(drop_empty_1L_lgl = T)}

\item{dv_url_pfx_1L_chr}{Dataverse url prefix (a character vector of length one), Default: character(0)}

\item{key_1L_chr}{Key (a character vector of length one), Default: NULL}

\item{server_1L_chr}{Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")}

\item{dev_pkg_nm_1L_chr}{Development package name (a character vector of length one), Default: deprecated()}

\item{dv_ds_nm_1L_chr}{Dataverse dataset name (a character vector of length one), Default: deprecated()}

\item{inc_pkg_meta_data_1L_lgl}{Include package meta data (a logical vector of length one), Default: deprecated()}

\item{pkg_ds_ls_ls}{Package dataset (a list of lists), Default: deprecated()}

\item{pkg_url_1L_chr}{Package url (a character vector of length one), Default: deprecated()}
}
\value{
Package setup (a list)
}
\description{
write_pkg_dss() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write package datasets. The function returns Package setup (a list).
}
\keyword{internal}
