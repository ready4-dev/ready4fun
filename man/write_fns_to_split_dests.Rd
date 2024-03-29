% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_write.R
\name{write_fns_to_split_dests}
\alias{write_fns_to_split_dests}
\title{Write functions to split destinations}
\usage{
write_fns_to_split_dests(
  pkg_depcy_ls,
  pkg_1_core_fns_chr,
  fns_dmt_tb,
  original_pkg_nm_1L_chr = get_dev_pkg_nm(),
  pkg_1_nm_1L_chr = "package_1",
  pkg_2_nm_1L_chr = "package_2",
  tmp_dir_path_1L_chr = "data-raw/pkg_migration",
  path_to_fns_dir_1L_chr = "data-raw/fns"
)
}
\arguments{
\item{pkg_depcy_ls}{Package dependency (a list)}

\item{pkg_1_core_fns_chr}{Package 1 core functions (a character vector)}

\item{fns_dmt_tb}{Functions documentation (a tibble)}

\item{original_pkg_nm_1L_chr}{Original package name (a character vector of length one), Default: get_dev_pkg_nm()}

\item{pkg_1_nm_1L_chr}{Package 1 name (a character vector of length one), Default: 'package_1'}

\item{pkg_2_nm_1L_chr}{Package 2 name (a character vector of length one), Default: 'package_2'}

\item{tmp_dir_path_1L_chr}{Temporary directory path (a character vector of length one), Default: 'data-raw/pkg_migration'}

\item{path_to_fns_dir_1L_chr}{Path to functions directory (a character vector of length one), Default: 'data-raw/fns'}
}
\value{
No return value, called for side effects.
}
\description{
write_fns_to_split_dests() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write functions to split destinations. The function is called for its side effects and does not return a value.
}
\keyword{internal}
