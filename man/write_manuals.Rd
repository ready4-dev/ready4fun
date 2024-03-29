% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_write.R
\name{write_manuals}
\alias{write_manuals}
\title{Write manuals}
\usage{
write_manuals(
  pkg_setup_ls,
  path_to_dmt_dir_1L_chr = deprecated(),
  dv_url_pfx_1L_chr = deprecated(),
  key_1L_chr = NULL,
  publish_dv_1L_lgl = T,
  server_1L_chr = deprecated(),
  pkg_desc_ls = deprecated()
)
}
\arguments{
\item{pkg_setup_ls}{Package setup (a list)}

\item{path_to_dmt_dir_1L_chr}{Path to documentation directory (a character vector of length one), Default: deprecated()}

\item{dv_url_pfx_1L_chr}{Dataverse url prefix (a character vector of length one), Default: deprecated()}

\item{key_1L_chr}{Key (a character vector of length one), Default: NULL}

\item{publish_dv_1L_lgl}{Publish dataverse (a logical vector of length one), Default: T}

\item{server_1L_chr}{Server (a character vector of length one), Default: deprecated()}

\item{pkg_desc_ls}{Package description (a list), Default: deprecated()}
}
\value{
No return value, called for side effects.
}
\description{
write_manuals() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write manuals. The function is called for its side effects and does not return a value.
}
\keyword{internal}
