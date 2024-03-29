% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_write.R
\name{write_to_replace_sfx_pair}
\alias{write_to_replace_sfx_pair}
\title{Write to replace suffix pair}
\usage{
write_to_replace_sfx_pair(
  args_nm_chr,
  sfcs_chr,
  replacements_chr,
  file_path_1L_chr = NA_character_,
  dir_path_1L_chr = NA_character_
)
}
\arguments{
\item{args_nm_chr}{Arguments name (a character vector)}

\item{sfcs_chr}{Suffices (a character vector)}

\item{replacements_chr}{Replacements (a character vector)}

\item{file_path_1L_chr}{File path (a character vector of length one), Default: 'NA'}

\item{dir_path_1L_chr}{Directory path (a character vector of length one), Default: 'NA'}
}
\value{
No return value, called for side effects.
}
\description{
write_to_replace_sfx_pair() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write to replace suffix pair. The function is called for its side effects and does not return a value.
}
\keyword{internal}
