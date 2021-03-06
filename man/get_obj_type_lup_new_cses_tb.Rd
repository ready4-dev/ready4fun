% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_get.R
\name{get_obj_type_lup_new_cses_tb}
\alias{get_obj_type_lup_new_cses_tb}
\title{Get object type lookup table new cases}
\usage{
get_obj_type_lup_new_cses_tb(
  updated_obj_type_lup_tb,
  old_obj_type_lup_tb = get_rds_from_dv("object_type_lup"),
  excluded_chr = NA_character_
)
}
\arguments{
\item{updated_obj_type_lup_tb}{Updated object type lookup table (a tibble)}

\item{old_obj_type_lup_tb}{Old object type lookup table (a tibble), Default: get_rds_from_dv("object_type_lup")}

\item{excluded_chr}{Excluded (a character vector), Default: 'NA'}
}
\value{
Object type lookup table new cases (a tibble)
}
\description{
get_obj_type_lup_new_cses_tb() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get object type lookup table new cases tibble. Function argument updated_obj_type_lup_tb specifies the where to look for the required object. The function returns Object type lookup table new cases (a tibble).
}
\keyword{internal}
