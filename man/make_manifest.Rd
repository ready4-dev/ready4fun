% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_manifest}
\alias{make_manifest}
\title{Make manifest}
\usage{
make_manifest(
  pkg_desc_ls,
  copyright_holders_chr,
  pkg_dmt_dv_dss_chr = deprecated(),
  add_gh_site_1L_lgl = T,
  addl_badges_ls = list(),
  addl_pkgs_ls = make_addl_pkgs_ls(),
  badges_lup = tibble::tibble(),
  build_ignore_ls = make_build_ignore_ls(),
  check_type_1L_chr = "ready4",
  classify_1L_lgl = T,
  cls_fn_ls = list(),
  custom_dmt_ls = make_custom_dmt_ls(),
  delete_r_dir_cnts_1L_lgl = T,
  dev_pkg_nm_1L_chr = get_dev_pkg_nm(getwd()),
  dev_pkgs_chr = NA_character_,
  dss_records_ls = list(),
  dv_url_pfx_1L_chr = character(0),
  gh_repo_1L_chr = NA_character_,
  import_from_chr = character(0),
  lifecycle_stage_1L_chr = "experimental",
  inc_pkg_meta_data_1L_lgl = F,
  incr_ver_1L_lgl = F,
  key_1L_chr = NULL,
  on_cran_1L_lgl = F,
  path_to_dmt_dir_1L_chr = normalizePath("../../../../../Documentation/Code"),
  path_to_pkg_logo_1L_chr = NA_character_,
  path_to_pkg_rt_1L_chr = getwd(),
  piggyback_to_1L_chr = NA_character_,
  pkg_ds_ls_ls = list(),
  ready4_type_1L_chr,
  s4_mthds_fn = NULL,
  s4_mthds_ls = NULL,
  server_1L_chr = Sys.getenv("DATAVERSE_SERVER"),
  user_manual_fns_chr = NA_character_,
  zenodo_badge_1L_chr = character(0)
)
}
\arguments{
\item{pkg_desc_ls}{Package description (a list)}

\item{copyright_holders_chr}{Copyright holders (a character vector)}

\item{pkg_dmt_dv_dss_chr}{Package documentation dataverse datasets (a character vector), Default: deprecated()}

\item{add_gh_site_1L_lgl}{Add github site (a logical vector of length one), Default: T}

\item{addl_badges_ls}{Additional badges (a list), Default: list()}

\item{addl_pkgs_ls}{Additional packages (a list), Default: make_addl_pkgs_ls()}

\item{badges_lup}{Badges (a lookup table), Default: tibble::tibble()}

\item{build_ignore_ls}{Build ignore (a list), Default: make_build_ignore_ls()}

\item{check_type_1L_chr}{Check type (a character vector of length one), Default: 'ready4'}

\item{classify_1L_lgl}{Classify (a logical vector of length one), Default: T}

\item{cls_fn_ls}{Class (a list of functions), Default: list()}

\item{custom_dmt_ls}{Custom documentation (a list), Default: make_custom_dmt_ls()}

\item{delete_r_dir_cnts_1L_lgl}{Delete r directory contents (a logical vector of length one), Default: T}

\item{dev_pkg_nm_1L_chr}{Development package name (a character vector of length one), Default: get_dev_pkg_nm(getwd())}

\item{dev_pkgs_chr}{Development packages (a character vector), Default: 'NA'}

\item{dss_records_ls}{Datasets records (a list), Default: list()}

\item{dv_url_pfx_1L_chr}{Dataverse url prefix (a character vector of length one), Default: character(0)}

\item{gh_repo_1L_chr}{Github repository (a character vector of length one), Default: 'NA'}

\item{import_from_chr}{Import from (a character vector), Default: character(0)}

\item{lifecycle_stage_1L_chr}{Lifecycle stage (a character vector of length one), Default: 'experimental'}

\item{inc_pkg_meta_data_1L_lgl}{Include package meta data (a logical vector of length one), Default: F}

\item{incr_ver_1L_lgl}{Increment version (a logical vector of length one), Default: F}

\item{key_1L_chr}{Key (a character vector of length one), Default: NULL}

\item{on_cran_1L_lgl}{On cran (a logical vector of length one), Default: F}

\item{path_to_dmt_dir_1L_chr}{Path to documentation directory (a character vector of length one), Default: normalizePath("../../../../../Documentation/Code")}

\item{path_to_pkg_logo_1L_chr}{Path to package logo (a character vector of length one), Default: 'NA'}

\item{path_to_pkg_rt_1L_chr}{Path to package root (a character vector of length one), Default: getwd()}

\item{piggyback_to_1L_chr}{Piggyback to (a character vector of length one), Default: 'NA'}

\item{pkg_ds_ls_ls}{Package dataset (a list of lists), Default: list()}

\item{ready4_type_1L_chr}{Ready4 type (a character vector of length one)}

\item{s4_mthds_fn}{S4 methods (a function), Default: NULL}

\item{s4_mthds_ls}{S4 methods (a list), Default: NULL}

\item{server_1L_chr}{Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")}

\item{user_manual_fns_chr}{User manual functions (a character vector), Default: 'NA'}

\item{zenodo_badge_1L_chr}{Zenodo badge (a character vector of length one), Default: character(0)}
}
\value{
Manifest (a list)
}
\description{
make_manifest() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make manifest. The function returns Manifest (a list).
}
\keyword{internal}
