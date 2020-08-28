#' Reset package files
#' @description reset_pkg_files_R() is a Reset function that edits an object, overwriting the current version with a default version. Specifically, this function implements an algorithm to reset a package files R.NA
#' @param package_chr Package (a character vector of length 1)
#' @param package_dir PARAM_DESCRIPTION, Default: getwd()
#' @param description_ls Description (a list), Default: NULL
#' @param keep_version_lgl Keep version (a logical vector of length 1), Default: T
#' @return NULL
#' @rdname reset_pkg_files_R
#' @export 
#' @importFrom devtools load_all document
#' @importFrom usethis use_description
#' @keywords internal
reset_pkg_files_R <- function (package_chr, package_dir = getwd(), description_ls = NULL, 
    keep_version_lgl = T) 
{
    devtools::load_all()
    if (keep_version_lgl) {
        desc_ls <- packageDescription(package_chr)
        description_ls$Version <- desc_ls$Version
    }
    usethis::use_description(fields = description_ls)
    file.remove(paste0(package_dir, "/NAMESPACE"))
    if (file.exists(paste0(package_dir, "/R/db_pt_lup.R"))) 
        file.remove(paste0(package_dir, "/R/db_pt_lup.R"))
    devtools::document()
    devtools::load_all()
}
