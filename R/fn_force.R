#' Force req package install
#' @description force_req_pkg_install() is a Force function that checks if a specified local or global environmental condition is met and if not, updates the specified environment to comply with the condition. Specifically, this function implements an algorithm to force req a package install.NA
#' @param package_nm_chr Package name (a character vector of length 1)
#' @return NULL
#' @rdname force_req_pkg_install
#' @export 

force_req_pkg_install <- function (package_nm_chr) 
{
    if (!package_nm_chr %in% installed.packages()) {
        install.packages(package_nm_chr, Sys.getenv("R_LIBS_USER"), 
            repos = "https://cran.ms.unimelb.edu.au/")
    }
    library(package_nm_chr, character.only = TRUE)
}
