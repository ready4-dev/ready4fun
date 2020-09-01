#' @keywords internal
force_req_pkg_install <- function (package_nm_1L_chr) 
{
    if (!package_nm_1L_chr %in% installed.packages()) {
        install.packages(package_nm_1L_chr, Sys.getenv("R_LIBS_USER"), 
            repos = "https://cran.ms.unimelb.edu.au/")
    }
    library(package_nm_1L_chr, character.only = TRUE)
}
