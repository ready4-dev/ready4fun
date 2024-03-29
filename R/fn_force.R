#' Force install of required package
#' @description force_instl_of_reqd_pkg() is a Force function that checks if a specified local or global environmental condition is met and if not, updates the specified environment to comply with the condition. Specifically, this function implements an algorithm to force install of required package. The function is called for its side effects and does not return a value.
#' @param package_nm_1L_chr Package name (a character vector of length one)
#' @return No return value, called for side effects.
#' @rdname force_instl_of_reqd_pkg
#' @export 
#' @importFrom utils installed.packages install.packages
#' @keywords internal
force_instl_of_reqd_pkg <- function (package_nm_1L_chr) 
{
    if (!package_nm_1L_chr %in% utils::installed.packages()) {
        utils::install.packages(package_nm_1L_chr, Sys.getenv("R_LIBS_USER"), 
            repos = "https://cran.ms.unimelb.edu.au/")
    }
    library(package_nm_1L_chr, character.only = TRUE)
}
