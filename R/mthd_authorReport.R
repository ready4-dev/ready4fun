#' Author and save a report
#' @description authorReport.ready4fun_manifest() is an authorReport method that authors and saves a report. This method is implemented for the ready4 submodule class for encapsulating the metadata required for package set-up. The function is called for its side effects and does not return a value.
#' @param x An instance of `ready4fun_manifest`, a ready4 submodule class for encapsulating the metadata required for package set-up.
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @return No return value, called for side effects.
#' @rdname authorReport-methods
#' @export 
#' @importFrom ready4 authorReport
authorReport.ready4fun_manifest <- function (x, key_1L_chr = Sys.getenv("DATAVERSE_KEY")) 
{
    write_manuals(pkg_setup_ls = x, key_1L_chr = key_1L_chr)
}
#' @rdname authorReport-methods
#' @aliases authorReport,ready4fun_manifest-method
#' @importFrom ready4 authorReport
methods::setMethod("authorReport", methods::className("ready4fun_manifest", package = "ready4fun"), authorReport.ready4fun_manifest)
