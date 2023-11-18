#' Author and document functions
#' @description authorFunctions.ready4fun_manifest() is an authorFunctions method that authors and saves files necessary for creating and documenting functions. This method is implemented for the ready4 submodule class for encapsulating the metadata required for package set-up. The function is called for its side effects and does not return a value.
#' @param x An instance of `ready4fun_manifest`, a ready4 submodule class for encapsulating the metadata required for package set-up.
#' @param list_generics_1L_lgl List generics (a logical vector of length one), Default: T
#' @return NULL
#' @rdname authorFunctions-methods
#' @export 
#' @importFrom ready4 authorFunctions
authorFunctions.ready4fun_manifest <- function (x, list_generics_1L_lgl = T) 
{
    write_and_doc_fn_fls(x, update_pkgdown_1L_lgl = T, list_generics_1L_lgl = list_generics_1L_lgl)
}
#' @rdname authorFunctions-methods
#' @aliases authorFunctions,ready4fun_manifest-method
#' @importFrom ready4 authorFunctions
methods::setMethod("authorFunctions", methods::className("ready4fun_manifest", package = "ready4fun"), authorFunctions.ready4fun_manifest)
