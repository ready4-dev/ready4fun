#' AuthorFunctions.ready4fun Manifest
#' @description authorFunctions.ready4fun_Manifest() is an AuthorFunctions function that authors and saves files necessary for creating and documenting functions. Specifically, this function implements an algorithm to authorfunctions.ready4fun manifest. The function is called for its side effects and does not return a value.
#' @param x An instance of 
#' @param list_generics_1L_lgl List generics (a logical vector of length one), Default: F
#' @return NULL
#' @rdname authorFunctions-methods
#' @export 

authorFunctions.ready4fun_Manifest <- function (x, list_generics_1L_lgl = F) 
{
    write_and_doc_fn_fls(x, update_pkgdown_1L_lgl = T, list_generics_1L_lgl = list_generics_1L_lgl)
}
#' @rdname authorFunctions-methods
#' @aliases authorFunctions,ready4fun_Manifest-method
methods::setMethod("authorFunctions", methods::className("ready4fun_Manifest", package = "ready4fun"), authorFunctions.ready4fun_Manifest)
