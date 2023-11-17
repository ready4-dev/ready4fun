#' Ratify that a dataset meets validity criteria
#' @description ratify.ready4fun_manifest() is a ratify method that ratifies that an instance of a class conforms to specified criteria This method is implemented for the ready4 S3 class for encapsulating the metadata required for package set-up. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class for encapsulating the metadata required for package set-up.
#' @param append_1L_lgl Append (a logical vector of length one), Default: F
#' @return X (ready4 S3 class for encapsulating the metadata required for package set-up.)
#' @rdname ratify-methods
#' @export 
#' @importFrom ready4 ratify
ratify.ready4fun_manifest <- function (x, append_1L_lgl = F) 
{
    x_ready4fun_manifest <- validate_pkg_setup(x, append_1L_lgl = append_1L_lgl, 
        is_method_1L_lgl = T)
    return(x_ready4fun_manifest)
}
#' @rdname ratify-methods
#' @aliases ratify,ready4fun_manifest-method
#' @importFrom ready4 ratify
methods::setMethod("ratify", methods::className("ready4fun_manifest", package = "ready4fun"), ratify.ready4fun_manifest)
