#' Ratify - a method that ratifies that an instance of a class conforms to specified criteria
#' @description ratify.ready4fun_manifest() is a ratify method that ratifies that an instance of a class conforms to specified criteria. This method is implemented for the ready4 S3 class for encapsulating the metadata required for package set-up. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class for encapsulating the metadata required for package set-up.
#' @return X (ready4 S3 class for encapsulating the metadata required for package set-up.)
#' @rdname ratify-methods
#' @export 
#' @importFrom ready4 ratify
ratify.ready4fun_manifest <- function (x) 
{
    x_ready4fun_manifest <- validate_pkg_setup(x, is_method_1L_lgl = T)
    return(x_ready4fun_manifest)
}
#' @rdname ratify-methods
#' @aliases ratify,ready4fun_manifest-method
#' @importFrom ready4 ratify
methods::setMethod("ratify", methods::className("ready4fun_manifest", package = "ready4fun"), ratify.ready4fun_manifest)
