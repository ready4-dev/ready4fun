#' AuthorData - a method that authors and saves files necessary for creating and documenting datasets.
#' @description authorData.ready4fun_manifest() is an AuthorData method that authors and saves files necessary for creating and documenting datasets. This method is implemented for the ready4 S3 class for encapsulating the metadata required for package set-up. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class for encapsulating the metadata required for package set-up.
#' @return x (An object)
#' @rdname authorData-methods
#' @export 
#' @importFrom ready4 authorData
authorData.ready4fun_manifest <- function (x) 
{
    x <- write_pkg_dss(x)
    return(x)
}
#' @rdname authorData-methods
#' @aliases authorData,ready4fun_manifest-method
#' @importFrom ready4 authorData
methods::setMethod("authorData", methods::className("ready4fun_manifest", package = "ready4fun"), authorData.ready4fun_manifest)
