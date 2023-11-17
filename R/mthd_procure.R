#' Procure items from a dataset
#' @description procure.ready4fun_manifest() is a procure method that procures data by executing a search and retrieval algorithm using data contained in an instance of a class This method is implemented for the ready4 S3 class for encapsulating the metadata required for package set-up. The function returns Value (an output object of multiple potential types).
#' @param x An instance of ready4 S3 class for encapsulating the metadata required for package set-up.
#' @param type_1L_chr Type (a character vector of length one)
#' @return Value (an output object of multiple potential types)
#' @rdname procure-methods
#' @export 
#' @importFrom ready4 procure
procure.ready4fun_manifest <- function (x, type_1L_chr) 
{
    if (type_1L_chr == "problems") {
        value_xx <- x$problems_ls
    }
    return(value_xx)
}
#' @rdname procure-methods
#' @aliases procure,ready4fun_manifest-method
#' @importFrom ready4 procure
methods::setMethod("procure", methods::className("ready4fun_manifest", package = "ready4fun"), procure.ready4fun_manifest)
