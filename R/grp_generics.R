#' Analyse
#' @rdname analyse-methods
#' @description analyse() is an Analyse generic that runs an analysis or analyses using specified data and parameters.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export 

analyse <- function (x, ...) 
{
    UseMethod("analyse", x)
}
methods::setGeneric("analyse")
#' Author
#' @rdname author-methods
#' @description author() is an Author generic that writes files to local or remote locations.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export 

author <- function (x, ...) 
{
    UseMethod("author", x)
}
methods::setGeneric("author")
#' Make
#' @rdname make-methods
#' @description make() is a Make generic that creates a new R object.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export 

make <- function (x, ...) 
{
    UseMethod("make", x)
}
methods::setGeneric("make")
#' Procure
#' @rdname procure-methods
#' @description procure() is a Procure generic that searches and retrieves requested data from a specified source.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export 

procure <- function (x, ...) 
{
    UseMethod("procure", x)
}
methods::setGeneric("procure")
#' Validate
#' @rdname validate-methods
#' @description validate() is a Validate generic that validates that an object conforms to required criteria.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export 

validate <- function (x, ...) 
{
    UseMethod("validate", x)
}
methods::setGeneric("validate")
