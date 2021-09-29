#' Analyse
#' @rdname analyse-methods
#' @description analyse() is an Analyse function that runs an analysis or analyses using specified data and parameters. Specifically, this function implements an algorithm to analyse. The function is called for its side effects and does not return a value.
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
#' @description author() is an Author function that writes files to local or remote locations. Specifically, this function implements an algorithm to author. The function is called for its side effects and does not return a value.
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
#' @description make() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make. The function is called for its side effects and does not return a value.
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
#' @description procure() is a Procure function that searches and retrieves requested data from a specified source. Specifically, this function implements an algorithm to procure. The function is called for its side effects and does not return a value.
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
#' @description validate() is a Validate function that validates that an object conforms to required criteria. Specifically, this function implements an algorithm to validate. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export 

validate <- function (x, ...) 
{
    UseMethod("validate", x)
}
methods::setGeneric("validate")
