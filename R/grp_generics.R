#' Analyse
#' @rdname analyse-methods
#' @description analyse() is an Analyse function that runs an analysis or analyses using specified data and parameters. Specifically, this function implements an algorithm to analyse.
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
#' @description author() is an Author function that writes files to local or remote locations. Specifically, this function implements an algorithm to author.
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
#' @description make() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make.
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
#' @description procure() is a Procure function that searches and retrieves requested data from a specified source. Specifically, this function implements an algorithm to procure.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export 

procure <- function (x, ...) 
{
    UseMethod("procure", x)
}
methods::setGeneric("procure")
#' Update
#' @rdname update-methods
#' @description update() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update. Function argument object specifies the object to be updated. Argument ... provides the object to be updated.
#' @param object PARAM_DESCRIPTION
#' @param ... Additional arguments (an additional arguments)
#' @export 

update <- function (object, ...) 
UseMethod("update")
methods::setGeneric("update")
#' Validate
#' @rdname validate-methods
#' @description validate() is a Validate function that validates that an object conforms to required criteria. Specifically, this function implements an algorithm to validate.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export 

validate <- function (x, ...) 
{
    UseMethod("validate", x)
}
methods::setGeneric("validate")
