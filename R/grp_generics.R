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
#' AuthorClasses
#' @rdname authorClasses-methods
#' @description authorClasses() is an AuthorClasses function that authors and saves files necessary for creating and documenting classes. Specifically, this function implements an algorithm to authorclasses. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export 

authorClasses <- function (x, ...) 
{
    UseMethod("authorClasses", x)
}
methods::setGeneric("authorClasses")
#' AuthorData
#' @rdname authorData-methods
#' @description authorData() is an AuthorData function that authors and saves files necessary for creating and documenting datasets. Specifically, this function implements an algorithm to authordata. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export 

authorData <- function (x, ...) 
{
    UseMethod("authorData", x)
}
methods::setGeneric("authorData")
#' AuthorFunctions
#' @rdname authorFunctions-methods
#' @description authorFunctions() is an AuthorFunctions function that authors and saves files necessary for creating and documenting functions. Specifically, this function implements an algorithm to authorfunctions. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export 

authorFunctions <- function (x, ...) 
{
    UseMethod("authorFunctions", x)
}
methods::setGeneric("authorFunctions")
#' Make
#' @rdname make-methods
#' @description manufacture() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export 

make <- function (x, ...) 
{
    UseMethod("make", x)
}
methods::setGeneric("make")
#' Metamorphose
#' @rdname metamorphose-methods
#' @description metamorphose() is a Metamorphose function that transforms an instance of a class into an object with different structural properties. Specifically, this function implements an algorithm to metamorphose. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export 

metamorphose <- function (x, ...) 
{
    UseMethod("metamorphose", x)
}
methods::setGeneric("metamorphose")
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
#' Renew
#' @rdname renew-methods
#' @description renew() is a Renew function that updates an instance of a class with new values. Specifically, this function implements an algorithm to renew. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export 

renew <- function (x, ...) 
{
    UseMethod("renew", x)
}
methods::setGeneric("renew")
#' Report
#' @rdname report-methods
#' @description report() is a Report function that authors a report. Specifically, this function implements an algorithm to report. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export 

report <- function (x, ...) 
{
    UseMethod("report", x)
}
methods::setGeneric("report")
#' Validate
#' @rdname validate-methods
#' @description validate() is a Validate function that validates that an object conforms to required criteria. Specifically, this function implements an algorithm to ratify. The function is called for its side effects and does not return a value.
#' @param x An object
#' @param ... Additional arguments (an additional arguments)
#' @export 

validate <- function (x, ...) 
{
    UseMethod("validate", x)
}
methods::setGeneric("validate")
