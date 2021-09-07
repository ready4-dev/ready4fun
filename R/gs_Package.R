#' Package
#' @name Package-ready4fun_pkg_desc
#' @description Get the value of the slot Package for S4 objects of class ready4fun_pkg_desc
#' @param x An object of class ready4fun_pkg_desc
#' @rdname Package-methods
#' @aliases Package,ready4fun_pkg_desc-method
methods::setMethod("Package", methods::className("ready4fun_pkg_desc"), function (x) 
{
    x@Package
})
#' Package<-
#' @name Package<--ready4fun_pkg_desc
#' @description Set the value of the slot Package for S4 objects of class ready4fun_pkg_desc
#' @param x An object of class ready4fun_pkg_desc
#' @rdname Package_set-methods
#' @aliases Package<-,ready4fun_pkg_desc-method
methods::setMethod("Package<-", methods::className("ready4fun_pkg_desc"), function (x, value) 
{
    x@Package <- value
    methods::validObject(x)
    x
})
