#' initial_ls
#' @description S4 Generic function to get the value of the slot initial_ls
#' @rdname initial_ls-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("initial_ls", function(x) standardGeneric("initial_ls"))
#' initial_ls
#' @name initial_ls-ready4fun_pkg_setup
#' @description Get the value of the slot initial_ls for S4 objects of class ready4fun_pkg_setup
#' @param x An object of class ready4fun_pkg_setup
#' @rdname initial_ls-methods
#' @aliases initial_ls,ready4fun_pkg_setup-method
methods::setMethod("initial_ls", methods::className("ready4fun_pkg_setup"), function (x) 
{
    x@initial_ls
})
#' initial_ls<-
#' @description S4 Generic function to set the value of the slot initial_ls
#' @rdname initial_ls_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("initial_ls<-", function(x, value) standardGeneric("initial_ls<-"))
#' initial_ls<-
#' @name initial_ls<--ready4fun_pkg_setup
#' @description Set the value of the slot initial_ls for S4 objects of class ready4fun_pkg_setup
#' @param x An object of class ready4fun_pkg_setup
#' @rdname initial_ls_set-methods
#' @aliases initial_ls<-,ready4fun_pkg_setup-method
methods::setMethod("initial_ls<-", methods::className("ready4fun_pkg_setup"), function (x, value) 
{
    x@initial_ls <- value
    methods::validObject(x)
    x
})
