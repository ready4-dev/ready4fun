#' pkg_desc_ls
#' @name pkg_desc_ls-ready4fun_pkg_setup_one
#' @description Get the value of the slot pkg_desc_ls for S4 objects of class ready4fun_pkg_setup_one
#' @param x An object of class ready4fun_pkg_setup_one
#' @rdname pkg_desc_ls-methods
#' @aliases pkg_desc_ls,ready4fun_pkg_setup_one-method
methods::setMethod("pkg_desc_ls", methods::className("ready4fun_pkg_setup_one"), function (x) 
{
    x@pkg_desc_ls
})
#' pkg_desc_ls<-
#' @name pkg_desc_ls<--ready4fun_pkg_setup_one
#' @description Set the value of the slot pkg_desc_ls for S4 objects of class ready4fun_pkg_setup_one
#' @param x An object of class ready4fun_pkg_setup_one
#' @rdname pkg_desc_ls_set-methods
#' @aliases pkg_desc_ls<-,ready4fun_pkg_setup_one-method
methods::setMethod("pkg_desc_ls<-", methods::className("ready4fun_pkg_setup_one"), function (x, value) 
{
    x@pkg_desc_ls <- value
    methods::validObject(x)
    x
})
