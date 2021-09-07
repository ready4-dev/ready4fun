#' addl_pkgs_ls
#' @name addl_pkgs_ls-ready4fun_pkg_setup_two
#' @description Get the value of the slot addl_pkgs_ls for S4 objects of class ready4fun_pkg_setup_two
#' @param x An object of class ready4fun_pkg_setup_two
#' @rdname addl_pkgs_ls-methods
#' @aliases addl_pkgs_ls,ready4fun_pkg_setup_two-method
methods::setMethod("addl_pkgs_ls", methods::className("ready4fun_pkg_setup_two"), function (x) 
{
    x@addl_pkgs_ls
})
#' addl_pkgs_ls<-
#' @name addl_pkgs_ls<--ready4fun_pkg_setup_two
#' @description Set the value of the slot addl_pkgs_ls for S4 objects of class ready4fun_pkg_setup_two
#' @param x An object of class ready4fun_pkg_setup_two
#' @rdname addl_pkgs_ls_set-methods
#' @aliases addl_pkgs_ls<-,ready4fun_pkg_setup_two-method
methods::setMethod("addl_pkgs_ls<-", methods::className("ready4fun_pkg_setup_two"), function (x, value) 
{
    x@addl_pkgs_ls <- value
    methods::validObject(x)
    x
})
