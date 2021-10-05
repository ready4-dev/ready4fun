#' Renew method applied to ready4 S3 class for encapsulating the metadata required for package set-up..
#' @description renew.ready4fun_manifest() is a Renew method that updates an instance of a class with new values. This method is implemented for the ready4 S3 class for encapsulating the metadata required for package set-up. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class for encapsulating the metadata required for package set-up.
#' @param type_1L_chr Type (a character vector of length one)
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @return X (ready4 S3 class for encapsulating the metadata required for package set-up.)
#' @rdname renew-methods
#' @export 
renew.ready4fun_manifest <- function (x, type_1L_chr, key_1L_chr = NULL) 
{
    if (type_1L_chr == "fns_dmt") 
        x_ready4fun_manifest <- add_fns_dmt_tb(pkg_setup_ls = x, 
            fns_env_ls = NULL, inc_methods_1L_lgl = T)
    return(x_ready4fun_manifest)
}
#' @rdname renew-methods
#' @aliases renew,ready4fun_manifest-method
methods::setMethod("renew", methods::className("ready4fun_manifest", package = "ready4fun"), renew.ready4fun_manifest)
