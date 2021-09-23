#' Author method applied to ready4 S4 class for package metadata required for package set-up..
#' @description author.ready4fun_pkg_setup() is an Author method that writes files to local or remote locations. This method is implemented for the ready4 S4 class for package metadata required for package set-up.. The function returns Ready4fun package package setup (a ready4 S3).
#' @param x An instance of ready4 S4 class for package metadata required for package set-up.
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: NULL
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param list_generics_1L_lgl List generics (a logical vector of length one), Default: F
#' @param self_serve_1L_lgl Self serve (a logical vector of length one), Default: F
#' @param self_serve_fn_ls Self serve (a list of functions), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return Ready4fun package package setup (a ready4 S3)
#' @rdname author-methods
#' @export 

author.ready4fun_pkg_setup <- function (x, dv_url_pfx_1L_chr = NULL, key_1L_chr = NULL, list_generics_1L_lgl = F, 
    self_serve_1L_lgl = F, self_serve_fn_ls = NULL, server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    ready4fun_pkg_setup_r3 <- write_package(x, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
        key_1L_chr = key_1L_chr, list_generics_1L_lgl = list_generics_1L_lgl, 
        publish_dv_1L_lgl = T, self_serve_1L_lgl = self_serve_1L_lgl, 
        self_serve_fn_ls = self_serve_fn_ls, server_1L_chr = server_1L_chr)
    return(ready4fun_pkg_setup_r3)
}
#' @rdname author-methods
#' @aliases author,ready4fun_pkg_setup-method
methods::setMethod("author", "ready4fun_pkg_setup", author.ready4fun_pkg_setup)
