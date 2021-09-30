#' Author method applied to ready4 S3 class for encapsulating the metadata required for package set-up..
#' @description author.ready4fun_manifest() is an Author method that writes files to local or remote locations. This method is implemented for the ready4 S3 class for encapsulating the metadata required for package set-up. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class for encapsulating the metadata required for package set-up.
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: NULL
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param list_generics_1L_lgl List generics (a logical vector of length one), Default: F
#' @param self_serve_1L_lgl Self serve (a logical vector of length one), Default: F
#' @param self_serve_fn_ls Self serve (a list of functions), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return NA ()
#' @rdname author-methods
#' @export 

author.ready4fun_manifest <- function (x, dv_url_pfx_1L_chr = NULL, key_1L_chr = NULL, list_generics_1L_lgl = F, 
    self_serve_1L_lgl = F, self_serve_fn_ls = NULL, server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    x <- validate(x)
    if (!is.null(x$problems_ls)) {
        message("Execution halted - fix issues with manifest before making a new call to author.")
    }
    else {
        message("Manifest has been validated. Proceeding to package set-up.")
        author(x$initial_ls)
        authorData(x)
        authorClasses(x, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, self_serve_1L_lgl = self_serve_1L_lgl, 
            self_serve_fn_ls = self_serve_fn_ls, server_1L_chr = server_1L_ch)
        x <- renew(x, type_1L_chr = "fns_dmt", dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
        authorFunctions(x, list_generics_1L_lgl = list_generics_1L_lgl)
        report(x, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, key_1L_chr = key_1L_chr, 
            server_1L_chr = server_1L_chr)
    }
    return(x)
}
#' @rdname author-methods
#' @aliases author,ready4fun_manifest-method
methods::setMethod("author", methods::className("ready4fun_manifest", package = "ready4fun"), author.ready4fun_manifest)
#' Author method applied to ready4 S3 class for package metadata required for initial package set-up step..
#' @description author.ready4fun_manifest_one() is an Author method that writes files to local or remote locations. This method is implemented for the ready4 S3 class for package metadata required for initial package set-up step. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class for package metadata required for initial package set-up step.
#' @return NULL
#' @rdname author-methods
#' @export 
#' @importFrom rlang exec
author.ready4fun_manifest_one <- function (x) 
{
    rlang::exec(write_pkg_setup_fls, !!!x)
}
#' @rdname author-methods
#' @aliases author,ready4fun_manifest_one-method
methods::setMethod("author", methods::className("ready4fun_manifest_one", package = "ready4fun"), author.ready4fun_manifest_one)
