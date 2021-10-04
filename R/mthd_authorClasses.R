#' AuthorClasses method applied to ready4 S3 class for encapsulating the metadata required for package set-up..
#' @description authorClasses.ready4fun_manifest() is an AuthorClasses method that authors and saves files necessary for creating and documenting classes. This method is implemented for the ready4 S3 class for encapsulating the metadata required for package set-up. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class for encapsulating the metadata required for package set-up.
<<<<<<< HEAD
=======
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: character(0)
>>>>>>> dev
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param self_serve_1L_lgl Self serve (a logical vector of length one), Default: F
#' @param self_serve_fn_ls Self serve (a list of functions), Default: NULL
#' @return NULL
#' @rdname authorClasses-methods
#' @export 

<<<<<<< HEAD
authorClasses.ready4fun_manifest <- function (x, key_1L_chr = NULL, self_serve_1L_lgl = F, self_serve_fn_ls = NULL) 
=======
authorClasses.ready4fun_manifest <- function (x, dv_url_pfx_1L_chr = character(0), key_1L_chr = NULL, 
    self_serve_1L_lgl = F, self_serve_fn_ls = NULL, server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
>>>>>>> dev
{
    write_clss(x, key_1L_chr = key_1L_chr, self_serve_1L_lgl = self_serve_1L_lgl, 
        self_serve_fn_ls = self_serve_fn_ls)
}
#' @rdname authorClasses-methods
#' @aliases authorClasses,ready4fun_manifest-method
methods::setMethod("authorClasses", methods::className("ready4fun_manifest", package = "ready4fun"), authorClasses.ready4fun_manifest)
