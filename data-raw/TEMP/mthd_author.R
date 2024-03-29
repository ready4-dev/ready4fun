#' Author method applied to ready4 S3 class for encapsulating the metadata required for package set-up..
#' @description author.ready4fun_manifest() is an Author method that writes files to local or remote locations. This method is implemented for the ready4 S3 class for encapsulating the metadata required for package set-up. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class for encapsulating the metadata required for package set-up.
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param list_generics_1L_lgl List generics (a logical vector of length one), Default: F
#' @param self_serve_1L_lgl Self serve (a logical vector of length one), Default: F
#' @param self_serve_fn_ls Self serve (a list of functions), Default: NULL
#' @return NA ()
#' @rdname author-methods
#' @export
#' @importFrom ready4 author
author.ready4fun_manifest <- function(
    x, key_1L_chr = Sys.getenv("DATAVERSE_KEY"), list_generics_1L_lgl = F,
    self_serve_1L_lgl = F, self_serve_fn_ls = NULL) {
  x <- ratify(x)
  if (!is.null(x$problems_ls)) {
    message("Execution halted - fix issues with manifest before making a new call to author.")
  } else {
    message("Manifest has been validated. Proceeding to package set-up.")
    author(x$initial_ls)
    x <- authorData(x)
    authorClasses(x,
      key_1L_chr = key_1L_chr, self_serve_1L_lgl = self_serve_1L_lgl,
      self_serve_fn_ls = self_serve_fn_ls
    )
    x <- renew(x, type_1L_chr = "fns_dmt", key_1L_chr = key_1L_chr)
    authorFunctions(x, list_generics_1L_lgl = list_generics_1L_lgl)
    report(x, key_1L_chr = key_1L_chr)
  }
  return(x)
}
#' @rdname author-methods
#' @aliases author,ready4fun_manifest-method
#' @importFrom ready4 author
methods::setMethod("author", methods::className("ready4fun_manifest", package = "ready4fun"), author.ready4fun_manifest)
#' Author method applied to ready4 S3 class for package metadata required for initial package set-up step..
#' @description author.ready4fun_metadata_a() is an Author method that writes files to local or remote locations. This method is implemented for the ready4 S3 class for package metadata required for initial package set-up step. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class for package metadata required for initial package set-up step.
#' @return NULL
#' @rdname author-methods
#' @export
#' @importFrom rlang exec
#' @importFrom ready4 author
author.ready4fun_metadata_a <- function(x) {
  rlang::exec(write_pkg_setup_fls, !!!x)
}
#' @rdname author-methods
#' @aliases author,ready4fun_metadata_a-method
#' @importFrom ready4 author
methods::setMethod("author", methods::className("ready4fun_metadata_a", package = "ready4fun"), author.ready4fun_metadata_a)
