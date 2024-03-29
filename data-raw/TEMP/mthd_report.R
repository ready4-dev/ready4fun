#' Report method applied to ready4 S3 class for encapsulating the metadata required for package set-up..
#' @description report.ready4fun_manifest() is a Report method that authors a report. This method is implemented for the ready4 S3 class for encapsulating the metadata required for package set-up. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class for encapsulating the metadata required for package set-up.
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: NULL
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @return NULL
#' @rdname report-methods
#' @export
#' @importFrom ready4 report
report.ready4fun_manifest <- function(x, dv_url_pfx_1L_chr = NULL, key_1L_chr = NULL) {
  write_manuals(
    pkg_setup_ls = x, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
    key_1L_chr = key_1L_chr
  )
}
#' @rdname report-methods
#' @aliases report,ready4fun_manifest-method
#' @importFrom ready4 report
methods::setMethod("report", methods::className("ready4fun_manifest", package = "ready4fun"), report.ready4fun_manifest)
