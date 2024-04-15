#' Exhibit features of model module data by printing them to the R console
#' @description exhibit.ready4fun_abbreviations() is an exhibit method that exhibits features of a class instance by printing to console. This method is implemented for the ready4 submodule class for tibble object lookup table of abbreviations. The function is called for its side effects and does not return a value.
#' @param x An instance of `ready4fun_abbreviations`, a ready4 submodule class for tibble object lookup table of abbreviations.
#' @param caption_1L_chr Caption (a character vector of length one), Default: NULL
#' @param mkdn_tbl_ref_1L_chr Markdown table reference (a character vector of length one), Default: NULL
#' @param output_type_1L_chr Output type (a character vector of length one), Default: 'HTML'
#' @param select_int Select (an integer vector), Default: NA
#' @param use_lbls_as_col_nms_1L_lgl Use labels as column names (a logical vector of length one), Default: T
#' @param ... Additional arguments
#' @return No return value, called for side effects.
#' @rdname exhibit-methods
#' @export 
#' @importFrom dplyr select
#' @importFrom ready4show print_from_chunk
#' @importFrom ready4 exhibit
exhibit.ready4fun_abbreviations <- function (x, caption_1L_chr = NULL, mkdn_tbl_ref_1L_chr = NULL, 
    output_type_1L_chr = "HTML", select_int = NA_integer_, use_lbls_as_col_nms_1L_lgl = T, 
    ...) 
{
    var_desc_chr <- c("Abbreviation", "Description", "Plural")
    if (!is.na(select_int[1])) {
        x <- dplyr::select(x, select_int)
        var_desc_chr <- var_desc_chr[select_int]
    }
    x %>% ready4show::print_from_chunk(caption_1L_chr = caption_1L_chr, 
        mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr, output_type_1L_chr = output_type_1L_chr, 
        use_lbls_as_col_nms_1L_lgl = use_lbls_as_col_nms_1L_lgl, 
        var_desc_chr = var_desc_chr, ...)
}
#' @rdname exhibit-methods
#' @aliases exhibit,ready4fun_abbreviations-method
#' @importFrom ready4 exhibit
methods::setMethod("exhibit", methods::className("ready4fun_abbreviations", package = "ready4fun"), exhibit.ready4fun_abbreviations)
#' Exhibit features of model module data by printing them to the R console
#' @description exhibit.ready4fun_functions() is an exhibit method that exhibits features of a class instance by printing to console. This method is implemented for the Function types lookup table. The function is called for its side effects and does not return a value.
#' @param x An instance of `ready4fun_functions`, a Function types lookup table.
#' @param caption_1L_chr Caption (a character vector of length one), Default: NULL
#' @param mkdn_tbl_ref_1L_chr Markdown table reference (a character vector of length one), Default: NULL
#' @param output_type_1L_chr Output type (a character vector of length one), Default: 'HTML'
#' @param select_int Select (an integer vector), Default: NA
#' @param use_lbls_as_col_nms_1L_lgl Use labels as column names (a logical vector of length one), Default: T
#' @param ... Additional arguments
#' @return No return value, called for side effects.
#' @rdname exhibit-methods
#' @export 
#' @importFrom dplyr select
#' @importFrom ready4show print_from_chunk
#' @importFrom ready4 exhibit
exhibit.ready4fun_functions <- function (x, caption_1L_chr = NULL, mkdn_tbl_ref_1L_chr = NULL, 
    output_type_1L_chr = "HTML", select_int = NA_integer_, use_lbls_as_col_nms_1L_lgl = T, 
    ...) 
{
    var_desc_chr <- c("Verb", "Description", "Argument One", 
        "Argument Two", "Generic", "Method")
    if (!is.na(select_int[1])) {
        x <- dplyr::select(x, select_int)
        var_desc_chr <- var_desc_chr[select_int]
    }
    x %>% ready4show::print_from_chunk(caption_1L_chr = caption_1L_chr, 
        mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr, output_type_1L_chr = output_type_1L_chr, 
        use_lbls_as_col_nms_1L_lgl = use_lbls_as_col_nms_1L_lgl, 
        var_desc_chr = var_desc_chr, ...)
}
#' @rdname exhibit-methods
#' @aliases exhibit,ready4fun_functions-method
#' @importFrom ready4 exhibit
methods::setMethod("exhibit", methods::className("ready4fun_functions", package = "ready4fun"), exhibit.ready4fun_functions)
#' Exhibit features of model module data by printing them to the R console
#' @description exhibit.ready4fun_objects() is an exhibit method that exhibits features of a class instance by printing to console. This method is implemented for the Object types lookup table. The function is called for its side effects and does not return a value.
#' @param x An instance of `ready4fun_objects`, a Object types lookup table.
#' @param caption_1L_chr Caption (a character vector of length one), Default: NULL
#' @param mkdn_tbl_ref_1L_chr Markdown table reference (a character vector of length one), Default: NULL
#' @param output_type_1L_chr Output type (a character vector of length one), Default: 'HTML'
#' @param select_int Select (an integer vector), Default: NA
#' @param use_lbls_as_col_nms_1L_lgl Use labels as column names (a logical vector of length one), Default: T
#' @param ... Additional arguments
#' @return No return value, called for side effects.
#' @rdname exhibit-methods
#' @export 
#' @importFrom dplyr select
#' @importFrom ready4show print_from_chunk
#' @importFrom ready4 exhibit
exhibit.ready4fun_objects <- function (x, caption_1L_chr = NULL, mkdn_tbl_ref_1L_chr = NULL, 
    output_type_1L_chr = "HTML", select_int = NA_integer_, use_lbls_as_col_nms_1L_lgl = T, 
    ...) 
{
    var_desc_chr <- c("Suffix", "Description", "Atomic", "Extendable")
    if (!is.na(select_int[1])) {
        x <- dplyr::select(x, select_int)
        var_desc_chr <- var_desc_chr[select_int]
    }
    x %>% ready4show::print_from_chunk(caption_1L_chr = caption_1L_chr, 
        mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr, output_type_1L_chr = output_type_1L_chr, 
        use_lbls_as_col_nms_1L_lgl = use_lbls_as_col_nms_1L_lgl, 
        var_desc_chr = var_desc_chr, ...)
}
#' @rdname exhibit-methods
#' @aliases exhibit,ready4fun_objects-method
#' @importFrom ready4 exhibit
methods::setMethod("exhibit", methods::className("ready4fun_objects", package = "ready4fun"), exhibit.ready4fun_objects)
