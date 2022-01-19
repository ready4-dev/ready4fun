#' Renew (update) a ready4 framework module (or sub-module)
#' @description renew.ready4fun_abbreviations() is a renew method that renews an instance of a class by updating it with new data. This method is implemented for the ready4 S3 class for tibble object lookup table of abbreviations. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class for tibble object lookup table of abbreviations.
#' @param short_name_chr Short name (a character vector), Default: 'NA'
#' @param long_name_chr Long name (a character vector), Default: 'NA'
#' @param plural_lgl Plural (a logical vector), Default: NA
#' @param filter_cdn_1L_chr Filter condition (a character vector of length one), Default: 'NA'
#' @param new_cases_r3 New cases (a ready4 S3), Default: NULL
#' @param slice_idxs_int Slice indices (an integer vector), Default: NA
#' @return x (An object)
#' @rdname renew-methods
#' @export 
#' @importFrom ready4 update_tb_r3 add_lups renew
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
renew.ready4fun_abbreviations <- function (x, short_name_chr = NA_character_, long_name_chr = NA_character_, 
    plural_lgl = NA, filter_cdn_1L_chr = NA_character_, new_cases_r3 = NULL, 
    slice_idxs_int = NA_integer_) 
{
    x <- ready4::update_tb_r3(x, filter_cdn_1L_chr = filter_cdn_1L_chr, 
        slice_idxs_int = slice_idxs_int)
    x <- dplyr::bind_rows(x, tibble::tibble(short_name_chr = short_name_chr, 
        long_name_chr = long_name_chr, plural_lgl = plural_lgl))
    if (!is.null(new_cases_r3)) {
        x <- ready4::add_lups(x, new_lup = new_cases_r3, key_var_nm_1L_chr = "short_name_chr")
    }
    return(x)
}
#' @rdname renew-methods
#' @aliases renew,ready4fun_abbreviations-method
#' @importFrom ready4 renew
methods::setMethod("renew", methods::className("ready4fun_abbreviations", package = "ready4fun"), renew.ready4fun_abbreviations)
#' Renew (update) a ready4 framework module (or sub-module)
#' @description renew.ready4fun_functions() is a renew method that renews an instance of a class by updating it with new data. This method is implemented for the Function types lookup table. The function is called for its side effects and does not return a value.
#' @param x An instance of Function types lookup table.
#' @param fn_type_nm_chr Function type name (a character vector), Default: 'NA'
#' @param fn_type_desc_chr Function type description (a character vector), Default: 'NA'
#' @param first_arg_desc_chr First argument description (a character vector), Default: 'NA'
#' @param second_arg_desc_chr Second argument description (a character vector), Default: 'NA'
#' @param is_generic_lgl Is generic (a logical vector), Default: NA
#' @param is_method_lgl Is method (a logical vector), Default: NA
#' @param filter_cdn_1L_chr Filter condition (a character vector of length one), Default: 'NA'
#' @param new_cases_r3 New cases (a ready4 S3), Default: NULL
#' @param slice_idxs_int Slice indices (an integer vector), Default: NA
#' @return x (An object)
#' @rdname renew-methods
#' @export 
#' @importFrom ready4 update_tb_r3 add_lups renew
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
renew.ready4fun_functions <- function (x, fn_type_nm_chr = NA_character_, fn_type_desc_chr = NA_character_, 
    first_arg_desc_chr = NA_character_, second_arg_desc_chr = NA_character_, 
    is_generic_lgl = NA, is_method_lgl = NA, filter_cdn_1L_chr = NA_character_, 
    new_cases_r3 = NULL, slice_idxs_int = NA_integer_) 
{
    x <- ready4::update_tb_r3(x, filter_cdn_1L_chr = filter_cdn_1L_chr, 
        slice_idxs_int = slice_idxs_int)
    x <- dplyr::bind_rows(x, tibble::tibble(fn_type_nm_chr = fn_type_nm_chr, 
        fn_type_desc_chr = fn_type_desc_chr, first_arg_desc_chr = first_arg_desc_chr, 
        second_arg_desc_chr = second_arg_desc_chr, is_generic_lgl = is_generic_lgl, 
        is_method_lgl = is_method_lgl))
    if (!is.null(new_cases_r3)) {
        x <- ready4::add_lups(x, new_lup = new_cases_r3, key_var_nm_1L_chr = "fn_type_nm_chr")
    }
    return(x)
}
#' @rdname renew-methods
#' @aliases renew,ready4fun_functions-method
#' @importFrom ready4 renew
methods::setMethod("renew", methods::className("ready4fun_functions", package = "ready4fun"), renew.ready4fun_functions)
#' Renew (update) a ready4 framework module (or sub-module)
#' @description renew.ready4fun_manifest() is a renew method that renews an instance of a class by updating it with new data. This method is implemented for the ready4 S3 class for encapsulating the metadata required for package set-up. The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class for encapsulating the metadata required for package set-up.
#' @param type_1L_chr Type (a character vector of length one)
#' @param are_words_chr Are words (a character vector), Default: character(0)
#' @param custom_plural_ls Custom plural (a list), Default: NULL
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param long_name_chr Long name (a character vector), Default: character(0)
#' @param no_plural_chr No plural (a character vector), Default: character(0)
#' @param not_obj_type_chr Not object type (a character vector), Default: character(0)
#' @param pfx_rgx Prefix (a regular expression vector), Default: character(0)
#' @param tf_to_singular_chr Transform to singular (a character vector), Default: character(0)
#' @return x (An object)
#' @rdname renew-methods
#' @export 
#' @importFrom ready4 renew
renew.ready4fun_manifest <- function (x, type_1L_chr, are_words_chr = character(0), custom_plural_ls = NULL, 
    key_1L_chr = Sys.getenv("DATAVERSE_KEY"), long_name_chr = character(0), 
    no_plural_chr = character(0), not_obj_type_chr = character(0), 
    pfx_rgx = character(0), tf_to_singular_chr = character(0)) 
{
    if (type_1L_chr == "fns_dmt") 
        x <- add_fns_dmt_tb(pkg_setup_ls = x, fns_env_ls = NULL, 
            inc_methods_1L_lgl = T)
    if (!identical(are_words_chr, character(0)) | !identical(tf_to_singular_chr, 
        character(0)) | !identical(not_obj_type_chr, character(0))) {
        if (identical(are_words_chr, character(0))) 
            are_words_chr <- NA_character_
        if (identical(tf_to_singular_chr, character(0))) 
            tf_to_singular_chr <- NA_character_
        if (identical(not_obj_type_chr, character(0))) 
            not_obj_type_chr <- NA_character_
        x <- update_msng_abbrs(x, are_words_chr = are_words_chr, 
            not_obj_type_chr = not_obj_type_chr, tf_to_singular_chr = tf_to_singular_chr)
        if (type_1L_chr == "words") 
            x <- write_new_words_vec(x, key_1L_chr = key_1L_chr)
    }
    if (type_1L_chr == "abbreviations") {
        if (identical(long_name_chr, character(0))) 
            long_name_chr <- NA_character_
        if (identical(no_plural_chr, character(0))) 
            no_plural_chr <- NA_character_
        if (identical(pfx_rgx, character(0))) 
            pfx_rgx <- NA_character_
        x <- write_new_abbrs(x, custom_plural_ls = custom_plural_ls, 
            key_1L_chr = key_1L_chr, long_name_chr = long_name_chr, 
            no_plural_chr = no_plural_chr, pfx_rgx = pfx_rgx)
    }
    return(x)
}
#' @rdname renew-methods
#' @aliases renew,ready4fun_manifest-method
#' @importFrom ready4 renew
methods::setMethod("renew", methods::className("ready4fun_manifest", package = "ready4fun"), renew.ready4fun_manifest)
#' Renew (update) a ready4 framework module (or sub-module)
#' @description renew.ready4fun_objects() is a renew method that renews an instance of a class by updating it with new data. This method is implemented for the Object types lookup table. The function is called for its side effects and does not return a value.
#' @param x An instance of Object types lookup table.
#' @param short_name_chr Short name (a character vector), Default: 'NA'
#' @param long_name_chr Long name (a character vector), Default: 'NA'
#' @param atomic_element_lgl Atomic element (a logical vector), Default: NA
#' @param r3_can_extend_lgl Ready4 S3 can extend (a logical vector), Default: NA
#' @param filter_cdn_1L_chr Filter condition (a character vector of length one), Default: 'NA'
#' @param new_cases_r3 New cases (a ready4 S3), Default: NULL
#' @param slice_idxs_int Slice indices (an integer vector), Default: NA
#' @return x (An object)
#' @rdname renew-methods
#' @export 
#' @importFrom ready4 update_tb_r3 add_lups renew
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
renew.ready4fun_objects <- function (x, short_name_chr = NA_character_, long_name_chr = NA_character_, 
    atomic_element_lgl = NA, r3_can_extend_lgl = NA, filter_cdn_1L_chr = NA_character_, 
    new_cases_r3 = NULL, slice_idxs_int = NA_integer_) 
{
    x <- ready4::update_tb_r3(x, filter_cdn_1L_chr = filter_cdn_1L_chr, 
        slice_idxs_int = slice_idxs_int)
    x <- dplyr::bind_rows(x, tibble::tibble(short_name_chr = short_name_chr, 
        long_name_chr = long_name_chr, atomic_element_lgl = atomic_element_lgl, 
        r3_can_extend_lgl = r3_can_extend_lgl))
    if (!is.null(new_cases_r3)) {
        x <- ready4::add_lups(x, new_lup = new_cases_r3, key_var_nm_1L_chr = "short_name_chr")
    }
    return(x)
}
#' @rdname renew-methods
#' @aliases renew,ready4fun_objects-method
#' @importFrom ready4 renew
methods::setMethod("renew", methods::className("ready4fun_objects", package = "ready4fun"), renew.ready4fun_objects)
