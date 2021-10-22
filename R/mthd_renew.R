#' Renew method applied to ready4 S3 class for encapsulating the metadata required for package set-up..
#' @description renew.ready4fun_manifest() is a Renew method that updates an instance of a class with new values. This method is implemented for the ready4 S3 class for encapsulating the metadata required for package set-up. The function is called for its side effects and does not return a value.
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
