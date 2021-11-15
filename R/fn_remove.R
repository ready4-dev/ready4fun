#' remove labels from dataframe
#' @description remove_lbls_from_df() is a Remove function that edits an object, removing a specified element or elements. Specifically, this function implements an algorithm to remove labels from dataframe. Function argument data_df specifies the object to be updated. The function returns Unlabelled data (a data.frame).
#' @param data_df Data (a data.frame)
#' @return Unlabelled data (a data.frame)
#' @rdname remove_lbls_from_df
#' @export 
#' @importFrom lifecycle deprecate_soft
#' @importFrom purrr reduce
#' @keywords internal
remove_lbls_from_df <- function (data_df) 
{
    lifecycle::deprecate_soft("0.0.0.9446", "ready4fun::remove_lbls_from_df()", 
        "ready4::remove_lbls_from_df()")
    unlabelled_data_df <- purrr::reduce(1:ncol(data_df), .init = data_df, 
        ~{
            class(.x[[.y]]) <- setdiff(class(.x[[.y]]), "labelled")
            attr(.x[[.y]], "label") <- NULL
            .x
        })
    return(unlabelled_data_df)
}
#' remove object type from name
#' @description remove_obj_type_from_nm() is a Remove function that edits an object, removing a specified element or elements. Specifically, this function implements an algorithm to remove object type from name. Function argument nms_chr specifies the object to be updated. Argument object_type_lup provides the object to be updated. The function returns Names (a character vector).
#' @param nms_chr Names (a character vector)
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: deprecated()
#' @param fn_types_lup Function types (a lookup table), Default: NULL
#' @param is_generic_lgl Is generic (a logical vector), Default: F
#' @param key_1L_chr Key (a character vector of length one), Default: deprecated()
#' @param server_1L_chr Server (a character vector of length one), Default: deprecated()
#' @return Names (a character vector)
#' @rdname remove_obj_type_from_nm
#' @export 
#' @importFrom purrr map2_chr map_lgl
#' @importFrom stringr str_remove
#' @importFrom ready4 get_from_lup_obj
#' @importFrom stringi stri_replace_last_fixed
#' @keywords internal
remove_obj_type_from_nm <- function (nms_chr, object_type_lup = NULL, abbreviations_lup = NULL, 
    dv_ds_nm_1L_chr = "ready4-dev/ready4", dv_url_pfx_1L_chr = deprecated(), 
    fn_types_lup = NULL, is_generic_lgl = F, key_1L_chr = deprecated(), 
    server_1L_chr = deprecated()) 
{
    if (is.null(abbreviations_lup)) 
        abbreviations_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "abbreviations_lup", 
            piggyback_to_1L_chr = dv_ds_nm_1L_chr)
    if (is.null(fn_types_lup)) 
        fn_types_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "fn_types_lup", 
            piggyback_to_1L_chr = dv_ds_nm_1L_chr)
    if (is.null(object_type_lup)) 
        object_type_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "object_type_lup", 
            piggyback_to_1L_chr = dv_ds_nm_1L_chr)
    output_chr <- make_arg_type_abbr(nms_chr, abbreviations_lup = abbreviations_lup, 
        object_type_lup = object_type_lup, dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, 
        dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, key_1L_chr = key_1L_chr, 
        server_1L_chr = server_1L_chr)
    suffices_chr <- output_chr %>% purrr::map2_chr(is_generic_lgl, 
        ~{
            ifelse(.x == "NO MATCH" | .y, "", .x)
        })
    names_chr <- purrr::map2_chr(nms_chr, suffices_chr, ~{
        name_1L_chr <- .x
        is_s3_mthd_1L_lgl <- purrr::map_lgl(abbreviations_lup$short_name_chr, 
            ~endsWith(name_1L_chr, paste0(".", .x))) %>% any()
        gnrc_part_1L_chr <- ifelse(is_s3_mthd_1L_lgl, name_1L_chr %>% 
            stringr::str_remove(paste0(".", abbreviations_lup$short_name_chr[purrr::map_lgl(abbreviations_lup$short_name_chr, 
                ~endsWith(name_1L_chr, paste0(".", .x)))])), 
            "")
        ifelse(is_s3_mthd_1L_lgl, paste0(gnrc_part_1L_chr, " - a method that ", 
            gnrc_part_1L_chr %>% ready4::get_from_lup_obj(fn_types_lup, 
                match_var_nm_1L_chr = "fn_type_nm_chr", match_value_xx = ., 
                target_var_nm_1L_chr = "fn_type_desc_chr", evaluate_1L_lgl = F) %>% 
                tolower()), ifelse(.y == "", .x, stringi::stri_replace_last_fixed(.x, 
            paste0("_", .y), "")))
    })
    return(names_chr)
}
