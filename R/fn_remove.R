#' Remove labels from
#' @description remove_lbls_from_df() is a Remove function that edits an object, removing a specified element or elements. Specifically, this function implements an algorithm to remove labels from data.frame. Function argument data_df specifies the object to be updated. The function returns Unlabelled data (a data.frame).
#' @param data_df Data (a data.frame)
#' @return Unlabelled data (a data.frame)
#' @rdname remove_lbls_from_df
#' @export 
#' @importFrom purrr reduce
#' @keywords internal
remove_lbls_from_df <- function (data_df) 
{
    unlabelled_data_df <- purrr::reduce(1:ncol(data_df), .init = data_df, 
        ~{
            class(.x[[.y]]) <- setdiff(class(.x[[.y]]), "labelled")
            attr(.x[[.y]], "label") <- NULL
            .x
        })
    return(unlabelled_data_df)
}
#' Remove object type from name
#' @description remove_obj_type_from_nm() is a Remove function that edits an object, removing a specified element or elements. Specifically, this function implements an algorithm to remove object type from name. Function argument nms_chr specifies the object to be updated. Argument object_type_lup provides the object to be updated. The function returns Names (a character vector).
#' @param nms_chr Names (a character vector)
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param is_generic_lgl Is generic (a logical vector), Default: F
#' @return Names (a character vector)
#' @rdname remove_obj_type_from_nm
#' @export 
#' @importFrom utils data
#' @importFrom purrr map2_chr map_lgl
#' @importFrom stringr str_remove
#' @importFrom stringi stri_replace_last_fixed
#' @keywords internal
remove_obj_type_from_nm <- function (nms_chr, object_type_lup = NULL, abbreviations_lup = NULL, 
    is_generic_lgl = F) 
{
    if (is.null(object_type_lup)) 
        object_type_lup <- get_rds_from_dv("object_type_lup")
    if (is.null(abbreviations_lup)) 
        utils::data("abbreviations_lup", package = "ready4fun", 
            envir = environment())
    output_chr <- make_arg_type_abbr(nms_chr, abbreviations_lup = abbreviations_lup, 
        object_type_lup = object_type_lup)
    suffices_chr <- output_chr %>% purrr::map2_chr(is_generic_lgl, 
        ~{
            ifelse(.x == "NO MATCH" | .y, "", .x)
        })
    names_chr <- purrr::map2_chr(nms_chr, suffices_chr, ~{
        name_1L_chr <- .x
        ifelse(purrr::map_lgl(abbreviations_lup$short_name_chr, 
            ~endsWith(name_1L_chr, paste0(".", .x))) %>% any(), 
            paste0(name_1L_chr %>% stringr::str_remove(paste0(".", 
                abbreviations_lup$short_name_chr[purrr::map_lgl(abbreviations_lup$short_name_chr, 
                  ~endsWith(name_1L_chr, paste0(".", .x)))])), 
                " method applied to ", abbreviations_lup$long_name_chr[purrr::map_lgl(abbreviations_lup$short_name_chr, 
                  ~endsWith(name_1L_chr, paste0(".", .x)))], 
                "."), ifelse(.y == "", .x, stringi::stri_replace_last_fixed(.x, 
                paste0("_", .y), "")))
    })
    return(names_chr)
}
