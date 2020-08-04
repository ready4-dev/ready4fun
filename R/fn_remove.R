#' Remove collate
#' @description remove_collate_chr_vec() is a Remove function that edits an object, removing a specified element or elements. Specifically, this function implements an algorithm to remove collate. Function argument description_chr specifies the object to be updated. Argument NA provides the object to be updated. The function returns a description (a character vector of length 1).
#' @param description_chr Description (a character vector of length 1)
#' @return Description (a character vector of length 1)
#' @rdname remove_collate_chr_vec
#' @export 

#' @keywords internal
remove_collate_chr_vec <- function (description_chr) 
{
    if (!identical(which(description_chr == "Collate: "), integer(0))) 
        description_chr <- description_chr[1:(which(description_chr == 
            "Collate: ") - 1)]
    return(description_chr)
}
#' Remove object type from name
#' @description remove_obj_type_from_nm_chr_vec() is a Remove function that edits an object, removing a specified element or elements. Specifically, this function implements an algorithm to remove an object type from a name. Function argument nms_chr_vec specifies the object to be updated. Argument object_type_lup provides the object to be updated. The function returns names (a character vector).
#' @param nms_chr_vec Names (a character vector)
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @return Names (a character vector)
#' @rdname remove_obj_type_from_nm_chr_vec
#' @export 
#' @importFrom purrr map_chr map2_chr
#' @importFrom stringi stri_replace_last_fixed
#' @keywords internal
remove_obj_type_from_nm_chr_vec <- function (nms_chr_vec, object_type_lup = NULL, abbreviations_lup = NULL) 
{
    if (is.null(object_type_lup)) 
        data("object_type_lup", package = "ready4fun", envir = environment())
    if (is.null(abbreviations_lup)) 
        data("abbreviations_lup", package = "ready4fun", envir = environment())
    output_chr_vec <- make_arg_type_abbr_chr_vec(nms_chr_vec, 
        object_type_lup = object_type_lup)
    suffices_chr_vec <- output_chr_vec %>% purrr::map_chr(~{
        ifelse(.x == "NO MATCH", "", .x)
    })
    names_chr_vec <- purrr::map2_chr(nms_chr_vec, suffices_chr_vec, 
        ~ifelse(.y == "", .x, stringi::stri_replace_last_fixed(.x, 
            paste0("_", .y), "")))
    return(names_chr_vec)
}
