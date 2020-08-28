#' Get dev package name 1L
#' @description get_dev_pkg_nm_1L_chr() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get dev a package name 1L. Function argument path_to_pkg_rt_chr specifies the where to look for the required object.The function returns dev package name (a character vector of length 1).
#' @param path_to_pkg_rt_chr Path to package rt (a character vector of length 1), Default: '.'
#' @return Dev package name (a character vector of length 1)
#' @rdname get_dev_pkg_nm_1L_chr
#' @export 
#' @importFrom stringr str_sub
#' @keywords internal
get_dev_pkg_nm_1L_chr <- function (path_to_pkg_rt_chr = ".") 
{
    dev_pkg_nm_chr <- readLines(paste0(path_to_pkg_rt_chr, "/DESCRIPTION"))[1] %>% 
        stringr::str_sub(start = 10)
    return(dev_pkg_nm_chr)
}
#' Get function arguments
#' @description get_fn_args_chr_vec() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get a function arguments. Function argument fn specifies the where to look for the required object.The function returns a function arguments (a character vector).
#' @param fn Function (a function)
#' @return Function arguments (a character vector)
#' @rdname get_fn_args_chr_vec
#' @export 
#' @importFrom purrr discard
#' @keywords internal
get_fn_args_chr_vec <- function (fn) 
{
    fn_args_chr_vec <- as.list(args(fn)) %>% names() %>% purrr::discard({
        . == ""
    })
    return(fn_args_chr_vec)
}
#' Get function names in file
#' @description get_fn_nms_in_file_chr() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get a function names in a file. Function argument path_chr specifies the where to look for the required object.The function returns local (a character vector of length 1).
#' @param path_chr Path (a character vector of length 1)
#' @return Local (a character vector of length 1)
#' @rdname get_fn_nms_in_file_chr
#' @export 
#' @importFrom purrr map_lgl
#' @keywords internal
get_fn_nms_in_file_chr <- function (path_chr) 
{
    source(path_chr, local = T)
    local_chr <- ls()
    local_chr <- local_chr[local_chr %>% purrr::map_lgl(~is.function(eval(parse(text = .x))))]
    return(local_chr)
}
#' Get from lookup table object
#' @description get_from_lup_obj() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get from a lookup table object. Function argument data_lookup_tb specifies the where to look for the required object.The function returns return object (an output object of multiple potential types).
#' @param data_lookup_tb Data lookup (a tibble)
#' @param match_value_xx Match value (an output object of multiple potential types)
#' @param match_var_nm_chr Match var name (a character vector of length 1)
#' @param target_var_nm_chr Target var name (a character vector of length 1)
#' @param evaluate_lgl Evaluate (a logical vector of length 1), Default: TRUE
#' @return Return object (an output object of multiple potential types)
#' @rdname get_from_lup_obj
#' @export 
#' @importFrom dplyr filter select pull
#' @importFrom rlang sym
#' @importFrom stringr str_detect str_locate str_sub
get_from_lup_obj <- function (data_lookup_tb, match_value_xx, match_var_nm_chr, target_var_nm_chr, 
    evaluate_lgl = TRUE) 
{
    return_object_ref <- data_lookup_tb %>% dplyr::filter(!!rlang::sym(match_var_nm_chr) == 
        match_value_xx) %>% dplyr::select(!!target_var_nm_chr) %>% 
        dplyr::pull()
    if (evaluate_lgl) {
        if (stringr::str_detect(return_object_ref, "::")) {
            colon_positions <- stringr::str_locate(return_object_ref, 
                "::")
            namespace_ref <- stringr::str_sub(return_object_ref, 
                start = 1, end = colon_positions[1, "start"] - 
                  1)
            object_ref <- stringr::str_sub(return_object_ref, 
                start = colon_positions[1, "end"] + 1)
            if (sum(stringr::str_detect(search(), paste0("package:", 
                namespace_ref))) == 0) {
                namespace_ref_sym <- rlang::sym(namespace_ref)
                attachNamespace(namespace_ref)
                return_object_xx <- get(x = object_ref, envir = as.environment(paste0("package:", 
                  namespace_ref)))
                detach(paste0("package:", namespace_ref), character.only = TRUE)
            }
            else {
                return_object_xx <- get(x = object_ref, envir = as.environment(paste0("package:", 
                  namespace_ref)))
            }
        }
        else {
            return_object_xx <- get(x = return_object_ref)
        }
    }
    else {
        return_object_xx <- return_object_ref
    }
    return(return_object_xx)
}
#' Get output object type
#' @description get_outp_obj_type_chr_vec() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get an output object type. Function argument fns_chr_vec specifies the where to look for the required object.The function returns an output object type (a character vector).
#' @param fns_chr_vec Functions (a character vector)
#' @return Output object type (a character vector)
#' @rdname get_outp_obj_type_chr_vec
#' @export 
#' @importFrom purrr map_chr
#' @keywords internal
get_outp_obj_type_chr_vec <- function (fns_chr_vec) 
{
    outp_obj_type_chr_vec <- purrr::map_chr(fns_chr_vec, ~{
        return_obj_chr <- get_return_obj_nm_chr(eval(parse(text = .x))) %>% 
            make_arg_desc_chr_vec()
        ifelse(return_obj_chr == "NO MATCH", "NULL", return_obj_chr)
    })
    return(outp_obj_type_chr_vec)
}
#' Get readyforwhatsnext S4 object slots
#' @description get_r4_obj_slots_chr_vec() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get a readyforwhatsnext S4 object slots. Function argument fn_name_chr specifies the where to look for the required object.The function returns slots (a character vector).
#' @param fn_name_chr Function name (a character vector of length 1)
#' @param package_chr Package (a character vector of length 1), Default: ''
#' @return Slots (a character vector)
#' @rdname get_r4_obj_slots_chr_vec
#' @export 
#' @importFrom methods getSlots
#' @importFrom purrr map_chr
#' @keywords internal
get_r4_obj_slots_chr_vec <- function (fn_name_chr, package_chr = "") 
{
    slots_ls <- className(fn_name_chr, update_ns_chr(package_chr)) %>% 
        methods::getSlots()
    slots_chr_vec <- purrr::map_chr(slots_ls, ~.x)
    return(slots_chr_vec)
}
#' Get return object name
#' @description get_return_obj_nm_chr() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get return an object name. Function argument fn specifies the where to look for the required object.The function returns return (a character vector of length 1).
#' @param fn Function (a function)
#' @return Return (a character vector of length 1)
#' @rdname get_return_obj_nm_chr
#' @export 
#' @importFrom stringr str_replace str_sub
#' @keywords internal
get_return_obj_nm_chr <- function (fn) 
{
    fn_chr <- deparse(fn)
    last_line_chr <- fn_chr[length(fn_chr) - 1] %>% trimws()
    if (startsWith(last_line_chr, "return(")) {
        return_chr <- stringr::str_replace(last_line_chr, "return", 
            "") %>% stringr::str_sub(start = 2, end = -2)
    }
    else {
        return_chr <- NA_character_
    }
    return(return_chr)
}
