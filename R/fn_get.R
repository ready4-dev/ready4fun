#' Get development package name
#' @description get_dev_pkg_nm() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get a development package name. Function argument path_to_pkg_rt_1L_chr specifies the where to look for the required object.The function returns a development package name (a character vector of length one).
#' @param path_to_pkg_rt_1L_chr Path to package root (a character vector of length one), Default: '.'
#' @return Development package name (a character vector of length one)
#' @rdname get_dev_pkg_nm
#' @export 
#' @importFrom stringr str_sub
#' @keywords internal
get_dev_pkg_nm <- function (path_to_pkg_rt_1L_chr = ".") 
{
    dev_pkg_nm_1L_chr <- readLines(paste0(path_to_pkg_rt_1L_chr, 
        "/DESCRIPTION"))[1] %>% stringr::str_sub(start = 10)
    return(dev_pkg_nm_1L_chr)
}
#' Get function arguments
#' @description get_fn_args() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get a function arguments. Function argument fn specifies the where to look for the required object.The function returns a function arguments (a character vector).
#' @param fn Function (a function)
#' @return Function arguments (a character vector)
#' @rdname get_fn_args
#' @export 
#' @importFrom purrr discard
#' @keywords internal
get_fn_args <- function (fn) 
{
    fn_args_chr <- as.list(args(fn)) %>% names() %>% purrr::discard({
        . == ""
    })
    return(fn_args_chr)
}
#' Get function names in file
#' @description get_fn_nms_in_file() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get a function names in a file. Function argument path_1L_chr specifies the where to look for the required object.The function returns local (a character vector).
#' @param path_1L_chr Path (a character vector of length one)
#' @return Local (a character vector)
#' @rdname get_fn_nms_in_file
#' @export 
#' @importFrom purrr map_lgl
#' @keywords internal
get_fn_nms_in_file <- function (path_1L_chr) 
{
    source(path_1L_chr, local = T)
    local_chr <- ls()
    local_chr <- local_chr[local_chr %>% purrr::map_lgl(~is.function(eval(parse(text = .x))))]
    return(local_chr)
}
#' Get from lookup table object
#' @description get_from_lup_obj() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get from a lookup table object. Function argument data_lookup_tb specifies the where to look for the required object.The function returns return object (an output object of multiple potential types).
#' @param data_lookup_tb Data lookup (a tibble)
#' @param match_value_xx Match value (an output object of multiple potential types)
#' @param match_var_nm_1L_chr Match var name (a character vector of length one)
#' @param target_var_nm_1L_chr Target var name (a character vector of length one)
#' @param evaluate_lgl Evaluate (a logical vector), Default: TRUE
#' @return Return object (an output object of multiple potential types)
#' @rdname get_from_lup_obj
#' @export 
#' @importFrom dplyr filter select pull
#' @importFrom rlang sym
#' @importFrom stringr str_detect str_locate str_sub
#' @keywords internal
get_from_lup_obj <- function (data_lookup_tb, match_value_xx, match_var_nm_1L_chr, 
    target_var_nm_1L_chr, evaluate_lgl = TRUE) 
{
    return_object_ref <- data_lookup_tb %>% dplyr::filter(!!rlang::sym(match_var_nm_1L_chr) == 
        match_value_xx) %>% dplyr::select(!!target_var_nm_1L_chr) %>% 
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
#' @description get_outp_obj_type() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get an output object type. Function argument fns_chr specifies the where to look for the required object.The function returns an output object type (a character vector).
#' @param fns_chr Functions (a character vector)
#' @return Output object type (a character vector)
#' @rdname get_outp_obj_type
#' @export 
#' @importFrom purrr map_chr
#' @keywords internal
get_outp_obj_type <- function (fns_chr) 
{
    outp_obj_type_chr <- purrr::map_chr(fns_chr, ~{
        return_obj_chr <- get_return_obj_nm(eval(parse(text = .x))) %>% 
            make_arg_desc()
        ifelse(return_obj_chr == "NO MATCH", "NULL", return_obj_chr)
    })
    return(outp_obj_type_chr)
}
#' Get readyforwhatsnext S4 object slots
#' @description get_r4_obj_slots() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get a readyforwhatsnext S4 object slots. Function argument fn_name_1L_chr specifies the where to look for the required object.NA
#' @param fn_name_1L_chr Function name (a character vector of length one)
#' @param package_1L_chr Package (a character vector of length one), Default: ''
#' @return NULL
#' @rdname get_r4_obj_slots
#' @export 
#' @importFrom methods getSlots
#' @importFrom purrr map_chr
#' @keywords internal
get_r4_obj_slots <- function (fn_name_1L_chr, package_1L_chr = "") 
{
    slots_ls <- className(fn_name_1L_chr, update_ns(package_1L_chr)) %>% 
        methods::getSlots()
    slots_chr_vec <- purrr::map_chr(slots_ls, ~.x)
    return(slots_chr_vec)
}
#' Get return object name
#' @description get_return_obj_nm() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get return an object name. Function argument fn specifies the where to look for the required object.The function returns return (a character vector of length one).
#' @param fn Function (a function)
#' @return Return (a character vector of length one)
#' @rdname get_return_obj_nm
#' @export 
#' @importFrom stringr str_replace str_sub
#' @keywords internal
get_return_obj_nm <- function (fn) 
{
    fn_chr <- deparse(fn)
    last_line_1L_chr <- fn_chr[length(fn_chr) - 1] %>% trimws()
    if (startsWith(last_line_1L_chr, "return(")) {
        return_1L_chr <- stringr::str_replace(last_line_1L_chr, 
            "return", "") %>% stringr::str_sub(start = 2, end = -2)
    }
    else {
        return_1L_chr <- NA_character_
    }
    return(return_1L_chr)
}
