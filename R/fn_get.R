#' Get all dependencies of functions
#' @description get_all_depcys_of_fns() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get all dependencies of functions. Function argument pkg_depcy_ls specifies the where to look for the required object. The function returns Functions to keep (a character vector).
#' @param pkg_depcy_ls Package dependency (a list)
#' @param fns_chr Functions (a character vector)
#' @return Functions to keep (a character vector)
#' @rdname get_all_depcys_of_fns
#' @export 
#' @importFrom dplyr filter pull
#' @keywords internal
get_all_depcys_of_fns <- function (pkg_depcy_ls, fns_chr) 
{
    arg_ls <- list(new_dbl = pkg_depcy_ls$Nomfun %>% dplyr::filter(label %in% 
        fns_chr) %>% dplyr::pull(id) %>% as.numeric(), solo_dbl = numeric(0), 
        upper_tb = data.frame(from = numeric(0), to = numeric(0)))
    while (!identical(arg_ls$new_dbl, numeric(0))) {
        arg_ls <- make_depnt_fns_ls(arg_ls, pkg_depcy_ls = pkg_depcy_ls)
    }
    fn_idcs_dbl <- c(arg_ls$upper_tb$to, arg_ls$solo_dbl) %>% 
        unique() %>% sort()
    fns_to_keep_chr <- pkg_depcy_ls$Nomfun %>% dplyr::filter(id %in% 
        fn_idcs_dbl) %>% dplyr::pull(2)
    return(fns_to_keep_chr)
}
#' Get argument object type
#' @description get_arg_obj_type() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get argument object type. Function argument argument_nm_1L_chr specifies the where to look for the required object. The function returns Argument object type (a character vector of length one).
#' @param argument_nm_1L_chr Argument name (a character vector of length one)
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @return Argument object type (a character vector of length one)
#' @rdname get_arg_obj_type
#' @export 
#' @importFrom dplyr filter mutate pull
#' @keywords internal
get_arg_obj_type <- function (argument_nm_1L_chr, object_type_lup = NULL) 
{
    if (is.null(object_type_lup)) 
        object_type_lup <- get_rds_from_dv("object_type_lup")
    nchar_int <- nchar(object_type_lup$short_name_chr)
    match_chr <- object_type_lup$long_name_chr[endsWith(argument_nm_1L_chr, 
        paste0(ifelse(nchar(argument_nm_1L_chr) == nchar_int, 
            "", "_"), object_type_lup$short_name_chr))]
    if (!identical(match_chr, character(0))) {
        arg_obj_type_1L_chr <- dplyr::filter(object_type_lup, 
            long_name_chr %in% match_chr) %>% dplyr::mutate(nchar_int = nchar(short_name_chr)) %>% 
            dplyr::filter(nchar_int == max(nchar_int)) %>% dplyr::pull(long_name_chr)
    }
    else {
        arg_obj_type_1L_chr <- character(0)
    }
    return(arg_obj_type_1L_chr)
}
#' Get development package name
#' @description get_dev_pkg_nm() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get development package name. Function argument path_to_pkg_rt_1L_chr specifies the where to look for the required object. The function returns Development package name (a character vector of length one).
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
#' Get dataverse files urls
#' @description get_dv_fls_urls() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get dataverse files urls. Function argument file_nms_chr specifies the where to look for the required object. The function returns Urls (a character vector).
#' @param file_nms_chr File names (a character vector)
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one)
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: 'https://dataverse.harvard.edu/api/access/datafile/'
#' @return Urls (a character vector)
#' @rdname get_dv_fls_urls
#' @export 
#' @importFrom dataverse dataset_files
#' @importFrom purrr map_chr
#' @keywords internal
get_dv_fls_urls <- function (file_nms_chr, dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = "https://dataverse.harvard.edu/api/access/datafile/") 
{
    ds_ls <- dataverse::dataset_files(pkg_dmt_dv_url_1L_chr)
    all_items_chr <- purrr::map_chr(ds_ls, ~.x$label)
    urls_chr <- file_nms_chr %>% purrr::map_chr(~{
        idx_1L_int <- which(all_items_chr == .x)
        paste0(dv_url_pfx_1L_chr, ds_ls[[idx_1L_int]]$dataFile$id)
    })
    return(urls_chr)
}
#' Get file identity from dataverse
#' @description get_fl_id_from_dv_ls() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get file identity from dataverse list. Function argument ds_ls specifies the where to look for the required object. The function returns Identity (a character vector of length one).
#' @param ds_ls Dataset (a list)
#' @param fl_nm_1L_chr File name (a character vector of length one)
#' @param nms_chr Names (a character vector), Default: 'NA'
#' @return Identity (a character vector of length one)
#' @rdname get_fl_id_from_dv_ls
#' @export 
#' @importFrom purrr map2_chr
#' @importFrom tibble as_tibble
#' @keywords internal
get_fl_id_from_dv_ls <- function (ds_ls, fl_nm_1L_chr, nms_chr = NA_character_) 
{
    if (is.na(nms_chr[1])) {
        nms_chr <- purrr::map2_chr(ds_ls$files$originalFileName, 
            ds_ls$files$filename, ~ifelse(is.na(.x), .y, .x))
    }
    if (fl_nm_1L_chr %in% nms_chr) {
        id_1L_chr <- get_from_lup_obj(ds_ls$files[, names(ds_ls$files) %>% 
            unique()] %>% tibble::as_tibble(), match_var_nm_1L_chr = ifelse(fl_nm_1L_chr %in% 
            ds_ls$files$originalFileName, "originalFileName", 
            "filename"), match_value_xx = fl_nm_1L_chr, target_var_nm_1L_chr = "id", 
            evaluate_lgl = F)
    }
    else {
        id_1L_chr <- NA_character_
    }
    return(id_1L_chr)
}
#' Get function arguments
#' @description get_fn_args() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get function arguments. Function argument fn specifies the where to look for the required object. The function returns Function arguments (a character vector).
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
#' @description get_fn_nms_in_file() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get function names in file. Function argument path_1L_chr specifies the where to look for the required object. The function returns Local (a character vector).
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
#' @description get_from_lup_obj() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get from lookup table object. Function argument data_lookup_tb specifies the where to look for the required object. The function returns Return object (an output object of multiple potential types).
#' @param data_lookup_tb Data lookup (a tibble)
#' @param match_value_xx Match value (an output object of multiple potential types)
#' @param match_var_nm_1L_chr Match variable name (a character vector of length one)
#' @param target_var_nm_1L_chr Target variable name (a character vector of length one)
#' @param evaluate_lgl Evaluate (a logical vector), Default: TRUE
#' @return Return object (an output object of multiple potential types)
#' @rdname get_from_lup_obj
#' @export 
#' @importFrom dplyr filter select pull
#' @importFrom rlang sym
#' @importFrom stringr str_detect str_locate str_sub
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
#' Get new function types
#' @description get_new_fn_types() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get new function types. Function argument abbreviations_lup specifies the where to look for the required object. The function returns New function types (a character vector).
#' @param abbreviations_lup Abbreviations (a lookup table)
#' @param fn_type_lup_tb Function type lookup table (a tibble)
#' @param fn_nms_ls Function names (a list), Default: make_fn_nms()
#' @param undmtd_fns_dir_chr Undocumented functions directory (a character vector), Default: make_undmtd_fns_dir_chr()
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @return New function types (a character vector)
#' @rdname get_new_fn_types
#' @export 
#' @importFrom purrr map2 flatten_chr
#' @importFrom stringr str_remove str_sub
#' @importFrom tools toTitleCase
#' @keywords internal
get_new_fn_types <- function (abbreviations_lup, fn_type_lup_tb, fn_nms_ls = make_fn_nms(), 
    undmtd_fns_dir_chr = make_undmtd_fns_dir_chr(), object_type_lup = NULL) 
{
    if (is.null(object_type_lup)) 
        object_type_lup <- get_rds_from_dv("object_type_lup")
    new_fn_types_chr <- purrr::map2(fn_nms_ls[c(1, 3)], undmtd_fns_dir_chr[c(1, 
        3)], ~stringr::str_remove(.x, paste0(.y, "/")) %>% stringr::str_sub(end = -3)) %>% 
        purrr::flatten_chr() %>% c(get_fn_nms_in_file(paste0(undmtd_fns_dir_chr[2], 
        "/generics.R"))) %>% unique() %>% sort() %>% make_fn_title(abbreviations_lup = abbreviations_lup, 
        object_type_lup = object_type_lup, is_generic_lgl = T) %>% 
        tools::toTitleCase() %>% setdiff(fn_type_lup_tb$fn_type_nm_chr)
    return(new_fn_types_chr)
}
#' Get object type lookup table new cases
#' @description get_obj_type_lup_new_cses_tb() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get object type lookup table new cases tibble. Function argument updated_obj_type_lup_tb specifies the where to look for the required object. The function returns Object type lookup table new cases (a tibble).
#' @param updated_obj_type_lup_tb Updated object type lookup table (a tibble)
#' @param old_obj_type_lup_tb Old object type lookup table (a tibble), Default: get_rds_from_dv("object_type_lup")
#' @param excluded_chr Excluded (a character vector), Default: 'NA'
#' @return Object type lookup table new cases (a tibble)
#' @rdname get_obj_type_lup_new_cses_tb
#' @export 
#' @importFrom dplyr filter
#' @keywords internal
get_obj_type_lup_new_cses_tb <- function (updated_obj_type_lup_tb, old_obj_type_lup_tb = get_rds_from_dv("object_type_lup"), 
    excluded_chr = NA_character_) 
{
    obj_type_lup_new_cses_tb <- updated_obj_type_lup_tb %>% dplyr::filter(!short_name_chr %in% 
        old_obj_type_lup_tb$short_name_chr)
    if (!is.na(excluded_chr[1])) 
        obj_type_lup_new_cses_tb <- obj_type_lup_new_cses_tb %>% 
            dplyr::filter(!short_name_chr %in% excluded_chr)
    return(obj_type_lup_new_cses_tb)
}
#' Get output object type
#' @description get_outp_obj_type() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get output object type. Function argument fns_chr specifies the where to look for the required object. The function returns Output object type (a character vector).
#' @param fns_chr Functions (a character vector)
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @return Output object type (a character vector)
#' @rdname get_outp_obj_type
#' @export 
#' @importFrom purrr map_chr
#' @keywords internal
get_outp_obj_type <- function (fns_chr, object_type_lup = NULL) 
{
    if (is.null(object_type_lup)) 
        object_type_lup <- get_rds_from_dv("object_type_lup")
    outp_obj_type_chr <- purrr::map_chr(fns_chr, ~{
        return_obj_chr <- get_return_obj_nm(eval(parse(text = .x))) %>% 
            make_arg_desc(object_type_lup = object_type_lup)
        ifelse(return_obj_chr == "NO MATCH", "NULL", return_obj_chr)
    })
    return(outp_obj_type_chr)
}
#' Get ready4 S4 object slots
#' @description get_r4_obj_slots() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get ready4 s4 object slots. Function argument fn_name_1L_chr specifies the where to look for the required object. The function returns Slots (a character vector).
#' @param fn_name_1L_chr Function name (a character vector of length one)
#' @param package_1L_chr Package (a character vector of length one), Default: ''
#' @return Slots (a character vector)
#' @rdname get_r4_obj_slots
#' @export 
#' @importFrom methods getSlots
#' @importFrom purrr map_chr
#' @keywords internal
get_r4_obj_slots <- function (fn_name_1L_chr, package_1L_chr = "") 
{
    slots_ls <- className(fn_name_1L_chr, update_ns(package_1L_chr)) %>% 
        methods::getSlots()
    slots_chr <- purrr::map_chr(slots_ls, ~.x)
    return(slots_chr)
}
#' Get rds from dataverse
#' @description get_rds_from_dv() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get rds from dataverse. Function argument file_nm_1L_chr specifies the where to look for the required object. The function returns R object (an output object of multiple potential types).
#' @param file_nm_1L_chr File name (a character vector of length one)
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one), Default: 'https://doi.org/10.7910/DVN/2Y9VF9'
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: 'https://dataverse.harvard.edu/api/access/datafile/'
#' @param server_1L_chr Server (a character vector of length one), Default: 'dataverse.harvard.edu'
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @return R object (an output object of multiple potential types)
#' @rdname get_rds_from_dv
#' @export 
#' @importFrom dataverse dataset_files
#' @importFrom purrr map_chr
get_rds_from_dv <- function (file_nm_1L_chr, dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9", 
    dv_url_pfx_1L_chr = "https://dataverse.harvard.edu/api/access/datafile/", 
    server_1L_chr = "dataverse.harvard.edu", key_1L_chr = NULL) 
{
    ds_ls <- dataverse::dataset_files(dv_ds_nm_1L_chr, server = server_1L_chr, 
        key = key_1L_chr)
    all_items_chr <- purrr::map_chr(ds_ls, ~.x$label)
    idx_1L_int <- which(all_items_chr == paste0(file_nm_1L_chr, 
        ".RDS"))
    r_object_xx <- readRDS(url(paste0(dv_url_pfx_1L_chr, ds_ls[[idx_1L_int]]$dataFile$id)))
    return(r_object_xx)
}
#' Get return object name
#' @description get_return_obj_nm() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get return object name. Function argument fn specifies the where to look for the required object. The function returns Return (a character vector of length one).
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
