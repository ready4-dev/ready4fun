#' Make additional packages list
#' @description make_addl_pkgs_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make additional packages list. The function returns Additional packages (a list).
#' @param depends_chr Depends (a character vector), Default: NULL
#' @param enhances_chr Enhances (a character vector), Default: NULL
#' @param imports_chr Imports (a character vector), Default: NULL
#' @param linking_to_chr Linking to (a character vector), Default: NULL
#' @param suggests_chr Suggests (a character vector), Default: NULL
#' @param append_ls Append (a list), Default: NULL
#' @return Additional packages (a list)
#' @rdname make_addl_pkgs_ls
#' @export 
#' @importFrom purrr discard
make_addl_pkgs_ls <- function (depends_chr = NULL, enhances_chr = NULL, imports_chr = NULL, 
    linking_to_chr = NULL, suggests_chr = NULL, append_ls = NULL) 
{
    addl_pkgs_ls <- append(list(Depends = depends_chr, Enhances = enhances_chr, 
        Imports = imports_chr, LinkingTo = linking_to_chr, Suggests = suggests_chr), 
        append_ls) %>% purrr::discard(is.null)
    if (length(addl_pkgs_ls) == 0) 
        addl_pkgs_ls <- NULL
    return(addl_pkgs_ls)
}
#' Make argument description
#' @description make_arg_desc() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make argument description. The function returns Argument description (a character vector).
#' @param fn_args_chr Function arguments (a character vector)
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one), Default: 'https://doi.org/10.7910/DVN/2Y9VF9'
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: NULL
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return Argument description (a character vector)
#' @rdname make_arg_desc
#' @export 

#' @keywords internal
make_arg_desc <- function (fn_args_chr, object_type_lup = NULL, abbreviations_lup = NULL, 
    dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9", dv_url_pfx_1L_chr = NULL, 
    key_1L_chr = NULL, server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    if (is.null(abbreviations_lup)) 
        abbreviations_lup <- get_rds_from_dv("abbreviations_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    if (is.null(object_type_lup)) 
        object_type_lup <- get_rds_from_dv("object_type_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    arg_desc_chr <- make_arg_type(fn_args_chr, object_type_lup = object_type_lup, 
        abbreviations_lup = abbreviations_lup, fn = make_arg_desc_spine, 
        dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
        key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    return(arg_desc_chr)
}
#' Make argument description list
#' @description make_arg_desc_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make argument description list. The function returns Argument description (a list).
#' @param fn_nms_chr Function names (a character vector)
#' @param fns_env_ls Functions (a list of environments)
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one), Default: 'https://doi.org/10.7910/DVN/2Y9VF9'
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: NULL
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return Argument description (a list)
#' @rdname make_arg_desc_ls
#' @export 
#' @importFrom purrr map
#' @importFrom stats setNames
#' @keywords internal
make_arg_desc_ls <- function (fn_nms_chr, fns_env_ls, abbreviations_lup = NULL, dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9", 
    dv_url_pfx_1L_chr = NULL, key_1L_chr = NULL, object_type_lup = NULL, 
    server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    if (is.null(abbreviations_lup)) 
        abbreviations_lup <- get_rds_from_dv("abbreviations_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    if (is.null(object_type_lup)) 
        object_type_lup <- get_rds_from_dv("object_type_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    arg_desc_ls <- purrr::map(fn_nms_chr, ~{
        if (!exists(.x)) {
            fn <- fns_env_ls$fns_env[[.x]]
        }
        else {
            fn <- eval(parse(text = .x))
        }
        get_fn_args(fn) %>% make_arg_desc(abbreviations_lup = abbreviations_lup, 
            object_type_lup = object_type_lup, dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, 
            dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, key_1L_chr = key_1L_chr, 
            server_1L_chr = server_1L_chr) %>% stats::setNames(get_fn_args(fn))
    })
    return(arg_desc_ls)
}
#' Make argument description spine
#' @description make_arg_desc_spine() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make argument description spine. The function is called for its side effects and does not return a value.
#' @param argument_nm_1L_chr Argument name (a character vector of length one)
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one), Default: 'https://doi.org/10.7910/DVN/2Y9VF9'
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: NULL
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param master_object_type_lup Master object type (a lookup table), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return NA ()
#' @rdname make_arg_desc_spine
#' @export 

#' @keywords internal
make_arg_desc_spine <- function (argument_nm_1L_chr, object_type_lup = NULL, abbreviations_lup = NULL, 
    dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9", dv_url_pfx_1L_chr = NULL, 
    key_1L_chr = NULL, master_object_type_lup = NULL, server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    if (is.null(abbreviations_lup)) 
        abbreviations_lup <- get_rds_from_dv("abbreviations_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    if (is.null(object_type_lup)) 
        object_type_lup <- get_rds_from_dv("object_type_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    if (is.null(master_object_type_lup)) 
        master_object_type_lup <- get_rds_from_dv("object_type_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    if (is.na(argument_nm_1L_chr)) {
        match_1L_chr <- character(0)
    }
    else {
        match_1L_chr <- get_arg_obj_type(argument_nm_1L_chr, 
            object_type_lup = object_type_lup)
    }
    arg_desc_spine <- ifelse(identical(match_1L_chr, character(0)), 
        NA_character_, paste0(argument_nm_1L_chr %>% make_arg_title(match_chr = match_1L_chr, 
            abbreviations_lup = abbreviations_lup, object_type_lup = master_object_type_lup), 
            " (", match_1L_chr %>% update_first_word_case() %>% 
                add_indefartls_to_phrases(abbreviations_lup = abbreviations_lup, 
                  ignore_phrs_not_in_lup_1L_lgl = F), ")"))
    return(arg_desc_spine)
}
#' Make argument title
#' @description make_arg_title() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make argument title. The function returns Title (a character vector).
#' @param args_chr Arguments (a character vector)
#' @param match_chr Match (a character vector)
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one), Default: 'https://doi.org/10.7910/DVN/2Y9VF9'
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: NULL
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return Title (a character vector)
#' @rdname make_arg_title
#' @export 
#' @importFrom purrr map_chr map2_chr
#' @importFrom stringi stri_replace_last_fixed
#' @importFrom stringr str_replace_all
#' @importFrom Hmisc capitalize
#' @keywords internal
make_arg_title <- function (args_chr, match_chr, object_type_lup = NULL, abbreviations_lup = NULL, 
    dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9", dv_url_pfx_1L_chr = NULL, 
    key_1L_chr = NULL, server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    if (is.null(abbreviations_lup)) 
        abbreviations_lup <- get_rds_from_dv("abbreviations_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    if (is.null(object_type_lup)) 
        object_type_lup <- get_rds_from_dv("object_type_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    suffices_chr <- match_chr %>% purrr::map_chr(~{
        ifelse(.x == "NO MATCH", "", get_from_lup_obj(object_type_lup, 
            match_value_xx = .x, match_var_nm_1L_chr = "long_name_chr", 
            target_var_nm_1L_chr = "short_name_chr", evaluate_lgl = F))
    })
    title_chr <- purrr::map2_chr(args_chr, suffices_chr, ~ifelse(.y == 
        "", .x, stringi::stri_replace_last_fixed(.x, paste0("_", 
        .y), ""))) %>% stringr::str_replace_all("_", " ") %>% 
        purrr::map_chr(~replace_abbr(.x, abbreviations_lup = abbreviations_lup) %>% 
            stringi::stri_replace_last_fixed(" R", "")) %>% Hmisc::capitalize()
    return(title_chr)
}
#' Make argument type
#' @description make_arg_type() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make argument type. The function returns Argument description (a character vector).
#' @param fn_args_chr Function arguments (a character vector)
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one), Default: 'https://doi.org/10.7910/DVN/2Y9VF9'
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: NULL
#' @param fn Function (a function)
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return Argument description (a character vector)
#' @rdname make_arg_type
#' @export 
#' @importFrom purrr map_chr discard pluck
#' @importFrom stats setNames
#' @importFrom rlang exec
#' @keywords internal
make_arg_type <- function (fn_args_chr, object_type_lup = NULL, abbreviations_lup = NULL, 
    dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9", dv_url_pfx_1L_chr = NULL, 
    fn, key_1L_chr = NULL, server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    if (is.null(abbreviations_lup)) 
        abbreviations_lup <- get_rds_from_dv("abbreviations_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    if (is.null(object_type_lup)) 
        object_type_lup <- get_rds_from_dv("object_type_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    lup_ls <- make_arg_type_lup_ls(object_type_lup)
    append_1L_lgl <- "abbreviations_lup" %in% get_fn_args(fn)
    append_master_1L_lgl <- "master_object_type_lup" %in% get_fn_args(fn)
    arg_desc_chr <- fn_args_chr %>% purrr::map_chr(~{
        argument_nm_1L_chr <- .x
        arg_desc_1L_chr <- purrr::map_chr(lup_ls, ~{
            args_ls <- list(argument_nm_1L_chr, .x) %>% stats::setNames(c("argument_nm_1L_chr", 
                "object_type_lup"))
            if (append_1L_lgl) 
                args_ls <- append(args_ls, list(abbreviations_lup = abbreviations_lup))
            if (append_master_1L_lgl) 
                args_ls <- append(args_ls, list(dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, 
                  dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, key_1L_chr = key_1L_chr, 
                  master_object_type_lup = object_type_lup, server_1L_chr = server_1L_chr))
            rlang::exec(fn, !!!args_ls)
        }) %>% purrr::discard(is.na) %>% purrr::pluck(1)
        if (is.null(arg_desc_1L_chr)) 
            arg_desc_1L_chr <- "NO MATCH"
        arg_desc_1L_chr
    })
    return(arg_desc_chr)
}
#' Make argument type abbreviation
#' @description make_arg_type_abbr() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make argument type abbreviation. The function returns Argument type abbreviation (a character vector).
#' @param fn_args_chr Function arguments (a character vector)
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one), Default: 'https://doi.org/10.7910/DVN/2Y9VF9'
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: NULL
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return Argument type abbreviation (a character vector)
#' @rdname make_arg_type_abbr
#' @export 

#' @keywords internal
make_arg_type_abbr <- function (fn_args_chr, object_type_lup = NULL, abbreviations_lup = NULL, 
    dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9", dv_url_pfx_1L_chr = NULL, 
    key_1L_chr = NULL, server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    if (is.null(abbreviations_lup)) 
        abbreviations_lup <- get_rds_from_dv("abbreviations_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    if (is.null(object_type_lup)) 
        object_type_lup <- get_rds_from_dv("object_type_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    arg_type_abbr_chr <- make_arg_type(fn_args_chr, object_type_lup = object_type_lup, 
        fn = make_arg_type_abbr_spine, abbreviations_lup = abbreviations_lup, 
        dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
        key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    return(arg_type_abbr_chr)
}
#' Make argument type abbreviation spine
#' @description make_arg_type_abbr_spine() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make argument type abbreviation spine. The function returns Argument type abbreviation spine (a character vector of length one).
#' @param argument_nm_1L_chr Argument name (a character vector of length one)
#' @param object_type_lup Object type (a lookup table)
#' @return Argument type abbreviation spine (a character vector of length one)
#' @rdname make_arg_type_abbr_spine
#' @export 

#' @keywords internal
make_arg_type_abbr_spine <- function (argument_nm_1L_chr, object_type_lup) 
{
    arg_type_1L_chr <- object_type_lup$short_name_chr[endsWith(argument_nm_1L_chr, 
        object_type_lup$short_name_chr)]
    arg_type_abbr_spine_1L_chr <- ifelse(identical(character(0), 
        arg_type_1L_chr), NA_character_, arg_type_1L_chr)
    return(arg_type_abbr_spine_1L_chr)
}
#' Make argument type lookup table list
#' @description make_arg_type_lup_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make argument type lookup table list. The function returns Lookup table list (a list of lookup tables).
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one), Default: 'https://doi.org/10.7910/DVN/2Y9VF9'
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: NULL
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return Lookup table list (a list of lookup tables)
#' @rdname make_arg_type_lup_ls
#' @export 
#' @importFrom dplyr mutate filter
#' @importFrom purrr map
#' @keywords internal
make_arg_type_lup_ls <- function (object_type_lup = NULL, dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9", 
    dv_url_pfx_1L_chr = NULL, key_1L_chr = NULL, server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    if (is.null(object_type_lup)) 
        object_type_lup <- get_rds_from_dv("object_type_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    new_lup <- object_type_lup %>% dplyr::mutate(nchar_int = nchar(short_name_chr))
    lup_ls <- new_lup$nchar_int %>% unique() %>% sort(decreasing = T) %>% 
        purrr::map(~dplyr::filter(new_lup, nchar_int == .x))
    return(lup_ls)
}
#' Make build ignore list
#' @description make_build_ignore_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make build ignore list. The function returns Build ignore (a list).
#' @param file_nms_chr File names (a character vector), Default: NULL
#' @param regulars_rgx Regulars (a regular expression vector), Default: NULL
#' @return Build ignore (a list)
#' @rdname make_build_ignore_ls
#' @export 

make_build_ignore_ls <- function (file_nms_chr = NULL, regulars_rgx = NULL) 
{
    build_ignore_ls = list(file_nms_chr = file_nms_chr, regulars_rgx = regulars_rgx)
    return(build_ignore_ls)
}
#' Make dependent functions list
#' @description make_depnt_fns_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make dependent functions list. The function returns Argument (a list).
#' @param arg_ls Argument (a list)
#' @param pkg_depcy_ls Package dependency (a list)
#' @return Argument (a list)
#' @rdname make_depnt_fns_ls
#' @export 
#' @importFrom purrr map_dfr
#' @importFrom dplyr filter bind_rows distinct
#' @keywords internal
make_depnt_fns_ls <- function (arg_ls, pkg_depcy_ls) 
{
    lower_tb <- purrr::map_dfr(arg_ls$new_dbl, ~pkg_depcy_ls[["fromto"]] %>% 
        dplyr::filter(to == .x))
    upper_tb <- dplyr::bind_rows(arg_ls$upper_tb, lower_tb) %>% 
        dplyr::distinct()
    new_dbl <- setdiff(upper_tb$from, c(arg_ls$new_dbl, arg_ls$solo_dbl, 
        arg_ls$upper_tb$from))
    solo_dbl <- c(arg_ls$solo_dbl, setdiff(lower_tb$from, new_dbl))
    arg_ls <- list(new_dbl = new_dbl, solo_dbl = solo_dbl, upper_tb = upper_tb)
    return(arg_ls)
}
#' Make documentation for all functions
#' @description make_dmt_for_all_fns() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make documentation for all functions. The function returns All functions documentation (a tibble).
#' @param paths_ls Paths (a list), Default: make_fn_nms()
#' @param undocumented_fns_dir_chr Undocumented functions directory (a character vector), Default: make_undmtd_fns_dir_chr(drop_empty_1L_lgl = T)
#' @param custom_dmt_ls Custom documentation (a list), Default: list(details_ls = NULL, inc_for_main_user_lgl_ls = list(force_true_chr = NA_character_, 
#'    force_false_chr = NA_character_), args_ls_ls = NULL)
#' @param fns_env_ls Functions (a list of environments), Default: NULL
#' @param fn_types_lup Function types (a lookup table)
#' @param abbreviations_lup Abbreviations (a lookup table)
#' @param object_type_lup Object type (a lookup table)
#' @param inc_all_mthds_1L_lgl Include all methods (a logical vector of length one), Default: T
#' @return All functions documentation (a tibble)
#' @rdname make_dmt_for_all_fns
#' @export 
#' @importFrom utils data
#' @importFrom purrr map2_dfr
#' @importFrom dplyr mutate case_when
#' @keywords internal
make_dmt_for_all_fns <- function (paths_ls = make_fn_nms(), undocumented_fns_dir_chr = make_undmtd_fns_dir_chr(drop_empty_1L_lgl = T), 
    custom_dmt_ls = list(details_ls = NULL, inc_for_main_user_lgl_ls = list(force_true_chr = NA_character_, 
        force_false_chr = NA_character_), args_ls_ls = NULL), 
    fns_env_ls = NULL, fn_types_lup, abbreviations_lup, object_type_lup, 
    inc_all_mthds_1L_lgl = T) 
{
    if (is.null(abbreviations_lup)) 
        utils::data("abbreviations_lup", package = "ready4fun", 
            envir = environment())
    if (is.null(fns_env_ls)) 
        fns_env_ls <- read_fns(undocumented_fns_dir_chr)
    all_fns_dmt_tb <- purrr::map2_dfr(paths_ls, undocumented_fns_dir_chr, 
        ~{
            fns_dmt_tb <- make_fn_dmt_tbl(.x, fns_dir_chr = .y, 
                custom_dmt_ls = custom_dmt_ls, append_1L_lgl = T, 
                fns_env_ls = fns_env_ls, fn_types_lup = fn_types_lup, 
                abbreviations_lup = abbreviations_lup, object_type_lup = object_type_lup)
            if (inc_all_mthds_1L_lgl) 
                fns_dmt_tb <- fns_dmt_tb %>% dplyr::mutate(inc_for_main_user_lgl = dplyr::case_when(file_pfx_chr %in% 
                  c("grp_", "mthd_") ~ T, TRUE ~ inc_for_main_user_lgl))
            fns_dmt_tb
        })
    return(all_fns_dmt_tb)
}
#' Make function description
#' @description make_fn_desc() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make function description. The function returns Function description (a character vector).
#' @param fns_chr Functions (a character vector)
#' @param title_chr Title (a character vector)
#' @param output_chr Output (a character vector)
#' @param fns_env_ls Functions (a list of environments)
#' @param fn_types_lup Function types (a lookup table), Default: NULL
#' @param abbreviations_lup Abbreviations (a lookup table)
#' @param test_for_write_R_warning_fn Test for write warning (a function), Default: NULL
#' @param is_generic_lgl Is generic (a logical vector), Default: F
#' @return Function description (a character vector)
#' @rdname make_fn_desc
#' @export 
#' @importFrom purrr pmap_chr
#' @importFrom stringr str_extract
#' @keywords internal
make_fn_desc <- function (fns_chr, title_chr, output_chr, fns_env_ls, fn_types_lup = NULL, 
    abbreviations_lup, test_for_write_R_warning_fn = NULL, is_generic_lgl = F) 
{
    if (is.null(test_for_write_R_warning_fn)) 
        test_for_write_R_warning_fn <- function(x) {
            startsWith(x, "write")
        }
    fn_desc_chr <- purrr::pmap_chr(list(fns_chr, title_chr, output_chr, 
        is_generic_lgl), ~{
        fn_type_1L_chr <- stringr::str_extract(..2, "[A-Za-z]+")
        fn_name_1L_chr <- ..1
        fn_title_1L_chr <- ..2
        fn_output_1L_chr <- ..3
        is_generic_1L_lgl <- ..4
        if (!exists(fn_name_1L_chr)) {
            fn <- fns_env_ls$fns_env[[fn_name_1L_chr]]
        }
        else {
            fn <- eval(parse(text = fn_name_1L_chr))
        }
        paste0(make_fn_desc_spine(fn, fn_name_1L_chr = fn_name_1L_chr, 
            fn_title_1L_chr = fn_title_1L_chr, fn_types_lup = fn_types_lup, 
            abbreviations_lup = abbreviations_lup, is_generic_1L_lgl = is_generic_1L_lgl), 
            ifelse(fn_output_1L_chr == "NULL", ifelse(is_generic_1L_lgl, 
                "", paste0(" The function is called for its side effects and does not return a value.", 
                  ifelse(fn_name_1L_chr %>% test_for_write_R_warning_fn, 
                    " WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour", 
                    ""))), paste0(" The function returns ", make_ret_obj_desc(fn, 
                abbreviations_lup = abbreviations_lup, starts_sentence_1L_lgl = T), 
                ".")))
    })
    return(fn_desc_chr)
}
#' Make function description spine
#' @description make_fn_desc_spine() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make function description spine. The function returns Function description spine (a character vector of length one).
#' @param fn Function (a function)
#' @param fn_name_1L_chr Function name (a character vector of length one)
#' @param fn_title_1L_chr Function title (a character vector of length one)
#' @param fn_types_lup Function types (a lookup table)
#' @param abbreviations_lup Abbreviations (a lookup table)
#' @param is_generic_1L_lgl Is generic (a logical vector of length one), Default: NULL
#' @return Function description spine (a character vector of length one)
#' @rdname make_fn_desc_spine
#' @export 
#' @importFrom purrr map_lgl map_chr
#' @importFrom tools toTitleCase
#' @keywords internal
make_fn_desc_spine <- function (fn, fn_name_1L_chr, fn_title_1L_chr, fn_types_lup, 
    abbreviations_lup, is_generic_1L_lgl = NULL) 
{
    fn_args_chr <- get_fn_args(fn)
    pfx_matches_chr <- fn_types_lup$fn_type_nm_chr[purrr::map_lgl(fn_types_lup$fn_type_nm_chr, 
        ~startsWith(fn_title_1L_chr %>% tools::toTitleCase(), 
            .x))]
    fn_type_chr <- pfx_matches_chr[nchar(pfx_matches_chr) == 
        max(nchar(pfx_matches_chr))]
    text_elements_chr <- names(fn_types_lup)[2:4] %>% purrr::map_chr(~get_from_lup_obj(fn_types_lup, 
        match_var_nm_1L_chr = "fn_type_nm_chr", match_value_xx = fn_type_chr[1], 
        target_var_nm_1L_chr = .x, evaluate_lgl = F))
    if (is.null(is_generic_1L_lgl)) {
        is_generic_1L_lgl <- fn_type_chr[1] == fn_name_1L_chr
    }
    treat_as_1L_chr <- ifelse(is_generic_1L_lgl, "Generic", ifelse(purrr::map_lgl(abbreviations_lup$short_name_chr, 
        ~endsWith(fn_name_1L_chr, paste0(".", .x))) %>% any(), 
        "Method", "Function"))
    fn_desc_spine_1L_chr <- paste0(fn_name_1L_chr, "() is ", 
        add_indef_artl_to_item(fn_type_chr[1], ignore_phrs_not_in_lup = F, 
            abbreviations_lup = abbreviations_lup), " ", tolower(treat_as_1L_chr), 
        " that ", update_first_word_case(text_elements_chr[1]), 
        ifelse(treat_as_1L_chr == "Generic", "", ifelse(treat_as_1L_chr == 
            "Method", paste0(" This method is implemented for the ", 
            abbreviations_lup$long_name_chr[purrr::map_lgl(abbreviations_lup$short_name_chr, 
                ~endsWith(fn_name_1L_chr, paste0(".", .x)))]), 
            paste0(" Specifically, this function implements an algorithm to ", 
                make_fn_title(fn_name_1L_chr, object_type_lup = abbreviations_lup, 
                  abbreviations_lup = abbreviations_lup, is_generic_lgl = T) %>% 
                  tolower(), "."))), ifelse(ifelse(is.null(fn_args_chr) | 
            is.na(text_elements_chr[2]), F, T), paste0(" Function argument ", 
            fn_args_chr[1], " specifies the ", update_first_word_case(text_elements_chr[2])), 
            ""), ifelse(ifelse(is.null(fn_args_chr) | is.na(text_elements_chr[3]), 
            F, length(fn_args_chr) > 1), paste0(" Argument ", 
            fn_args_chr[2], " provides the ", update_first_word_case(text_elements_chr[3])), 
            ""))
    return(fn_desc_spine_1L_chr)
}
#' Make function documentation spine
#' @description make_fn_dmt_spine() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make function documentation spine. The function returns Function documentation spine (a list of character vectors).
#' @param fn_name_1L_chr Function name (a character vector of length one)
#' @param fn_type_1L_chr Function type (a character vector of length one)
#' @param fn_title_1L_chr Function title (a character vector of length one), Default: 'NA'
#' @param fn Function (a function)
#' @param details_1L_chr Details (a character vector of length one), Default: 'NA'
#' @param example_1L_lgl Example (a logical vector of length one), Default: F
#' @param export_1L_lgl Export (a logical vector of length one), Default: T
#' @param class_name_1L_chr Class name (a character vector of length one)
#' @param doc_in_class_1L_lgl Document in class (a logical vector of length one)
#' @return Function documentation spine (a list of character vectors)
#' @rdname make_fn_dmt_spine
#' @export 

#' @keywords internal
make_fn_dmt_spine <- function (fn_name_1L_chr, fn_type_1L_chr, fn_title_1L_chr = NA_character_, 
    fn, details_1L_chr = NA_character_, example_1L_lgl = F, export_1L_lgl = T, 
    class_name_1L_chr, doc_in_class_1L_lgl) 
{
    get_set_chr <- c("gen_get_slot", "meth_get_slot", "gen_set_slot", 
        "meth_set_slot")
    if (!fn_type_1L_chr %in% get_set_chr) {
        fn_dmt_spine_chr_ls <- make_std_fn_dmt_spine(fn_name_1L_chr = fn_name_1L_chr, 
            fn_type_1L_chr = fn_type_1L_chr, fn_title_1L_chr = fn_title_1L_chr, 
            fn = fn, details_1L_chr = details_1L_chr, example_1L_lgl = example_1L_lgl, 
            export_1L_lgl = export_1L_lgl, class_name_1L_chr = class_name_1L_chr, 
            exclude_if_match_chr = get_set_chr)
    }
    else {
        fn_dmt_spine_chr_ls <- make_gtr_str_dmt_spine(fn_type_1L_chr = fn_type_1L_chr, 
            fn_name_1L_chr = fn_name_1L_chr, class_name_1L_chr = class_name_1L_chr, 
            doc_in_class_1L_lgl = doc_in_class_1L_lgl, example_1L_lgl = example_1L_lgl)
    }
    return(fn_dmt_spine_chr_ls)
}
#' Make function documentation table
#' @description make_fn_dmt_tbl() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make function documentation table. The function returns Function documentation table (a tibble).
#' @param fns_path_chr Functions path (a character vector)
#' @param fns_dir_chr Functions directory (a character vector), Default: make_undmtd_fns_dir_chr(drop_empty_1L_lgl = T)
#' @param custom_dmt_ls Custom documentation (a list), Default: list(title_ls = NULL, desc_ls = NULL, details_ls = NULL, inc_for_main_user_lgl_ls = NULL, 
#'    output_ls = NULL, example_ls = NULL, args_ls_ls = NULL)
#' @param append_1L_lgl Append (a logical vector of length one), Default: T
#' @param fns_env_ls Functions (a list of environments), Default: NULL
#' @param fn_types_lup Function types (a lookup table), Default: NULL
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one), Default: 'https://doi.org/10.7910/DVN/2Y9VF9'
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: NULL
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @param test_for_write_R_warning_fn Test for write warning (a function), Default: NULL
#' @return Function documentation table (a tibble)
#' @rdname make_fn_dmt_tbl
#' @export 
#' @importFrom purrr map_lgl discard
#' @importFrom rlang exec
#' @keywords internal
make_fn_dmt_tbl <- function (fns_path_chr, fns_dir_chr = make_undmtd_fns_dir_chr(drop_empty_1L_lgl = T), 
    custom_dmt_ls = list(title_ls = NULL, desc_ls = NULL, details_ls = NULL, 
        inc_for_main_user_lgl_ls = NULL, output_ls = NULL, example_ls = NULL, 
        args_ls_ls = NULL), append_1L_lgl = T, fns_env_ls = NULL, 
    fn_types_lup = NULL, abbreviations_lup = NULL, dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9", 
    dv_url_pfx_1L_chr = NULL, key_1L_chr = NULL, object_type_lup = NULL, 
    server_1L_chr = Sys.getenv("DATAVERSE_SERVER"), test_for_write_R_warning_fn = NULL) 
{
    if (is.null(fns_env_ls)) 
        fns_env_ls <- read_fns(fns_dir_chr)
    if (is.null(abbreviations_lup)) 
        abbreviations_lup <- get_rds_from_dv("abbreviations_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    if (is.null(object_type_lup)) 
        object_type_lup <- get_rds_from_dv("object_type_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    fn_dmt_tbl_tb <- make_fn_dmt_tbl_tmpl(fns_path_chr, fns_dir_chr = fns_dir_chr, 
        fns_env_ls = fns_env_ls, fn_types_lup = fn_types_lup, 
        abbreviations_lup = abbreviations_lup, object_type_lup = object_type_lup, 
        test_for_write_R_warning_fn = test_for_write_R_warning_fn)
    if (purrr::map_lgl(custom_dmt_ls, ~!is.null(.x)) %>% any()) {
        args_ls <- append(custom_dmt_ls, list(append_1L_lgl = append_1L_lgl)) %>% 
            purrr::discard(is.null)
        fn_dmt_tbl_tb <- rlang::exec(update_fns_dmt_tb, fns_dmt_tb = fn_dmt_tbl_tb, 
            !!!args_ls)
    }
    return(fn_dmt_tbl_tb)
}
#' Make function documentation table template
#' @description make_fn_dmt_tbl_tmpl() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make function documentation table template. The function returns Function documentation table (a tibble).
#' @param fns_path_chr Functions path (a character vector)
#' @param fns_dir_chr Functions directory (a character vector), Default: make_undmtd_fns_dir_chr(drop_empty_1L_lgl = T)
#' @param fns_env_ls Functions (a list of environments), Default: NULL
#' @param fn_types_lup Function types (a lookup table), Default: NULL
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one), Default: 'https://doi.org/10.7910/DVN/2Y9VF9'
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: NULL
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @param test_for_write_R_warning_fn Test for write warning (a function), Default: NULL
#' @return Function documentation table (a tibble)
#' @rdname make_fn_dmt_tbl_tmpl
#' @export 
#' @importFrom stringr str_replace
#' @importFrom purrr map_dfr map_lgl
#' @importFrom tibble tibble
#' @importFrom dplyr mutate filter
#' @importFrom tools toTitleCase
#' @keywords internal
make_fn_dmt_tbl_tmpl <- function (fns_path_chr, fns_dir_chr = make_undmtd_fns_dir_chr(drop_empty_1L_lgl = T), 
    fns_env_ls = NULL, fn_types_lup = NULL, abbreviations_lup = NULL, 
    dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9", dv_url_pfx_1L_chr = NULL, 
    key_1L_chr = NULL, object_type_lup = NULL, server_1L_chr = Sys.getenv("DATAVERSE_SERVER"), 
    test_for_write_R_warning_fn = NULL) 
{
    if (is.null(fns_env_ls)) 
        fns_env_ls <- read_fns(fns_dir_chr)
    if (is.null(abbreviations_lup)) 
        abbreviations_lup <- get_rds_from_dv("abbreviations_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    if (is.null(object_type_lup)) 
        object_type_lup <- get_rds_from_dv("object_type_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    file_pfx_chr <- fns_dir_chr %>% stringr::str_replace("data-raw/", 
        "") %>% switch(fns = "fn_", s3 = "C3_", gnrcs = "grp_", 
        mthds = "mthd_", "s4 = C4_")
    fn_dmt_tbl_tb <- fns_path_chr %>% purrr::map_dfr(~tibble::tibble(fns_chr = get_fn_nms_in_file(.x), 
        title_chr = NA_character_, desc_chr = NA_character_, 
        details_chr = NA_character_, inc_for_main_user_lgl = F, 
        output_chr = NA_character_, example_lgl = F, args_ls = list(NULL), 
        file_nm_chr = .x %>% stringr::str_replace(paste0(fns_dir_chr, 
            "/"), ""), file_pfx_chr = file_pfx_chr))
    fn_dmt_tbl_tb <- fn_dmt_tbl_tb %>% dplyr::mutate(title_chr = make_fn_title(fns_chr, 
        abbreviations_lup = abbreviations_lup, object_type_lup = object_type_lup, 
        is_generic_lgl = T))
    fn_dmt_tbl_tb <- fn_dmt_tbl_tb %>% dplyr::filter(title_chr %>% 
        tools::toTitleCase() %>% purrr::map_lgl(~{
        startsWith(.x, fn_types_lup$fn_type_nm_chr) %>% any()
    }))
    fn_dmt_tbl_tb <- fn_dmt_tbl_tb %>% dplyr::mutate(output_chr = get_outp_obj_type(fns_chr, 
        abbreviations_lup = abbreviations_lup, fns_env_ls = fns_env_ls, 
        is_generic_lgl = purrr::map_lgl(file_nm_chr, ~.x == "generics.R"), 
        object_type_lup = object_type_lup))
    fn_dmt_tbl_tb <- fn_dmt_tbl_tb %>% dplyr::mutate(desc_chr = make_fn_desc(fns_chr, 
        title_chr = title_chr, output_chr = output_chr, fns_env_ls = fns_env_ls, 
        fn_types_lup = fn_types_lup, abbreviations_lup = abbreviations_lup, 
        test_for_write_R_warning_fn = test_for_write_R_warning_fn, 
        is_generic_lgl = purrr::map_lgl(file_nm_chr, ~.x == "generics.R")))
    fn_dmt_tbl_tb <- fn_dmt_tbl_tb %>% dplyr::mutate(args_ls = make_arg_desc_ls(fns_chr, 
        fns_env_ls = fns_env_ls, abbreviations_lup = abbreviations_lup, 
        object_type_lup = object_type_lup))
    return(fn_dmt_tbl_tb)
}
#' Make function names
#' @description make_fn_nms() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make function names. The function returns Functions (a list of character vectors of length one).
#' @param path_1L_chr Path (a character vector of length one), Default: 'data-raw'
#' @return Functions (a list of character vectors of length one)
#' @rdname make_fn_nms
#' @export 
#' @importFrom purrr map_lgl map discard
#' @importFrom stats setNames
#' @keywords internal
make_fn_nms <- function (path_1L_chr = "data-raw") 
{
    undmtd_fns_dir_chr <- make_undmtd_fns_dir_chr(path_1L_chr, 
        drop_empty_1L_lgl = T)
    fn_types_chr <- make_fn_types()
    fn_types_chr <- fn_types_chr[fn_types_chr %>% purrr::map_lgl(~{
        suffix_1L_lgl <- .x
        undmtd_fns_dir_chr %>% purrr::map_lgl(~endsWith(.x, suffix_1L_lgl)) %>% 
            any()
    })]
    fns_1L_chr_ls <- undmtd_fns_dir_chr %>% purrr::map(~list.files(.x, 
        pattern = "*.R$", full.names = TRUE, ignore.case = TRUE)) %>% 
        stats::setNames(fn_types_chr)
    fns_1L_chr_ls <- fns_1L_chr_ls %>% purrr::discard(~identical(.x, 
        character(0)))
    return(fns_1L_chr_ls)
}
#' Make function title
#' @description make_fn_title() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make function title. The function returns Title (a character vector).
#' @param fns_chr Functions (a character vector)
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one), Default: 'https://doi.org/10.7910/DVN/2Y9VF9'
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: NULL
#' @param is_generic_lgl Is generic (a logical vector), Default: F
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return Title (a character vector)
#' @rdname make_fn_title
#' @export 
#' @importFrom stringr str_replace_all
#' @importFrom Hmisc capitalize
#' @importFrom purrr map_chr
#' @importFrom stringi stri_replace_last_fixed
#' @keywords internal
make_fn_title <- function (fns_chr, object_type_lup = NULL, abbreviations_lup = NULL, 
    dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9", dv_url_pfx_1L_chr = NULL, 
    is_generic_lgl = F, key_1L_chr = NULL, server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    if (is.null(abbreviations_lup)) 
        abbreviations_lup <- get_rds_from_dv("abbreviations_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    if (is.null(object_type_lup)) 
        object_type_lup <- get_rds_from_dv("object_type_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    title_chr <- remove_obj_type_from_nm(fns_chr, object_type_lup = object_type_lup, 
        abbreviations_lup = abbreviations_lup, is_generic_lgl = T) %>% 
        stringr::str_replace_all("_", " ") %>% Hmisc::capitalize() %>% 
        purrr::map_chr(~replace_abbr(.x, abbreviations_lup = abbreviations_lup) %>% 
            stringi::stri_replace_last_fixed(" R", ""))
    return(title_chr)
}
#' Make function type lookup table
#' @description make_fn_type_lup() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make function type lookup table. The function returns Function types (a lookup table).
#' @param fn_type_nm_chr Function type name (a character vector), Default: character(0)
#' @param fn_type_desc_chr Function type description (a character vector), Default: character(0)
#' @param first_arg_desc_chr First argument description (a character vector), Default: character(0)
#' @param second_arg_desc_chr Second argument description (a character vector), Default: character(0)
#' @param is_generic_lgl Is generic (a logical vector), Default: logical(0)
#' @param is_method_lgl Is method (a logical vector), Default: logical(0)
#' @return Function types (a lookup table)
#' @rdname make_fn_type_lup
#' @export 
#' @importFrom tibble tibble
#' @importFrom dplyr arrange
#' @keywords internal
make_fn_type_lup <- function (fn_type_nm_chr = character(0), fn_type_desc_chr = character(0), 
    first_arg_desc_chr = character(0), second_arg_desc_chr = character(0), 
    is_generic_lgl = logical(0), is_method_lgl = logical(0)) 
{
    fn_types_lup <- tibble::tibble(fn_type_nm_chr = fn_type_nm_chr, 
        fn_type_desc_chr = fn_type_desc_chr, first_arg_desc_chr = first_arg_desc_chr, 
        second_arg_desc_chr = second_arg_desc_chr, is_generic_lgl = is_generic_lgl, 
        is_method_lgl = is_method_lgl) %>% dplyr::arrange(fn_type_nm_chr)
    return(fn_types_lup)
}
#' Make function types
#' @description make_fn_types() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make function types. The function returns Functions type (a character vector).

#' @return Functions type (a character vector)
#' @rdname make_fn_types
#' @export 

#' @keywords internal
make_fn_types <- function () 
{
    fns_type_chr <- c("fns", "gnrcs", "mthds")
    return(fns_type_chr)
}
#' Make getter setter documentation spine
#' @description make_gtr_str_dmt_spine() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make getter setter documentation spine. The function returns Getter setter documentation spine (a list of character vectors).
#' @param fn_type_1L_chr Function type (a character vector of length one)
#' @param fn_name_1L_chr Function name (a character vector of length one)
#' @param class_name_1L_chr Class name (a character vector of length one)
#' @param doc_in_class_1L_lgl Document in class (a logical vector of length one)
#' @param example_1L_lgl Example (a logical vector of length one), Default: F
#' @return Getter setter documentation spine (a list of character vectors)
#' @rdname make_gtr_str_dmt_spine
#' @export 
#' @importFrom stringr str_replace
#' @keywords internal
make_gtr_str_dmt_spine <- function (fn_type_1L_chr, fn_name_1L_chr, class_name_1L_chr, 
    doc_in_class_1L_lgl, example_1L_lgl = F) 
{
    if (fn_type_1L_chr %in% c("gen_set_slot", "meth_set_slot")) {
        ref_slot_1L_chr <- stringr::str_replace(fn_name_1L_chr, 
            "<-", "")
    }
    else {
        ref_slot_1L_chr <- fn_name_1L_chr
    }
    if (fn_type_1L_chr %in% c("gen_get_slot", "gen_set_slot")) 
        fn_tags_1L_chr <- paste0("#' FUNCTION_TITLE\n", "#' @description S4 Generic function to ", 
            ifelse(fn_type_1L_chr == "gen_get_slot", "get", "set"), 
            " the value of the slot ", ref_slot_1L_chr, "\n", 
            "#' @rdname ", ref_slot_1L_chr, ifelse(fn_type_1L_chr == 
                "gen_set_slot", "_set", ""), "-methods\n", "#' @param x An object ", 
            class_name_1L_chr, "\n", ifelse(fn_type_1L_chr == 
                "gen_set_slot", "#' @param value Value to be assigned to x\n", 
                ""), "#' @details DETAILS\n", "#' @export\n")
    if (fn_type_1L_chr %in% c("meth_get_slot", "meth_set_slot")) {
        fn_tags_1L_chr <- paste0("#' ", fn_name_1L_chr, "\n#' @name ", 
            fn_name_1L_chr, "-", class_name_1L_chr, "\n", "#' @description FUNCTION_DESCRIPTION", 
            " for S4 objects of class ", class_name_1L_chr, "\n", 
            "#' @param x An object of class ", class_name_1L_chr, 
            "\n", ifelse(fn_type_1L_chr == "gen_set_slot", "#' @param value Value to be assigned to x\n", 
                ""), ifelse(example_1L_lgl, paste0("#' @examples\n", 
                "#' \\dontrun{\n", "#' if(interactive()){\n", 
                "#'  #EXAMPLE1\n", "#'  }\n", "#' }\n"), ""), 
            "#' @rdname ", ifelse(doc_in_class_1L_lgl, class_name_1L_chr, 
                paste0(ref_slot_1L_chr, ifelse(fn_type_1L_chr == 
                  "meth_set_slot", "_set", ""), "-methods\n")), 
            paste0("#' @aliases ", fn_name_1L_chr, ",", class_name_1L_chr, 
                "-method"))
    }
    gtr_str_dmt_spine_chr_ls <- list(fn_tags_1L_chr = fn_tags_1L_chr, 
        ref_slot_1L_chr = ref_slot_1L_chr)
    return(gtr_str_dmt_spine_chr_ls)
}
#' Make lines for function documentation
#' @description make_lines_for_fn_dmt() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make lines for function documentation. The function is called for its side effects and does not return a value.
#' @param fn_name_1L_chr Function name (a character vector of length one)
#' @param fn_type_1L_chr Function type (a character vector of length one)
#' @param fn Function (a function), Default: NULL
#' @param fn_desc_1L_chr Function description (a character vector of length one), Default: 'NA'
#' @param fn_out_type_1L_chr Function out type (a character vector of length one), Default: 'NA'
#' @param fn_title_1L_chr Function title (a character vector of length one), Default: 'NA'
#' @param example_1L_lgl Example (a logical vector of length one), Default: F
#' @param export_1L_lgl Export (a logical vector of length one), Default: T
#' @param class_name_1L_chr Class name (a character vector of length one), Default: ''
#' @param details_1L_chr Details (a character vector of length one), Default: 'DETAILS'
#' @param args_ls Arguments (a list), Default: NULL
#' @param import_chr Import (a character vector), Default: 'NA'
#' @param doc_in_class_1L_lgl Document in class (a logical vector of length one), Default: F
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one), Default: 'https://doi.org/10.7910/DVN/2Y9VF9'
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: NULL
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return NULL
#' @rdname make_lines_for_fn_dmt
#' @export 

#' @keywords internal
make_lines_for_fn_dmt <- function (fn_name_1L_chr, fn_type_1L_chr, fn = NULL, fn_desc_1L_chr = NA_character_, 
    fn_out_type_1L_chr = NA_character_, fn_title_1L_chr = NA_character_, 
    example_1L_lgl = F, export_1L_lgl = T, class_name_1L_chr = "", 
    details_1L_chr = "DETAILS", args_ls = NULL, import_chr = NA_character_, 
    doc_in_class_1L_lgl = F, abbreviations_lup = NULL, dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9", 
    dv_url_pfx_1L_chr = NULL, key_1L_chr = NULL, object_type_lup = NULL, 
    server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    if (is.null(abbreviations_lup)) 
        abbreviations_lup <- get_rds_from_dv("abbreviations_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    if (is.null(object_type_lup)) 
        object_type_lup <- get_rds_from_dv("object_type_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    fn_tags_spine_ls <- make_fn_dmt_spine(fn_name_1L_chr = fn_name_1L_chr, 
        fn_type_1L_chr = fn_type_1L_chr, fn_title_1L_chr = fn_title_1L_chr, 
        fn = fn, example_1L_lgl = example_1L_lgl, export_1L_lgl = export_1L_lgl, 
        details_1L_chr = details_1L_chr, class_name_1L_chr = class_name_1L_chr, 
        doc_in_class_1L_lgl = doc_in_class_1L_lgl)
    new_tag_chr_ls <- make_new_fn_dmt(fn_type_1L_chr = fn_type_1L_chr, 
        fn_name_1L_chr = fn_name_1L_chr, fn_desc_1L_chr = fn_desc_1L_chr, 
        fn_det_1L_chr = details_1L_chr, fn_out_type_1L_chr = fn_out_type_1L_chr, 
        args_ls = args_ls, fn = fn, abbreviations_lup = abbreviations_lup, 
        object_type_lup = object_type_lup)
    fn_tags_chr <- update_fn_dmt(fn_tags_spine_ls = fn_tags_spine_ls, 
        new_tag_chr_ls = new_tag_chr_ls, fn_name_1L_chr = fn_name_1L_chr, 
        fn_type_1L_chr = fn_type_1L_chr, import_chr = import_chr, 
        abbreviations_lup = abbreviations_lup)
    writeLines(fn_tags_chr)
}
#' Make list phrase
#' @description make_list_phrase() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make list phrase. The function returns List phrase (a character vector of length one).
#' @param items_chr Items (a character vector)
#' @return List phrase (a character vector of length one)
#' @rdname make_list_phrase
#' @export 
#' @importFrom stringr str_c
#' @importFrom stringi stri_replace_last
#' @keywords internal
make_list_phrase <- function (items_chr) 
{
    list_phrase_1L_chr <- items_chr %>% stringr::str_c(sep = "", 
        collapse = ", ") %>% stringi::stri_replace_last(fixed = ",", 
        replacement = " and")
    return(list_phrase_1L_chr)
}
#' Make new entries tibble
#' @description make_new_entries_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make new entries tibble. The function returns New entries (a tibble).
#' @param short_name_chr Short name (a character vector)
#' @param long_name_chr Long name (a character vector)
#' @param atomic_element_lgl Atomic element (a logical vector), Default: F
#' @param r3_can_extend_lgl Ready4 S3 can extend (a logical vector), Default: F
#' @return New entries (a tibble)
#' @rdname make_new_entries_tb
#' @export 
#' @importFrom tibble tibble
#' @keywords internal
make_new_entries_tb <- function (short_name_chr, long_name_chr, atomic_element_lgl = F, 
    r3_can_extend_lgl = F) 
{
    new_entries_tb <- tibble::tibble(short_name_chr, long_name_chr, 
        atomic_element_lgl = atomic_element_lgl, r3_can_extend_lgl = r3_can_extend_lgl)
    return(new_entries_tb)
}
#' Make new function documentation
#' @description make_new_fn_dmt() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make new function documentation. The function returns New function documentation (a list of character vectors).
#' @param fn_type_1L_chr Function type (a character vector of length one)
#' @param fn_name_1L_chr Function name (a character vector of length one)
#' @param fn_desc_1L_chr Function description (a character vector of length one), Default: 'NA'
#' @param fn_det_1L_chr Function detail (a character vector of length one), Default: 'NA'
#' @param fn_out_type_1L_chr Function out type (a character vector of length one), Default: 'NA'
#' @param args_ls Arguments (a list), Default: NULL
#' @param fn Function (a function), Default: NULL
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one), Default: 'https://doi.org/10.7910/DVN/2Y9VF9'
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: NULL
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return New function documentation (a list of character vectors)
#' @rdname make_new_fn_dmt
#' @export 
#' @importFrom stringr str_replace str_sub
#' @importFrom stringi stri_locate_last_fixed
#' @importFrom purrr flatten_chr
#' @importFrom stats setNames
#' @keywords internal
make_new_fn_dmt <- function (fn_type_1L_chr, fn_name_1L_chr, fn_desc_1L_chr = NA_character_, 
    fn_det_1L_chr = NA_character_, fn_out_type_1L_chr = NA_character_, 
    args_ls = NULL, fn = NULL, abbreviations_lup = NULL, dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9", 
    dv_url_pfx_1L_chr = NULL, key_1L_chr = NULL, object_type_lup = NULL, 
    server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    if (is.null(abbreviations_lup)) 
        abbreviations_lup <- get_rds_from_dv("abbreviations_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    if (is.null(object_type_lup)) 
        object_type_lup <- get_rds_from_dv("object_type_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    s3_class_main_1L_chr <- x_param_desc_1L_chr <- NULL
    if (!is.null(fn)) {
        fn_args_chr <- get_fn_args(fn)
        fn_out_type_1L_chr <- make_ret_obj_desc(fn, abbreviations_lup = abbreviations_lup)
    }
    else {
        fn_args_chr <- NA_character_
    }
    if (fn_type_1L_chr == "set_class" | startsWith(fn_type_1L_chr, 
        "s3_")) {
        if (fn_type_1L_chr %in% c("set_class", "s3_valid_instance")) 
            short_class_desc_1L_chr <- get_from_lup_obj(abbreviations_lup, 
                match_var_nm_1L_chr = "short_name_chr", match_value_xx = fn_name_1L_chr, 
                target_var_nm_1L_chr = "long_name_chr", evaluate_lgl = F)
        if (fn_type_1L_chr == "s3_valid_instance") {
            s3_class_main_1L_chr <- short_class_desc_1L_chr %>% 
                `names<-`(fn_name_1L_chr)
        }
        if (fn_type_1L_chr == "s3_unvalidated_instance") {
            s3_class_main_1L_chr <- stringr::str_replace(fn_name_1L_chr, 
                "make_new_", "") %>% `names<-`(fn_name_1L_chr)
        }
        if (fn_type_1L_chr == "s3_prototype") {
            s3_class_main_1L_chr <- stringr::str_replace(fn_name_1L_chr, 
                "make_pt_", "") %>% `names<-`(fn_name_1L_chr)
        }
        if (fn_type_1L_chr == "s3_validator") {
            s3_class_main_1L_chr <- stringr::str_replace(fn_name_1L_chr, 
                "validate_", "") %>% `names<-`(fn_name_1L_chr)
        }
        if (fn_type_1L_chr == "s3_checker") {
            s3_class_main_1L_chr <- stringr::str_replace(fn_name_1L_chr, 
                "is_", "") %>% `names<-`(fn_name_1L_chr)
        }
        if (!fn_type_1L_chr %in% c("set_class", "s3_valid_instance")) 
            short_class_desc_1L_chr <- get_from_lup_obj(abbreviations_lup, 
                match_var_nm_1L_chr = "short_name_chr", match_value_xx = s3_class_main_1L_chr, 
                target_var_nm_1L_chr = "long_name_chr", evaluate_lgl = F)
        if (fn_type_1L_chr == "set_class") {
            desc_start_1L_chr <- "Create a new S4 object of the class:"
            output_txt_1L_chr <- paste0("An S4 object of the ", 
                short_class_desc_1L_chr)
        }
        if (fn_type_1L_chr == "s3_valid_instance") {
            desc_start_1L_chr <- paste0("Create a new valid instance of the ", 
                short_class_desc_1L_chr)
            output_txt_1L_chr <- paste0("A validated instance of the ", 
                short_class_desc_1L_chr)
            x_param_desc_1L_chr <- paste0("A prototype for the ", 
                short_class_desc_1L_chr)
        }
        if (fn_type_1L_chr == "s3_unvalidated_instance") {
            desc_start_1L_chr <- paste0("Create a new unvalidated instance of the ", 
                short_class_desc_1L_chr)
            x_param_desc_1L_chr <- paste0("A prototype for the ", 
                short_class_desc_1L_chr)
            output_txt_1L_chr <- paste0("An unvalidated instance of the ", 
                short_class_desc_1L_chr)
        }
        if (fn_type_1L_chr == "s3_prototype") {
            desc_start_1L_chr <- paste0("Create a new prototype for the ", 
                short_class_desc_1L_chr)
            output_txt_1L_chr <- paste0("A prototype for ", short_class_desc_1L_chr)
        }
        if (fn_type_1L_chr == "s3_validator") {
            desc_start_1L_chr <- paste0("Validate an instance of the ", 
                short_class_desc_1L_chr)
            x_param_desc_1L_chr <- paste0("An unvalidated instance of the ", 
                short_class_desc_1L_chr)
            output_txt_1L_chr <- paste0("A prototpe for ", short_class_desc_1L_chr)
        }
        if (fn_type_1L_chr == "s3_checker") {
            desc_start_1L_chr <- paste0("Check whether an object is a valid instance of the ", 
                short_class_desc_1L_chr)
            x_param_desc_1L_chr <- "An object of any type"
            output_txt_1L_chr <- paste0("A logical value, TRUE if a valid instance of the ", 
                short_class_desc_1L_chr)
        }
    }
    if (fn_type_1L_chr %in% c("gen_get_slot", "meth_get_slot")) {
        desc_start_1L_chr <- "Get the value of the slot "
        output_txt_1L_chr <- "A XXX ..."
    }
    if (fn_type_1L_chr %in% c("gen_set_slot", "meth_set_slot")) {
        desc_start_1L_chr <- "Set the value of the slot "
        output_txt_1L_chr <- "NULL"
    }
    if (fn_type_1L_chr %in% c("fn", "gen_std_s3_mthd", "meth_std_s3_mthd", 
        "gen_std_s4_mthd", "meth_std_s4_mthd")) {
        desc_start_1L_chr <- fn_desc_1L_chr
        output_txt_1L_chr <- fn_out_type_1L_chr
        if (fn_type_1L_chr == "meth_std_s3_mthd") {
            x_param_desc_1L_chr <- paste0("An instance of ", 
                stringr::str_sub(fn_name_1L_chr, start = (1 + 
                  stringi::stri_locate_last_fixed(fn_name_1L_chr, 
                    ".")[1, 1])) %>% get_from_lup_obj(abbreviations_lup, 
                  match_var_nm_1L_chr = "short_name_chr", match_value_xx = ., 
                  target_var_nm_1L_chr = "long_name_chr", evaluate_lgl = F))
        }
        if (fn_type_1L_chr == "gen_std_s3_mthd") {
            x_param_desc_1L_chr <- "An object"
        }
    }
    if (is.null(args_ls)) {
        arg_desc_chr <- NULL
        if (any(!is.na(fn_args_chr)) & !is.null(object_type_lup)) {
            arg_desc_chr <- make_arg_desc(fn_args_chr, abbreviations_lup = abbreviations_lup, 
                object_type_lup = object_type_lup, dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, 
                dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, key_1L_chr = key_1L_chr, 
                server_1L_chr = server_1L_chr)
            if (!is.null(arg_desc_chr)) {
                names(arg_desc_chr) <- fn_args_chr
            }
        }
    }
    else {
        arg_desc_chr <- args_ls %>% purrr::flatten_chr() %>% 
            stats::setNames(names(args_ls))
    }
    if (!is.null(x_param_desc_1L_chr)) {
        x_param_desc_1L_chr <- x_param_desc_1L_chr %>% `names<-`("x")
        if (is.null(arg_desc_chr)) {
            arg_desc_chr <- x_param_desc_1L_chr
        }
        else {
            arg_desc_chr <- c(x_param_desc_1L_chr, arg_desc_chr[names(arg_desc_chr) != 
                "x"])
        }
    }
    new_fn_dmt_chr_ls <- list(desc_start_1L_chr = desc_start_1L_chr, 
        s3_class_main_1L_chr = s3_class_main_1L_chr, output_txt_1L_chr = output_txt_1L_chr, 
        fn_det_1L_chr = fn_det_1L_chr, arg_desc_chr = arg_desc_chr)
    return(new_fn_dmt_chr_ls)
}
#' Make object lookup table
#' @description make_obj_lup() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make object lookup table. The function returns Object (a tibble).
#' @param obj_lup_spine PARAM_DESCRIPTION, Default: make_obj_lup_spine()
#' @return Object (a tibble)
#' @rdname make_obj_lup
#' @export 
#' @importFrom dplyr bind_rows mutate filter select
#' @importFrom purrr map2_chr map_chr
#' @importFrom stringr str_sub str_replace
#' @importFrom tibble tibble
#' @keywords internal
make_obj_lup <- function (obj_lup_spine = make_obj_lup_spine()) 
{
    obj_tb <- obj_lup_spine
    obj_tb <- dplyr::bind_rows(obj_tb %>% dplyr::mutate(long_name_chr = purrr::map2_chr(long_name_chr, 
        atomic_element_lgl, ~ifelse(.y, paste0(.x, " vector"), 
            .x))), obj_tb %>% dplyr::filter(atomic_element_lgl) %>% 
        dplyr::mutate(short_name_chr = short_name_chr %>% purrr::map_chr(~paste0(stringr::str_sub(.x, 
            end = -5), "1L_", stringr::str_sub(.x, start = -4))), 
            long_name_chr = paste0(long_name_chr, " vector of length one")), 
        obj_tb %>% dplyr::filter(r3_can_extend_lgl) %>% dplyr::mutate(short_name_chr = paste0(short_name_chr, 
            purrr::map_chr(atomic_element_lgl, ~""), "_r3"), 
            long_name_chr = paste0("ready4 S3 extension of ", 
                long_name_chr, purrr::map_chr(atomic_element_lgl, 
                  ~ifelse(.x, " vector", ""))))) %>% dplyr::select(-3, 
        -4)
    obj_tb <- dplyr::bind_rows(obj_tb, obj_tb %>% dplyr::mutate(short_name_chr = paste0(short_name_chr, 
        "_ls"), long_name_chr = paste0("list of ", long_name_chr)) %>% 
        dplyr::bind_rows(obj_tb %>% dplyr::mutate(short_name_chr = paste0(short_name_chr, 
            "_r4"), long_name_chr = paste0("ready4 S4 collection of ", 
            long_name_chr))) %>% dplyr::mutate(long_name_chr = purrr::map_chr(long_name_chr, 
        ~ifelse(endsWith(.x, "vector of length one"), stringr::str_replace(.x, 
            "vector", "vectors"), ifelse(endsWith(.x, "matrix"), 
            stringr::str_replace(.x, "matrix", "matrices"), paste0(.x, 
                "s"))))), tibble::tibble(short_name_chr = "xx", 
        long_name_chr = "output object of multiple potential types"))
    obj_tb <- obj_tb %>% dplyr::mutate(plural_lgl = F)
    return(obj_tb)
}
#' Make object lookup table spine
#' @description make_obj_lup_spine() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make object lookup table spine. The function is called for its side effects and does not return a value.
#' @param seed_obj_type_lup Seed object type (a lookup table), Default: get_rds_from_dv("seed_obj_type_lup")
#' @param new_entries_tb New entries (a tibble), Default: NULL
#' @return NA ()
#' @rdname make_obj_lup_spine
#' @export 
#' @importFrom tibble tibble
#' @importFrom dplyr arrange
#' @keywords internal
make_obj_lup_spine <- function (seed_obj_type_lup = get_rds_from_dv("seed_obj_type_lup"), 
    new_entries_tb = NULL) 
{
    if (is.null(seed_obj_type_lup)) {
        seed_obj_type_lup <- tibble::tibble(short_name_chr = c("df", 
            "env", "fn", "ls", "plt", "r3", "r4", "s3", "s4", 
            "sf", "tb", "arr", "chr", "dbl", "dtm", "fct", "int", 
            "lgl", "lup", "mat", "mdl", "prsn", "rgx"), long_name_chr = c("data.frame", 
            "environment", "function", "list", "plot", "ready4 S3", 
            "ready4 S4", "S3", "S4", "simple features object", 
            "tibble", "array", "character", "double", "date", 
            "factor", "integer", "logical", "lookup table", "matrix", 
            "model", "person", "regular expression"), atomic_element_lgl = c(rep(F, 
            12), rep(T, 6), rep(F, 4), T), r3_can_extend_lgl = c(T, 
            F, F, T, F, rep(F, 4), rep(T, 14))) %>% dplyr::arrange(short_name_chr)
    }
    obj_lup_spine <- seed_obj_type_lup
    if (!is.null(new_entries_tb)) {
        obj_lup_spine <- add_lups(obj_lup_spine, new_lup = new_entries_tb, 
            key_var_nm_1L_chr = "short_name_chr")
    }
    return(obj_lup_spine)
}
#' Make package description list
#' @description make_pkg_desc_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make package description list. The function returns Package description (a list).
#' @param pkg_nm_1L_chr Package name (a character vector of length one), Default: get_dev_pkg_nm()
#' @param pkg_title_1L_chr Package title (a character vector of length one)
#' @param pkg_desc_1L_chr Package description (a character vector of length one)
#' @param authors_prsn Authors (a person)
#' @param urls_chr Urls (a character vector)
#' @return Package description (a list)
#' @rdname make_pkg_desc_ls
#' @export 
#' @importFrom purrr map_lgl
#' @importFrom stringr str_detect str_sub str_locate str_trim
#' @importFrom tools toTitleCase
#' @importFrom usethis use_gpl3_license
make_pkg_desc_ls <- function (pkg_nm_1L_chr = get_dev_pkg_nm(), pkg_title_1L_chr, 
    pkg_desc_1L_chr, authors_prsn, urls_chr) 
{
    cpyr_hldr_1L_chr <- authors_prsn[authors_prsn %>% as.character() %>% 
        purrr::map_lgl(~stringr::str_detect(.x, "\\[cph") | stringr::str_detect(.x, 
            " cph, "))] %>% as.character()
    cpyr_hldr_1L_chr <- cpyr_hldr_1L_chr %>% stringr::str_sub(end = -1 + 
        (cpyr_hldr_1L_chr %>% stringr::str_locate("\\["))[1, 
            1] %>% unname()) %>% stringr::str_trim()
    pkg_desc_ls <- list(Package = pkg_nm_1L_chr, Title = pkg_title_1L_chr %>% 
        tools::toTitleCase(), Description = pkg_desc_1L_chr, 
        `Authors@R` = authors_prsn, License = usethis::use_gpl3_license(), 
        URL = paste0(urls_chr, collapse = ", "))
    return(pkg_desc_ls)
}
#' Make package dataset list
#' @description make_pkg_ds_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make package dataset list. The function returns Package dataset (a list).
#' @param db_df Database (a data.frame)
#' @param db_1L_chr Database (a character vector of length one)
#' @param title_1L_chr Title (a character vector of length one)
#' @param desc_1L_chr Description (a character vector of length one)
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param format_1L_chr Format (a character vector of length one), Default: 'A tibble'
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param simple_lup_1L_lgl Simple lookup table (a logical vector of length one), Default: F
#' @param url_1L_chr Url (a character vector of length one), Default: 'NA'
#' @param vars_ls Variables (a list), Default: NULL
#' @return Package dataset (a list)
#' @rdname make_pkg_ds_ls
#' @export 

make_pkg_ds_ls <- function (db_df, db_1L_chr, title_1L_chr, desc_1L_chr, abbreviations_lup = NULL, 
    format_1L_chr = "A tibble", object_type_lup = NULL, simple_lup_1L_lgl = F, 
    url_1L_chr = NA_character_, vars_ls = NULL) 
{
    pkg_ds_ls <- list(db_df = db_df, db_1L_chr = db_1L_chr, title_1L_chr = title_1L_chr, 
        desc_1L_chr = desc_1L_chr, abbreviations_lup = abbreviations_lup, 
        format_1L_chr = format_1L_chr, object_type_lup = object_type_lup, 
        simple_lup_1L_lgl = simple_lup_1L_lgl, url_1L_chr = url_1L_chr, 
        vars_ls = vars_ls)
    return(pkg_ds_ls)
}
#' Make package setup list
#' @description make_pkg_setup_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make package setup list. The function returns Package setup (a list).
#' @param pkg_desc_ls Package description (a list)
#' @param copyright_holders_chr Copyright holders (a character vector)
#' @param pkg_dmt_dv_dss_chr Package documentation dataverse datasets (a character vector)
#' @param add_gh_site_1L_lgl Add github site (a logical vector of length one), Default: T
#' @param addl_badges_ls Additional badges (a list), Default: NULL
#' @param addl_pkgs_ls Additional packages (a list), Default: make_addl_pkgs_ls()
#' @param badges_lup Badges (a lookup table), Default: NULL
#' @param build_ignore_ls Build ignore (a list), Default: make_build_ignore_ls()
#' @param check_type_1L_chr Check type (a character vector of length one), Default: 'ready4'
#' @param classify_1L_lgl Classify (a logical vector of length one), Default: T
#' @param cls_fn_ls Class (a list of functions), Default: NULL
#' @param delete_r_dir_cnts_1L_lgl Delete r directory contents (a logical vector of length one), Default: T
#' @param dev_pkg_nm_1L_chr Development package name (a character vector of length one), Default: get_dev_pkg_nm(getwd())
#' @param dev_pkgs_chr Development packages (a character vector), Default: 'NA'
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: NULL
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'NA'
#' @param lifecycle_stage_1L_chr Lifecycle stage (a character vector of length one), Default: 'experimental'
#' @param inc_pkg_meta_data_1L_lgl Include package meta data (a logical vector of length one), Default: F
#' @param incr_ver_1L_lgl Increment version (a logical vector of length one), Default: F
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param on_cran_1L_lgl On cran (a logical vector of length one), Default: F
#' @param path_to_dmt_dir_1L_chr Path to documentation directory (a character vector of length one), Default: normalizePath("../../../../../Documentation/Code")
#' @param path_to_pkg_logo_1L_chr Path to package logo (a character vector of length one), Default: 'NA'
#' @param path_to_pkg_rt_1L_chr Path to package root (a character vector of length one), Default: getwd()
#' @param pkg_ds_ls_ls Package dataset (a list of lists), Default: NULL
#' @param ready4_type_1L_chr Ready4 type (a character vector of length one)
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @param user_manual_fns_chr User manual functions (a character vector), Default: 'NA'
#' @return Package setup (a list)
#' @rdname make_pkg_setup_ls
#' @export 
#' @importFrom utils data
#' @importFrom purrr pluck discard
#' @importFrom stringr str_trim str_remove
make_pkg_setup_ls <- function (pkg_desc_ls, copyright_holders_chr, pkg_dmt_dv_dss_chr, 
    add_gh_site_1L_lgl = T, addl_badges_ls = NULL, addl_pkgs_ls = make_addl_pkgs_ls(), 
    badges_lup = NULL, build_ignore_ls = make_build_ignore_ls(), 
    check_type_1L_chr = "ready4", classify_1L_lgl = T, cls_fn_ls = NULL, 
    delete_r_dir_cnts_1L_lgl = T, dev_pkg_nm_1L_chr = get_dev_pkg_nm(getwd()), 
    dev_pkgs_chr = NA_character_, dv_url_pfx_1L_chr = NULL, gh_repo_1L_chr = NA_character_, 
    lifecycle_stage_1L_chr = "experimental", inc_pkg_meta_data_1L_lgl = F, 
    incr_ver_1L_lgl = F, key_1L_chr = NULL, on_cran_1L_lgl = F, 
    path_to_dmt_dir_1L_chr = normalizePath("../../../../../Documentation/Code"), 
    path_to_pkg_logo_1L_chr = NA_character_, path_to_pkg_rt_1L_chr = getwd(), 
    pkg_ds_ls_ls = NULL, ready4_type_1L_chr, server_1L_chr = Sys.getenv("DATAVERSE_SERVER"), 
    user_manual_fns_chr = NA_character_) 
{
    if (length(pkg_dmt_dv_dss_chr) < 2) {
        pkg_dmt_dv_dss_chr <- rep(pkg_dmt_dv_dss_chr, 2)
    }
    if (!is.na(ready4_type_1L_chr)) {
        append_ls <- list(ready4 = ready4_type_1L_chr)
    }
    else {
        append_ls <- NULL
    }
    if (is.null(badges_lup)) {
        utils::data("badges_lup", package = "ready4fun", envir = environment())
    }
    if (is.na(gh_repo_1L_chr)) 
        gh_repo_1L_chr <- pkg_desc_ls$URL %>% strsplit(",") %>% 
            unlist() %>% purrr::pluck(2) %>% stringr::str_trim() %>% 
            stringr::str_remove("https://github.com/")
    addl_badges_ls <- append(addl_badges_ls, append_ls) %>% purrr::discard(is.null)
    if (length(addl_badges_ls) == 0) 
        addl_badges_ls <- NULL
    pkg_setup_ls <- list(initial_ls = list(pkg_desc_ls = pkg_desc_ls, 
        copyright_holders_chr = copyright_holders_chr, gh_repo_1L_chr = gh_repo_1L_chr, 
        add_gh_site_1L_lgl = add_gh_site_1L_lgl, addl_badges_ls = addl_badges_ls, 
        badges_lup = badges_lup, check_type_1L_chr = check_type_1L_chr, 
        delete_r_dir_cnts_1L_lgl = delete_r_dir_cnts_1L_lgl, 
        dev_pkg_nm_1L_chr = dev_pkg_nm_1L_chr, lifecycle_stage_1L_chr = lifecycle_stage_1L_chr, 
        incr_ver_1L_lgl = incr_ver_1L_lgl, on_cran_1L_lgl = on_cran_1L_lgl, 
        path_to_pkg_logo_1L_chr = path_to_pkg_logo_1L_chr, path_to_pkg_rt_1L_chr = path_to_pkg_rt_1L_chr), 
        subsequent_ls = list(abbreviations_lup = get_rds_from_dv("abbreviations_lup", 
            dv_ds_nm_1L_chr = pkg_dmt_dv_dss_chr[2], dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr), 
            addl_pkgs_ls = addl_pkgs_ls, build_ignore_ls = build_ignore_ls, 
            cls_fn_ls = cls_fn_ls, inc_pkg_meta_data_1L_lgl = inc_pkg_meta_data_1L_lgl, 
            path_to_dmt_dir_1L_chr = path_to_dmt_dir_1L_chr, 
            pkg_ds_ls_ls = pkg_ds_ls_ls, dev_pkgs_chr = dev_pkgs_chr, 
            dv_ds_nm_1L_chr = pkg_dmt_dv_dss_chr[2], dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            fn_types_lup = get_rds_from_dv("fn_types_lup", dv_ds_nm_1L_chr = pkg_dmt_dv_dss_chr[2], 
                dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, key_1L_chr = key_1L_chr, 
                server_1L_chr = server_1L_chr), key_1L_chr = key_1L_chr, 
            object_type_lup = get_rds_from_dv("object_type_lup", 
                dv_ds_nm_1L_chr = pkg_dmt_dv_dss_chr[2], dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
                key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr), 
            pkg_dmt_dv_dss_chr = pkg_dmt_dv_dss_chr, seed_obj_type_lup = get_rds_from_dv("seed_obj_type_lup", 
                dv_ds_nm_1L_chr = pkg_dmt_dv_dss_chr[2], dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
                key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr), 
            server_1L_chr = server_1L_chr, treat_as_words_chr = get_rds_from_dv("treat_as_words_chr", 
                dv_ds_nm_1L_chr = pkg_dmt_dv_dss_chr[2], dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
                key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr), 
            user_manual_fns_chr = user_manual_fns_chr))
    if (classify_1L_lgl) {
        pkg_setup_ls$initial_ls$badges_lup <- pkg_setup_ls$initial_ls$badges_lup %>% 
            ready4fun_badges()
        pkg_setup_ls$initial_ls$pkg_desc_ls <- pkg_setup_ls$initial_ls$pkg_desc_ls %>% 
            ready4fun_pkg_desc()
        pkg_setup_ls$initial_ls <- pkg_setup_ls$initial_ls %>% 
            ready4fun_pkg_setup_one()
        pkg_setup_ls$subsequent_ls <- pkg_setup_ls$subsequent_ls %>% 
            ready4fun_pkg_setup_two()
        pkg_setup_ls <- pkg_setup_ls %>% ready4fun_pkg_setup()
    }
    return(pkg_setup_ls)
}
#' Make prompt
#' @description make_prompt() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make prompt. The function returns Response (a character vector of length one).
#' @param prompt_1L_chr Prompt (a character vector of length one)
#' @param options_chr Options (a character vector), Default: NULL
#' @param force_from_opts_1L_chr Force from opts (a character vector of length one), Default: F
#' @return Response (a character vector of length one)
#' @rdname make_prompt
#' @export 

#' @keywords internal
make_prompt <- function (prompt_1L_chr, options_chr = NULL, force_from_opts_1L_chr = F) 
{
    acknowledgement_1L_chr <- "This function is based on: https://debruine.github.io/posts/interactive-test/"
    con_conn <- getOption("prompt_opts.con", stdin())
    options_1L_chr <- paste(options_chr, collapse = "|")
    prompt_with_options_1L_chr <- paste0(prompt_1L_chr, " [", 
        options_1L_chr, "]\n")
    cat(prompt_with_options_1L_chr)
    response_1L_chr <- readLines(con = con_conn, n = 1)
    if (!is.null(options_chr) & !response_1L_chr %in% options_chr & 
        force_from_opts_1L_chr) {
        response_1L_chr <- make_prompt(prompt_1L_chr, options_chr, 
            force_from_opts_1L_chr = T)
    }
    return(response_1L_chr)
}
#' Make return object description
#' @description make_ret_obj_desc() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make return object description. The function returns Return object description (a character vector of length one).
#' @param fn Function (a function)
#' @param abbreviations_lup Abbreviations (a lookup table)
#' @param starts_sentence_1L_lgl Starts sentence (a logical vector of length one), Default: T
#' @return Return object description (a character vector of length one)
#' @rdname make_ret_obj_desc
#' @export 

#' @keywords internal
make_ret_obj_desc <- function (fn, abbreviations_lup, starts_sentence_1L_lgl = T) 
{
    ret_obj_nm_1L_chr <- get_return_obj_nm(fn)
    if (is.na(ret_obj_nm_1L_chr)) {
        ret_obj_desc_1L_chr <- "NULL"
    }
    else {
        obj_type_1L_chr <- get_arg_obj_type(ret_obj_nm_1L_chr, 
            object_type_lup = abbreviations_lup)
        ret_obj_desc_1L_chr <- paste0(ret_obj_nm_1L_chr %>% make_arg_title(match_chr = obj_type_1L_chr, 
            abbreviations_lup = abbreviations_lup, object_type_lup = abbreviations_lup) %>% 
            add_indef_artl_to_item(abbreviations_lup = abbreviations_lup) %>% 
            ifelse(!starts_sentence_1L_lgl, tolower(.), .), " (", 
            obj_type_1L_chr %>% add_indef_artl_to_item(abbreviations_lup = abbreviations_lup), 
            ")")
    }
    return(ret_obj_desc_1L_chr)
}
#' Make short long names vector
#' @description make_short_long_nms_vec() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make short long names vector. The function returns Short and long vector (a character vector).
#' @param long_vecs_chr Long vectors (a character vector), Default: character(0)
#' @param short_vecs_chr Short vectors (a character vector), Default: character(0)
#' @return Short and long vector (a character vector)
#' @rdname make_short_long_nms_vec
#' @export 

#' @keywords internal
make_short_long_nms_vec <- function (long_vecs_chr = character(0), short_vecs_chr = character(0)) 
{
    short_vecs_chr <- paste0(short_vecs_chr, "_vec")
    if (short_vecs_chr[1] == "_vec") {
        short_vecs_chr <- character(0)
    }
    short_and_long_vec_chr <- c(long_vecs_chr, short_vecs_chr)
    return(short_and_long_vec_chr)
}
#' Make standard function documentation spine
#' @description make_std_fn_dmt_spine() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make standard function documentation spine. The function returns Standard function documentation spine (a list of character vectors).
#' @param fn_name_1L_chr Function name (a character vector of length one)
#' @param fn_type_1L_chr Function type (a character vector of length one)
#' @param fn_title_1L_chr Function title (a character vector of length one)
#' @param fn Function (a function)
#' @param details_1L_chr Details (a character vector of length one), Default: 'NA'
#' @param example_1L_lgl Example (a logical vector of length one), Default: F
#' @param export_1L_lgl Export (a logical vector of length one), Default: T
#' @param class_name_1L_chr Class name (a character vector of length one), Default: ''
#' @param exclude_if_match_chr Exclude if match (a character vector)
#' @return Standard function documentation spine (a list of character vectors)
#' @rdname make_std_fn_dmt_spine
#' @export 
#' @importFrom sinew makeOxygen
#' @importFrom stringr str_replace
#' @importFrom purrr discard
#' @keywords internal
make_std_fn_dmt_spine <- function (fn_name_1L_chr, fn_type_1L_chr, fn_title_1L_chr, fn, 
    details_1L_chr = NA_character_, example_1L_lgl = F, export_1L_lgl = T, 
    class_name_1L_chr = "", exclude_if_match_chr) 
{
    assert_inp_does_not_match_terms(input_chr = fn_type_1L_chr, 
        exclude_if_match_chr = exclude_if_match_chr)
    if (!is.na(details_1L_chr)) {
        if (details_1L_chr == "DETAILS") 
            details_1L_chr <- NA_character_
    }
    if (startsWith(fn_type_1L_chr, "gen_")) {
        fn_tags_1L_chr <- sinew::makeOxygen(fn, print = FALSE, 
            add_fields = c("export")) %>% stringr::str_replace("#' @return OUTPUT_DESCRIPTION\n", 
            "")
    }
    else {
        fn_tags_1L_chr <- sinew::makeOxygen(fn, print = FALSE, 
            add_fields = c(ifelse(!is.na(details_1L_chr), "details", 
                NA_character_), ifelse(example_1L_lgl, "examples", 
                NA_character_), "rdname", ifelse(export_1L_lgl, 
                "export", NA_character_)) %>% purrr::discard(is.na)) %>% 
            stringr::str_replace("#' @title FUNCTION_TITLE\n", 
                "")
    }
    fn_tags_1L_chr <- fn_tags_1L_chr %>% stringr::str_replace("@title ", 
        "@name ") %>% stringr::str_replace("@rdname fn", paste0("@rdname ", 
        fn_name_1L_chr))
    fn_tags_1L_chr <- paste0("#' ", ifelse((startsWith(fn_type_1L_chr, 
        "gen_") | fn_type_1L_chr %in% c("fn", "meth_std_s3_mthd") | 
        startsWith(fn_type_1L_chr, "s3_")), fn_title_1L_chr, 
        ""), "\n", fn_tags_1L_chr)
    if (!fn_type_1L_chr %>% startsWith("s3") & !fn_type_1L_chr %in% 
        c("fn", "gen_std_s3_mthd", "meth_std_s3_mthd", "gen_std_s4_mthd", 
            "meth_std_s4_mthd")) 
        fn_tags_1L_chr <- update_fn_dmt_with_slots(fn_name_1L_chr = fn_name_1L_chr, 
            fn_dmt_1L_chr = fn_tags_1L_chr)
    std_fn_dmt_spine_chr_ls <- list(fn_tags_1L_chr = fn_tags_1L_chr, 
        ref_slot_1L_chr = fn_name_1L_chr)
    return(std_fn_dmt_spine_chr_ls)
}
#' Make undocumented functions directory character vector
#' @description make_undmtd_fns_dir_chr() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make undocumented functions directory character vector. The function returns Undocumented functions directory (a character vector).
#' @param path_1L_chr Path (a character vector of length one), Default: 'data-raw'
#' @param drop_empty_1L_lgl Drop empty (a logical vector of length one), Default: F
#' @return Undocumented functions directory (a character vector)
#' @rdname make_undmtd_fns_dir_chr
#' @export 
#' @importFrom purrr map_lgl
#' @keywords internal
make_undmtd_fns_dir_chr <- function (path_1L_chr = "data-raw", drop_empty_1L_lgl = F) 
{
    undocumented_fns_dir_chr <- paste0(path_1L_chr, "/", make_fn_types())
    if (drop_empty_1L_lgl) 
        undocumented_fns_dir_chr <- undocumented_fns_dir_chr[undocumented_fns_dir_chr %>% 
            purrr::map_lgl(~{
                exists_1L_lgl <- dir.exists(.x)
                ifelse(exists_1L_lgl, !identical(list.files(.x), 
                  character(0)), exists_1L_lgl)
            })]
    return(undocumented_fns_dir_chr)
}
