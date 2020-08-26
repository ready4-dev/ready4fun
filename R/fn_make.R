#' Make abbreviation lookup table
#' @description make_abbr_lup_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make an abbreviation lookup table.NA
#' @param short_name_chr_vec Short name (a character vector), Default: 'NA'
#' @param long_name_chr_vec Long name (a character vector), Default: 'NA'
#' @param no_plural_chr_vec No plural (a character vector), Default: 'NA'
#' @param custom_plural_ls Custom plural (a list), Default: NULL
#' @param overwrite_lgl Overwrite (a logical vector of length 1), Default: T
#' @param seed_lup Seed (a lookup table), Default: NULL
#' @param url_chr Url (a character vector of length 1)
#' @param pkg_nm_chr Package name (a character vector of length 1)
#' @return NULL
#' @rdname make_abbr_lup_tb
#' @export 

make_abbr_lup_tb <- function (short_name_chr_vec = NA_character_, long_name_chr_vec = NA_character_, 
    no_plural_chr_vec = NA_character_, custom_plural_ls = NULL, 
    overwrite_lgl = T, seed_lup = NULL, url_chr, pkg_nm_chr) 
{
    if (is.null(seed_lup)) {
        data("object_type_lup", package = "ready4fun", envir = environment())
        seed_lup <- object_type_lup
    }
    update_abbr_lup_tb(seed_lup, short_name_chr_vec = short_name_chr_vec, 
        long_name_chr_vec = long_name_chr_vec, no_plural_chr_vec = no_plural_chr_vec, 
        custom_plural_ls = custom_plural_ls) %>% write_and_doc_ds_R(db = ., 
        overwrite_lgl = overwrite_lgl, db_chr = "abbreviations_lup", 
        title_chr = "Common abbreviations lookup table", desc_chr = paste0("A lookup table for abbreviations commonly used in object names in the ", 
            pkg_nm_chr, "package."), format_chr = "A tibble", 
        url_chr = url_chr, abbreviations_lup = .)
}
#' Make all functions dmt
#' @description make_all_fns_dmt_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make all a functions dmt.The function returns all functions dmt (a tibble).
#' @param paths_ls Paths (a list), Default: make_fns_chr_ls()
#' @param undocumented_fns_dir_chr Undocumented functions directory (a character vector of length 1), Default: make_undmtd_fns_dir_chr()
#' @param custom_dmt_ls Custom dmt (a list), Default: list(details_ls = NULL, export_ls = list(force_true_chr_vec = NA_character_, 
#'    force_false_chr_vec = NA_character_), args_ls_ls = NULL)
#' @param fn_type_lup_tb Function type lookup table (a tibble)
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @return All functions dmt (a tibble)
#' @rdname make_all_fns_dmt_tb
#' @export 
#' @importFrom purrr pmap_dfr
#' @importFrom dplyr filter
make_all_fns_dmt_tb <- function (paths_ls = make_fns_chr_ls(), undocumented_fns_dir_chr = make_undmtd_fns_dir_chr(), 
    custom_dmt_ls = list(details_ls = NULL, export_ls = list(force_true_chr_vec = NA_character_, 
        force_false_chr_vec = NA_character_), args_ls_ls = NULL), 
    fn_type_lup_tb, abbreviations_lup = NULL) 
{
    if (is.null(abbreviations_lup)) 
        data("abbreviations_lup", package = "ready4fun", envir = environment())
    all_fns_dmt_tb <- purrr::pmap_dfr(list(paths_ls, undocumented_fns_dir_chr, 
        names(paths_ls)), ~{
        if (..3 == "fns") 
            tb <- fn_type_lup_tb %>% dplyr::filter(!is_generic_lgl & 
                !is_method_lgl)
        if (..3 == "gnrcs") 
            tb <- fn_type_lup_tb %>% dplyr::filter(is_generic_lgl)
        if (..3 == "mthds") 
            tb <- fn_type_lup_tb %>% dplyr::filter(is_method_lgl)
        make_fn_dmt_tbl_tb(..1, fns_dir_chr = ..2, custom_dmt_ls = custom_dmt_ls, 
            append_lgl = T, fn_type_lup_tb = tb, abbreviations_lup = abbreviations_lup)
    })
    return(all_fns_dmt_tb)
}
#' Make and document function type
#' @description make_and_doc_fn_type_R() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make and a document function type R.NA
#' @param fn_type_lup_tb Function type lookup table (a tibble), Default: make_fn_type_lup_tb()
#' @param overwrite_lgl Overwrite (a logical vector of length 1), Default: T
#' @param pkg_nm_chr Package name (a character vector of length 1)
#' @param url_chr Url (a character vector of length 1), Default: url_chr
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @return NULL
#' @rdname make_and_doc_fn_type_R
#' @export 

make_and_doc_fn_type_R <- function (fn_type_lup_tb = make_fn_type_lup_tb(), overwrite_lgl = T, 
    pkg_nm_chr, url_chr = url_chr, abbreviations_lup = NULL) 
{
    if (is.null(abbreviations_lup)) 
        data("abbreviations_lup", package = "ready4fun", envir = environment())
    fn_type_lup_tb %>% write_and_doc_ds_R(overwrite_lgl = overwrite_lgl, 
        db_chr = "fn_type_lup_tb", title_chr = "Function type lookup table", 
        desc_chr = paste0("A lookup table to find descriptions for different types of functions used within the ", 
            pkg_nm_chr, " package suite."), format_chr = "A tibble", 
        url_chr = url_chr, abbreviations_lup = abbreviations_lup)
}
#' Make and document generics tibble
#' @description make_and_doc_generics_tb_R() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make and a document generics a tibble R.NA
#' @param generic_nm_chr Generic name (a character vector of length 1)
#' @param description_chr Description (a character vector of length 1)
#' @param overwrite_lgl Overwrite (a logical vector of length 1), Default: T
#' @param pkg_nm_chr Package name (a character vector of length 1)
#' @param url_chr Url (a character vector of length 1), Default: 'NA'
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @return NULL
#' @rdname make_and_doc_generics_tb_R
#' @export 
#' @importFrom tibble tibble
#' @importFrom dplyr arrange
#' @keywords internal
make_and_doc_generics_tb_R <- function (generic_nm_chr, description_chr, overwrite_lgl = T, 
    pkg_nm_chr, url_chr = NA_character_, abbreviations_lup = NULL) 
{
    if (is.null(abbreviations_lup)) 
        data("abbreviations_lup", package = "ready4fun", envir = environment())
    tibble::tibble(fn_type_nm_chr = generic_nm_chr, fn_type_desc_chr = description_chr, 
        first_arg_desc_chr = NA_character_, second_arg_desc_chr = NA_character_, 
        is_generic_lgl = T) %>% dplyr::arrange(fn_type_nm_chr) %>% 
        write_and_doc_ds_R(overwrite_lgl = overwrite_lgl, db_chr = "generics_lup_tb", 
            title_chr = "Generics lookup table", desc_chr = paste0("A lookup table to find descriptions of generics exported with the ", 
                pkg_nm_chr, " package suite."), format_chr = "A tibble", 
            url_chr = url_chr, abbreviations_lup = abbreviations_lup)
}
#' Make argument description
#' @description make_arg_desc_chr_vec() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make an argument description.The function returns an argument description (a character vector).
#' @param fn_args_chr_vec Function arguments (a character vector)
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @return Argument description (a character vector)
#' @rdname make_arg_desc_chr_vec
#' @export 

#' @keywords internal
make_arg_desc_chr_vec <- function (fn_args_chr_vec, object_type_lup = NULL, abbreviations_lup = NULL) 
{
    if (is.null(abbreviations_lup)) 
        data("abbreviations_lup", package = "ready4fun", envir = environment())
    if (is.null(object_type_lup)) 
        data("object_type_lup", package = "ready4fun", envir = environment())
    arg_desc_chr_vec <- make_arg_type_chr_vec(fn_args_chr_vec, 
        object_type_lup = object_type_lup, abbreviations_lup = abbreviations_lup, 
        fn = make_arg_desc_spine_chr)
    return(arg_desc_chr_vec)
}
#' Make argument description
#' @description make_arg_desc_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make an argument description.NA
#' @param fn_nms_chr_vec Function names (a character vector)
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @return NULL
#' @rdname make_arg_desc_ls
#' @export 
#' @importFrom purrr map
#' @importFrom stats setNames
#' @keywords internal
make_arg_desc_ls <- function (fn_nms_chr_vec, abbreviations_lup = NULL, object_type_lup = NULL) 
{
    if (is.null(abbreviations_lup)) 
        data("abbreviations_lup", package = "ready4fun", envir = environment())
    if (is.null(object_type_lup)) 
        data("object_type_lup", package = "ready4fun", envir = environment())
    purrr::map(fn_nms_chr_vec, ~{
        eval(parse(text = paste0("fn <- ", .x)))
        get_fn_args_chr_vec(fn) %>% make_arg_desc_chr_vec(abbreviations_lup = abbreviations_lup, 
            object_type_lup = object_type_lup) %>% stats::setNames(get_fn_args_chr_vec(fn))
    })
}
#' Make argument description spine
#' @description make_arg_desc_spine_chr() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make an argument description spine.The function returns an argument description spine (a character vector of length 1).
#' @param argument_nm_chr Argument name (a character vector of length 1)
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @return Argument description spine (a character vector of length 1)
#' @rdname make_arg_desc_spine_chr
#' @export 

#' @keywords internal
make_arg_desc_spine_chr <- function (argument_nm_chr, object_type_lup = NULL, abbreviations_lup = NULL) 
{
    if (is.null(object_type_lup)) 
        data("object_type_lup", package = "ready4fun", envir = environment())
    if (is.null(abbreviations_lup)) 
        data("abbreviations_lup", package = "ready4fun", envir = environment())
    if (is.na(argument_nm_chr)) {
        match_chr <- character(0)
    }
    else {
        nchar_int <- nchar(object_type_lup$short_name_chr)
        match_chr <- object_type_lup$long_name_chr[endsWith(argument_nm_chr, 
            paste0(ifelse(nchar(argument_nm_chr) == nchar_int, 
                "", "_"), object_type_lup$short_name_chr))]
    }
    arg_desc_spine_chr <- ifelse(identical(match_chr, character(0)), 
        NA_character_, paste0(argument_nm_chr %>% make_arg_title_chr_vec(match_chr_vec = match_chr, 
            abbreviations_lup = abbreviations_lup), " (", match_chr %>% 
            update_first_word_case_chr() %>% add_indefartls_to_phrases_chr_vec(abbreviations_lup = abbreviations_lup, 
            ignore_phrs_not_in_lup_lgl = F), ")"))
    return(arg_desc_spine_chr)
}
#' Make argument title
#' @description make_arg_title_chr_vec() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make an argument title.The function returns title (a character vector).
#' @param args_chr_vec Arguments (a character vector)
#' @param match_chr_vec Match (a character vector)
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @return Title (a character vector)
#' @rdname make_arg_title_chr_vec
#' @export 
#' @importFrom purrr map_chr map2_chr
#' @importFrom stringi stri_replace_last_fixed
#' @importFrom stringr str_replace_all
#' @importFrom Hmisc capitalize
#' @keywords internal
make_arg_title_chr_vec <- function (args_chr_vec, match_chr_vec, object_type_lup = NULL, 
    abbreviations_lup = NULL) 
{
    if (is.null(object_type_lup)) 
        data("object_type_lup", package = "ready4fun", envir = environment())
    if (is.null(abbreviations_lup)) 
        data("abbreviations_lup", package = "ready4fun", envir = environment())
    suffices_chr_vec <- match_chr_vec %>% purrr::map_chr(~{
        ifelse(.x == "NO MATCH", "", get_from_lup_obj(object_type_lup, 
            match_value_xx = .x, match_var_nm_chr = "long_name_chr", 
            target_var_nm_chr = "short_name_chr", evaluate_lgl = F))
    })
    title_chr_vec <- purrr::map2_chr(args_chr_vec, suffices_chr_vec, 
        ~ifelse(.y == "", .x, stringi::stri_replace_last_fixed(.x, 
            paste0("_", .y), ""))) %>% stringr::str_replace_all("_", 
        " ") %>% purrr::map_chr(~replace_abbr_chr(.x, abbreviations_lup = abbreviations_lup) %>% 
        stringi::stri_replace_last_fixed(" R", "")) %>% Hmisc::capitalize()
    return(title_chr_vec)
}
#' Make argument type abbreviation
#' @description make_arg_type_abbr_chr_vec() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make an argument type an abbreviation.The function returns an argument type abbreviation (a character vector).
#' @param fn_args_chr_vec Function arguments (a character vector)
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @return Argument type abbreviation (a character vector)
#' @rdname make_arg_type_abbr_chr_vec
#' @export 

#' @keywords internal
make_arg_type_abbr_chr_vec <- function (fn_args_chr_vec, object_type_lup = NULL, abbreviations_lup = NULL) 
{
    if (is.null(abbreviations_lup)) 
        data("abbreviations_lup", package = "ready4fun", envir = environment())
    if (is.null(object_type_lup)) 
        data("object_type_lup", package = "ready4fun", envir = environment())
    arg_type_abbr_chr_vec <- make_arg_type_chr_vec(fn_args_chr_vec, 
        object_type_lup = object_type_lup, fn = make_arg_type_abbr_spine_chr, 
        abbreviations_lup = abbreviations_lup)
    return(arg_type_abbr_chr_vec)
}
#' Make argument type abbreviation spine
#' @description make_arg_type_abbr_spine_chr() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make an argument type an abbreviation spine.The function returns an argument type abbreviation spine (a character vector of length 1).
#' @param argument_nm_chr Argument name (a character vector of length 1)
#' @param lup_tb Lookup table (a tibble)
#' @return Argument type abbreviation spine (a character vector of length 1)
#' @rdname make_arg_type_abbr_spine_chr
#' @export 

#' @keywords internal
make_arg_type_abbr_spine_chr <- function (argument_nm_chr, lup_tb) 
{
    arg_type_chr <- lup_tb$short_name_chr[endsWith(argument_nm_chr, 
        lup_tb$short_name_chr)]
    arg_type_abbr_spine_chr <- ifelse(identical(character(0), 
        arg_type_chr), NA_character_, arg_type_chr)
    return(arg_type_abbr_spine_chr)
}
#' Make argument type
#' @description make_arg_type_chr_vec() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make an argument type.The function returns an argument description (a character vector).
#' @param fn_args_chr_vec Function arguments (a character vector)
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param fn Function (a function)
#' @return Argument description (a character vector)
#' @rdname make_arg_type_chr_vec
#' @export 
#' @importFrom purrr map_chr discard pluck
#' @importFrom rlang exec
#' @keywords internal
make_arg_type_chr_vec <- function (fn_args_chr_vec, object_type_lup = NULL, abbreviations_lup = NULL, 
    fn) 
{
    if (is.null(object_type_lup)) 
        data("object_type_lup", package = "ready4fun", envir = environment())
    if (is.null(abbreviations_lup)) 
        data("abbreviations_lup", package = "ready4fun", envir = environment())
    lup_ls <- make_arg_type_lup_ls(object_type_lup)
    append_lgl <- "abbreviations_lup" %in% get_fn_args_chr_vec(fn)
    arg_desc_chr_vec <- fn_args_chr_vec %>% purrr::map_chr(~{
        argument_nm_chr <- .x
        arg_desc_chr <- purrr::map_chr(lup_ls, ~{
            args_ls <- list(argument_nm_chr, .x)
            if (append_lgl) 
                args_ls <- append(args_ls, list(abbreviations_lup))
            rlang::exec(fn, !!!args_ls)
        }) %>% purrr::discard(is.na) %>% purrr::pluck(1)
        if (is.null(arg_desc_chr)) 
            arg_desc_chr <- "NO MATCH"
        arg_desc_chr
    })
    return(arg_desc_chr_vec)
}
#' Make argument type
#' @description make_arg_type_lup_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make an argument type.The function returns a lookup table list (a list of lookup tables).
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @return Lookup table list (a list of lookup tables)
#' @rdname make_arg_type_lup_ls
#' @export 
#' @importFrom dplyr mutate filter
#' @importFrom purrr map
#' @keywords internal
make_arg_type_lup_ls <- function (object_type_lup = NULL) 
{
    if (is.null(object_type_lup)) 
        data("object_type_lup", package = "ready4fun", envir = environment())
    new_lup <- object_type_lup %>% dplyr::mutate(nchar_int = nchar(short_name_chr))
    lup_ls <- new_lup$nchar_int %>% unique() %>% sort(decreasing = T) %>% 
        purrr::map(~dplyr::filter(new_lup, nchar_int == .x))
    return(lup_ls)
}
#' Make function description
#' @description make_fn_desc_chr_vec() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make a function description.The function returns a function description (a character vector).
#' @param fns_chr_vec Functions (a character vector)
#' @param title_chr_vec Title (a character vector)
#' @param output_chr_vec Output (a character vector)
#' @param fn_type_lup_tb Function type lookup table (a tibble), Default: NULL
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @return Function description (a character vector)
#' @rdname make_fn_desc_chr_vec
#' @export 
#' @importFrom purrr pmap_chr
#' @importFrom stringr str_extract
#' @importFrom tools toTitleCase
#' @keywords internal
make_fn_desc_chr_vec <- function (fns_chr_vec, title_chr_vec, output_chr_vec, fn_type_lup_tb = NULL, 
    abbreviations_lup = NULL) 
{
    if (is.null(abbreviations_lup)) 
        data("abbreviations_lup", package = "ready4fun", envir = environment())
    fn_desc_chr_vec <- purrr::pmap_chr(list(fns_chr_vec, title_chr_vec, 
        output_chr_vec), ~{
        fn_type_chr <- stringr::str_extract(..2, "[A-Za-z]+")
        paste0(make_fn_desc_spine_chr_vec(fn_name_chr = ..1, 
            fn_title_chr = ..2, fn_type_lup_tb = fn_type_lup_tb, 
            abbreviations_lup = abbreviations_lup), ifelse(..3 == 
            "NULL", ifelse(get_from_lup_obj(fn_type_lup_tb, match_var_nm_chr = "fn_type_nm_chr", 
            match_value_xx = ..1 %>% make_fn_title_chr_vec(abbreviations_lup = abbreviations_lup) %>% 
                tools::toTitleCase(), target_var_nm_chr = "is_generic_lgl", 
            evaluate_lgl = F), "", paste0("The function is called for its side effects and does not return a value.", 
            ifelse(endsWith(..1, "_R"), " WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour", 
                ""))), paste0("The function returns ", ..3 %>% 
            tolower() %>% add_indef_artl_to_item_chr_vec(abbreviations_lup = abbreviations_lup), 
            ".")))
    })
    return(fn_desc_chr_vec)
}
#' Make function description spine
#' @description make_fn_desc_spine_chr_vec() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make a function description spine.The function returns a function description spine (a character vector).
#' @param fn_name_chr Function name (a character vector of length 1)
#' @param fn_title_chr Function title (a character vector of length 1)
#' @param fn_type_lup_tb Function type lookup table (a tibble), Default: NULL
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @return Function description spine (a character vector)
#' @rdname make_fn_desc_spine_chr_vec
#' @export 
#' @importFrom purrr map_lgl map_chr
#' @importFrom tools toTitleCase
#' @keywords internal
make_fn_desc_spine_chr_vec <- function (fn_name_chr, fn_title_chr, fn_type_lup_tb = NULL, abbreviations_lup = NULL) 
{
    if (is.null(fn_type_lup_tb)) 
        data("fn_type_lup_tb", package = "ready4fun", envir = environment())
    if (is.null(abbreviations_lup)) 
        data("abbreviations_lup", package = "ready4fun", envir = environment())
    fn_args_chr_vec <- get_fn_args_chr_vec(eval(parse(text = fn_name_chr)))
    pfx_matches_chr_vec <- fn_type_lup_tb$fn_type_nm_chr[purrr::map_lgl(fn_type_lup_tb$fn_type_nm_chr, 
        ~startsWith(fn_title_chr %>% tools::toTitleCase(), .x))]
    fn_type_chr_vec <- pfx_matches_chr_vec[nchar(pfx_matches_chr_vec) == 
        max(nchar(pfx_matches_chr_vec))]
    text_elements_chr_vec <- names(fn_type_lup_tb)[2:4] %>% purrr::map_chr(~get_from_lup_obj(fn_type_lup_tb, 
        match_var_nm_chr = "fn_type_nm_chr", match_value_xx = fn_type_chr_vec[1], 
        target_var_nm_chr = .x, evaluate_lgl = F))
    is_generic_lgl <- get_from_lup_obj(fn_type_lup_tb, match_var_nm_chr = "fn_type_nm_chr", 
        match_value_xx = fn_type_chr_vec[1], target_var_nm_chr = "is_generic_lgl", 
        evaluate_lgl = F)
    treat_as_chr <- ifelse(is_generic_lgl, ifelse(purrr::map_lgl(abbreviations_lup$short_name_chr, 
        ~endsWith(fn_name_chr, paste0(".", .x))) %>% any(), "Method", 
        "Generic"), "Function")
    fn_desc_spine_chr_vec <- paste0(fn_name_chr, "() is ", add_indef_artl_to_item_chr_vec(fn_type_chr_vec[1], 
        ignore_phrs_not_in_lup = F, abbreviations_lup = abbreviations_lup), 
        " ", tolower(treat_as_chr), " that ", update_first_word_case_chr(text_elements_chr_vec[1]), 
        ifelse(treat_as_chr == "Generic", "", ifelse(treat_as_chr == 
            "Method", paste0(" This method is implemented for the ", 
            abbreviations_lup$long_name_chr[purrr::map_lgl(abbreviations_lup$short_name_chr, 
                ~endsWith(fn_name_chr, paste0(".", .x)))], "."), 
            paste0(" Specifically, this function implements an algorithm to ", 
                fn_name_chr %>% remove_obj_type_from_nm_chr_vec(abbreviations_lup = abbreviations_lup) %>% 
                  add_indefartls_to_phrases_chr_vec(abbreviations_lup = abbreviations_lup), 
                "."))), ifelse(ifelse(is.null(fn_args_chr_vec) | 
            is.na(text_elements_chr_vec[2]), F, T), paste0(" Function argument ", 
            fn_args_chr_vec[1], " specifies the ", update_first_word_case_chr(text_elements_chr_vec[2])), 
            ""), ifelse(ifelse(is.null(fn_args_chr_vec) | is.na(text_elements_chr_vec[3]), 
            F, length(fn_args_chr_vec > 1)), paste0(" Argument ", 
            fn_args_chr_vec[2], " provides the ", update_first_word_case_chr(text_elements_chr_vec[3])), 
            ""))
    return(fn_desc_spine_chr_vec)
}
#' Make function dmt spine
#' @description make_fn_dmt_spine_chr_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make a function dmt spine.The function returns a function dmt spine (a list of character vectors of length 1).
#' @param fn_name_chr Function name (a character vector of length 1)
#' @param fn_type_chr Function type (a character vector of length 1)
#' @param fn_title_chr Function title (a character vector of length 1), Default: 'NA'
#' @param fn Function (a function)
#' @param details_chr Details (a character vector of length 1), Default: 'NA'
#' @param example_lgl Example (a logical vector of length 1), Default: F
#' @param export_lgl Export (a logical vector of length 1), Default: T
#' @param class_name_chr Class name (a character vector of length 1)
#' @param doc_in_class_lgl Document in class (a logical vector of length 1)
#' @return Function dmt spine (a list of character vectors of length 1)
#' @rdname make_fn_dmt_spine_chr_ls
#' @export 

#' @keywords internal
make_fn_dmt_spine_chr_ls <- function (fn_name_chr, fn_type_chr, fn_title_chr = NA_character_, 
    fn, details_chr = NA_character_, example_lgl = F, export_lgl = T, 
    class_name_chr, doc_in_class_lgl) 
{
    get_set_chr_vec <- c("gen_get_slot", "meth_get_slot", "gen_set_slot", 
        "meth_set_slot")
    if (!fn_type_chr %in% get_set_chr_vec) {
        fn_dmt_spine_chr_ls <- make_std_fn_dmt_spine_chr_ls(fn_name_chr = fn_name_chr, 
            fn_type_chr = fn_type_chr, fn_title_chr = fn_title_chr, 
            fn = fn, details_chr = details_chr, example_lgl = example_lgl, 
            export_lgl = export_lgl, class_name_chr = class_name_chr, 
            exclude_if_match_chr_vec = get_set_chr_vec)
    }
    else {
        fn_dmt_spine_chr_ls <- make_gtr_str_dmt_spine_chr_ls(fn_type_chr = fn_type_chr, 
            fn_name_chr = fn_name_chr, class_name_chr = class_name_chr, 
            doc_in_class_lgl = doc_in_class_lgl, example_lgl = example_lgl)
    }
    return(fn_dmt_spine_chr_ls)
}
#' Make function dmt table
#' @description make_fn_dmt_tbl_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make a function dmt a table.The function returns a function dmt table (a tibble).
#' @param fns_path_chr_vec Functions path (a character vector)
#' @param fns_dir_chr Functions directory (a character vector of length 1)
#' @param pkg_nm_chr Package name (a character vector of length 1)
#' @param custom_dmt_ls Custom dmt (a list), Default: list(title_ls = NULL, desc_ls = NULL, details_ls = NULL, export_ls = NULL, 
#'    output_ls = NULL, example_ls = NULL, args_ls_ls = NULL)
#' @param append_lgl Append (a logical vector of length 1), Default: T
#' @param fn_type_lup_tb Function type lookup table (a tibble), Default: NULL
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @return Function dmt table (a tibble)
#' @rdname make_fn_dmt_tbl_tb
#' @export 
#' @importFrom purrr map_lgl discard
#' @importFrom rlang exec
make_fn_dmt_tbl_tb <- function (fns_path_chr_vec, fns_dir_chr, pkg_nm_chr, custom_dmt_ls = list(title_ls = NULL, 
    desc_ls = NULL, details_ls = NULL, export_ls = NULL, output_ls = NULL, 
    example_ls = NULL, args_ls_ls = NULL), append_lgl = T, fn_type_lup_tb = NULL, 
    abbreviations_lup = NULL, object_type_lup = NULL) 
{
    if (is.null(abbreviations_lup)) 
        data("abbreviations_lup", package = "ready4fun", envir = environment())
    if (is.null(object_type_lup)) 
        data("object_type_lup", package = "ready4fun", envir = environment())
    fn_dmt_tbl_tb <- make_fn_dmt_tbl_tpl_tb(fns_path_chr_vec, 
        fns_dir_chr = fns_dir_chr, fn_type_lup_tb = fn_type_lup_tb, 
        abbreviations_lup = abbreviations_lup, object_type_lup = object_type_lup)
    if (purrr::map_lgl(custom_dmt_ls, ~!is.null(.x)) %>% any()) {
        args_ls <- append(custom_dmt_ls, list(append_lgl = append_lgl)) %>% 
            purrr::discard(is.null)
        fn_dmt_tbl_tb <- rlang::exec(update_fns_dmt_tb_tb, fns_dmt_tb = fn_dmt_tbl_tb, 
            !!!args_ls)
    }
    return(fn_dmt_tbl_tb)
}
#' Make function dmt table template
#' @description make_fn_dmt_tbl_tpl_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make a function dmt a table template.The function returns a function dmt table (a tibble).
#' @param fns_path_chr_vec Functions path (a character vector)
#' @param fns_dir_chr Functions directory (a character vector of length 1)
#' @param fn_type_lup_tb Function type lookup table (a tibble), Default: NULL
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @return Function dmt table (a tibble)
#' @rdname make_fn_dmt_tbl_tpl_tb
#' @export 
#' @importFrom stringr str_replace
#' @importFrom purrr map_dfr map_lgl
#' @importFrom tibble tibble
#' @importFrom dplyr mutate filter
#' @importFrom tools toTitleCase
#' @keywords internal
make_fn_dmt_tbl_tpl_tb <- function (fns_path_chr_vec, fns_dir_chr, fn_type_lup_tb = NULL, 
    abbreviations_lup = NULL, object_type_lup = NULL) 
{
    if (is.null(abbreviations_lup)) 
        data("abbreviations_lup", package = "ready4fun", envir = environment())
    if (is.null(object_type_lup)) 
        data("object_type_lup", package = "ready4fun", envir = environment())
    file_pfx_chr <- fns_dir_chr %>% stringr::str_replace("data-raw/", 
        "") %>% switch(fns = "fn_", s3 = "C3_", gnrcs = "grp_", 
        mthds = "mthd_", "s4 = C4_")
    fn_dmt_tbl_tb <- fns_path_chr_vec %>% purrr::map_dfr(~tibble::tibble(fns_chr = get_fn_nms_in_file_chr(.x), 
        title_chr = NA_character_, desc_chr = NA_character_, 
        details_chr = NA_character_, export_lgl = F, output_chr = NA_character_, 
        example_lgl = F, args_ls = list(NULL), file_nm_chr = .x %>% 
            stringr::str_replace(paste0(fns_dir_chr, "/"), ""), 
        file_pfx_chr = file_pfx_chr))
    fn_dmt_tbl_tb <- fn_dmt_tbl_tb %>% dplyr::mutate(title_chr = make_fn_title_chr_vec(fns_chr, 
        abbreviations_lup = abbreviations_lup, is_generic_lgl = purrr::map_lgl(file_nm_chr, 
            ~.x == "generics.R")))
    fn_dmt_tbl_tb <- fn_dmt_tbl_tb %>% dplyr::filter(title_chr %>% 
        tools::toTitleCase() %>% purrr::map_lgl(~{
        startsWith(.x, fn_type_lup_tb$fn_type_nm_chr) %>% any()
    }))
    fn_dmt_tbl_tb <- fn_dmt_tbl_tb %>% dplyr::mutate(output_chr = get_outp_obj_type_chr_vec(fns_chr))
    fn_dmt_tbl_tb <- fn_dmt_tbl_tb %>% dplyr::mutate(desc_chr = make_fn_desc_chr_vec(fns_chr, 
        title_chr_vec = title_chr, output_chr_vec = output_chr, 
        fn_type_lup_tb = fn_type_lup_tb, abbreviations_lup = abbreviations_lup))
    fn_dmt_tbl_tb <- fn_dmt_tbl_tb %>% dplyr::mutate(args_ls = make_arg_desc_ls(fns_chr, 
        abbreviations_lup = abbreviations_lup, object_type_lup = object_type_lup))
    return(fn_dmt_tbl_tb)
}
#' Make function title
#' @description make_fn_title_chr_vec() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make a function title.The function returns title (a character vector).
#' @param fns_chr_vec Functions (a character vector)
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param is_generic_lgl Is generic (a logical vector of length 1), Default: F
#' @return Title (a character vector)
#' @rdname make_fn_title_chr_vec
#' @export 
#' @importFrom stringr str_replace_all
#' @importFrom Hmisc capitalize
#' @importFrom purrr map_chr
#' @importFrom stringi stri_replace_last_fixed
#' @keywords internal
make_fn_title_chr_vec <- function (fns_chr_vec, object_type_lup = NULL, abbreviations_lup = NULL, 
    is_generic_lgl = F) 
{
    if (is.null(object_type_lup)) 
        data("object_type_lup", package = "ready4fun", envir = environment())
    if (is.null(abbreviations_lup)) 
        data("abbreviations_lup", package = "ready4fun", envir = environment())
    title_chr_vec <- remove_obj_type_from_nm_chr_vec(fns_chr_vec, 
        object_type_lup = object_type_lup, abbreviations_lup = abbreviations_lup, 
        is_generic_lgl = is_generic_lgl) %>% stringr::str_replace_all("_", 
        " ") %>% Hmisc::capitalize() %>% purrr::map_chr(~replace_abbr_chr(.x, 
        abbreviations_lup = abbreviations_lup) %>% stringi::stri_replace_last_fixed(" R", 
        ""))
    return(title_chr_vec)
}
#' Make function type lookup table
#' @description make_fn_type_lup_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make a function type a lookup table.The function returns a function type lookup table (a tibble).
#' @param fn_type_nm_chr Function type name (a character vector of length 1), Default: character(0)
#' @param fn_type_desc_chr Function type description (a character vector of length 1), Default: character(0)
#' @param first_arg_desc_chr First argument description (a character vector of length 1), Default: character(0)
#' @param second_arg_desc_chr Second argument description (a character vector of length 1), Default: character(0)
#' @param is_generic_lgl Is generic (a logical vector of length 1), Default: logical(0)
#' @param is_method_lgl Is method (a logical vector of length 1), Default: logical(0)
#' @return Function type lookup table (a tibble)
#' @rdname make_fn_type_lup_tb
#' @export 
#' @importFrom tibble tibble
#' @importFrom dplyr arrange
#' @keywords internal
make_fn_type_lup_tb <- function (fn_type_nm_chr = character(0), fn_type_desc_chr = character(0), 
    first_arg_desc_chr = character(0), second_arg_desc_chr = character(0), 
    is_generic_lgl = logical(0), is_method_lgl = logical(0)) 
{
    fn_type_lup_tb <- tibble::tibble(fn_type_nm_chr = fn_type_nm_chr, 
        fn_type_desc_chr = fn_type_desc_chr, first_arg_desc_chr = first_arg_desc_chr, 
        second_arg_desc_chr = second_arg_desc_chr, is_generic_lgl = is_generic_lgl, 
        is_method_lgl = is_method_lgl) %>% dplyr::arrange(fn_type_nm_chr)
    return(fn_type_lup_tb)
}
#' Make functions
#' @description make_fns_chr_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make a functions.The function returns a functions (a list of character vectors of length 1).
#' @param path_1L_chr Path 1L (a character vector of length 1), Default: 'data-raw'
#' @return Functions (a list of character vectors of length 1)
#' @rdname make_fns_chr_ls
#' @export 
#' @importFrom purrr map discard
#' @importFrom stats setNames
#' @keywords internal
make_fns_chr_ls <- function (path_1L_chr = "data-raw") 
{
    fns_chr_ls <- make_undmtd_fns_dir_chr(path_1L_chr) %>% purrr::map(~read_fns(.x)) %>% 
        stats::setNames(make_fns_type_chr())
    fns_chr_ls <- fns_chr_ls %>% purrr::discard(~identical(.x, 
        character(0)))
    return(fns_chr_ls)
}
#' Make functions type
#' @description make_fns_type_chr() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make a functions type.The function returns a functions type (a character vector of length 1).

#' @return Functions type (a character vector of length 1)
#' @rdname make_fns_type_chr
#' @export 

#' @keywords internal
make_fns_type_chr <- function () 
{
    fns_type_chr <- c("fns", "gnrcs", "mthds")
    return(fns_type_chr)
}
#' Make getter setter dmt spine
#' @description make_gtr_str_dmt_spine_chr_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make a getter setter dmt spine.The function returns a getter setter dmt spine (a list of character vectors of length 1).
#' @param fn_type_chr Function type (a character vector of length 1)
#' @param fn_name_chr Function name (a character vector of length 1)
#' @param class_name_chr Class name (a character vector of length 1)
#' @param doc_in_class_lgl Document in class (a logical vector of length 1)
#' @param example_lgl Example (a logical vector of length 1), Default: F
#' @return Getter setter dmt spine (a list of character vectors of length 1)
#' @rdname make_gtr_str_dmt_spine_chr_ls
#' @export 
#' @importFrom stringr str_replace str_sub
#' @keywords internal
make_gtr_str_dmt_spine_chr_ls <- function (fn_type_chr, fn_name_chr, class_name_chr, doc_in_class_lgl, 
    example_lgl = F) 
{
    if (fn_type_chr %in% c("gen_set_slot", "meth_set_slot")) {
        ref_slot_chr <- stringr::str_replace(fn_name_chr, "<-", 
            "")
    }
    else {
        ref_slot_chr <- fn_name_chr
    }
    if (fn_type_chr %in% c("gen_get_slot", "gen_set_slot")) 
        fn_tags_chr <- paste0("#' FUNCTION_TITLE\n", "#' @description S4 Generic function to ", 
            ifelse(fn_type_chr == "gen_get_slot", "get", "set"), 
            " the value of the slot ", ref_slot_chr, "\n", "#' @name ", 
            fn_name_chr, "\n", "#' @param x An object ", class_name_chr, 
            "\n", "#' @details DETAILS\n", "#' @export\n")
    if (fn_type_chr %in% c("meth_get_slot", "meth_set_slot")) {
        fn_tags_chr <- paste0("#' ", fn_name_chr, "\n#' @name ", 
            fn_name_chr, "-", class_name_chr, "\n", "#' @description FUNCTION_DESCRIPTION", 
            " for S4 objects of class ", class_name_chr, "\n", 
            "#' @param x An object of class ", class_name_chr, 
            "\n", ifelse(example_lgl, paste0("#' @examples\n", 
                "#' \\dontrun{\n", "#' if(interactive()){\n", 
                "#'  #EXAMPLE1\n", "#'  }\n", "#' }\n"), ""), 
            "#' @rdname ", ifelse(doc_in_class_lgl, class_name_chr, 
                ifelse(fn_type_chr == "meth_get_slot", fn_name_chr, 
                  paste0(stringr::str_sub(fn_name_chr, end = -3), 
                    "-set"))))
    }
    gtr_str_dmt_spine_chr_ls <- list(fn_tags_chr = fn_tags_chr, 
        ref_slot_chr = ref_slot_chr)
    return(gtr_str_dmt_spine_chr_ls)
}
#' Make new function dmt
#' @description make_new_fn_dmt_chr_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make new a function dmt.The function returns new function dmt (a list of character vectors of length 1).
#' @param fn_type_chr Function type (a character vector of length 1)
#' @param fn_name_chr Function name (a character vector of length 1)
#' @param fn_desc_chr Function description (a character vector of length 1), Default: 'NA'
#' @param fn_det_chr Function det (a character vector of length 1), Default: 'NA'
#' @param fn_out_type_chr Function out type (a character vector of length 1), Default: 'NA'
#' @param args_ls Arguments (a list), Default: NULL
#' @param fn Function (a function), Default: NULL
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @return New function dmt (a list of character vectors of length 1)
#' @rdname make_new_fn_dmt_chr_ls
#' @export 
#' @importFrom stringr str_replace
#' @importFrom purrr flatten_chr
#' @importFrom stats setNames
#' @keywords internal
make_new_fn_dmt_chr_ls <- function (fn_type_chr, fn_name_chr, fn_desc_chr = NA_character_, 
    fn_det_chr = NA_character_, fn_out_type_chr = NA_character_, 
    args_ls = NULL, fn = NULL, abbreviations_lup = NULL, object_type_lup = NULL) 
{
    if (is.null(abbreviations_lup)) 
        data("abbreviations_lup", package = "ready4fun", envir = environment())
    if (is.null(object_type_lup)) 
        data("object_type_lup", package = "ready4fun", envir = environment())
    s3_class_main <- NULL
    if (!is.null(fn)) {
        fn_args_chr_vec <- get_fn_args_chr_vec(fn)
        fn_out_type_chr <- ifelse(is.na(fn_out_type_chr), get_return_obj_nm_chr(fn) %>% 
            make_arg_desc_chr_vec(abbreviations_lup = abbreviations_lup, 
                object_type_lup = object_type_lup), fn_out_type_chr)
    }
    else {
        fn_args_chr_vec <- NA_character_
    }
    if (fn_type_chr == "set_class") {
        desc_start <- "Create a new S4 object of the class:"
        output_txt <- paste0("An S4 object of the ", fn_name_chr, 
            " class")
    }
    if (fn_type_chr == "s3_valid_instance") {
        desc_start <- "Create a new valid instance of the S3 class: "
        output_txt <- paste0("A validated instance of the ", 
            fn_name_chr, " class")
    }
    if (fn_type_chr == "s3_unvalidated_instance") {
        desc_start <- "Create a new unvalidated instance of the S3 class: "
        s3_class_main <- stringr::str_replace(fn_name_chr, "new_", 
            "")
        output_txt <- paste0("An unvalidated instance of the ", 
            s3_class_main, " class")
    }
    if (fn_type_chr == "s3_prototype") {
        desc_start <- "Create a new prototype for S3 class: "
        s3_class_main <- stringr::str_replace(fn_name_chr, "make_prototype_", 
            "")
        output_txt <- paste0("A prototpe for ", s3_class_main, 
            " class")
    }
    if (fn_type_chr == "s3_validator") {
        desc_start <- "Validate an instance of the S3 class: "
        s3_class_main <- stringr::str_replace(fn_name_chr, "validate_", 
            "")
        output_txt <- paste0("A prototpe for ", s3_class_main, 
            " class")
    }
    if (fn_type_chr == "s3_checker") {
        desc_start <- "Check whether an object is a valid instance of the S3 class: "
        s3_class_main <- stringr::str_replace(fn_name_chr, "is_", 
            "")
        output_txt <- paste0("A logical value, TRUE if a valid instance of the ", 
            s3_class_main, " class")
    }
    if (fn_type_chr %in% c("gen_get_slot", "meth_get_slot")) {
        desc_start <- "Get the value of the slot "
        output_txt <- "A XXX ..."
    }
    if (fn_type_chr %in% c("gen_set_slot", "meth_set_slot")) {
        desc_start <- "Set the value of the slot "
        output_txt <- "NULL"
    }
    if (fn_type_chr %in% c("fn", "gen_std_s3_mthd", "meth_std_s3_mthd", 
        "gen_std_s4_mthd", "meth_std_s4_mthd")) {
        desc_start <- fn_desc_chr
        output_txt <- fn_out_type_chr
    }
    if (is.null(args_ls)) {
        arg_desc_chr_vec <- NULL
        if (any(!is.na(fn_args_chr_vec)) & !is.null(object_type_lup)) {
            arg_desc_chr_vec <- make_arg_desc_chr_vec(fn_args_chr_vec, 
                abbreviations_lup = abbreviations_lup, object_type_lup = object_type_lup)
            if (!is.null(arg_desc_chr_vec)) {
                names(arg_desc_chr_vec) <- fn_args_chr_vec
            }
        }
    }
    else {
        arg_desc_chr_vec <- args_ls %>% purrr::flatten_chr() %>% 
            stats::setNames(names(args_ls))
    }
    new_fn_dmt_chr_ls <- list(desc_start = desc_start, s3_class_main = s3_class_main, 
        output_txt = output_txt, fn_det_chr = fn_det_chr, arg_desc_chr_vec = arg_desc_chr_vec)
    return(new_fn_dmt_chr_ls)
}
#' Make object lookup table
#' @description make_obj_lup_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make an object lookup table.The function returns an object (a tibble).

#' @return Object (a tibble)
#' @rdname make_obj_lup_tb
#' @export 
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows mutate filter select
#' @importFrom purrr map2_chr map_chr
#' @importFrom stringr str_replace
#' @keywords internal
make_obj_lup_tb <- function () 
{
    obj_tb <- tibble::tibble(short_name_chr = c("df", "fn", "ls", 
        "r3", "r4", "s3", "s4", "sf", "tb", "arr", "chr", "dbl", 
        "fct", "int", "lgl", "lup", "mat"), long_name_chr = c("data.frame", 
        "function", "list", "readyforwhatsnext S3", "readyforwhatsnext S4", 
        "S3", "S4", "simple features object", "tibble", "array", 
        "character", "double", "factor", "integer", "logical", 
        "lookup table", "matrix"), atomic_element_lgl = c(rep(F, 
        10), rep(T, 2), F, rep(T, 2), rep(F, 2)), r3_element_lgl = c(T, 
        F, T, rep(F, 4), rep(T, 10)))
    obj_tb <- dplyr::bind_rows(obj_tb %>% dplyr::mutate(long_name_chr = purrr::map2_chr(long_name_chr, 
        atomic_element_lgl, ~ifelse(.y, paste0(.x, " vector of length 1"), 
            .x))), obj_tb %>% dplyr::filter(atomic_element_lgl) %>% 
        dplyr::mutate(short_name_chr = paste0(short_name_chr, 
            "_vec"), long_name_chr = paste0(long_name_chr, " vector")), 
        obj_tb %>% dplyr::filter(r3_element_lgl) %>% dplyr::mutate(short_name_chr = paste0(short_name_chr, 
            purrr::map_chr(atomic_element_lgl, ~ifelse(.x, "_vec", 
                "")), "_r3"), long_name_chr = paste0("readyforwhatsnext S3 extension of ", 
            long_name_chr, purrr::map_chr(atomic_element_lgl, 
                ~ifelse(.x, " vector", ""))))) %>% dplyr::select(-3, 
        -4)
    obj_tb <- dplyr::bind_rows(obj_tb, obj_tb %>% dplyr::mutate(short_name_chr = paste0(short_name_chr, 
        "_ls"), long_name_chr = paste0("list of ", long_name_chr)) %>% 
        dplyr::bind_rows(obj_tb %>% dplyr::mutate(short_name_chr = paste0(short_name_chr, 
            "_r4"), long_name_chr = paste0("readyforwhatsnext S4 collection of ", 
            long_name_chr))) %>% dplyr::mutate(long_name_chr = purrr::map_chr(long_name_chr, 
        ~ifelse(endsWith(.x, "vector of length 1"), stringr::str_replace(.x, 
            "vector", "vectors"), ifelse(endsWith(.x, "matrix"), 
            stringr::str_replace(.x, "matrix", "matrices"), paste0(.x, 
                "s"))))), tibble::tibble(short_name_chr = "xx", 
        long_name_chr = "output object of multiple potential types"))
    obj_tb <- obj_tb %>% dplyr::mutate(plural_lgl = F)
    return(obj_tb)
}
#' Make standard function dmt spine
#' @description make_std_fn_dmt_spine_chr_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make a standard function dmt spine.The function returns a standard function dmt spine (a list of character vectors of length 1).
#' @param fn_name_chr Function name (a character vector of length 1)
#' @param fn_type_chr Function type (a character vector of length 1)
#' @param fn_title_chr Function title (a character vector of length 1)
#' @param fn Function (a function)
#' @param details_chr Details (a character vector of length 1), Default: 'NA'
#' @param example_lgl Example (a logical vector of length 1), Default: F
#' @param export_lgl Export (a logical vector of length 1), Default: T
#' @param class_name_chr Class name (a character vector of length 1), Default: ''
#' @param exclude_if_match_chr_vec Exclude if match (a character vector)
#' @return Standard function dmt spine (a list of character vectors of length 1)
#' @rdname make_std_fn_dmt_spine_chr_ls
#' @export 
#' @importFrom sinew makeOxygen
#' @importFrom stringr str_replace
#' @importFrom purrr discard
#' @keywords internal
make_std_fn_dmt_spine_chr_ls <- function (fn_name_chr, fn_type_chr, fn_title_chr, fn, details_chr = NA_character_, 
    example_lgl = F, export_lgl = T, class_name_chr = "", exclude_if_match_chr_vec) 
{
    assert_does_not_match_terms(input_chr_vec = fn_type_chr, 
        exclude_if_match_chr_vec = exclude_if_match_chr_vec)
    if (!is.na(details_chr)) {
        if (details_chr == "DETAILS") 
            details_chr <- NA_character_
    }
    if (startsWith(fn_type_chr, "gen_")) {
        fn_tags_chr <- sinew::makeOxygen(fn, print = FALSE, add_fields = c("export")) %>% 
            stringr::str_replace("#' @return OUTPUT_DESCRIPTION\n", 
                "")
    }
    else {
        fn_tags_chr <- sinew::makeOxygen(fn, print = FALSE, add_fields = c(ifelse(!is.na(details_chr), 
            "details", NA_character_), ifelse(example_lgl, "examples", 
            NA_character_), "rdname", ifelse(export_lgl, "export", 
            NA_character_)) %>% purrr::discard(is.na)) %>% stringr::str_replace("#' @title FUNCTION_TITLE\n", 
            "")
    }
    fn_tags_chr <- fn_tags_chr %>% stringr::str_replace("@title ", 
        "@name ") %>% stringr::str_replace("@rdname fn", paste0("@rdname ", 
        fn_name_chr))
    fn_tags_chr <- paste0("#' ", ifelse((startsWith(fn_type_chr, 
        "gen_") | fn_type_chr == "fn"), fn_title_chr, ""), "\n", 
        fn_tags_chr)
    if (!fn_type_chr %>% startsWith("s3") & !fn_type_chr %in% 
        c("fn", "gen_std_s3_mthd", "meth_std_s3_mthd", "gen_std_s4_mthd", 
            "meth_std_s4_mthd")) 
        fn_tags_chr <- update_fn_dmt_with_slots_chr(fn_name_chr = fn_name_chr, 
            fn_tags_chr = fn_tags_chr)
    std_fn_dmt_spine_chr_ls <- list(fn_tags_chr = fn_tags_chr, 
        ref_slot_chr = fn_name_chr)
    return(std_fn_dmt_spine_chr_ls)
}
#' Make undmtd functions directory
#' @description make_undmtd_fns_dir_chr() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make undmtd a functions directory.The function returns undocumented functions directory (a character vector of length 1).
#' @param path_1L_chr Path 1L (a character vector of length 1), Default: 'data-raw'
#' @return Undocumented functions directory (a character vector of length 1)
#' @rdname make_undmtd_fns_dir_chr
#' @export 

#' @keywords internal
make_undmtd_fns_dir_chr <- function (path_1L_chr = "data-raw") 
{
    undocumented_fns_dir_chr <- paste0(path_1L_chr, "/", make_fns_type_chr())
    return(undocumented_fns_dir_chr)
}
