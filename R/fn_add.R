#' Add additional packages
#' @description add_addl_pkgs() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add additional packages. The function is called for its side effects and does not return a value.
#' @param addl_pkgs_ls Additional packages (a list)
#' @return No return value, called for side effects.
#' @rdname add_addl_pkgs
#' @export 
#' @importFrom purrr walk2 walk
#' @importFrom usethis use_package
#' @keywords internal
add_addl_pkgs <- function (addl_pkgs_ls) 
{
    if (!is.null(addl_pkgs_ls)) {
        addl_pkgs_ls %>% purrr::walk2(names(addl_pkgs_ls), ~{
            pkgs_chr <- .x
            type_1L_chr <- .y
            pkgs_chr %>% purrr::walk(~usethis::use_package(.x, 
                type = type_1L_chr))
        })
    }
}
#' Add build ignore
#' @description add_build_ignore() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add build ignore. The function is called for its side effects and does not return a value.
#' @param build_ignore_ls Build ignore (a list)
#' @return No return value, called for side effects.
#' @rdname add_build_ignore
#' @export 
#' @importFrom purrr walk
#' @importFrom usethis use_build_ignore
#' @keywords internal
add_build_ignore <- function (build_ignore_ls) 
{
    if (!is.null(build_ignore_ls$file_nms_chr)) {
        build_ignore_ls$file_nms_chr %>% purrr::walk(~usethis::use_build_ignore(.x))
    }
    if (!is.null(build_ignore_ls$regulars_rgx)) {
        build_ignore_ls$regulars_rgx %>% purrr::walk(~usethis::use_build_ignore(.x, 
            escape = FALSE))
    }
}
#' Add functions documentation tibble
#' @description add_fns_dmt_tb() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add functions documentation tibble. The function returns Package setup (a list).
#' @param pkg_setup_ls Package setup (a list)
#' @param append_1L_lgl Append (a logical vector of length one), Default: T
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: character(0)
#' @param fns_env_ls Functions (a list of environments), Default: NULL
#' @param inc_methods_1L_lgl Include methods (a logical vector of length one), Default: F
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @return Package setup (a list)
#' @rdname add_fns_dmt_tb
#' @export 
#' @importFrom purrr map_chr map_lgl pluck
#' @importFrom stringr str_sub
#' @importFrom tibble add_case tibble
#' @importFrom dplyr filter bind_rows distinct
#' @keywords internal
add_fns_dmt_tb <- function (pkg_setup_ls, append_1L_lgl = T, dv_url_pfx_1L_chr = character(0), 
    fns_env_ls = NULL, inc_methods_1L_lgl = F, key_1L_chr = NULL) 
{
    paths_ls <- make_fn_nms(paste0(pkg_setup_ls$initial_ls$path_to_pkg_rt_1L_chr, 
        "/data-raw"))
    undocumented_fns_dir_chr <- make_undmtd_fns_dir_chr(drop_empty_1L_lgl = T)
    if (!inc_methods_1L_lgl) {
        if ("mthds" %in% names(paths_ls)) {
            method_nms_chr <- paths_ls$mthds %>% purrr::map_chr(~basename(.x) %>% 
                stringr::str_sub(end = -3))
            paths_ls$mthds <- NULL
            undocumented_fns_dir_chr <- undocumented_fns_dir_chr[undocumented_fns_dir_chr %>% 
                purrr::map_lgl(~!endsWith(.x, "mthds"))]
        }
        else {
            method_nms_chr <- character(0)
        }
        pkg_setup_ls$subsequent_ls$fns_dmt_tb <- make_dmt_for_all_fns(paths_ls = paths_ls, 
            abbreviations_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup, 
            append_1L_lgl = append_1L_lgl, custom_dmt_ls = pkg_setup_ls$subsequent_ls$custom_dmt_ls, 
            fns_env_ls = fns_env_ls, fn_types_lup = pkg_setup_ls$subsequent_ls$fn_types_lup, 
            inc_all_mthds_1L_lgl = T, object_type_lup = pkg_setup_ls$subsequent_ls$object_type_lup, 
            undocumented_fns_dir_chr = undocumented_fns_dir_chr)
        new_nms_chr <- setdiff(method_nms_chr, pkg_setup_ls$subsequent_ls$fns_dmt_tb$fns_chr)
        if (!identical(character(0), new_nms_chr)) {
            if (nrow(pkg_setup_ls$subsequent_ls$fns_dmt_tb) == 
                0) {
                data("fns_dmt_tb", package = "ready4fun", envir = environment())
                pkg_setup_ls$subsequent_ls$fns_dmt_tb <- fns_dmt_tb[0, 
                  ]
            }
            pkg_setup_ls$subsequent_ls$fns_dmt_tb <- tibble::add_case(pkg_setup_ls$subsequent_ls$fns_dmt_tb, 
                fns_chr = new_nms_chr, args_ls = list(character(0)))
        }
    }
    else {
        pkg_setup_ls$subsequent_ls$fns_dmt_tb <- pkg_setup_ls$subsequent_ls$fns_dmt_tb %>% 
            dplyr::filter(!is.na(file_nm_chr))
        paths_ls <- make_fn_nms(paste0(pkg_setup_ls$initial_ls$path_to_pkg_rt_1L_chr, 
            "/data-raw"))
        undocumented_fns_dir_chr <- make_undmtd_fns_dir_chr(drop_empty_1L_lgl = T)
        if ("mthds" %in% names(paths_ls)) {
            paths_ls <- list(mthds = paths_ls$mthds)
            undocumented_fns_dir_chr <- undocumented_fns_dir_chr[undocumented_fns_dir_chr %>% 
                purrr::map_lgl(~endsWith(.x, "mthds"))]
        }
        fns_dmt_tb <- make_dmt_for_all_fns(paths_ls = paths_ls, 
            abbreviations_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup, 
            append_1L_lgl = append_1L_lgl, custom_dmt_ls = pkg_setup_ls$subsequent_ls$custom_dmt_ls, 
            fns_env_ls = fns_env_ls, fn_types_lup = pkg_setup_ls$subsequent_ls$fn_types_lup, 
            inc_all_mthds_1L_lgl = T, object_type_lup = pkg_setup_ls$subsequent_ls$object_type_lup, 
            undocumented_fns_dir_chr = undocumented_fns_dir_chr)
        if (identical(pkg_setup_ls$subsequent_ls$fns_dmt_tb, 
            tibble::tibble()) | !"mthds" %in% names(paths_ls)) {
            pkg_setup_ls$subsequent_ls$fns_dmt_tb <- fns_dmt_tb
        }
        else {
            pkg_setup_ls$subsequent_ls$fns_dmt_tb <- dplyr::bind_rows(pkg_setup_ls$subsequent_ls$fns_dmt_tb, 
                fns_dmt_tb)
        }
        pkg_setup_ls$subsequent_ls$fns_dmt_tb <- pkg_setup_ls$subsequent_ls$fns_dmt_tb %>% 
            dplyr::distinct()
        if (pkg_setup_ls$subsequent_ls$inc_pkg_meta_data_1L_lgl & 
            !(is.null(pkg_setup_ls$subsequent_ls$server_1L_chr) | 
                identical(pkg_setup_ls$subsequent_ls$server_1L_chr, 
                  character(0)))) {
            pkg_dss_tb <- fns_dmt_tb %>% write_and_doc_ds(overwrite_1L_lgl = T, 
                db_1L_chr = "fns_dmt_tb", title_1L_chr = paste0(pkg_setup_ls$initial_ls$pkg_desc_ls$Package, 
                  " function documentation table"), desc_1L_chr = paste0("A table with the summary information on functions included in the ", 
                  pkg_setup_ls$initial_ls$pkg_desc_ls$Package, 
                  " package."), format_1L_chr = "A tibble", url_1L_chr = pkg_setup_ls$initial_ls$pkg_desc_ls$URL %>% 
                  strsplit(",") %>% unlist() %>% purrr::pluck(1), 
                abbreviations_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup, 
                object_type_lup = pkg_setup_ls$subsequent_ls$object_type_lup, 
                pkg_dss_tb = pkg_setup_ls$subsequent_ls$dss_records_ls$pkg_dss_tb, 
                dv_ds_nm_1L_chr = pkg_setup_ls$subsequent_ls$dv_ds_nm_1L_chr, 
                dv_url_pfx_1L_chr = pkg_setup_ls$subsequent_ls$dv_url_pfx_1L_chr, 
                key_1L_chr = key_1L_chr, server_1L_chr = pkg_setup_ls$subsequent_ls$server_1L_chr)
        }
    }
    return(pkg_setup_ls)
}
#' Add indefinite article to item
#' @description add_indef_artl_to_item() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add indefinite article to item. The function returns Indefinite item (a character vector).
#' @param phrase_chr Phrase (a character vector)
#' @param abbreviations_lup Abbreviations (a lookup table)
#' @param ignore_phrs_not_in_lup_1L_lgl Ignore phrases not in lookup table (a logical vector of length one), Default: T
#' @return Indefinite item (a character vector)
#' @rdname add_indef_artl_to_item
#' @export 
#' @importFrom purrr map_chr
#' @importFrom ready4 get_from_lup_obj
#' @importFrom stringr str_sub
#' @keywords internal
add_indef_artl_to_item <- function (phrase_chr, abbreviations_lup, ignore_phrs_not_in_lup_1L_lgl = T) 
{
    indefinite_item_chr <- purrr::map_chr(phrase_chr, ~{
        phrase_1L_chr <- .x
        matching_prefix_chr <- abbreviations_lup$long_name_chr[phrase_1L_chr %>% 
            startsWith(abbreviations_lup$long_name_chr)]
        if (identical(matching_prefix_chr, character(0))) {
            prefix_chr <- phrase_1L_chr
        }
        else {
            n_char_matches_dbl <- matching_prefix_chr %>% nchar()
            prefix_chr <- matching_prefix_chr[which(n_char_matches_dbl == 
                max(n_char_matches_dbl))]
        }
        plural_1L_lgl <- ifelse(prefix_chr %in% abbreviations_lup$long_name_chr, 
            ready4::get_from_lup_obj(abbreviations_lup, match_var_nm_1L_chr = "long_name_chr", 
                match_value_xx = prefix_chr, target_var_nm_1L_chr = "plural_lgl", 
                evaluate_1L_lgl = F), ignore_phrs_not_in_lup_1L_lgl)
        ifelse(is.na(plural_1L_lgl), .x, ifelse(plural_1L_lgl, 
            .x, ifelse(.x %>% tolower() %>% stringr::str_sub(end = 1) %in% 
                c("a", "e", "i", "o", "u"), paste0("an ", .x), 
                paste0("a ", .x))))
    })
    return(indefinite_item_chr)
}
#' Add indefinite articles to phrases
#' @description add_indefartls_to_phrases() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add indefinite articles to phrases. The function returns Phrases (a character vector).
#' @param abbreviated_phrase_1L_chr Abbreviated phrase (a character vector of length one)
#' @param abbreviations_lup Abbreviations (a lookup table)
#' @param ignore_phrs_not_in_lup_1L_lgl Ignore phrases not in lookup table (a logical vector of length one), Default: T
#' @return Phrases (a character vector)
#' @rdname add_indefartls_to_phrases
#' @export 
#' @importFrom purrr map_chr discard
#' @keywords internal
add_indefartls_to_phrases <- function (abbreviated_phrase_1L_chr, abbreviations_lup, ignore_phrs_not_in_lup_1L_lgl = T) 
{
    phrases_chr <- abbreviated_phrase_1L_chr %>% purrr::map_chr(~{
        words_chr_ls <- strsplit(.x, "_")
        words_chr_ls %>% purrr::map_chr(~{
            expanded_chr <- replace_abbr(.x, abbreviations_lup = abbreviations_lup, 
                collapse_lgl = F)
            indefinite_chr <- add_indef_artl_to_item(expanded_chr, 
                abbreviations_lup = abbreviations_lup, ignore_phrs_not_in_lup_1L_lgl = ignore_phrs_not_in_lup_1L_lgl)
            matches_lgl <- expanded_chr == indefinite_chr
            run_length_ls <- rle(matches_lgl)
            1:length(run_length_ls$lengths) %>% purrr::map_chr(~{
                ifelse(run_length_ls$values[.x], paste0(indefinite_chr[ifelse(.x == 
                  1, .x, sum(run_length_ls$lengths[1:.x - 1]) + 
                  1):sum(run_length_ls$lengths[1:.x])], collapse = " "), 
                  paste0(c(indefinite_chr[ifelse(.x == 1, .x, 
                    sum(run_length_ls$lengths[1:.x - 1]) + 1)], 
                    ifelse(run_length_ls$lengths[.x] == 1, NA_character_, 
                      paste0(expanded_chr[ifelse(.x == 1, .x + 
                        1, sum(run_length_ls$lengths[1:.x - 1]) + 
                        2):sum(run_length_ls$lengths[1:.x])], 
                        collapse = " ")) %>% purrr::discard(is.na)), 
                    collapse = " "))
            }) %>% paste0(collapse = " ")
        })
    })
    return(phrases_chr)
}
#' Add new class prototypes
#' @description add_new_cls_pts() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add new class prototypes. The function returns Package setup (a list).
#' @param pkg_setup_ls Package setup (a list)
#' @param addl_cls_pts_tb Additional class prototypes (a tibble), Default: NULL
#' @param purge_pkg_clss_1L_lgl Purge package classes (a logical vector of length one), Default: T
#' @return Package setup (a list)
#' @rdname add_new_cls_pts
#' @export 
#' @importFrom dplyr filter arrange
#' @importFrom ready4 add_lups
#' @keywords internal
add_new_cls_pts <- function (pkg_setup_ls, addl_cls_pts_tb = NULL, purge_pkg_clss_1L_lgl = T) 
{
    if (is.null(pkg_setup_ls$subsequent_ls$prototype_lup)) {
        pkg_setup_ls$subsequent_ls$prototype_lup <- get_rds_from_pkg_dmt(pkg_setup_ls, 
            fl_nm_1L_chr = "prototype_lup")
    }
    if (purge_pkg_clss_1L_lgl) {
        pkg_setup_ls$subsequent_ls$prototype_lup <- pkg_setup_ls$subsequent_ls$prototype_lup %>% 
            dplyr::filter(pt_ns_chr != pkg_setup_ls$initial_ls$pkg_desc_ls$Package)
    }
    if (!is.null(addl_cls_pts_tb)) {
        pkg_setup_ls$subsequent_ls$prototype_lup <- ready4::add_lups(pkg_setup_ls$subsequent_ls$prototype_lup, 
            new_lup = addl_cls_pts_tb, key_var_nm_1L_chr = "type_chr") %>% 
            dplyr::arrange(pt_ns_chr, type_chr)
        if (!is.null(pkg_setup_ls$problems_ls$missing_cls_pts_chr)) {
            pkg_setup_ls <- update_pkg_setup_msgs(pkg_setup_ls, 
                list_element_1L_chr = "missing_cls_pts_chr")
        }
    }
    return(pkg_setup_ls)
}
#' Add plurals to abbreviation lookup table
#' @description add_plurals_to_abbr_lup() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add plurals to abbreviation lookup table. The function returns Abbreviation (a tibble).
#' @param abbr_tb Abbreviation (a tibble)
#' @param no_plural_chr No plural (a character vector), Default: 'NA'
#' @param custom_plural_ls Custom plural (a list), Default: NULL
#' @return Abbreviation (a tibble)
#' @rdname add_plurals_to_abbr_lup
#' @export 
#' @importFrom dplyr filter mutate_all mutate bind_rows arrange
#' @importFrom purrr map_dfr map2_lgl
#' @importFrom tibble tibble
#' @keywords internal
add_plurals_to_abbr_lup <- function (abbr_tb, no_plural_chr = NA_character_, custom_plural_ls = NULL) 
{
    non_standard_1L_chr <- no_plural_chr
    if (!is.null(custom_plural_ls)) {
        non_standard_1L_chr <- c(non_standard_1L_chr, names(custom_plural_ls))
    }
    standard_chr <- setdiff(abbr_tb$long_name_chr, non_standard_1L_chr)
    new_tb <- standard_tb <- abbr_tb %>% dplyr::filter(long_name_chr %in% 
        standard_chr) %>% dplyr::mutate_all(~paste0(.x, "s"))
    if (!is.null(custom_plural_ls)) {
        custom_tb <- 1:length(custom_plural_ls) %>% purrr::map_dfr(~{
            match_1L_chr <- names(custom_plural_ls)[.x]
            long_plural_1L_chr <- custom_plural_ls[[.x]][1]
            short_plural_1L_chr <- ifelse(length(custom_plural_ls[[.x]]) > 
                1, custom_plural_ls[[.x]][2], NA_character_)
            abbr_tb %>% dplyr::filter(long_name_chr == match_1L_chr) %>% 
                dplyr::mutate(long_name_chr = long_plural_1L_chr, 
                  short_name_chr = ifelse(is.na(short_plural_1L_chr), 
                    paste0(short_name_chr, "s"), short_plural_1L_chr))
        })
        new_tb <- dplyr::bind_rows(standard_tb, custom_tb) %>% 
            dplyr::arrange()
    }
    abbr_tb <- tibble::tibble(short_name_chr = make.unique(c(abbr_tb$short_name_chr, 
        new_tb$short_name_chr)), long_name_chr = make.unique(c(abbr_tb$long_name_chr, 
        new_tb$long_name_chr)), plural_lgl = c(rep(F, length(abbr_tb$long_name_chr)), 
        rep(T, length(new_tb$long_name_chr)))) %>% dplyr::mutate(plural_lgl = purrr::map2_lgl(plural_lgl, 
        long_name_chr, ~ifelse(.y %in% no_plural_chr, NA, .x))) %>% 
        dplyr::arrange(short_name_chr)
    return(abbr_tb)
}
#' Add rows to function type lookup table
#' @description add_rows_to_fn_type_lup() is an Add function that updates an object by adding new values to new or empty fields. Specifically, this function implements an algorithm to add rows to function type lookup table. The function returns Updated function types (a lookup table).
#' @param fn_types_lup Function types (a lookup table), Default: make_fn_type_lup()
#' @param fn_type_nm_chr Function type name (a character vector), Default: 'NA'
#' @param fn_type_desc_chr Function type description (a character vector), Default: 'NA'
#' @param first_arg_desc_chr First argument description (a character vector), Default: 'NA'
#' @param second_arg_desc_chr Second argument description (a character vector), Default: 'NA'
#' @param is_generic_lgl Is generic (a logical vector), Default: F
#' @param is_method_lgl Is method (a logical vector), Default: F
#' @return Updated function types (a lookup table)
#' @rdname add_rows_to_fn_type_lup
#' @export 
#' @importFrom ready4 add_lups
#' @importFrom tibble tibble
#' @keywords internal
add_rows_to_fn_type_lup <- function (fn_types_lup = make_fn_type_lup(), fn_type_nm_chr = NA_character_, 
    fn_type_desc_chr = NA_character_, first_arg_desc_chr = NA_character_, 
    second_arg_desc_chr = NA_character_, is_generic_lgl = F, 
    is_method_lgl = F) 
{
    if (length(fn_type_nm_chr) > 0) {
        updated_fn_types_lup <- ready4::add_lups(fn_types_lup, 
            new_lup = tibble::tibble(fn_type_nm_chr = fn_type_nm_chr, 
                fn_type_desc_chr = fn_type_desc_chr, first_arg_desc_chr = first_arg_desc_chr, 
                second_arg_desc_chr = second_arg_desc_chr, is_generic_lgl = is_generic_lgl, 
                is_method_lgl = is_method_lgl), key_var_nm_1L_chr = "fn_type_nm_chr")
    }
    else {
        updated_fn_types_lup <- fn_types_lup
    }
    return(updated_fn_types_lup)
}
