#' Validate package setup
#' @description validate_pkg_setup() is a Validate function that validates that an object conforms to required criteria. Specifically, this function implements an algorithm to validate package setup. The function returns Package setup (a list).
#' @param pkg_setup_ls Package setup (a list)
#' @return Package setup (a list)
#' @rdname validate_pkg_setup
#' @export 

#' @keywords internal
validate_pkg_setup <- function (pkg_setup_ls) 
{
    pkg_setup_ls$problems_ls <- NULL
    missing_fn_types_chr <- get_new_fn_types(pkg_setup_ls)
    if (!identical(missing_fn_types_chr, character(0))) {
        message(paste0("The following function type", ifelse(length(missing_fn_types_chr) > 
            1, "s are", " is"), " not yet defined: \n", missing_fn_types_chr %>% 
            make_list_phrase(), ".\nAdd the missing definition", 
            ifelse(length(missing_fn_types_chr) > 1, "s", ""), 
            " by using the 'write_new_fn_types' function"))
        pkg_setup_ls$problems_ls$missing_fn_types_chr <- missing_fn_types_chr
    }
    else {
        if (is.null(pkg_setup_ls$subsequent_ls$seed_obj_type_lup) | 
            is.null(pkg_setup_ls$subsequent_ls$object_type_lup)) {
            pkg_setup_ls <- write_new_obj_types(pkg_setup_ls = pkg_setup_ls)
        }
        else {
            if (is.null(pkg_setup_ls$subsequent_ls$abbreviations_lup)) {
                pkg_setup_ls <- write_new_abbrs(pkg_setup_ls)
            }
        }
        if (!is.null(pkg_setup_ls$subsequent_ls$cls_fn_ls)) {
            if (!is.null(pkg_setup_ls$subsequent_ls$cls_fn_ls$args_ls$x)) {
                missing_class_abbrs_chr <- setdiff(paste0(pkg_setup_ls$initial_ls$pkg_desc_ls$Package, 
                  "_", pkg_setup_ls$subsequent_ls$cls_fn_ls$args_ls$x$name_stub_chr), 
                  pkg_setup_ls$subsequent_ls$abbreviations_lup$short_name_chr)
                if (!identical(missing_class_abbrs_chr, character(0))) {
                  pkg_setup_ls$problems_ls$missing_class_abbrs_chr <- missing_class_abbrs_chr
                  pkg_setup_ls <- write_new_abbrs(pkg_setup_ls)
                }
            }
            if (is.null(pkg_setup_ls$subsequent_ls$prototype_lup)) {
                pkg_setup_ls <- add_new_cls_pts(pkg_setup_ls)
            }
            missing_cls_pts_chr <- get_new_cls_pts(pkg_setup_ls)
            if (!identical(missing_cls_pts_chr, character(0))) {
                message(paste0("The following potential class prototype", 
                  ifelse(length(missing_cls_pts_chr) > 1, "s are", 
                    " is"), " not defined in the prototype_lup object: \n", 
                  missing_cls_pts_chr %>% make_list_phrase(), 
                  ".\nAdd the missing object type definition", 
                  ifelse(length(missing_cls_pts_chr) > 1, "s", 
                    ""), " by using the 'add_new_cls_pts' function."))
                pkg_setup_ls$problems_ls$missing_cls_pts_chr <- missing_cls_pts_chr
            }
        }
        if (is.null(fns_env_ls)) 
            fns_env_ls <- read_fns(make_undmtd_fns_dir_chr(path_1L_chr = paste0(pkg_setup_ls$initial_ls$path_to_pkg_rt_1L_chr, 
                "/data-raw"), drop_empty_1L_lgl = T))
        fns_dmt_tb <- make_dmt_for_all_fns(paths_ls = make_fn_nms(), 
            abbreviations_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup, 
            custom_dmt_ls = list(details_ls = NULL, inc_for_main_user_lgl_ls = list(force_true_chr = pkg_setup_ls$subsequent_ls$user_manual_fns_chr, 
                force_false_chr = NA_character_), args_ls_ls = NULL), 
            fns_env_ls = fns_env_ls, fn_types_lup = pkg_setup_ls$subsequent_ls$fn_types_lup, 
            inc_all_mthds_1L_lgl = T, object_type_lup = pkg_setup_ls$subsequent_ls$object_type_lup, 
            undocumented_fns_dir_chr = make_undmtd_fns_dir_chr(drop_empty_1L_lgl = T))
        missing_obj_types_chr <- get_new_abbrs(pkg_setup_ls, 
            classes_to_make_tb = pkg_setup_ls$subsequent_ls$cls_fn_ls$args_ls$x, 
            fns_dmt_tb = fns_dmt_tb, pkg_ds_ls_ls = pkg_setup_ls$subsequent_ls$pkg_ds_ls_ls, 
            use_last_1L_int = 1)
        if (!identical(missing_obj_types_chr, character(0))) {
            message(paste0("The following potential object type", 
                ifelse(length(missing_obj_types_chr) > 1, "s are", 
                  " is"), " neither defined nor contained in the 'treat_as_words_chr' object: \n", 
                missing_obj_types_chr %>% make_list_phrase(), 
                ".\nAdd the missing object type definition", 
                ifelse(length(missing_obj_types_chr) > 1, "s", 
                  ""), " and/or update the 'treat_as_words_chr' by using the 'write_new_obj_types' function."))
            pkg_setup_ls$problems_ls$missing_obj_types_chr <- missing_obj_types_chr
        }
        else {
            missing_abbrs_chr <- get_new_abbrs(pkg_setup_ls, 
                classes_to_make_tb = pkg_setup_ls$subsequent_ls$cls_fn_ls$args_ls$x, 
                fns_dmt_tb = fns_dmt_tb, pkg_ds_ls_ls = pkg_setup_ls$subsequent_ls$pkg_ds_ls_ls)
            if (!identical(missing_abbrs_chr, character(0))) {
                message(paste0("The following potential abbreviation", 
                  ifelse(length(missing_abbrs_chr) > 1, "s are", 
                    " is"), " neither defined nor contained in the 'treat_as_words_chr' object: \n", 
                  missing_abbrs_chr %>% make_list_phrase(), ".\nAdd the missing abbreviation definition", 
                  ifelse(length(missing_abbrs_chr) > 1, "s", 
                    ""), " and/or update the 'treat_as_words_chr' by using the 'write_new_abbrs' function"))
                pkg_setup_ls$problems_ls$missing_abbrs_chr <- missing_abbrs_chr
            }
        }
    }
    return(pkg_setup_ls)
}
