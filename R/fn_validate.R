#' Validate package setup
#' @description validate_pkg_setup() is a Validate function that validates that an object conforms to required criteria. Specifically, this function implements an algorithm to validate package setup. The function returns Package setup (a list).
#' @param pkg_setup_ls Package setup (a list)
#' @param append_1L_lgl Append (a logical vector of length one), Default: F
#' @param is_method_1L_lgl Is method (a logical vector of length one), Default: F
#' @return Package setup (a list)
#' @rdname validate_pkg_setup
#' @export 
#' @importFrom ready4 make_list_phrase
#' @importFrom purrr map_chr
#' @importFrom stringr str_sub
#' @importFrom Hmisc capitalize
#' @keywords internal
validate_pkg_setup <- function (pkg_setup_ls, append_1L_lgl = F, is_method_1L_lgl = F) 
{
    message(paste0("Validating ", ifelse(is_method_1L_lgl, "manifest", 
        "pkg_setup_ls"), ". This may take a couple of minutes."))
    pkg_setup_ls$problems_ls <- NULL
    missing_fn_types_chr <- get_new_fn_types(pkg_setup_ls)
    if (!identical(missing_fn_types_chr, character(0))) {
        message(paste0("The following function type", ifelse(length(missing_fn_types_chr) > 
            1, "s are", " is"), " not yet defined: \n", missing_fn_types_chr %>% 
            ready4::make_list_phrase(), ".\nAdd the missing definition", 
            ifelse(length(missing_fn_types_chr) > 1, "s", ""), 
            " by using the ", ifelse(is_method_1L_lgl, "'renew' method", 
                "'write_new_fn_types' function.")))
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
        if (is_method_1L_lgl) {
            test_2_1L_lgl <- identical(pkg_setup_ls$subsequent_ls$cls_fn_ls, 
                ready4fun_executor())
        }
        else {
            test_2_1L_lgl <- F
        }
        if (!(identical(pkg_setup_ls$subsequent_ls$cls_fn_ls, 
            list()) | test_2_1L_lgl)) {
            if (!is.null(pkg_setup_ls$subsequent_ls$cls_fn_ls$args_ls$x)) {
                name_pfx_1L_chr <- paste0(pkg_setup_ls$initial_ls$pkg_desc_ls$Package, 
                  "_")
                missing_class_abbrs_chr <- setdiff(paste0(purrr::map_chr(pkg_setup_ls$subsequent_ls$cls_fn_ls$args_ls$x$make_s3_lgl, 
                  ~ifelse(.x, name_pfx_1L_chr, stringr::str_sub(name_pfx_1L_chr, 
                    end = -2) %>% Hmisc::capitalize())), pkg_setup_ls$subsequent_ls$cls_fn_ls$args_ls$x$name_stub_chr), 
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
                  missing_cls_pts_chr %>% ready4::make_list_phrase(), 
                  ".\nAdd the missing class prototypes", ifelse(length(missing_cls_pts_chr) > 
                    1, "s", ""), " by using the ", ifelse(is_method_1L_lgl, 
                    "'renew' method", "'add_new_cls_pts' function.")))
                pkg_setup_ls$problems_ls$missing_cls_pts_chr <- missing_cls_pts_chr
            }
        }
        fns_env_ls <- read_fns(make_undmtd_fns_dir_chr(path_1L_chr = paste0(pkg_setup_ls$initial_ls$path_to_pkg_rt_1L_chr, 
            "/data-raw"), drop_empty_1L_lgl = T))
        pkg_setup_ls <- add_fns_dmt_tb(pkg_setup_ls, append_1L_lgl = append_1L_lgl, 
            fns_env_ls = fns_env_ls)
        missing_obj_types_chr <- get_new_abbrs(pkg_setup_ls, 
            append_1L_lgl = append_1L_lgl, classes_to_make_tb = pkg_setup_ls$subsequent_ls$cls_fn_ls$args_ls$x, 
            pkg_ds_ls_ls = pkg_setup_ls$subsequent_ls$pkg_ds_ls_ls, 
            use_last_1L_int = 1)
        if (!identical(missing_obj_types_chr, character(0))) {
            message(paste0("The following potential object type", 
                ifelse(length(missing_obj_types_chr) > 1, "s are", 
                  " is"), " neither defined nor contained in the 'treat_as_words_chr' object: \n", 
                missing_obj_types_chr %>% ready4::make_list_phrase(), 
                ".\nAdd the missing object type definition", 
                ifelse(length(missing_obj_types_chr) > 1, "s", 
                  ""), " and/or update the 'treat_as_words_chr'", 
                " by using the ", ifelse(is_method_1L_lgl, "'renew' method", 
                  "'write_new_obj_types' function.")))
            pkg_setup_ls$problems_ls$missing_obj_types_chr <- missing_obj_types_chr
        }
        else {
            missing_abbrs_chr <- get_new_abbrs(pkg_setup_ls, 
                append_1L_lgl = append_1L_lgl, classes_to_make_tb = pkg_setup_ls$subsequent_ls$cls_fn_ls$args_ls$x, 
                pkg_ds_ls_ls = pkg_setup_ls$subsequent_ls$pkg_ds_ls_ls)
            if (!identical(missing_abbrs_chr, character(0))) {
                message(paste0("The following potential abbreviation", 
                  ifelse(length(missing_abbrs_chr) > 1, "s are", 
                    " is"), " neither defined nor contained in the 'treat_as_words_chr' object: \n", 
                  missing_abbrs_chr %>% ready4::make_list_phrase(), 
                  ".\nAdd the missing abbreviation definition", 
                  ifelse(length(missing_abbrs_chr) > 1, "s", 
                    ""), " and/or update the 'treat_as_words_chr'", 
                  " by using the ", ifelse(is_method_1L_lgl, 
                    "'renew' method", "'write_new_abbrs' function.")))
                pkg_setup_ls$problems_ls$missing_abbrs_chr <- missing_abbrs_chr
            }
        }
    }
    return(pkg_setup_ls)
}
