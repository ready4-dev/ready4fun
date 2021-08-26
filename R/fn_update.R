#' Update abbreviation
#' @description update_abbr_lup() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update abbreviation lookup table. Function argument abbr_tb specifies the object to be updated. Argument short_name_chr provides the object to be updated. The function returns Abbreviation (a tibble).
#' @param abbr_tb Abbreviation (a tibble)
#' @param short_name_chr Short name (a character vector)
#' @param long_name_chr Long name (a character vector)
#' @param no_plural_chr No plural (a character vector), Default: 'NA'
#' @param custom_plural_ls Custom plural (a list), Default: NULL
#' @param pfx_rgx Prefix (a regular expression vector), Default: 'NA'
#' @return Abbreviation (a tibble)
#' @rdname update_abbr_lup
#' @export 
#' @importFrom dplyr mutate
#' @importFrom purrr map_chr
#' @importFrom stringi stri_replace_first_regex
#' @importFrom tibble tibble
update_abbr_lup <- function (abbr_tb, short_name_chr, long_name_chr, no_plural_chr = NA_character_, 
    custom_plural_ls = NULL, pfx_rgx = NA_character_) 
{
    if (!"plural_lgl" %in% names(abbr_tb)) 
        abbr_tb <- dplyr::mutate(abbr_tb, plural_lgl = NA)
    if (!is.na(pfx_rgx)) 
        abbr_tb <- abbr_tb %>% dplyr::mutate(long_name_chr = purrr::map_chr(long_name_chr, 
            ~stringi::stri_replace_first_regex(.x, pfx_rgx, "")))
    new_tb <- tibble::tibble(short_name_chr = short_name_chr, 
        long_name_chr = long_name_chr) %>% add_plurals_to_abbr_lup(no_plural_chr = no_plural_chr, 
        custom_plural_ls = custom_plural_ls)
    abbr_tb <- add_lups(abbr_tb, new_lup = new_tb, key_var_nm_1L_chr = "short_name_chr")
    return(abbr_tb)
}
#' Update first word case
#' @description update_first_word_case() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update first word case. Function argument phrase_1L_chr specifies the object to be updated. Argument fn provides the object to be updated. The function returns Phrase (a character vector of length one).
#' @param phrase_1L_chr Phrase (a character vector of length one)
#' @param fn Function (a function), Default: tolower
#' @return Phrase (a character vector of length one)
#' @rdname update_first_word_case
#' @export 
#' @importFrom stringr str_sub
update_first_word_case <- function (phrase_1L_chr, fn = tolower) 
{
    phrase_1L_chr <- paste0(phrase_1L_chr %>% stringr::str_sub(end = 1) %>% 
        fn, phrase_1L_chr %>% stringr::str_sub(start = 2))
    return(phrase_1L_chr)
}
#' Update function documentation
#' @description update_fn_dmt() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update function documentation. Function argument fn_tags_spine_ls specifies the object to be updated. Argument new_tag_chr_ls provides the object to be updated. The function returns Function documentation (a character vector of length one).
#' @param fn_tags_spine_ls Function tags spine (a list)
#' @param new_tag_chr_ls New tag (a list of character vectors)
#' @param fn_name_1L_chr Function name (a character vector of length one)
#' @param fn_type_1L_chr Function type (a character vector of length one)
#' @param import_chr Import (a character vector)
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @return Function documentation (a character vector of length one)
#' @rdname update_fn_dmt
#' @export 
#' @importFrom utils data
#' @importFrom stringr str_replace str_c str_sub str_locate
#' @importFrom purrr reduce
update_fn_dmt <- function (fn_tags_spine_ls, new_tag_chr_ls, fn_name_1L_chr, fn_type_1L_chr, 
    import_chr, abbreviations_lup = NULL) 
{
    if (is.null(abbreviations_lup)) 
        utils::data("abbreviations_lup", package = "ready4fun", 
            envir = environment())
    fn_dmt_1L_chr <- fn_tags_spine_ls$fn_tags_1L_chr
    fn_dmt_1L_chr <- fn_dmt_1L_chr %>% stringr::str_replace("FUNCTION_TITLE", 
        fn_name_1L_chr) %>% stringr::str_replace("FUNCTION_DESCRIPTION", 
        paste0(ifelse(is.na(new_tag_chr_ls$desc_start_1L_chr), 
            "FUNCTION_DESCRIPTION", new_tag_chr_ls$desc_start_1L_chr), 
            ifelse((fn_type_1L_chr %in% c("fn", "gen_std_s3_mthd", 
                "meth_std_s3_mthd", "gen_std_s4_mthd", "meth_std_s4_mthd") | 
                startsWith(fn_type_1L_chr, "s3_")), "", fn_tags_spine_ls$ref_slot_1L_chr))) %>% 
        stringr::str_replace("OUTPUT_DESCRIPTION", new_tag_chr_ls$output_txt_1L_chr)
    fn_dmt_1L_chr <- fn_dmt_1L_chr %>% stringr::str_replace("@details DETAILS", 
        ifelse(fn_type_1L_chr == "s3_valid_instance" | ifelse(is.na(new_tag_chr_ls$fn_det_1L_chr), 
            F, new_tag_chr_ls$fn_det_1L_chr != "DETAILS"), paste0("@details ", 
            new_tag_chr_ls$fn_det_1L_chr), ""))
    if (!is.null(new_tag_chr_ls$arg_desc_chr)) {
        fn_dmt_1L_chr <- purrr::reduce(1:length(new_tag_chr_ls$arg_desc_chr), 
            .init = fn_dmt_1L_chr, ~{
                stringr::str_replace(.x, paste0("@param ", names(new_tag_chr_ls$arg_desc_chr)[.y], 
                  " PARAM_DESCRIPTION"), paste0("@param ", names(new_tag_chr_ls$arg_desc_chr)[.y], 
                  " ", ifelse(new_tag_chr_ls$arg_desc_chr[.y] == 
                    "NO MATCH", ifelse(names(new_tag_chr_ls$arg_desc_chr[.y]) != 
                    "x", "PARAM_DESCRIPTION", "An object"), new_tag_chr_ls$arg_desc_chr[.y])))
            })
    }
    fn_dmt_1L_chr <- fn_dmt_1L_chr %>% stringr::str_replace("@param ... PARAM_DESCRIPTION", 
        paste0("@param ... ", "Additional arguments"))
    if (!is.null(new_tag_chr_ls$s3_class_main_1L_chr)) {
        if (fn_type_1L_chr == "s3_valid_instance") {
            fn_dmt_1L_chr <- stringr::str_replace(fn_dmt_1L_chr, 
                names(new_tag_chr_ls$s3_class_main_1L_chr), new_tag_chr_ls$s3_class_main_1L_chr)
        }
        else {
            fn_dmt_1L_chr <- fn_dmt_1L_chr %>% stringr::str_replace(names(new_tag_chr_ls$s3_class_main_1L_chr), 
                paste0(make_fn_title(names(new_tag_chr_ls$s3_class_main_1L_chr), 
                  object_type_lup = abbreviations_lup, abbreviations_lup = abbreviations_lup), 
                  " ", get_arg_obj_type(new_tag_chr_ls$s3_class_main_1L_chr, 
                    object_type_lup = abbreviations_lup)))
        }
    }
    if (!is.na(import_chr)) 
        fn_dmt_1L_chr <- paste0(fn_dmt_1L_chr, "\n#' @import ", 
            stringr::str_c(import_chr, collapse = " "))
    if (fn_type_1L_chr == "gen_std_s3_mthd") {
        fn_dmt_1L_chr <- stringr::str_replace(fn_dmt_1L_chr, 
            paste0("@name ", fn_name_1L_chr), paste0("@rdname ", 
                fn_name_1L_chr, "-methods"))
    }
    if (fn_type_1L_chr == "meth_std_s3_mthd") {
        fn_dmt_1L_chr <- stringr::str_replace(fn_dmt_1L_chr, 
            paste0("@rdname ", fn_name_1L_chr), paste0("@rdname ", 
                fn_name_1L_chr %>% stringr::str_sub(end = -1 + 
                  stringr::str_locate(fn_name_1L_chr, "\\.")[1, 
                    1] %>% as.vector()), "-methods"))
    }
    return(fn_dmt_1L_chr)
}
#' Update function documentation with slots
#' @description update_fn_dmt_with_slots() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update function documentation with slots. Function argument fn_name_1L_chr specifies the object to be updated. Argument fn_dmt_1L_chr provides the object to be updated. The function returns Function documentation (a character vector of length one).
#' @param fn_name_1L_chr Function name (a character vector of length one)
#' @param fn_dmt_1L_chr Function documentation (a character vector of length one)
#' @return Function documentation (a character vector of length one)
#' @rdname update_fn_dmt_with_slots
#' @export 
#' @importFrom purrr reduce
#' @importFrom stringr str_replace
update_fn_dmt_with_slots <- function (fn_name_1L_chr, fn_dmt_1L_chr) 
{
    slots_chr <- get_r4_obj_slots(fn_name_1L_chr)
    fn_dmt_1L_chr <- purrr::reduce(1:length(slots_chr), .init = fn_dmt_1L_chr, 
        ~.x %>% stringr::str_replace(paste0(names(slots_chr)[.y], 
            " PARAM_DESCRIPTION"), paste0(names(slots_chr)[.y], 
            " ", slots_chr[.y])))
    return(fn_dmt_1L_chr)
}
#' Update functions documentation
#' @description update_fns_dmt_tb() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update functions documentation tibble. Function argument fns_dmt_tb specifies the object to be updated. Argument title_ls provides the object to be updated. The function returns Functions documentation (a tibble).
#' @param fns_dmt_tb Functions documentation (a tibble)
#' @param title_ls Title (a list), Default: NULL
#' @param desc_ls Description (a list), Default: NULL
#' @param details_ls Details (a list), Default: NULL
#' @param inc_for_main_user_lgl_ls Include for main user (a list of logical vectors), Default: NULL
#' @param output_ls Output (a list), Default: NULL
#' @param example_ls Example (a list), Default: NULL
#' @param args_ls_ls Arguments (a list of lists), Default: NULL
#' @param append_1L_lgl Append (a logical vector of length one), Default: T
#' @return Functions documentation (a tibble)
#' @rdname update_fns_dmt_tb
#' @export 
#' @importFrom purrr map_lgl reduce map
#' @importFrom rlang exec
update_fns_dmt_tb <- function (fns_dmt_tb, title_ls = NULL, desc_ls = NULL, details_ls = NULL, 
    inc_for_main_user_lgl_ls = NULL, output_ls = NULL, example_ls = NULL, 
    args_ls_ls = NULL, append_1L_lgl = T) 
{
    lgl_vecs_ls <- list(chr_vars_to_upd_lgl = list(title_ls, 
        desc_ls, details_ls, output_ls) %>% purrr::map_lgl(~!is.null(.x)), 
        lgl_vars_to_upd_lgl = list(inc_for_main_user_lgl_ls, 
            example_ls) %>% purrr::map_lgl(~!is.null(.x)), arg_ls_to_upd_lgl = !is.null(args_ls_ls))
    input_ls_ls <- list(chr_input_ls = list(variable_chr = c("title_chr", 
        "desc_chr", "details_chr", "output_chr"), data_chr = c("title_ls", 
        "desc_ls", "details_ls", "output_ls")), lgl_input_ls = list(variable_chr = c("inc_for_main_user_lgl", 
        "example_lgl"), data_chr = c("inc_for_main_user_lgl_ls", 
        "example_ls")), ls_input_ls = list(variable_chr = c("args_ls"), 
        data_chr = c("args_ls_ls")))
    fns_dmt_tb <- purrr::reduce(1:3, .init = fns_dmt_tb, ~{
        updated_fns_dmt_tb <- .x
        idx_1L_dbl <- .y
        fn <- list(update_fns_dmt_tb_chr_vars, update_fns_dmt_tb_lgl_vars, 
            update_fns_dmt_tb_ls_vars)[[idx_1L_dbl]]
        if (any(lgl_vecs_ls[[idx_1L_dbl]])) {
            input_ls <- input_ls_ls[[idx_1L_dbl]] %>% purrr::map(~.x[lgl_vecs_ls[[idx_1L_dbl]]])
            updated_fns_dmt_tb <- purrr::reduce(1:length(lgl_vecs_ls[[idx_1L_dbl]]), 
                .init = updated_fns_dmt_tb, ~{
                  eval(parse(text = paste0("new_ls <- ", input_ls[[2]])))
                  args_ls <- list(.x, data_1L_chr = input_ls[[1]], 
                    new_ls = new_ls, append_1L_lgl = append_1L_lgl)
                  if (idx_1L_dbl == 2) 
                    args_ls$append_1L_lgl <- NULL
                  rlang::exec(fn, !!!args_ls)
                })
        }
        updated_fns_dmt_tb
    })
    return(fns_dmt_tb)
}
#' Update functions documentation tibble character vector variables
#' @description update_fns_dmt_tb_chr_vars() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update functions documentation tibble character vector variables. Function argument fns_dmt_tb specifies the object to be updated. Argument data_1L_chr provides the object to be updated. The function returns Functions documentation (a tibble).
#' @param fns_dmt_tb Functions documentation (a tibble)
#' @param data_1L_chr Data (a character vector of length one)
#' @param new_ls New (a list)
#' @param append_1L_lgl Append (a logical vector of length one)
#' @return Functions documentation (a tibble)
#' @rdname update_fns_dmt_tb_chr_vars
#' @export 
#' @importFrom dplyr mutate case_when
#' @importFrom rlang sym
#' @importFrom purrr map_chr
update_fns_dmt_tb_chr_vars <- function (fns_dmt_tb, data_1L_chr, new_ls, append_1L_lgl) 
{
    if (is.na(data_1L_chr)) {
        fns_dmt_tb <- fns_dmt_tb
    }
    else {
        fns_dmt_tb <- dplyr::mutate(fns_dmt_tb, `:=`(!!rlang::sym(data_1L_chr), 
            dplyr::case_when(fns_chr %in% names(new_ls) ~ paste0(ifelse(append_1L_lgl, 
                paste0(ifelse(is.na(!!rlang::sym(data_1L_chr)), 
                  "", !!rlang::sym(data_1L_chr)), ""), ""), fns_chr %>% 
                purrr::map_chr(~{
                  ifelse(.x %in% names(new_ls), new_ls[[.x]], 
                    NA_character_)
                })), TRUE ~ !!rlang::sym(data_1L_chr))))
    }
    return(fns_dmt_tb)
}
#' Update functions documentation tibble logical vector variables
#' @description update_fns_dmt_tb_lgl_vars() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update functions documentation tibble logical vector variables. Function argument fns_dmt_tb specifies the object to be updated. Argument data_1L_chr provides the object to be updated. The function returns Functions documentation (a tibble).
#' @param fns_dmt_tb Functions documentation (a tibble)
#' @param data_1L_chr Data (a character vector of length one)
#' @param new_ls New (a list)
#' @return Functions documentation (a tibble)
#' @rdname update_fns_dmt_tb_lgl_vars
#' @export 
#' @importFrom dplyr mutate case_when
#' @importFrom rlang sym
update_fns_dmt_tb_lgl_vars <- function (fns_dmt_tb, data_1L_chr, new_ls) 
{
    if (is.na(data_1L_chr)) {
        fns_dmt_tb <- fns_dmt_tb
    }
    else {
        fns_dmt_tb <- dplyr::mutate(fns_dmt_tb, `:=`(!!rlang::sym(data_1L_chr), 
            dplyr::case_when(fns_chr %in% new_ls$force_true_chr ~ 
                T, fns_chr %in% new_ls$force_false_chr ~ F, TRUE ~ 
                !!rlang::sym(data_1L_chr))))
    }
    return(fns_dmt_tb)
}
#' Update functions documentation tibble list variables
#' @description update_fns_dmt_tb_ls_vars() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update functions documentation tibble list variables. Function argument fns_dmt_tb specifies the object to be updated. Argument data_1L_chr provides the object to be updated. The function returns Functions documentation (a tibble).
#' @param fns_dmt_tb Functions documentation (a tibble)
#' @param data_1L_chr Data (a character vector of length one)
#' @param new_ls New (a list)
#' @param append_1L_lgl Append (a logical vector of length one)
#' @return Functions documentation (a tibble)
#' @rdname update_fns_dmt_tb_ls_vars
#' @export 
#' @importFrom dplyr mutate case_when
#' @importFrom rlang sym
#' @importFrom purrr map2 map2_chr map_chr
#' @importFrom testit assert
#' @importFrom stats setNames
update_fns_dmt_tb_ls_vars <- function (fns_dmt_tb, data_1L_chr, new_ls, append_1L_lgl) 
{
    if (is.na(data_1L_chr)) {
        fns_dmt_tb <- fns_dmt_tb
    }
    else {
        fns_dmt_tb <- dplyr::mutate(fns_dmt_tb, `:=`(!!rlang::sym(data_1L_chr), 
            dplyr::case_when(fns_chr %in% names(new_ls) ~ purrr::map2(new_ls[names(new_ls) %in% 
                fns_chr], names(new_ls)[names(new_ls) %in% fns_chr], 
                ~{
                  fn_args_chr <- .x
                  fn_nm_1L_chr <- .y
                  old_args_chr <- fns_dmt_tb$args_ls[fns_dmt_tb$fns_chr == 
                    fn_nm_1L_chr][[1]]
                  if (!append_1L_lgl) 
                    testit::assert("When not appending, each function whose argument description text is being updated must have new argument descriptions for ALL arguments.", 
                      ifelse(length(old_args_chr) == length(fn_args_chr), 
                        names(old_args_chr) %>% sort() == names(fn_args_chr) %>% 
                          sort(), F))
                  new_args_chr <- purrr::map2_chr(fn_args_chr, 
                    names(fn_args_chr), ~{
                      if (append_1L_lgl) {
                        paste0(old_args_chr[.y], ". ", .x)
                      }
                      else {
                        .x
                      }
                    })
                  purrr::map_chr(names(old_args_chr), ~ifelse(.x %in% 
                    names(new_args_chr), new_args_chr[.x], old_args_chr[.x])) %>% 
                    stats::setNames(names(old_args_chr))
                }), TRUE ~ !!rlang::sym(data_1L_chr))))
    }
    return(fns_dmt_tb)
}
#' Update namespace
#' @description update_ns() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update namespace. Function argument package_1L_chr specifies the object to be updated. The function returns Package name (a character vector).
#' @param package_1L_chr Package (a character vector of length one)
#' @return Package name (a character vector)
#' @rdname update_ns
#' @export 

update_ns <- function (package_1L_chr) 
{
    package_nm_chr <- ifelse(package_1L_chr == "", ".GlobalEnv", 
        package_1L_chr)
    return(package_nm_chr)
}
