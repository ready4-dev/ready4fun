#' Update abbreviation lookup table
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
#' @importFrom testit assert
#' @importFrom ready4 make_list_phrase add_lups
#' @importFrom dplyr mutate
#' @importFrom purrr map_chr
#' @importFrom stringi stri_replace_first_regex
#' @importFrom tibble tibble
update_abbr_lup <- function (abbr_tb, short_name_chr, long_name_chr, no_plural_chr = NA_character_, 
    custom_plural_ls = NULL, pfx_rgx = NA_character_) 
{
    testit::assert(paste0("No duplicates are allowed in an abbreviations lookup table. The following duplicates are in the short_name_chr column:\n", 
        abbr_tb$short_name_chr[duplicated(abbr_tb$short_name_chr)] %>% 
            ready4::make_list_phrase()), !any(duplicated(abbr_tb$short_name_chr)))
    testit::assert(paste0("No duplicates are allowed in an abbreviations lookup table. The following duplicates are in the long_name_chr column:\n", 
        abbr_tb$long_name_chr[duplicated(abbr_tb$long_name_chr)] %>% 
            ready4::make_list_phrase()), !any(duplicated(abbr_tb$long_name_chr)))
    if (!"plural_lgl" %in% names(abbr_tb)) 
        abbr_tb <- dplyr::mutate(abbr_tb, plural_lgl = NA)
    if (!is.na(pfx_rgx)) 
        abbr_tb <- abbr_tb %>% dplyr::mutate(long_name_chr = purrr::map_chr(long_name_chr, 
            ~stringi::stri_replace_first_regex(.x, pfx_rgx, "")))
    new_tb <- tibble::tibble(short_name_chr = short_name_chr, 
        long_name_chr = long_name_chr) %>% add_plurals_to_abbr_lup(no_plural_chr = no_plural_chr, 
        custom_plural_ls = custom_plural_ls)
    abbr_tb <- ready4::add_lups(abbr_tb, new_lup = new_tb, key_var_nm_1L_chr = "short_name_chr")
    return(abbr_tb)
}
#' Update abbreviations
#' @description update_abbrs() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update abbreviations. Function argument pkg_setup_ls specifies the object to be updated. Argument short_name_chr provides the object to be updated. The function returns Package setup (a list).
#' @param pkg_setup_ls Package setup (a list)
#' @param short_name_chr Short name (a character vector)
#' @param long_name_chr Long name (a character vector)
#' @param no_plural_chr No plural (a character vector), Default: 'NA'
#' @param custom_plural_ls Custom plural (a list), Default: NULL
#' @param pfx_rgx Prefix (a regular expression vector), Default: 'NA'
#' @return Package setup (a list)
#' @rdname update_abbrs
#' @export 
#' @importFrom testit assert
#' @importFrom ready4 make_list_phrase
#' @importFrom dplyr filter
#' @keywords internal
update_abbrs <- function (pkg_setup_ls, short_name_chr, long_name_chr, no_plural_chr = NA_character_, 
    custom_plural_ls = NULL, pfx_rgx = NA_character_) 
{
    short_dupls_chr <- intersect(short_name_chr, pkg_setup_ls$subsequent_ls$abbreviations_lup$short_name_chr)
    long_dupls_chr <- intersect(long_name_chr, pkg_setup_ls$subsequent_ls$abbreviations_lup$long_name_chr)
    testit::assert(paste0("No duplicates are allowed in the abbreviations lookup table. You are attempting to add the following duplicate values to the short_name_chr column:\n", 
        short_dupls_chr %>% ready4::make_list_phrase()), identical(short_dupls_chr, 
        character(0)))
    testit::assert(paste0("No duplicates are allowed in the abbreviations lookup table. You are attempting to add the following duplicate values from the 'long_name_chr' argument to the long_name_chr column of the abbreviations lookup tbale:\n", 
        long_dupls_chr %>% ready4::make_list_phrase()), identical(long_dupls_chr, 
        character(0)))
    if (is.null(pkg_setup_ls$subsequent_ls$abbreviations_lup)) 
        pkg_setup_ls$subsequent_ls$abbreviations_lup <- make_obj_lup(obj_lup_spine = make_obj_lup_spine(NULL)) %>% 
            dplyr::filter(F)
    pkg_setup_ls$subsequent_ls$abbreviations_lup <- pkg_setup_ls$subsequent_ls$abbreviations_lup %>% 
        update_abbr_lup(short_name_chr = short_name_chr, long_name_chr = long_name_chr, 
            no_plural_chr = no_plural_chr, custom_plural_ls = custom_plural_ls, 
            pfx_rgx = pfx_rgx)
    return(pkg_setup_ls)
}
#' Update first word case
#' @description update_first_word_case() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update first word case. Function argument phrase_1L_chr specifies the object to be updated. Argument fn provides the object to be updated. The function returns Phrase (a character vector of length one).
#' @param phrase_1L_chr Phrase (a character vector of length one)
#' @param fn Function (a function), Default: tolower
#' @return Phrase (a character vector of length one)
#' @rdname update_first_word_case
#' @export 
#' @importFrom stringr str_sub
#' @keywords internal
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
#' @param import_from_chr Import from (a character vector), Default: 'NA'
#' @param abbreviations_lup Abbreviations (a lookup table)
#' @param fn_types_lup Function types (a lookup table)
#' @return Function documentation (a character vector of length one)
#' @rdname update_fn_dmt
#' @export 
#' @importFrom stringr str_replace str_c str_sub str_locate
#' @importFrom purrr reduce pluck map_lgl flatten_chr
#' @keywords internal
update_fn_dmt <- function (fn_tags_spine_ls, new_tag_chr_ls, fn_name_1L_chr, fn_type_1L_chr, 
    import_chr, import_from_chr = NA_character_, abbreviations_lup, 
    fn_types_lup) 
{
    fn_dmt_1L_chr <- fn_tags_spine_ls$fn_tags_1L_chr
    fn_dmt_1L_chr <- fn_dmt_1L_chr %>% stringr::str_replace(pattern = "FUNCTION_TITLE", 
        replacement = fn_name_1L_chr) %>% stringr::str_replace("FUNCTION_DESCRIPTION", 
        paste0(ifelse(is.na(new_tag_chr_ls$desc_start_1L_chr), 
            "FUNCTION_DESCRIPTION", new_tag_chr_ls$desc_start_1L_chr), 
            ifelse((fn_type_1L_chr %in% c("fn", "gen_std_s3_mthd", 
                "meth_std_s3_mthd", "gen_std_s4_mthd", "meth_std_s4_mthd") | 
                startsWith(fn_type_1L_chr, "s3_")), "", fn_tags_spine_ls$ref_slot_1L_chr))) %>% 
        stringr::str_replace("OUTPUT_DESCRIPTION", new_tag_chr_ls$output_txt_1L_chr)
    fn_dmt_1L_chr <- fn_dmt_1L_chr %>% stringr::str_replace("@details DETAILS", 
        ifelse(fn_type_1L_chr == "s3_valid_instance" | ifelse(is.na(new_tag_chr_ls$fn_det_1L_chr), 
            F, ifelse(fn_type_1L_chr %in% c("s3_prototype", "s3_checker"), 
                F, new_tag_chr_ls$fn_det_1L_chr != "DETAILS")), 
            paste0("@details ", new_tag_chr_ls$fn_det_1L_chr), 
            ""))
    if (!is.null(new_tag_chr_ls$arg_desc_chr)) {
        fn_dmt_1L_chr <- purrr::reduce(1:length(new_tag_chr_ls$arg_desc_chr), 
            .init = fn_dmt_1L_chr, ~{
                stringr::str_replace(.x, paste0("@param ", names(new_tag_chr_ls$arg_desc_chr)[.y], 
                  " PARAM_DESCRIPTION"), paste0("@param ", names(new_tag_chr_ls$arg_desc_chr)[.y], 
                  " ", ifelse(new_tag_chr_ls$arg_desc_chr[.y] == 
                    "NO MATCH", ifelse(!names(new_tag_chr_ls$arg_desc_chr[.y]) %in% 
                    c(letters, LETTERS), "PARAM_DESCRIPTION", 
                    "An object"), new_tag_chr_ls$arg_desc_chr[.y])))
            })
    }
    fn_dmt_1L_chr <- fn_dmt_1L_chr %>% stringr::str_replace("@param \\... PARAM_DESCRIPTION", 
        paste0("@param ... ", "Additional arguments"))
    if (!is.null(new_tag_chr_ls$s3_class_main_1L_chr)) {
        if (fn_type_1L_chr == "s3_valid_instance") {
            fn_dmt_1L_chr <- stringr::str_replace(fn_dmt_1L_chr, 
                names(new_tag_chr_ls$s3_class_main_1L_chr), stringr::str_replace(new_tag_chr_ls$s3_class_main_1L_chr, 
                  pattern = "ready4 S3 class ", replacement = ""))
        }
        else {
            fn_dmt_1L_chr <- fn_dmt_1L_chr %>% stringr::str_replace(names(new_tag_chr_ls$s3_class_main_1L_chr), 
                paste0(make_fn_title(names(new_tag_chr_ls$s3_class_main_1L_chr), 
                  object_type_lup = abbreviations_lup, fn_types_lup = fn_types_lup, 
                  abbreviations_lup = abbreviations_lup), " ", 
                  get_arg_obj_type(new_tag_chr_ls$s3_class_main_1L_chr, 
                    object_type_lup = abbreviations_lup)))
        }
    }
    split_fn_dmt_chr <- fn_dmt_1L_chr %>% strsplit("\n") %>% 
        purrr::pluck(1)
    if (!is.na(import_chr)) {
        import_idx_1L_int <- which(startsWith(split_fn_dmt_chr, 
            "#' @import "))
        if (import_idx_1L_int != 0) {
            import_txt_1L_chr <- split_fn_dmt_chr[import_idx_1L_int]
            import_txt_1L_chr <- paste0(import_txt_1L_chr, paste0(" ", 
                import_chr[import_chr %>% purrr::map_lgl(~!.x %in% 
                  (import_txt_1L_chr %>% strsplit(" ") %>% purrr::pluck(1)))]))
            split_fn_dmt_chr[import_idx_1L_int] <- import_txt_1L_chr
        }
        else {
            import_txt_1L_chr <- paste0("#' @import ", stringr::str_c(import_chr, 
                collapse = " "))
            split_fn_dmt_chr <- c(split_fn_dmt_chr, import_txt_1L_chr)
        }
    }
    gnrc_part_1L_chr <- fn_name_1L_chr %>% strsplit("\\.") %>% 
        purrr::flatten_chr() %>% purrr::pluck(1)
    if (gnrc_part_1L_chr %in% names(import_from_chr) & fn_type_1L_chr %in% 
        c("meth_std_s3_mthd", "meth_std_s4_mthd")) {
        ns_1L_chr <- unname(import_from_chr[names(import_from_chr) == 
            gnrc_part_1L_chr])
        import_idx_1L_int <- which(startsWith(split_fn_dmt_chr, 
            paste0("#' @importFrom ", ns_1L_chr, " ")))
        if (!identical(import_idx_1L_int, integer(0))) {
            import_txt_1L_chr <- split_fn_dmt_chr[import_idx_1L_int]
            import_txt_1L_chr <- paste0(import_txt_1L_chr, ifelse(gnrc_part_1L_chr %in% 
                (import_txt_1L_chr %>% strsplit(" ") %>% purrr::pluck(1)), 
                "", paste0(" ", gnrc_part_1L_chr)))
            split_fn_dmt_chr[import_idx_1L_int] <- import_txt_1L_chr
        }
        else {
            import_txt_1L_chr <- paste0("#' @importFrom ", ns_1L_chr, 
                " ", names(import_from_chr)[names(import_from_chr) == 
                  gnrc_part_1L_chr])
            split_fn_dmt_chr <- c(split_fn_dmt_chr, import_txt_1L_chr)
        }
    }
    if (fn_type_1L_chr %in% c("s3_prototype", "s3_checker")) {
        desc_idx_1L_int <- which(startsWith(split_fn_dmt_chr, 
            "#' @description "))
        split_fn_dmt_chr <- split_fn_dmt_chr[-desc_idx_1L_int]
        rd_nm_idx_1L_int <- which(startsWith(split_fn_dmt_chr, 
            "#' @rdname "))
        split_fn_dmt_chr[rd_nm_idx_1L_int] <- stringr::str_replace(split_fn_dmt_chr[rd_nm_idx_1L_int], 
            pattern = ifelse(fn_type_1L_chr == "s3_prototype", 
                "make_pt_", "is_"), replacement = "")
    }
    fn_dmt_1L_chr <- paste0(split_fn_dmt_chr, collapse = "\n")
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
    if (fn_type_1L_chr %in% c("s3_unvalidated_instance", "s3_validator")) 
        fn_dmt_1L_chr <- paste0(fn_dmt_1L_chr, "\n#' @keywords internal")
    return(fn_dmt_1L_chr)
}
#' Update function documentation with slots
#' @description update_fn_dmt_with_slots() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update function documentation with slots. Function argument fn_name_1L_chr specifies the object to be updated. Argument fn_dmt_1L_chr provides the object to be updated. The function returns Function documentation (a character vector of length one).
#' @param fn_name_1L_chr Function name (a character vector of length one)
#' @param fn_dmt_1L_chr Function documentation (a character vector of length one)
#' @return Function documentation (a character vector of length one)
#' @rdname update_fn_dmt_with_slots
#' @export 
#' @importFrom ready4 get_r4_obj_slots
#' @importFrom purrr reduce
#' @importFrom stringr str_replace
#' @keywords internal
update_fn_dmt_with_slots <- function (fn_name_1L_chr, fn_dmt_1L_chr) 
{
    slots_chr <- ready4::get_r4_obj_slots(fn_name_1L_chr)
    fn_dmt_1L_chr <- purrr::reduce(1:length(slots_chr), .init = fn_dmt_1L_chr, 
        ~.x %>% stringr::str_replace(paste0(names(slots_chr)[.y], 
            " PARAM_DESCRIPTION"), paste0(names(slots_chr)[.y], 
            " ", slots_chr[.y])))
    return(fn_dmt_1L_chr)
}
#' Update functions documentation tibble
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' Update missing abbreviations
#' @description update_msng_abbrs() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update missing abbreviations. Function argument pkg_setup_ls specifies the object to be updated. Argument are_words_chr provides the object to be updated. The function returns Package setup (a list).
#' @param pkg_setup_ls Package setup (a list)
#' @param are_words_chr Are words (a character vector), Default: 'NA'
#' @param tf_to_singular_chr Transform to singular (a character vector), Default: 'NA'
#' @param not_obj_type_chr Not object type (a character vector), Default: 'NA'
#' @return Package setup (a list)
#' @rdname update_msng_abbrs
#' @export 
#' @importFrom testit assert
#' @importFrom purrr discard
update_msng_abbrs <- function (pkg_setup_ls, are_words_chr = NA_character_, tf_to_singular_chr = NA_character_, 
    not_obj_type_chr = NA_character_) 
{
    if (!is.null(pkg_setup_ls$problems_ls$missing_abbrs_chr)) {
        if (!is.na(tf_to_singular_chr[1])) {
            testit::assert("'tf_to_singular_chr' needs to be a named vector. The name of each vector element should be the desired new name for that element.", 
                length(names(tf_to_singular_chr) %>% purrr::discard(~.x == 
                  "")) == length(tf_to_singular_chr %>% purrr::discard(is.na)))
            pkg_setup_ls$problems_ls$missing_abbrs_chr <- c(setdiff(pkg_setup_ls$problems_ls$missing_abbrs_chr, 
                tf_to_singular_chr), names(tf_to_singular_chr)) %>% 
                unique() %>% sort()
        }
        pkg_setup_ls$problems_ls$missing_abbrs_chr <- setdiff(pkg_setup_ls$problems_ls$missing_abbrs_chr, 
            are_words_chr)
        pkg_setup_ls$problems_ls$missing_words_chr <- are_words_chr
    }
    if (!is.null(pkg_setup_ls$problems_ls$missing_obj_types_chr)) {
        pkg_setup_ls$problems_ls$missing_obj_types_chr <- setdiff(pkg_setup_ls$problems_ls$missing_obj_types_chr, 
            c(not_obj_type_chr, are_words_chr)) %>% unique() %>% 
            sort()
        pkg_setup_ls$problems_ls$missing_words_chr <- are_words_chr
    }
    return(pkg_setup_ls)
}
#' Update namespace
#' @description update_ns() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update namespace. Function argument package_1L_chr specifies the object to be updated. The function returns Package name (a character vector).
#' @param package_1L_chr Package (a character vector of length one)
#' @return Package name (a character vector)
#' @rdname update_ns
#' @export 
#' @keywords internal
update_ns <- function (package_1L_chr) 
{
    package_nm_chr <- ifelse(package_1L_chr == "", ".GlobalEnv", 
        package_1L_chr)
    return(package_nm_chr)
}
#' Update package setup messages
#' @description update_pkg_setup_msgs() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update package setup messages. Function argument pkg_setup_ls specifies the object to be updated. Argument list_element_1L_chr provides the object to be updated. The function returns Package setup (a list).
#' @param pkg_setup_ls Package setup (a list)
#' @param list_element_1L_chr List element (a character vector of length one)
#' @return Package setup (a list)
#' @rdname update_pkg_setup_msgs
#' @export 
#' @keywords internal
update_pkg_setup_msgs <- function (pkg_setup_ls, list_element_1L_chr) 
{
    pkg_setup_ls$problems_ls[[which(names(pkg_setup_ls$problems_ls) == 
        list_element_1L_chr)]] <- NULL
    if (length(pkg_setup_ls$problems_ls) == 0) 
        pkg_setup_ls[[which(names(pkg_setup_ls) == "problems_ls")]] <- NULL
    return(pkg_setup_ls)
}
