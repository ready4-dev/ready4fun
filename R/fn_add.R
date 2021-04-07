#' Add indefinite article to item
#' @description add_indef_artl_to_item() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add indefinite article to item. Function argument phrase_chr specifies the object to be updated. The function returns Indefinite item (a character vector).
#' @param phrase_chr Phrase (a character vector)
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param ignore_phrs_not_in_lup_1L_lgl Ignore phrases not in lookup table (a logical vector of length one), Default: T
#' @return Indefinite item (a character vector)
#' @rdname add_indef_artl_to_item
#' @export 
#' @importFrom utils data
#' @importFrom purrr map_chr
#' @importFrom stringr str_sub
#' @keywords internal
add_indef_artl_to_item <- function (phrase_chr, abbreviations_lup = NULL, ignore_phrs_not_in_lup_1L_lgl = T) 
{
    if (is.null(abbreviations_lup)) 
        utils::data("abbreviations_lup", package = "ready4fun", 
            envir = environment())
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
            get_from_lup_obj(abbreviations_lup, match_var_nm_1L_chr = "long_name_chr", 
                match_value_xx = prefix_chr, target_var_nm_1L_chr = "plural_lgl", 
                evaluate_lgl = F), ignore_phrs_not_in_lup_1L_lgl)
        ifelse(is.na(plural_1L_lgl), .x, ifelse(plural_1L_lgl, 
            .x, ifelse(.x %>% tolower() %>% stringr::str_sub(end = 1) %in% 
                c("a", "e", "i", "o", "u"), paste0("an ", .x), 
                paste0("a ", .x))))
    })
    return(indefinite_item_chr)
}
#' Add indefinite articles to phrases
#' @description add_indefartls_to_phrases() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add indefinite articles to phrases. Function argument abbreviated_phrase_1L_chr specifies the object to be updated. The function returns Phrases (a character vector).
#' @param abbreviated_phrase_1L_chr Abbreviated phrase (a character vector of length one)
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param ignore_phrs_not_in_lup_1L_lgl Ignore phrases not in lookup table (a logical vector of length one), Default: T
#' @return Phrases (a character vector)
#' @rdname add_indefartls_to_phrases
#' @export 
#' @importFrom utils data
#' @importFrom purrr map_chr discard
#' @keywords internal
add_indefartls_to_phrases <- function (abbreviated_phrase_1L_chr, abbreviations_lup = NULL, 
    ignore_phrs_not_in_lup_1L_lgl = T) 
{
    if (is.null(abbreviations_lup)) 
        utils::data("abbreviations_lup", package = "ready4fun", 
            envir = environment())
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
#' Add lups
#' @description add_lups() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add lups. Function argument template_lup specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param template_lup Template (a lookup table)
#' @param new_lup New (a lookup table)
#' @param key_var_nm_1L_chr Key var name (a character vector of length one)
#' @param priority_lup_for_dupls_1L_chr Priority lookup table for dupls (a character vector of length one), Default: 'template'
#' @return NA ()
#' @rdname add_lups
#' @export 
#' @importFrom testit assert
#' @importFrom dplyr filter pull bind_rows arrange
#' @importFrom rlang sym
#' @importFrom Hmisc label
#' @keywords internal
add_lups <- function (template_lup, new_lup, key_var_nm_1L_chr, priority_lup_for_dupls_1L_chr = "template") 
{
    testit::assert("Look up tables must have same column names", 
        names(template_lup) == names(new_lup))
    if (priority_lup_for_dupls_1L_chr == "template") {
        new_lup <- new_lup %>% dplyr::filter(!(!!rlang::sym(key_var_nm_1L_chr) %in% 
            (template_lup %>% dplyr::pull(!!rlang::sym(key_var_nm_1L_chr)))))
        labels_chr <- Hmisc::label(template_lup) %>% unname()
    }
    else {
        template_lup <- template_lup %>% dplyr::filter(!(!!rlang::sym(key_var_nm_1L_chr) %in% 
            (new_lup %>% dplyr::pull(!!rlang::sym(key_var_nm_1L_chr)))))
        labels_chr <- Hmisc::label(new_lup) %>% unname()
    }
    if (!all(labels_chr %>% unique() == "")) {
        template_lup <- template_lup %>% remove_lbls_from_df()
        new_lup <- new_lup %>% remove_lbls_from_df()
        Hmisc::label(template_lup) <- as.list(labels_chr %>% 
            unname())
        Hmisc::label(new_lup) <- as.list(labels_chr %>% unname())
    }
    combined_lups <- dplyr::bind_rows(template_lup, new_lup) %>% 
        dplyr::arrange(!!rlang::sym(key_var_nm_1L_chr))
    combined_lups <- combined_lups[rowSums(is.na(combined_lups)) != 
        ncol(combined_lups), ]
    return(combined_lups)
}
#' Add plurals to abbreviation
#' @description add_plurals_to_abbr_lup() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add plurals to abbreviation lookup table. Function argument abbr_tb specifies the object to be updated. The function returns Abbreviation (a tibble).
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
#' Add rows to function type
#' @description add_rows_to_fn_type_lup() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add rows to function type lookup table. Function argument fn_type_lup_tb specifies the object to be updated. The function returns Updated function type lookup table (a tibble).
#' @param fn_type_lup_tb Function type lookup table (a tibble), Default: make_fn_type_lup()
#' @param fn_type_nm_chr Function type name (a character vector), Default: 'NA'
#' @param fn_type_desc_chr Function type description (a character vector), Default: 'NA'
#' @param first_arg_desc_chr First argument description (a character vector), Default: 'NA'
#' @param second_arg_desc_chr Second argument description (a character vector), Default: 'NA'
#' @param is_generic_lgl Is generic (a logical vector), Default: F
#' @param is_method_lgl Is method (a logical vector), Default: F
#' @return Updated function type lookup table (a tibble)
#' @rdname add_rows_to_fn_type_lup
#' @export 
#' @importFrom tibble tibble
#' @keywords internal
add_rows_to_fn_type_lup <- function (fn_type_lup_tb = make_fn_type_lup(), fn_type_nm_chr = NA_character_, 
    fn_type_desc_chr = NA_character_, first_arg_desc_chr = NA_character_, 
    second_arg_desc_chr = NA_character_, is_generic_lgl = F, 
    is_method_lgl = F) 
{
    updated_fn_type_lup_tb <- add_lups(fn_type_lup_tb, new_lup = tibble::tibble(fn_type_nm_chr = fn_type_nm_chr, 
        fn_type_desc_chr = fn_type_desc_chr, first_arg_desc_chr = first_arg_desc_chr, 
        second_arg_desc_chr = second_arg_desc_chr, is_generic_lgl = is_generic_lgl, 
        is_method_lgl = is_method_lgl), key_var_nm_1L_chr = "fn_type_nm_chr")
    return(updated_fn_type_lup_tb)
}
