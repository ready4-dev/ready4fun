#' Add indefinite article to item
#' @description add_indef_artl_to_item_chr_vec() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add an indefinite article to item. Function argument phrase_chr_vec specifies the object to be updated.The function returns an indefinite item (a character vector).
#' @param phrase_chr_vec Phrase (a character vector)
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param ignore_phrs_not_in_lup_lgl Ignore phrases not in lookup table (a logical vector of length 1), Default: T
#' @return Indefinite item (a character vector)
#' @rdname add_indef_artl_to_item_chr_vec
#' @export 
#' @importFrom purrr map_chr
#' @importFrom stringr str_sub
#' @keywords internal
add_indef_artl_to_item_chr_vec <- function (phrase_chr_vec, abbreviations_lup = NULL, ignore_phrs_not_in_lup_lgl = T) 
{
    if (is.null(abbreviations_lup)) 
        data("abbreviations_lup", package = "ready4fun", envir = environment())
    indefinite_item_chr_vec <- purrr::map_chr(phrase_chr_vec, 
        ~{
            phrase_chr <- .x
            matching_prefix_chr_vec <- abbreviations_lup$long_name_chr[phrase_chr %>% 
                startsWith(abbreviations_lup$long_name_chr)]
            if (identical(matching_prefix_chr_vec, character(0))) {
                prefix_chr <- phrase_chr
            }
            else {
                n_char_matches_dbl_vec <- matching_prefix_chr_vec %>% 
                  nchar()
                prefix_chr <- matching_prefix_chr_vec[which(n_char_matches_dbl_vec == 
                  max(n_char_matches_dbl_vec))]
            }
            plural_lgl <- ifelse(prefix_chr %in% abbreviations_lup$long_name_chr, 
                get_from_lup_obj(abbreviations_lup, match_var_nm_chr = "long_name_chr", 
                  match_value_xx = prefix_chr, target_var_nm_chr = "plural_lgl", 
                  evaluate_lgl = F), ignore_phrs_not_in_lup_lgl)
            ifelse(is.na(plural_lgl), .x, ifelse(plural_lgl, 
                .x, ifelse(.x %>% tolower() %>% stringr::str_sub(end = 1) %in% 
                  c("a", "e", "i", "o", "u"), paste0("an ", .x), 
                  paste0("a ", .x))))
        })
    return(indefinite_item_chr_vec)
}
#' Add indefinite articles to phrases
#' @description add_indefartls_to_phrases_chr_vec() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add indefinite articles to phrases. Function argument abbreviated_phrase_chr_vec specifies the object to be updated.The function returns phrases (a character vector).
#' @param abbreviated_phrase_chr_vec Abbreviated phrase (a character vector)
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param ignore_phrs_not_in_lup_lgl Ignore phrases not in lookup table (a logical vector of length 1), Default: T
#' @return Phrases (a character vector)
#' @rdname add_indefartls_to_phrases_chr_vec
#' @export 
#' @importFrom purrr map_chr discard
#' @keywords internal
add_indefartls_to_phrases_chr_vec <- function (abbreviated_phrase_chr_vec, abbreviations_lup = NULL, 
    ignore_phrs_not_in_lup_lgl = T) 
{
    if (is.null(abbreviations_lup)) 
        data("abbreviations_lup", package = "ready4fun", envir = environment())
    phrases_chr_vec <- abbreviated_phrase_chr_vec %>% purrr::map_chr(~{
        words_chr_vec_ls <- strsplit(.x, "_")
        words_chr_vec_ls %>% purrr::map_chr(~{
            expanded_chr_vec <- replace_abbr_chr(.x, collapse_lgl = F)
            indefinite_chr_vec <- add_indef_artl_to_item_chr_vec(expanded_chr_vec, 
                abbreviations_lup = abbreviations_lup, ignore_phrs_not_in_lup_lgl = ignore_phrs_not_in_lup_lgl)
            matches_lgl_vec <- expanded_chr_vec == indefinite_chr_vec
            run_length_ls <- rle(matches_lgl_vec)
            1:length(run_length_ls$lengths) %>% purrr::map_chr(~{
                ifelse(run_length_ls$values[.x], paste0(indefinite_chr_vec[ifelse(.x == 
                  1, .x, sum(run_length_ls$lengths[1:.x - 1]) + 
                  1):sum(run_length_ls$lengths[1:.x])], collapse = " "), 
                  paste0(c(indefinite_chr_vec[ifelse(.x == 1, 
                    .x, sum(run_length_ls$lengths[1:.x - 1]) + 
                      1)], ifelse(run_length_ls$lengths[.x] == 
                    1, NA_character_, paste0(expanded_chr_vec[ifelse(.x == 
                    1, .x + 1, sum(run_length_ls$lengths[1:.x - 
                    1]) + 2):sum(run_length_ls$lengths[1:.x])], 
                    collapse = " ")) %>% purrr::discard(is.na)), 
                    collapse = " "))
            }) %>% paste0(collapse = " ")
        })
    })
    return(phrases_chr_vec)
}
#' Add plurals to abbreviation lookup table
#' @description add_plurals_to_abbr_lup_tb() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add plurals to an abbreviation lookup table. Function argument abbr_tb specifies the object to be updated.The function returns an abbreviation (a tibble).
#' @param abbr_tb Abbreviation (a tibble)
#' @param no_plural_chr_vec No plural (a character vector), Default: 'NA'
#' @param custom_plural_ls Custom plural (a list), Default: NULL
#' @return Abbreviation (a tibble)
#' @rdname add_plurals_to_abbr_lup_tb
#' @export 
#' @importFrom dplyr filter mutate_all mutate bind_rows arrange
#' @importFrom purrr map_dfr map2_lgl
#' @importFrom tibble tibble
add_plurals_to_abbr_lup_tb <- function (abbr_tb, no_plural_chr_vec = NA_character_, custom_plural_ls = NULL) 
{
    non_standard_chr_vec <- no_plural_chr_vec
    if (!is.null(custom_plural_ls)) {
        non_standard_chr_vec <- c(non_standard_chr_vec, names(custom_plural_ls))
    }
    standard_chr_vec <- setdiff(abbr_tb$long_name_chr, non_standard_chr_vec)
    new_tb <- standard_tb <- abbr_tb %>% dplyr::filter(long_name_chr %in% 
        standard_chr_vec) %>% dplyr::mutate_all(~paste0(.x, "s"))
    if (!is.null(custom_plural_ls)) {
        custom_tb <- 1:length(custom_plural_ls) %>% purrr::map_dfr(~{
            match_chr <- names(custom_plural_ls)[.x]
            long_plural_chr <- custom_plural_ls[[.x]][1]
            short_plural_chr <- ifelse(length(custom_plural_ls[[.x]]) > 
                1, custom_plural_ls[[.x]][2], NA_character_)
            abbr_tb %>% dplyr::filter(long_name_chr == match_chr) %>% 
                dplyr::mutate(long_name_chr = long_plural_chr, 
                  short_name_chr = ifelse(is.na(short_plural_chr), 
                    paste0(short_name_chr, "s"), short_plural_chr))
        })
        new_tb <- dplyr::bind_rows(standard_tb, custom_tb) %>% 
            dplyr::arrange()
    }
    abbr_tb <- tibble::tibble(short_name_chr = make.unique(c(abbr_tb$short_name_chr, 
        new_tb$short_name_chr)), long_name_chr = make.unique(c(abbr_tb$long_name_chr, 
        new_tb$long_name_chr)), plural_lgl = c(rep(F, length(abbr_tb$long_name_chr)), 
        rep(T, length(new_tb$long_name_chr)))) %>% dplyr::mutate(plural_lgl = purrr::map2_lgl(plural_lgl, 
        long_name_chr, ~ifelse(.y %in% no_plural_chr_vec, NA, 
            .x))) %>% dplyr::arrange(short_name_chr)
    return(abbr_tb)
}
#' Add rows to function type lookup table
#' @description add_rows_to_fn_type_lup_tb() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add rows to a function type a lookup table. Function argument fn_type_lup_tb specifies the object to be updated.The function returns an updated function type lookup table (a tibble).
#' @param fn_type_lup_tb Function type lookup table (a tibble), Default: make_fn_type_lup_tb()
#' @param fn_type_nm_chr Function type name (a character vector of length 1), Default: 'NA'
#' @param fn_type_desc_chr Function type description (a character vector of length 1), Default: 'NA'
#' @param first_arg_desc_chr First argument description (a character vector of length 1), Default: 'NA'
#' @param second_arg_desc_chr Second argument description (a character vector of length 1), Default: 'NA'
#' @param is_generic_lgl Is generic (a logical vector of length 1), Default: F
#' @param is_method_lgl Is method (a logical vector of length 1), Default: F
#' @return Updated function type lookup table (a tibble)
#' @rdname add_rows_to_fn_type_lup_tb
#' @export 
#' @importFrom dplyr bind_rows arrange
#' @importFrom tibble tibble
#' @keywords internal
add_rows_to_fn_type_lup_tb <- function (fn_type_lup_tb = make_fn_type_lup_tb(), fn_type_nm_chr = NA_character_, 
    fn_type_desc_chr = NA_character_, first_arg_desc_chr = NA_character_, 
    second_arg_desc_chr = NA_character_, is_generic_lgl = F, 
    is_method_lgl = F) 
{
    updated_fn_type_lup_tb <- fn_type_lup_tb %>% dplyr::bind_rows(tibble::tibble(fn_type_nm_chr = fn_type_nm_chr, 
        fn_type_desc_chr = fn_type_desc_chr, first_arg_desc_chr = first_arg_desc_chr, 
        second_arg_desc_chr = second_arg_desc_chr, is_generic_lgl = is_generic_lgl, 
        is_method_lgl = is_method_lgl)) %>% dplyr::arrange(fn_type_nm_chr)
    return(updated_fn_type_lup_tb)
}
