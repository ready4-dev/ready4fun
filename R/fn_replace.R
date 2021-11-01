#' Replace abbreviation
#' @description replace_abbr() is a Replace function that edits an object, replacing a specified element with another specified element. Specifically, this function implements an algorithm to replace abbreviation. Function argument title_chr specifies the object to be updated. Argument abbreviations_lup provides the object to be updated. The function returns Title (a character vector).
#' @param title_chr Title (a character vector)
#' @param abbreviations_lup Abbreviations (a lookup table)
#' @param collapse_lgl Collapse (a logical vector), Default: T
#' @return Title (a character vector)
#' @rdname replace_abbr
#' @export 
#' @importFrom purrr flatten_chr map_chr
#' @importFrom ready4 get_from_lup_obj
#' @keywords internal
replace_abbr <- function (title_chr, abbreviations_lup, collapse_lgl = T) 
{
    title_chr <- title_chr %>% strsplit(" ") %>% purrr::flatten_chr() %>% 
        purrr::map_chr(~{
            match_lgl_vec <- .x == abbreviations_lup$short_name_chr
            ifelse(match_lgl_vec %>% any(), ifelse(.x %in% abbreviations_lup$short_name_chr[match_lgl_vec], 
                ready4::get_from_lup_obj(abbreviations_lup, match_value_xx = ifelse(.x == 
                  abbreviations_lup$short_name_chr[match_lgl_vec], 
                  .x, abbreviations_lup$short_name_chr[match_lgl_vec]), 
                  match_var_nm_1L_chr = "short_name_chr", target_var_nm_1L_chr = "long_name_chr", 
                  evaluate_1L_lgl = F), .x), .x)
        })
    if (collapse_lgl) 
        title_chr <- title_chr %>% paste0(collapse = " ")
    return(title_chr)
}
