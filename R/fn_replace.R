#' Replace abbreviation
#' @description replace_abbr() is a Replace function that edits an object, replacing a specified element with another specified element. Specifically, this function implements an algorithm to a replace abbreviation. Function argument title_chr specifies the object to be updated. Argument abbreviations_lup provides the object to be updated.The function returns title (a character vector).
#' @param title_chr Title (a character vector)
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param collapse_lgl Collapse (a logical vector), Default: T
#' @return Title (a character vector)
#' @rdname replace_abbr
#' @export 
#' @importFrom purrr flatten_chr map_chr
#' @keywords internal
replace_abbr <- function (title_chr, abbreviations_lup = NULL, collapse_lgl = T) 
{
    if (is.null(abbreviations_lup)) 
        data("abbreviations_lup", package = "ready4fun", envir = environment())
    title_chr <- title_chr %>% strsplit(" ") %>% purrr::flatten_chr() %>% 
        purrr::map_chr(~{
            match_lgl_vec <- .x == abbreviations_lup$short_name_chr
            ifelse(match_lgl_vec %>% any(), ifelse(.x %in% abbreviations_lup$short_name_chr[match_lgl_vec], 
                get_from_lup_obj(abbreviations_lup, match_value_xx = ifelse(.x == 
                  abbreviations_lup$short_name_chr[match_lgl_vec], 
                  .x, abbreviations_lup$short_name_chr[match_lgl_vec]), 
                  match_var_nm_1L_chr = "short_name_chr", target_var_nm_1L_chr = "long_name_chr", 
                  evaluate_lgl = F), .x), .x)
        })
    if (collapse_lgl) 
        title_chr <- title_chr %>% paste0(collapse = " ")
    return(title_chr)
}
#' Replace function names
#' @description replace_fn_nms() is a Replace function that edits an object, replacing a specified element with another specified element. Specifically, this function implements an algorithm to a replace function names. Function argument rename_tb specifies the object to be updated. Argument undocumented_fns_dir_chr provides the object to be updated.The function is called for its side effects and does not return a value.
#' @param rename_tb Rename (a tibble)
#' @param undocumented_fns_dir_chr Undocumented functions directory (a character vector), Default: make_undmtd_fns_dir_chr()
#' @param rt_dev_dir_path_1L_chr Root development directory path (a character vector of length one), Default: normalizePath("../../../")
#' @param dev_pkg_nm_1L_chr Development package name (a character vector of length one), Default: get_dev_pkg_nm()
#' @return NULL
#' @rdname replace_fn_nms
#' @export 
#' @importFrom dplyr filter select
#' @importFrom purrr pwalk walk
#' @importFrom xfun gsub_dir
#' @keywords internal
replace_fn_nms <- function (rename_tb, undocumented_fns_dir_chr = make_undmtd_fns_dir_chr(), 
    rt_dev_dir_path_1L_chr = normalizePath("../../../"), dev_pkg_nm_1L_chr = get_dev_pkg_nm()) 
{
    if (any(rename_tb$duplicated_lgl)) 
        stop("Duplicates in rename table")
    rename_tb <- rename_tb %>% dplyr::filter(fns_chr != new_nm) %>% 
        dplyr::select(fns_chr, new_nm)
    purrr::pwalk(rename_tb, ~{
        pattern_1L_chr <- ..1
        replacement_1L_chr <- ..2
        purrr::walk(undocumented_fns_dir_chr, ~xfun::gsub_dir(undocumented_fns_dir_chr, 
            pattern = pattern_1L_chr, replacement = replacement_1L_chr))
        xfun::gsub_dir(dir = rt_dev_dir_path_1L_chr, pattern = paste0(dev_pkg_nm_1L_chr, 
            "::", pattern_1L_chr), replacement = paste0(dev_pkg_nm_1L_chr, 
            "::", replacement_1L_chr), ext = "R", fixed = T)
    })
}
