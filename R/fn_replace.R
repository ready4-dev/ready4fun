#' @keywords internal
replace_1L_and_indefL_sfxs_R <- function (indefL_arg_nm_1L_chr, file_path_1L_chr = NA_character_, 
    dir_path_1L_chr = NA_character_) 
{
    sfxs_chr <- c(indefL_arg_nm_1L_chr %>% stringr::str_sub(start = -8, 
        end = -5), indefL_arg_nm_1L_chr %>% stringr::str_sub(start = -8))
    replace_sfx_pair_R(args_nm_chr = paste0(indefL_arg_nm_1L_chr %>% 
        stringr::str_sub(end = -9), sfxs_chr), sfxs_chr = sfxs_chr, 
        replacements_chr = paste0(c("_1L", ""), sfxs_chr[1]), 
        file_path_1L_chr = file_path_1L_chr, dir_path_1L_chr = dir_path_1L_chr)
}
#' @keywords internal
replace_abbr_chr <- function (title_chr, abbreviations_lup = NULL, collapse_lgl = T) 
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
#' @keywords internal
replace_sfx_pair_R <- function (args_nm_chr, sfxs_chr, replacements_chr, file_path_1L_chr = NA_character_, 
    dir_path_1L_chr = NA_character_) 
{
    fn <- ifelse(is.na(file_path_1L_chr), xfun::gsub_dir, xfun::gsub_file)
    path_chr <- ifelse(is.na(file_path_1L_chr), dir_path_1L_chr, 
        file_path_1L_chr)
    args_ls <- list(pattern = paste0(args_nm_chr[1], "(?!", stringr::str_remove(sfxs_chr[2], 
        sfxs_chr[1]), ")"), replacement = paste0(stringr::str_remove(args_nm_chr[1], 
        sfxs_chr[1]), replacements_chr[1]), perl = T)
    rlang::exec(fn, path_chr, !!!args_ls)
    args_ls <- list(pattern = args_nm_chr[2], replacement = paste0(stringr::str_remove(args_nm_chr[2], 
        sfxs_chr[2]), replacements_chr[2]), perl = T)
    rlang::exec(fn, path_chr, !!!args_ls)
}
