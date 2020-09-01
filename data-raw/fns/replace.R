write_to_rpl_1L_and_indefL_sfcs <- function(indefL_arg_nm_1L_chr,
                                         file_path_1L_chr = NA_character_,
                                         dir_path_1L_chr = NA_character_){
  sfxs_chr <- c(indefL_arg_nm_1L_chr %>% stringr::str_sub(start = -8, end=-5),
                    indefL_arg_nm_1L_chr %>% stringr::str_sub(start = -8))
  write_to_replace_sfx_pair(args_nm_chr =   paste0(indefL_arg_nm_1L_chr %>% stringr::str_sub(end=-9),
                                                sfxs_chr),
                     sfxs_chr = sfxs_chr,
                     replacements_chr = paste0(c("_1L",""),sfxs_chr[1]),
                     file_path_1L_chr = file_path_1L_chr,
                     dir_path_1L_chr = dir_path_1L_chr)

}
replace_abbr <- function(title_chr,
                             abbreviations_lup = NULL,
                             collapse_lgl = T){
  if(is.null(abbreviations_lup))
    data("abbreviations_lup",package="ready4fun",envir = environment())
  title_chr <- title_chr %>%
    strsplit(" ") %>%
    purrr::flatten_chr() %>%
    purrr::map_chr(~{
      match_lgl_vec <- .x==abbreviations_lup$short_name_chr
      ifelse(match_lgl_vec %>%
               any(),
             ifelse(.x %in% abbreviations_lup$short_name_chr[match_lgl_vec],
                    get_from_lup_obj(abbreviations_lup,
                                     match_value_xx = ifelse(.x == abbreviations_lup$short_name_chr[match_lgl_vec],
                                                             .x,
                                                             abbreviations_lup$short_name_chr[match_lgl_vec]),
                                     match_var_nm_1L_chr = "short_name_chr",
                                     target_var_nm_1L_chr = "long_name_chr",
                                     evaluate_lgl = F),
                    .x),
             .x)
    })
  if(collapse_lgl)
    title_chr <- title_chr %>% paste0(collapse = " ")
  return(title_chr)
}
replace_fn_nms <- function(rename_tb,
                           undocumented_fns_dir_chr = make_undmtd_fns_dir_chr(),
                           rt_dev_dir_path_1L_chr = normalizePath("../../../"),
                           dev_pkg_nm_1L_chr = get_dev_pkg_nm()){
  if(any(rename_tb$duplicated_lgl))
    stop("Duplicates in rename table")
  rename_tb <- rename_tb %>%
    dplyr::filter(fns_chr != new_nm) %>%
    dplyr::select(fns_chr,new_nm)
  purrr::pwalk(rename_tb, ~  {
    pattern_1L_chr <- ..1
    replacement_1L_chr <- ..2
    purrr::walk(undocumented_fns_dir_chr,
                ~ xfun::gsub_dir(undocumented_fns_dir_chr,
                                 pattern = pattern_1L_chr,
                                 replacement = replacement_1L_chr))
    xfun::gsub_dir(dir = rt_dev_dir_path_1L_chr,
                   pattern = paste0(dev_pkg_nm_1L_chr,"::",pattern_1L_chr),
                   replacement = paste0(dev_pkg_nm_1L_chr,"::",replacement_1L_chr),
                   ext = "R",
                   fixed = T)
  })
}
write_to_replace_sfx_pair <- function(args_nm_chr,
                               sfxs_chr,
                               replacements_chr,
                               file_path_1L_chr = NA_character_,
                               dir_path_1L_chr = NA_character_){
  fn <- ifelse(is.na(file_path_1L_chr),xfun::gsub_dir,xfun::gsub_file)
  path_chr <- ifelse(is.na(file_path_1L_chr),dir_path_1L_chr,file_path_1L_chr)
  args_ls <- list(pattern = paste0(args_nm_chr[1],
                                   "(?!",
                                   stringr::str_remove(sfxs_chr[2],
                                                       sfxs_chr[1]),
                                   ")"),
                  replacement = paste0(stringr::str_remove(args_nm_chr[1],
                                                           sfxs_chr[1]),
                                       replacements_chr[1]),
                  perl=T)
  rlang::exec(fn, path_chr, !!!args_ls)
  args_ls <- list(pattern = args_nm_chr[2],
                  replacement = paste0(stringr::str_remove(args_nm_chr[2],
                                                           sfxs_chr[2]),
                                       replacements_chr[2]),
                  perl=T)
  rlang::exec(fn, path_chr, !!!args_ls)
}
