add_indef_artl_to_item_chr_vec <- function(phrase_chr_vec,
                                           abbreviations_lup = NULL,
                                           ignore_phrs_not_in_lup_lgl = T){
  if(is.null(abbreviations_lup))
    data("abbreviations_lup",package="ready4fun",envir = environment())
  indefinite_item_chr_vec <- purrr::map_chr(phrase_chr_vec,
                                            ~{
                                              phrase_chr <- .x
                                              matching_prefix_chr_vec <- abbreviations_lup$long_name_chr[phrase_chr %>% startsWith(abbreviations_lup$long_name_chr)]
                                              if(identical(matching_prefix_chr_vec,character(0))){
                                                prefix_chr <- phrase_chr
                                              }else{
                                                n_char_matches_dbl_vec <- matching_prefix_chr_vec %>% nchar()
                                                prefix_chr <- matching_prefix_chr_vec[which(n_char_matches_dbl_vec == max(n_char_matches_dbl_vec))]#stringr::word()
                                              }
                                              plural_lgl <- ifelse(prefix_chr %in% abbreviations_lup$long_name_chr,
                                                                   get_from_lup_obj(abbreviations_lup,
                                                                                    match_var_nm_chr = "long_name_chr",
                                                                                    match_value_xx = prefix_chr,
                                                                                    target_var_nm_chr = "plural_lgl",
                                                                                    evaluate_lgl = F),
                                                                   ignore_phrs_not_in_lup_lgl)
                                              ifelse(is.na(plural_lgl),
                                                     .x,
                                                     ifelse(plural_lgl,
                                                            .x,
                                                            ifelse(.x %>% tolower() %>% stringr::str_sub(end=1) %in% c("a","e","i","o","u"),
                                                                   paste0("an ", .x),
                                                                   paste0("a ", .x))))

                                            } )
  return(indefinite_item_chr_vec)

}
add_indefartls_to_phrases_chr_vec <- function(abbreviated_phrase_chr_vec,
                                               abbreviations_lup = NULL,
                                               ignore_phrs_not_in_lup_lgl = T){
  if(is.null(abbreviations_lup))
    data("abbreviations_lup",package="ready4fun",envir = environment())
  phrases_chr_vec <- abbreviated_phrase_chr_vec %>%
    purrr::map_chr(~{
      words_chr_vec_ls <- strsplit(.x,"_")
      words_chr_vec_ls %>%
        purrr::map_chr(~{
          expanded_chr_vec <- replace_abbr_chr(.x,collapse_lgl = F)
          indefinite_chr_vec <- add_indef_artl_to_item_chr_vec(expanded_chr_vec,
                                                               abbreviations_lup = abbreviations_lup,
                                                               ignore_phrs_not_in_lup_lgl = ignore_phrs_not_in_lup_lgl)
          matches_lgl_vec <- expanded_chr_vec==indefinite_chr_vec
          run_length_ls <- rle(matches_lgl_vec)
          1:length(run_length_ls$lengths) %>%
            purrr::map_chr(~
                             {
                               ifelse(run_length_ls$values[.x],
                                      paste0(indefinite_chr_vec[ifelse(.x==1,
                                                                       .x,
                                                                       sum(run_length_ls$lengths[1:.x-1])+1):sum(run_length_ls$lengths[1:.x])],collapse=" "),
                                      paste0(c(indefinite_chr_vec[ifelse(.x==1,
                                                                         .x,
                                                                         sum(run_length_ls$lengths[1:.x-1])+1)],
                                               ifelse(run_length_ls$lengths[.x]==1,
                                                      NA_character_,
                                                      paste0(expanded_chr_vec[ifelse(.x==1,
                                                                                     .x+1,
                                                                                     sum(run_length_ls$lengths[1:.x-1])+2):sum(run_length_ls$lengths[1:.x])],
                                                             collapse = " ")) %>%
                                                 purrr::discard(is.na))
                                               ,
                                             collapse=" "))


                             }) %>%
            paste0(collapse = " ")
        })
    })
  return(phrases_chr_vec)
}
add_plurals_to_abbr_lup_tb <- function(abbr_tb,
                                       no_plural_chr_vec = NA_character_,
                                       custom_plural_ls = NULL){
  non_standard_chr_vec <- no_plural_chr_vec
  if(!is.null(custom_plural_ls)){
    non_standard_chr_vec <- c(non_standard_chr_vec,names(custom_plural_ls))
  }
  standard_chr_vec <- setdiff(abbr_tb$long_name_chr,non_standard_chr_vec)
  new_tb <- standard_tb <- abbr_tb %>%
    dplyr::filter(long_name_chr %in% standard_chr_vec) %>%
    dplyr::mutate_all(~paste0(.x,"s"))
  if(!is.null(custom_plural_ls)){
    custom_tb <- 1:length(custom_plural_ls) %>%
      purrr::map_dfr(~{
        match_chr <- names(custom_plural_ls)[.x]
        long_plural_chr <- custom_plural_ls[[.x]][1]
        short_plural_chr <- ifelse(length(custom_plural_ls[[.x]])>1,
                                   custom_plural_ls[[.x]][2],
                                   NA_character_)
        abbr_tb %>%
          dplyr::filter(long_name_chr == match_chr) %>%
          dplyr::mutate(long_name_chr = long_plural_chr,
                        short_name_chr = ifelse(is.na(short_plural_chr),
                                                paste0(short_name_chr,"s"),
                                                short_plural_chr))
      })
    new_tb <- dplyr::bind_rows(standard_tb,
                               custom_tb) %>%
      dplyr::arrange()
  }
  abbr_tb <- tibble::tibble(short_name_chr = make.unique(c(abbr_tb$short_name_chr,new_tb$short_name_chr)),
                            long_name_chr = make.unique(c(abbr_tb$long_name_chr,new_tb$long_name_chr)),
                            plural_lgl = c(rep(F,length(abbr_tb$long_name_chr)),rep(T,length(new_tb$long_name_chr)))
  ) %>%
    dplyr::mutate(plural_lgl = purrr::map2_lgl(plural_lgl, long_name_chr, ~ ifelse(.y %in% no_plural_chr_vec,
                                                                                   NA,
                                                                                   .x))) %>%
    dplyr::arrange(short_name_chr)
  return(abbr_tb)
}
add_rows_to_fn_type_lup_tb <- function(fn_type_lup_tb = make_fn_type_lup_tb(),
                                       fn_type_nm_chr = NA_character_,
                                       fn_type_desc_chr = NA_character_,
                                       first_arg_desc_chr = NA_character_,
                                       second_arg_desc_chr = NA_character_,
                                       is_generic_lgl = F,
                                       is_method_lgl = F){
  updated_fn_type_lup_tb <- fn_type_lup_tb %>%
    dplyr::bind_rows(tibble::tibble(fn_type_nm_chr = fn_type_nm_chr,
                                    fn_type_desc_chr = fn_type_desc_chr,
                                    first_arg_desc_chr = first_arg_desc_chr,
                                    second_arg_desc_chr = second_arg_desc_chr,
                                    is_generic_lgl = is_generic_lgl,
                                    is_method_lgl = is_method_lgl)) %>%
    dplyr::arrange(fn_type_nm_chr)
  return(updated_fn_type_lup_tb)
}
