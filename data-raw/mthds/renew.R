renew.ready4fun_abbreviations <- function(x,
                                          short_name_chr = NA_character_,
                                          long_name_chr = NA_character_,
                                          plural_lgl = NA,
                                          filter_cdn_1L_chr = NA_character_,
                                          new_cases_r3 = NULL,
                                          slice_idxs_int = NA_integer_){
  x <- ready4::update_tb_r3(x,
                            filter_cdn_1L_chr = filter_cdn_1L_chr,
                            slice_idxs_int = slice_idxs_int)
  x <- dplyr::bind_rows(x,
                        tibble::tibble(short_name_chr = short_name_chr,
                                       long_name_chr =long_name_chr,
                                       plural_lgl = plural_lgl))
  if(!is.null(new_cases_r3)){
    x <- ready4::add_lups(x,
                          new_lup = new_cases_r3,
                          key_var_nm_1L_chr = "short_name_chr")
  }
  return(x)
}
renew.ready4fun_functions <- function(x,
                                      fn_type_nm_chr = NA_character_,
                                      fn_type_desc_chr = NA_character_,
                                      first_arg_desc_chr = NA_character_,
                                      second_arg_desc_chr = NA_character_,
                                      is_generic_lgl = NA,
                                      is_method_lgl = NA,
                                      filter_cdn_1L_chr = NA_character_,
                                      new_cases_r3 = NULL,
                                      slice_idxs_int = NA_integer_){
  x <- ready4::update_tb_r3(x,
                            filter_cdn_1L_chr = filter_cdn_1L_chr,
                            slice_idxs_int = slice_idxs_int)
  x <- dplyr::bind_rows(x,
                        tibble::tibble(fn_type_nm_chr = fn_type_nm_chr,
                                       fn_type_desc_chr = fn_type_desc_chr,
                                       first_arg_desc_chr = first_arg_desc_chr,
                                       second_arg_desc_chr = second_arg_desc_chr,
                                       is_generic_lgl = is_generic_lgl,
                                       is_method_lgl = is_method_lgl))
  if(!is.null(new_cases_r3)){
    x <- ready4::add_lups(x,
                          new_lup = new_cases_r3,
                          key_var_nm_1L_chr = "fn_type_nm_chr")
  }
  return(x)
}
renew.ready4fun_manifest <- function(x,
                                     type_1L_chr,
                                     are_words_chr = character(0),
                                     custom_plural_ls = NULL,
                                     key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
                                     long_name_chr = character(0),
                                     no_plural_chr = character(0),
                                     not_obj_type_chr = character(0),
                                     pfx_rgx = character(0),
                                     tf_to_singular_chr = character(0)){
  if(type_1L_chr == "fns_dmt")
    x <- add_fns_dmt_tb(pkg_setup_ls = x,
                        fns_env_ls = NULL,
                        inc_methods_1L_lgl = T)
  if(!identical(are_words_chr, character(0)) | !identical(tf_to_singular_chr, character(0)) | !identical(not_obj_type_chr, character(0))){
    if(identical(are_words_chr, character(0)))
      are_words_chr <- NA_character_
    if(identical(tf_to_singular_chr, character(0)))
      tf_to_singular_chr <- NA_character_
    if(identical(not_obj_type_chr, character(0)))
      not_obj_type_chr <- NA_character_
    x <- update_msng_abbrs(x,
                           are_words_chr = are_words_chr,
                           not_obj_type_chr = not_obj_type_chr,
                           tf_to_singular_chr = tf_to_singular_chr)
    if(type_1L_chr == "words")
      x <- write_new_words_vec(x,
                               key_1L_chr = key_1L_chr)
  }
  if(type_1L_chr == "abbreviations"){
    if(identical(long_name_chr, character(0)))
      long_name_chr <- NA_character_
    if(identical(no_plural_chr, character(0)))
      no_plural_chr <- NA_character_
    if(identical(pfx_rgx, character(0)))
      pfx_rgx <- NA_character_
    x <- write_new_abbrs(x,
                         custom_plural_ls = custom_plural_ls,
                         key_1L_chr = key_1L_chr,
                         long_name_chr = long_name_chr,
                         no_plural_chr = no_plural_chr,
                         pfx_rgx = pfx_rgx)
  }
  return(x)
}
renew.ready4fun_objects <- function(x,
                                    short_name_chr = NA_character_,
                                    long_name_chr = NA_character_,
                                    atomic_element_lgl = NA,
                                    r3_can_extend_lgl = NA,
                                    filter_cdn_1L_chr = NA_character_,
                                    new_cases_r3 = NULL,
                                    slice_idxs_int = NA_integer_){
  x <- ready4::update_tb_r3(x,
                            filter_cdn_1L_chr = filter_cdn_1L_chr,
                            slice_idxs_int = slice_idxs_int)
  x <- dplyr::bind_rows(x,
                        tibble::tibble(short_name_chr = short_name_chr,
                                       long_name_chr = long_name_chr,
                                       atomic_element_lgl = atomic_element_lgl,
                                       r3_can_extend_lgl = r3_can_extend_lgl))
  if(!is.null(new_cases_r3)){
    x <- ready4::add_lups(x,
                          new_lup = new_cases_r3,
                          key_var_nm_1L_chr = "short_name_chr")
  }
  return(x)
}
