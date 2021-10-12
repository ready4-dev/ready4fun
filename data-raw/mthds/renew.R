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
