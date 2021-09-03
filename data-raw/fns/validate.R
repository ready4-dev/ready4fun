validate_pkg_setup <- function(pkg_setup_ls,
                               classes_to_make_tb = classes_to_make_tb,
                               pkg_ds_ls_ls = pkg_ds_ls_ls){
  pkg_setup_ls$problems_ls <- NULL
  missing_fn_types_chr <- get_new_fn_types(pkg_setup_ls)
  if(!identical(missing_fn_types_chr, character(0))){
    message(paste0("The following function type",
                   ifelse(length(missing_fn_types_chr) >1,"s are"," is"),
                   " not yet defined: \n",
                   missing_fn_types_chr %>% make_list_phrase(),
                   ".\nAdd the missing definition",
                   ifelse(length(missing_fn_types_chr) >1,"s",""),
                   " by using the 'write_new_fn_types' function"))
    pkg_setup_ls$problems_ls$missing_fn_types_chr <- missing_fn_types_chr
  }else{
    missing_abbrs_chr <- get_new_abbrs(pkg_setup_ls,
                                       classes_to_make_tb = classes_to_make_tb,
                                       pkg_ds_ls_ls = pkg_ds_ls_ls,
                                       treat_as_words_chr = pkg_setup_ls$subsequent_ls$treat_as_words_chr)
    if(!identical(missing_abbrs_chr, character(0))){
      message(paste0("The following potential abbreviations",
                     ifelse(length(missing_abbrs_chr) >1,"s are"," is"),
                     " neither defined nor contained in the 'treat_as_words_chr' object: \n",
                     missing_abbrs_chr %>% make_list_phrase(),
                     ".\nAdd the missing abbreviation definition",
                     ifelse(length(missing_abbrs_chr) >1,"s",""),
                     " or update the 'treat_as_words_chr' by using the 'write_new_abbrs' function"))
      pkg_setup_ls$problems_ls$missing_abbrs_chr <- missing_abbrs_chr
    }
    missing_class_abbrs_chr <- setdiff(paste0(pkg_setup_ls$initial_ls$pkg_desc_ls$Package,
                                              "_",
                                              classes_to_make_tb$name_stub_chr),
                                       pkg_setup_ls$subsequent_ls$abbreviations_lup$short_name_chr)
    if(!identical(missing_class_abbrs_chr, character(0))){
      message(paste0("The following class names",
                     ifelse(length(missing_class_abbrs_chr) >1,"s are"," is"),
                     " not defined in the abbreviations lookup table: \n",
                     missing_class_abbrs_chr %>% make_list_phrase(),
                     ".\nAdd the missing class definition",
                     ifelse(length(missing_class_abbrs_chr) >1,"s",""),
                     " by using the 'write_new_abbrs' function"))
      pkg_setup_ls$problems_ls$missing_class_abbrs_chr <- missing_class_abbrs_chr
    }

  }
  return(pkg_setup_ls)
}
