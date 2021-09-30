update_abbr_lup <- function(abbr_tb,
                            short_name_chr,
                            long_name_chr,
                            no_plural_chr = NA_character_,
                            custom_plural_ls = NULL,
                            pfx_rgx = NA_character_){
  testit::assert(paste0("No duplicates are allowed in an abbreviations lookup table. The following duplicates are in the short_name_chr column:\n",
                        abbr_tb$short_name_chr[duplicated(abbr_tb$short_name_chr)] %>% make_list_phrase()),
                 !any(duplicated(abbr_tb$short_name_chr)))
  testit::assert(paste0("No duplicates are allowed in an abbreviations lookup table. The following duplicates are in the long_name_chr column:\n",
                        abbr_tb$long_name_chr[duplicated(abbr_tb$long_name_chr)] %>% make_list_phrase()),
                 !any(duplicated(abbr_tb$long_name_chr)))
  if(!"plural_lgl" %in% names(abbr_tb))
    abbr_tb <- dplyr::mutate(abbr_tb, plural_lgl = NA)
  if(!is.na(pfx_rgx))
    abbr_tb <- abbr_tb %>%
    dplyr::mutate(long_name_chr = purrr::map_chr(long_name_chr,
                                                 ~ stringi::stri_replace_first_regex(.x,
                                                                                     pfx_rgx,
                                                                                     "")))
  new_tb <- tibble::tibble(short_name_chr = short_name_chr,
                           long_name_chr = long_name_chr) %>%
    add_plurals_to_abbr_lup(no_plural_chr = no_plural_chr,
                            custom_plural_ls = custom_plural_ls) #%>% tidyr::drop_na()
  abbr_tb <- add_lups(abbr_tb,
                      new_lup = new_tb,
                      key_var_nm_1L_chr = "short_name_chr")
  # tibble::tibble(short_name_chr = make.unique(c(abbr_tb$short_name_chr,new_tb$short_name_chr)),
  #                         long_name_chr = make.unique(c(abbr_tb$long_name_chr,new_tb$long_name_chr)),
  #                         plural_lgl = c(abbr_tb$plural_lgl,new_tb$plural_lgl)) %>%
  # dplyr::arrange(short_name_chr) %>%
  # dplyr::distinct()
  return(abbr_tb)
}
update_abbrs <- function(pkg_setup_ls,
                         short_name_chr,
                         long_name_chr,
                         no_plural_chr = NA_character_,
                         custom_plural_ls = NULL,
                         pfx_rgx = NA_character_){
  short_dupls_chr <- intersect(short_name_chr,
                               pkg_setup_ls$subsequent_ls$abbreviations_lup$short_name_chr)
  long_dupls_chr <- intersect(long_name_chr,
                              pkg_setup_ls$subsequent_ls$abbreviations_lup$long_name_chr)
  testit::assert(paste0("No duplicates are allowed in the abbreviations lookup table. You are attempting to add the following duplicate values to the short_name_chr column:\n",
                        short_dupls_chr %>% make_list_phrase()),
                 identical(short_dupls_chr, character(0)))
  testit::assert(paste0("No duplicates are allowed in the abbreviations lookup table. You are attempting to add the following duplicate values from the 'long_name_chr' argument to the long_name_chr column of the abbreviations lookup tbale:\n",
                        long_dupls_chr %>% make_list_phrase()),
                 identical(long_dupls_chr, character(0)))
  if(is.null(pkg_setup_ls$subsequent_ls$abbreviations_lup))
    pkg_setup_ls$subsequent_ls$abbreviations_lup <- make_obj_lup(obj_lup_spine = make_obj_lup_spine(NULL)) %>%
    dplyr::filter(F)
  pkg_setup_ls$subsequent_ls$abbreviations_lup <- pkg_setup_ls$subsequent_ls$abbreviations_lup %>%
    update_abbr_lup(short_name_chr = short_name_chr,
                    long_name_chr = long_name_chr,
                    no_plural_chr = no_plural_chr,
                    custom_plural_ls = custom_plural_ls,
                    pfx_rgx = pfx_rgx)
  return(pkg_setup_ls)
}
update_first_word_case <- function(phrase_1L_chr,
                                   fn = tolower){
  phrase_1L_chr <- paste0(phrase_1L_chr %>% stringr::str_sub(end=1) %>% fn,
                          phrase_1L_chr %>% stringr::str_sub(start=2))
  return(phrase_1L_chr)
}
update_fn_dmt_with_slots <- function(fn_name_1L_chr,
                                     fn_dmt_1L_chr){
  slots_chr <- get_r4_obj_slots(fn_name_1L_chr)
  fn_dmt_1L_chr <- purrr::reduce(1:length(slots_chr),
                                 .init = fn_dmt_1L_chr,
                                 ~ .x %>%
                                   stringr::str_replace(paste0(names(slots_chr)[.y], " PARAM_DESCRIPTION"),
                                                        paste0(names(slots_chr)[.y]," ", slots_chr[.y])))
  return(fn_dmt_1L_chr)

}
update_fn_dmt <- function(fn_tags_spine_ls,
                          new_tag_chr_ls,
                          fn_name_1L_chr,
                          fn_type_1L_chr,
                          import_chr,
                          import_from_chr = NA_character_,
                          import_mthds_from_chr = NA_character_,
                          abbreviations_lup){
  fn_dmt_1L_chr <- fn_tags_spine_ls$fn_tags_1L_chr
  fn_dmt_1L_chr <- fn_dmt_1L_chr %>%
    stringr::str_replace("FUNCTION_TITLE",fn_name_1L_chr) %>%
    stringr::str_replace("FUNCTION_DESCRIPTION",
                         paste0(ifelse(is.na(new_tag_chr_ls$desc_start_1L_chr),
                                       "FUNCTION_DESCRIPTION",
                                       new_tag_chr_ls$desc_start_1L_chr),
                                ifelse((fn_type_1L_chr %in% c("fn","gen_std_s3_mthd",
                                                              "meth_std_s3_mthd",
                                                              "gen_std_s4_mthd",
                                                              "meth_std_s4_mthd") | startsWith(fn_type_1L_chr,"s3_")) ,
                                       "",
                                       fn_tags_spine_ls$ref_slot_1L_chr))) %>%
    stringr::str_replace("OUTPUT_DESCRIPTION",new_tag_chr_ls$output_txt_1L_chr)
  fn_dmt_1L_chr <- fn_dmt_1L_chr %>%
    stringr::str_replace("@details DETAILS",
                         ifelse(fn_type_1L_chr == "s3_valid_instance" | ifelse(is.na(new_tag_chr_ls$fn_det_1L_chr),
                                                                               F,
                                                                               new_tag_chr_ls$fn_det_1L_chr!="DETAILS"),
                                paste0("@details ",new_tag_chr_ls$fn_det_1L_chr),
                                ""))
  if(!is.null(new_tag_chr_ls$arg_desc_chr)){
    fn_dmt_1L_chr <- purrr::reduce(1:length(new_tag_chr_ls$arg_desc_chr),
                                   .init = fn_dmt_1L_chr,
                                   ~{
                                     stringr::str_replace(.x,
                                                          paste0("@param ",names(new_tag_chr_ls$arg_desc_chr)[.y]," PARAM_DESCRIPTION"),
                                                          paste0("@param ",names(new_tag_chr_ls$arg_desc_chr)[.y]," ",ifelse(new_tag_chr_ls$arg_desc_chr[.y]=="NO MATCH",
                                                                                                                             ifelse(names(new_tag_chr_ls$arg_desc_chr[.y])!="x",
                                                                                                                                    "PARAM_DESCRIPTION",
                                                                                                                                    "An object"),
                                                                                                                             #"PARAM_DESCRIPTION",
                                                                                                                             new_tag_chr_ls$arg_desc_chr[.y])))
                                   })

  }
  fn_dmt_1L_chr <- fn_dmt_1L_chr %>%
    stringr::str_replace("@param \\... PARAM_DESCRIPTION",
                         paste0("@param ... ", "Additional arguments"))
  if(!is.null(new_tag_chr_ls$s3_class_main_1L_chr)){
    if(fn_type_1L_chr == "s3_valid_instance"){
      fn_dmt_1L_chr <- stringr::str_replace(fn_dmt_1L_chr,
                                            names(new_tag_chr_ls$s3_class_main_1L_chr),
                                            new_tag_chr_ls$s3_class_main_1L_chr)
    }else{
      fn_dmt_1L_chr <- fn_dmt_1L_chr %>%
        stringr::str_replace(names(new_tag_chr_ls$s3_class_main_1L_chr),
                             paste0(make_fn_title(names(new_tag_chr_ls$s3_class_main_1L_chr),
                                                  object_type_lup = abbreviations_lup,
                                                  abbreviations_lup = abbreviations_lup),
                                    " ",
                                    get_arg_obj_type(new_tag_chr_ls$s3_class_main_1L_chr,
                                                     object_type_lup = abbreviations_lup))
        )
    }
  }
  if(!is.na(import_chr))
    fn_dmt_1L_chr <- paste0(fn_dmt_1L_chr,
                            "\n#' @import ",
                            stringr::str_c(import_chr,collapse = " "))
  if(!is.na(import_from_chr))
    fn_dmt_1L_chr <- paste0(fn_dmt_1L_chr,
                            "\n#' @import_from_chr ",
                            stringr::str_c(import_from_chr,collapse = " "))
  if(!is.na(import_mthds_from_chr))
    fn_dmt_1L_chr <- paste0(fn_dmt_1L_chr,
                            "\n#' @import_mthds_from_chr ",
                            stringr::str_c(import_mthds_from_chr,collapse = " "))
  if(fn_type_1L_chr == "gen_std_s3_mthd"){
    fn_dmt_1L_chr <- stringr::str_replace(fn_dmt_1L_chr,
                                          paste0("@name ", fn_name_1L_chr),
                                          paste0("@rdname ", fn_name_1L_chr,"-methods"))
  }
  if(fn_type_1L_chr == "meth_std_s3_mthd"){
    fn_dmt_1L_chr <- stringr::str_replace(fn_dmt_1L_chr,
                                          paste0("@rdname ", fn_name_1L_chr),
                                          paste0("@rdname ",
                                                 fn_name_1L_chr %>%
                                                   stringr::str_sub(end = -1 + stringr::str_locate(fn_name_1L_chr,"\\.")[1,1] %>%
                                                                      as.vector())
                                                 ,"-methods"))
  }
  return(fn_dmt_1L_chr)
}
update_fns_dmt_tb <- function(fns_dmt_tb,
                              title_ls = NULL,
                              desc_ls = NULL,
                              details_ls = NULL,
                              inc_for_main_user_lgl_ls = NULL,
                              output_ls = NULL,
                              example_ls = NULL,
                              args_ls_ls = NULL,
                              append_1L_lgl = T){
  lgl_vecs_ls <- list(chr_vars_to_upd_lgl = list(title_ls,desc_ls,details_ls,output_ls) %>% purrr::map_lgl(~!is.null(.x)),
                      lgl_vars_to_upd_lgl = list(inc_for_main_user_lgl_ls,example_ls) %>% purrr::map_lgl(~!is.null(.x)),
                      arg_ls_to_upd_lgl = !is.null(args_ls_ls))
  input_ls_ls <- list(chr_input_ls = list(variable_chr = c("title_chr","desc_chr","details_chr","output_chr"),
                                          data_chr = c("title_ls","desc_ls","details_ls","output_ls")),
                      lgl_input_ls = list(variable_chr = c("inc_for_main_user_lgl","example_lgl"),
                                          data_chr = c("inc_for_main_user_lgl_ls","example_ls")),
                      ls_input_ls = list(variable_chr = c("args_ls"),
                                         data_chr = c("args_ls_ls")))
  fns_dmt_tb <- purrr::reduce(1:3,
                              .init = fns_dmt_tb,
                              ~ {
                                updated_fns_dmt_tb <- .x
                                idx_1L_dbl <- .y
                                fn <- list(update_fns_dmt_tb_chr_vars,
                                           update_fns_dmt_tb_lgl_vars,
                                           update_fns_dmt_tb_ls_vars)[[idx_1L_dbl]]

                                if(any(lgl_vecs_ls[[idx_1L_dbl]])){
                                  input_ls <- input_ls_ls[[idx_1L_dbl]] %>% purrr::map(~.x[lgl_vecs_ls[[idx_1L_dbl]]])
                                  updated_fns_dmt_tb <- purrr::reduce(1:length(lgl_vecs_ls[[idx_1L_dbl]]),
                                                                      .init = updated_fns_dmt_tb,
                                                                      ~ {
                                                                        eval(parse(text = paste0("new_ls <- ",input_ls[[2]]#[.y]
                                                                        )))
                                                                        args_ls <- list(.x,
                                                                                        data_1L_chr = input_ls[[1]],#[.y],
                                                                                        new_ls =  new_ls,
                                                                                        append_1L_lgl = append_1L_lgl)
                                                                        if(idx_1L_dbl==2)
                                                                          args_ls$append_1L_lgl <- NULL
                                                                        rlang::exec(fn,
                                                                                    !!!args_ls)
                                                                      })

                                }
                                updated_fns_dmt_tb
                              })
  return(fns_dmt_tb)
}
update_fns_dmt_tb_ls_vars <- function(fns_dmt_tb,
                                      data_1L_chr,
                                      new_ls,
                                      append_1L_lgl){
  if(is.na(data_1L_chr)){
    fns_dmt_tb <- fns_dmt_tb
  }else{
    fns_dmt_tb <- dplyr::mutate(fns_dmt_tb,!!rlang::sym(data_1L_chr) := dplyr::case_when(fns_chr %in% names(new_ls) ~  purrr::map2(new_ls[names(new_ls) %in% fns_chr],
                                                                                                                                   names(new_ls)[names(new_ls) %in% fns_chr],
                                                                                                                                   ~{
                                                                                                                                     fn_args_chr <- .x
                                                                                                                                     fn_nm_1L_chr <- .y
                                                                                                                                     old_args_chr <- fns_dmt_tb$args_ls[fns_dmt_tb$fns_chr == fn_nm_1L_chr][[1]]
                                                                                                                                     if(!append_1L_lgl)
                                                                                                                                       testit::assert("When not appending, each function whose argument description text is being updated must have new argument descriptions for ALL arguments.",
                                                                                                                                                      ifelse(length(old_args_chr)==length(fn_args_chr),names(old_args_chr) %>% sort()==names(fn_args_chr) %>% sort(),F))
                                                                                                                                     new_args_chr <- purrr::map2_chr(fn_args_chr,
                                                                                                                                                                     names(fn_args_chr),
                                                                                                                                                                     ~ {
                                                                                                                                                                       if(append_1L_lgl){
                                                                                                                                                                         paste0(old_args_chr[.y],". ",.x)
                                                                                                                                                                       }else{
                                                                                                                                                                         .x
                                                                                                                                                                       }
                                                                                                                                                                     })
                                                                                                                                     purrr::map_chr(names(old_args_chr),
                                                                                                                                                    ~ ifelse(.x %in% names(new_args_chr),
                                                                                                                                                             new_args_chr[.x],
                                                                                                                                                             old_args_chr[.x])) %>%
                                                                                                                                       stats::setNames(names(old_args_chr))
                                                                                                                                   }),
                                                                                         TRUE ~ !!rlang::sym(data_1L_chr))
    )
  }
  return(fns_dmt_tb)
}
update_fns_dmt_tb_lgl_vars <- function(fns_dmt_tb,
                                       data_1L_chr,
                                       new_ls){
  if(is.na(data_1L_chr)){
    fns_dmt_tb <- fns_dmt_tb
  }else{
    fns_dmt_tb <- dplyr::mutate(fns_dmt_tb,!!rlang::sym(data_1L_chr) := dplyr::case_when(fns_chr %in% new_ls$force_true_chr ~ T,
                                                                                         fns_chr %in% new_ls$force_false_chr ~ F,
                                                                                         TRUE ~ !!rlang::sym(data_1L_chr))
    )
  }
  return(fns_dmt_tb)
}
update_fns_dmt_tb_chr_vars <- function(fns_dmt_tb,
                                       data_1L_chr,
                                       new_ls,
                                       append_1L_lgl){
  if(is.na(data_1L_chr)){
    fns_dmt_tb <- fns_dmt_tb
  }else{
    fns_dmt_tb <- dplyr::mutate(fns_dmt_tb,!!rlang::sym(data_1L_chr) := dplyr::case_when(fns_chr %in% names(new_ls) ~ paste0(ifelse(append_1L_lgl,
                                                                                                                                    paste0(ifelse(is.na(!!rlang::sym(data_1L_chr)),
                                                                                                                                                  "",
                                                                                                                                                  !!rlang::sym(data_1L_chr)),
                                                                                                                                           ""),
                                                                                                                                    ""),
                                                                                                                             fns_chr %>% purrr::map_chr(~ {
                                                                                                                               ifelse(.x %in% names(new_ls),
                                                                                                                                      new_ls[[.x]],
                                                                                                                                      NA_character_)
                                                                                                                             }
                                                                                                                             )
    ),
    TRUE ~ !!rlang::sym(data_1L_chr))
    )
  }
  return(fns_dmt_tb)
}
update_msng_abbrs <- function(pkg_setup_ls,
                              are_words_chr = NA_character_,
                              tf_to_singular_chr = NA_character_,
                              not_obj_type_chr = NA_character_){
  # Note: This works as part of current workflow as only one missing message for object type and abbrs lookup is generated at a time.
  if(!is.null(pkg_setup_ls$problems_ls$missing_abbrs_chr)){
    if(!is.na(tf_to_singular_chr[1])){
      testit::assert("'tf_to_singular_chr' needs to be a named vector. The name of each vector element should be the desired new name for that element.",
                     length(names(tf_to_singular_chr) %>%
                              purrr::discard(~.x==""))== length(tf_to_singular_chr %>%
                                                                  purrr::discard(is.na)))
      pkg_setup_ls$problems_ls$missing_abbrs_chr <- c(setdiff(pkg_setup_ls$problems_ls$missing_abbrs_chr,
                                                              tf_to_singular_chr),
                                                      names(tf_to_singular_chr)) %>%
        unique() %>%
        sort()
    }
    pkg_setup_ls$problems_ls$missing_abbrs_chr <- setdiff(pkg_setup_ls$problems_ls$missing_abbrs_chr,
                                                          are_words_chr)
    pkg_setup_ls$problems_ls$missing_words_chr <- are_words_chr
  }
  if(!is.null(pkg_setup_ls$problems_ls$missing_obj_types_chr)){
    pkg_setup_ls$problems_ls$missing_obj_types_chr <- setdiff(pkg_setup_ls$problems_ls$missing_obj_types_chr,
                                                              c(not_obj_type_chr, are_words_chr)) %>%
      unique() %>%
      sort()
    pkg_setup_ls$problems_ls$missing_words_chr <- are_words_chr
  }
  return(pkg_setup_ls)
}
update_ns <- function(package_1L_chr){
  package_nm_chr <- ifelse(package_1L_chr=="",".GlobalEnv",package_1L_chr)
  return(package_nm_chr)
}
update_pkg_setup_msgs <- function(pkg_setup_ls,
                                  list_element_1L_chr){
  pkg_setup_ls$problems_ls[[which(names(pkg_setup_ls$problems_ls)==list_element_1L_chr)]] <- NULL
  if(length(pkg_setup_ls$problems_ls)==0)
    pkg_setup_ls[[which(names(pkg_setup_ls)=="problems_ls")]] <- NULL
  return(pkg_setup_ls)
}
update_pt_fn_args_ls <- function(args_ls){
  arg_lgths_dbl <- args_ls %>% purrr::map_dbl(~length(.x))
  arg_max_lgth_1L_dbl <- max(arg_lgths_dbl)
  updated_args_ls <- purrr::map2(args_ls %>% unname(),
                                 unname(arg_lgths_dbl==0 & arg_lgths_dbl != arg_max_lgth_1L_dbl),
                                 ~{
                                   val_xx <- .x
                                   if(.y){
                                     val_xx <- parse(text=paste0(
                                       ifelse(is.character(val_xx),
                                              "NA_character_",
                                              ifelse(is.integer(val_xx),
                                                     "NA_integer_",
                                                     ifelse(is.complex(val_xx),
                                                            "NA_complex_",
                                                            ifelse(is.numeric(val_xx),
                                                                   "NA_real_",
                                                                   ifelse(is.logical(val_xx),
                                                                          "NA",
                                                                          "list(NULL)"))))))) %>% eval()
                                   }
                                   val_xx
                                 }) %>%
    stats::setNames(names(args_ls))
  return(updated_args_ls)
}
