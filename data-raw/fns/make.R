make_addl_pkgs_ls <- function(depends_chr = NULL,
                              enhances_chr = NULL,
                              imports_chr = NULL,
                              linking_to_chr = NULL,
                              suggests_chr = NULL,
                              append_ls = NULL){
  addl_pkgs_ls <- append(list(Depends = depends_chr,
                              Enhances = enhances_chr,
                              Imports = imports_chr,
                              LinkingTo = linking_to_chr,
                              Suggests = suggests_chr),
                         append_ls) %>%
    purrr::discard(is.null)
  if(length(addl_pkgs_ls)==0)
    addl_pkgs_ls <- NULL
  return(addl_pkgs_ls)
}
make_arg_desc <- function(fn_args_chr,
                          object_type_lup = NULL,
                          abbreviations_lup = NULL,
                          dv_ds_nm_1L_chr = "ready4-dev/ready4",
                          dv_url_pfx_1L_chr = deprecated(),
                          key_1L_chr = deprecated(),
                          server_1L_chr = deprecated()){
  if(is.null(abbreviations_lup))
    abbreviations_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "abbreviations_lup",
                                               piggyback_to_1L_chr = dv_ds_nm_1L_chr)
  if(is.null(object_type_lup))
    object_type_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "object_type_lup",
                                             piggyback_to_1L_chr = dv_ds_nm_1L_chr)
  arg_desc_chr <- make_arg_type(fn_args_chr,
                                object_type_lup = object_type_lup,
                                abbreviations_lup = abbreviations_lup,
                                fn = make_arg_desc_spine,
                                dv_ds_nm_1L_chr = dv_ds_nm_1L_chr,
                                dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                key_1L_chr = key_1L_chr,
                                server_1L_chr = server_1L_chr)
  arg_desc_chr <- arg_desc_chr %>% stringr::str_remove(" \\(an additional arguments\\)")
  return(arg_desc_chr)
}
make_arg_desc_ls <- function(fn_nms_chr,
                             fns_env_ls,
                             abbreviations_lup = NULL,
                             dv_ds_nm_1L_chr = "ready4-dev/ready4",
                             dv_url_pfx_1L_chr = deprecated(),
                             key_1L_chr = deprecated(),
                             object_type_lup = NULL,
                             server_1L_chr = deprecated()){
  if(is.null(abbreviations_lup))
    abbreviations_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "abbreviations_lup",
                                               piggyback_to_1L_chr = dv_ds_nm_1L_chr)
  if(is.null(object_type_lup))
    object_type_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "object_type_lup",
                                             piggyback_to_1L_chr = dv_ds_nm_1L_chr)
  arg_desc_ls <- purrr::map(fn_nms_chr,
                            ~ {
                              if(!exists(.x)){
                                fn <- fns_env_ls$fns_env[[.x]]
                              }else{
                                fn <- eval(parse(text=.x))
                              }
                              # eval(parse(text = paste0("fn <- ",.x)))
                              get_fn_args(fn) %>% make_arg_desc(abbreviations_lup = abbreviations_lup,
                                                                object_type_lup = object_type_lup,
                                                                dv_ds_nm_1L_chr = dv_ds_nm_1L_chr,
                                                                dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                                                key_1L_chr = key_1L_chr,
                                                                server_1L_chr = server_1L_chr) %>%
                                stats::setNames(get_fn_args(fn))
                            }
  )
  return(arg_desc_ls)
}
make_arg_desc_spine <- function(argument_nm_1L_chr,
                                object_type_lup = NULL,
                                abbreviations_lup = NULL,
                                dv_ds_nm_1L_chr = "ready4-dev/ready4",
                                dv_url_pfx_1L_chr = deprecated(),
                                key_1L_chr = deprecated(),
                                master_object_type_lup = NULL,
                                server_1L_chr = deprecated()){
  if(is.null(abbreviations_lup))
    abbreviations_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "abbreviations_lup",
                                               piggyback_to_1L_chr = dv_ds_nm_1L_chr)
  if(is.null(object_type_lup))
    object_type_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "object_type_lup",
                                             piggyback_to_1L_chr = dv_ds_nm_1L_chr)
  if(is.null(master_object_type_lup))
    master_object_type_lup <- object_type_lup
  if(is.na(argument_nm_1L_chr)){
    match_1L_chr <- character(0)
  }else{
    match_1L_chr <- get_arg_obj_type(argument_nm_1L_chr,
                                     object_type_lup = object_type_lup)
  }
  arg_desc_spine_1L_chr <- ifelse(identical(match_1L_chr,character(0)),
                           NA_character_,
                           paste0(argument_nm_1L_chr %>% make_arg_title(match_chr = match_1L_chr,
                                                                        abbreviations_lup = abbreviations_lup,
                                                                        object_type_lup = master_object_type_lup),
                                  " (",
                                  match_1L_chr %>% update_first_word_case() %>%
                                    add_indefartls_to_phrases(abbreviations_lup = abbreviations_lup,
                                                              ignore_phrs_not_in_lup_1L_lgl = F),
                                  ")"))
  return(arg_desc_spine_1L_chr)
}
make_arg_title <- function(args_chr,
                           match_chr,
                           object_type_lup = NULL,
                           abbreviations_lup = NULL,
                           dv_ds_nm_1L_chr = "ready4-dev/ready4",
                           dv_url_pfx_1L_chr = deprecated(),
                           key_1L_chr = deprecated(),
                           server_1L_chr = deprecated()){
  if(is.null(abbreviations_lup))
    abbreviations_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "abbreviations_lup",
                                               piggyback_to_1L_chr = dv_ds_nm_1L_chr)
  if(is.null(object_type_lup))
    object_type_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "object_type_lup",
                                             piggyback_to_1L_chr = dv_ds_nm_1L_chr)
  suffices_chr <- match_chr %>% purrr::map_chr(~{
    ifelse(.x=="NO MATCH",
           "",
           ready4::get_from_lup_obj(object_type_lup,
                                    match_value_xx = .x,
                                    match_var_nm_1L_chr = "long_name_chr",
                                    target_var_nm_1L_chr = "short_name_chr",
                                    evaluate_1L_lgl = F))

  })
  title_chr <- purrr::map2_chr(args_chr,
                               suffices_chr,
                               ~ ifelse(.y=="",
                                        .x,
                                        stringi::stri_replace_last_fixed(.x,
                                                                         paste0("_",.y),
                                                                         ""))) %>%
    stringr::str_replace_all("_"," ") %>%
    purrr::map_chr(~replace_abbr(.x,
                                 abbreviations_lup = abbreviations_lup) %>%
                     stringi::stri_replace_last_fixed(" R","")) %>%
    Hmisc::capitalize()
  return(title_chr)
}
make_arg_type_abbr <- function(fn_args_chr,
                               object_type_lup = NULL,
                               abbreviations_lup = NULL,
                               dv_ds_nm_1L_chr = "ready4-dev/ready4",
                               dv_url_pfx_1L_chr = deprecated(),
                               key_1L_chr = deprecated(),
                               server_1L_chr = deprecated()){#
  if(is.null(abbreviations_lup))
    abbreviations_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "abbreviations_lup",
                                               piggyback_to_1L_chr = dv_ds_nm_1L_chr)
  if(is.null(object_type_lup))
    object_type_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "object_type_lup",
                                             piggyback_to_1L_chr = dv_ds_nm_1L_chr)
  arg_type_abbr_chr <- make_arg_type(fn_args_chr,
                                     object_type_lup = object_type_lup,
                                     fn = make_arg_type_abbr_spine,
                                     abbreviations_lup = abbreviations_lup,
                                     dv_ds_nm_1L_chr = dv_ds_nm_1L_chr,
                                     dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                     key_1L_chr = key_1L_chr,
                                     server_1L_chr = server_1L_chr)
  return(arg_type_abbr_chr)
}
make_arg_type_abbr_spine <- function(argument_nm_1L_chr,
                                     object_type_lup){
  arg_type_1L_chr <- object_type_lup$short_name_chr[endsWith(argument_nm_1L_chr,
                                                             object_type_lup$short_name_chr)]
  arg_type_abbr_spine_1L_chr <- ifelse(identical(character(0),
                                                 arg_type_1L_chr),
                                       NA_character_,
                                       arg_type_1L_chr)
  return(arg_type_abbr_spine_1L_chr)
}
make_arg_type <- function(fn_args_chr,
                          object_type_lup = NULL,
                          abbreviations_lup = NULL,
                          dv_ds_nm_1L_chr = "ready4-dev/ready4",
                          dv_url_pfx_1L_chr = deprecated(),
                          fn,
                          key_1L_chr = deprecated(),
                          server_1L_chr = deprecated()){
  if(is.null(abbreviations_lup))
    abbreviations_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "abbreviations_lup",
                                               piggyback_to_1L_chr = dv_ds_nm_1L_chr)
  if(is.null(object_type_lup))
    object_type_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "object_type_lup",
                                             piggyback_to_1L_chr = dv_ds_nm_1L_chr)
  lup_ls <- make_arg_type_lup_ls(object_type_lup)
  append_1L_lgl <- "abbreviations_lup" %in% get_fn_args(fn)
  append_master_1L_lgl <- "master_object_type_lup" %in% get_fn_args(fn)
  arg_desc_chr <- fn_args_chr %>%
    purrr::map_chr(~{
      argument_nm_1L_chr <- .x
      arg_desc_1L_chr <- purrr::map_chr(lup_ls,
                                        ~ {
                                          args_ls <- list(argument_nm_1L_chr,
                                                          .x) %>%
                                            stats::setNames(c("argument_nm_1L_chr",
                                                              "object_type_lup"))
                                          if(append_1L_lgl)
                                            args_ls <- append(args_ls, list(abbreviations_lup = abbreviations_lup))
                                          if(append_master_1L_lgl)
                                            args_ls <- append(args_ls,
                                                              list(dv_ds_nm_1L_chr = dv_ds_nm_1L_chr,
                                                                   dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                                                   key_1L_chr = key_1L_chr,
                                                                   master_object_type_lup = object_type_lup,
                                                                   server_1L_chr = server_1L_chr))
                                          rlang::exec(fn,!!!args_ls)
                                        }) %>%
        purrr::discard(is.na) %>%
        purrr::pluck(1)
      if(is.null(arg_desc_1L_chr))
        arg_desc_1L_chr <- "NO MATCH"
      arg_desc_1L_chr
    })
  return(arg_desc_chr)
}
make_arg_type_lup_ls <- function(object_type_lup = NULL,
                                 dv_ds_nm_1L_chr = "ready4-dev/ready4",
                                 dv_url_pfx_1L_chr = deprecated(),
                                 key_1L_chr = deprecated(),
                                 server_1L_chr = deprecated()){
  if(is.null(object_type_lup))
    object_type_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "object_type_lup",
                                             piggyback_to_1L_chr = dv_ds_nm_1L_chr)
  new_lup <- object_type_lup %>%
    dplyr::mutate(nchar_int = nchar(short_name_chr))
  lup_ls <- new_lup$nchar_int %>% unique() %>%
    sort(decreasing = T) %>%
    purrr::map(~dplyr::filter(new_lup,nchar_int==.x))
  return(lup_ls)
}
make_build_ignore_ls <- function(file_nms_chr = NULL,
                                 regulars_rgx = NULL){
  build_ignore_ls = list(file_nms_chr = file_nms_chr,
                         regulars_rgx = regulars_rgx)
  return(build_ignore_ls)
}
make_custom_dmt_ls <- function(args_ls_ls = NULL,
                               desc_ls = NULL,
                               details_ls = NULL,
                               example_ls = NULL,
                               output_ls = NULL,
                               title_ls = NULL,
                               user_manual_fns_chr = NA_character_){
  inc_for_main_user_lgl_ls <- NULL
  if(!is.na(user_manual_fns_chr[1]))
    inc_for_main_user_lgl_ls <- list(force_true_chr = user_manual_fns_chr,
                                     force_false_chr = NA_character_)
  custom_dmt_ls = list(args_ls_ls = args_ls_ls,
                       desc_ls = desc_ls,
                       details_ls = details_ls,
                       example_ls = example_ls,
                       inc_for_main_user_lgl_ls = inc_for_main_user_lgl_ls,
                       output_ls = output_ls,
                       title_ls = title_ls)
  return(custom_dmt_ls)
}
make_depnt_fns_ls <- function(arg_ls,
                              pkg_depcy_ls){
  lower_tb <- purrr::map_dfr(arg_ls$new_dbl,
                             ~ pkg_depcy_ls[["fromto"]] %>% dplyr::filter(to == .x))
  upper_tb <- dplyr::bind_rows(arg_ls$upper_tb, lower_tb) %>% dplyr::distinct()
  new_dbl <- setdiff(upper_tb$from,c(arg_ls$new_dbl,arg_ls$solo_dbl,arg_ls$upper_tb$from))
  solo_dbl <- c(arg_ls$solo_dbl,setdiff(lower_tb$from,new_dbl))
  arg_ls <- list(new_dbl = new_dbl,
                 solo_dbl = solo_dbl,
                 upper_tb = upper_tb)
  return(arg_ls)
}
make_dmt_for_all_fns <- function(paths_ls = make_fn_nms(),
                                 undocumented_fns_dir_chr = make_undmtd_fns_dir_chr(drop_empty_1L_lgl = T),
                                 custom_dmt_ls = make_custom_dmt_ls(),
                                 fns_env_ls = NULL,
                                 fn_types_lup,
                                 abbreviations_lup,
                                 object_type_lup,
                                 inc_all_mthds_1L_lgl = T){
  # add assert - same length inputs to purrr
  if (is.null(abbreviations_lup))
    utils::data("abbreviations_lup",
                package = "ready4fun",
                envir = environment())
  if(is.null(fns_env_ls))
    fns_env_ls <- read_fns(undocumented_fns_dir_chr)
  all_fns_dmt_tb <- purrr::map2_dfr(paths_ls,
                                    undocumented_fns_dir_chr,
                                    ~ {
                                      fns_dmt_tb <- make_fn_dmt_tbl(.x,
                                                                    fns_dir_chr = .y,
                                                                    custom_dmt_ls = custom_dmt_ls,
                                                                    append_1L_lgl = T,
                                                                    fns_env_ls = fns_env_ls,
                                                                    fn_types_lup = fn_types_lup,
                                                                    abbreviations_lup = abbreviations_lup,
                                                                    object_type_lup = object_type_lup)
                                      if(inc_all_mthds_1L_lgl)
                                        fns_dmt_tb <- fns_dmt_tb %>%
                                          dplyr::mutate(inc_for_main_user_lgl = dplyr::case_when(file_pfx_chr %in% c("grp_","mthd_") ~ T,
                                                                                                 TRUE ~ inc_for_main_user_lgl))
                                      fns_dmt_tb
                                    })
  return(all_fns_dmt_tb)
}
make_fn_desc <-  function(fns_chr,
                          title_chr,
                          output_chr,
                          fns_env_ls,
                          fn_types_lup = NULL,
                          abbreviations_lup,
                          test_for_write_R_warning_fn = NULL,
                          is_generic_lgl = F){
  if(is.null(test_for_write_R_warning_fn))
    test_for_write_R_warning_fn <- function(x){startsWith(x,"write")}
  fn_desc_chr <- purrr::pmap_chr(list(fns_chr,
                                      title_chr,
                                      output_chr,
                                      is_generic_lgl),
                                 ~ {
                                   fn_type_1L_chr <- stringr::str_extract(..2, '[A-Za-z]+')
                                   fn_name_1L_chr <- ..1
                                   fn_title_1L_chr <- ..2
                                   fn_output_1L_chr <- ..3
                                   is_generic_1L_lgl <- ..4
                                   if(!exists(fn_name_1L_chr)){
                                     fn <- fns_env_ls$fns_env[[fn_name_1L_chr]]
                                   }else{
                                     fn <- eval(parse(text=fn_name_1L_chr))
                                   }
                                   paste0(make_fn_desc_spine(fn,
                                                             fn_name_1L_chr = fn_name_1L_chr,
                                                             fn_title_1L_chr = fn_title_1L_chr,
                                                             fn_types_lup = fn_types_lup,
                                                             abbreviations_lup = abbreviations_lup,
                                                             is_generic_1L_lgl = is_generic_1L_lgl),
                                          ifelse(fn_output_1L_chr=="NULL",
                                                 ifelse(is_generic_1L_lgl,
                                                        "",
                                                        paste0(" The function is called for its side effects and does not return a value.",
                                                               ifelse(fn_name_1L_chr %>% test_for_write_R_warning_fn,#startsWith(fn_name_1L_chr,"write"),
                                                                      " WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour",
                                                                      ""))),
                                                 paste0(" The function returns ",
                                                        make_ret_obj_desc(fn,
                                                                          abbreviations_lup = abbreviations_lup,
                                                                          starts_sentence_1L_lgl = T),
                                                        ".")
                                          )
                                   )
                                 }
  )
  return(fn_desc_chr)
}
make_fn_desc_spine <- function(fn,
                               fn_name_1L_chr,
                               fn_title_1L_chr,
                               fn_types_lup,
                               abbreviations_lup,
                               is_generic_1L_lgl = NULL){
  fn_args_chr <- get_fn_args(fn)
  types_chr <- fn_types_lup$fn_type_nm_chr
  substr(types_chr, 1, 1) <- tolower(substr(types_chr, 1, 1))
  pfx_matches_chr <- fn_types_lup$fn_type_nm_chr[purrr::map_lgl(types_chr,#%>% Hmisc::capitalize()
                                                                ~ startsWith(fn_title_1L_chr,#%>% tools::toTitleCase()
                                                                             .x))]
  fn_type_chr <- pfx_matches_chr[nchar(pfx_matches_chr) == max(nchar(pfx_matches_chr))]
  text_elements_chr <- names(fn_types_lup)[2:4] %>%
    purrr::map_chr(~ ready4::get_from_lup_obj(fn_types_lup,
                                              match_var_nm_1L_chr = "fn_type_nm_chr",
                                              match_value_xx = fn_type_chr[1],
                                              target_var_nm_1L_chr = .x,
                                              evaluate_1L_lgl = F))
  if(is.null(is_generic_1L_lgl)){
    is_generic_1L_lgl <- fn_type_chr[1] == fn_name_1L_chr
  }
  treat_as_1L_chr <- ifelse(is_generic_1L_lgl,
                            "Generic",#
                            ifelse(purrr::map_lgl(abbreviations_lup$short_name_chr,
                                                  ~ endsWith(fn_name_1L_chr,paste0(".",.x))) %>% any(),
                                   "Method",
                                   "Function"))#
  fn_desc_spine_1L_chr <- paste0(fn_name_1L_chr,
                                 "() is ",
                                 add_indef_artl_to_item(fn_type_chr[1],
                                                        ignore_phrs_not_in_lup = F,
                                                        abbreviations_lup = abbreviations_lup),
                                 " ",
                                 tolower(treat_as_1L_chr),
                                 " that ",
                                 update_first_word_case(text_elements_chr[1]),
                                 ifelse(treat_as_1L_chr=="Generic",
                                        "",
                                        ifelse(treat_as_1L_chr == "Method",
                                               paste0(" This method is implemented for the ",
                                                      abbreviations_lup$long_name_chr[purrr::map_lgl(abbreviations_lup$short_name_chr,
                                                                                                     ~ endsWith(fn_name_1L_chr,paste0(".",.x)))]),
                                               paste0( " Specifically, this function implements an algorithm to ",
                                                       make_fn_title(fn_name_1L_chr,
                                                                     object_type_lup = abbreviations_lup,
                                                                     abbreviations_lup = abbreviations_lup,
                                                                     fn_types_lup = fn_types_lup,
                                                                     is_generic_lgl = T) %>% tolower(),
                                                       "."))),
                                 ifelse(ifelse(is.null(fn_args_chr)|is.na(text_elements_chr[2]),
                                               F,
                                               T),
                                        paste0(" Function argument ",
                                               fn_args_chr[1],
                                               " specifies the ",
                                               update_first_word_case(text_elements_chr[2])),
                                        ""),
                                 ifelse(ifelse(is.null(fn_args_chr)|is.na(text_elements_chr[3]),
                                               F,
                                               length(fn_args_chr)>1),
                                        paste0(" Argument ",
                                               fn_args_chr[2],
                                               " provides the ",
                                               update_first_word_case(text_elements_chr[3])),
                                        ""))
  return(fn_desc_spine_1L_chr)
}
make_fn_dmt_spine <- function(fn_name_1L_chr,
                              fn_type_1L_chr,
                              fn_title_1L_chr = NA_character_,
                              fn,
                              fn_types_lup = NULL,
                              details_1L_chr = NA_character_,
                              example_1L_lgl = F,
                              export_1L_lgl = T,
                              class_name_1L_chr,
                              doc_in_class_1L_lgl){
  get_set_chr <- c("gen_get_slot","meth_get_slot","gen_set_slot","meth_set_slot")
  if(!fn_type_1L_chr %in% get_set_chr){
    fn_dmt_spine_chr_ls <- make_std_fn_dmt_spine(fn_name_1L_chr = fn_name_1L_chr,
                                                 fn_type_1L_chr = fn_type_1L_chr,
                                                 fn_title_1L_chr = fn_title_1L_chr,
                                                 fn = fn,
                                                 fn_types_lup = fn_types_lup,
                                                 details_1L_chr = details_1L_chr,
                                                 doc_in_class_1L_lgl = doc_in_class_1L_lgl,
                                                 example_1L_lgl = example_1L_lgl,
                                                 export_1L_lgl = export_1L_lgl,
                                                 class_name_1L_chr = class_name_1L_chr,
                                                 exclude_if_match_chr = get_set_chr)
  }else{
    fn_dmt_spine_chr_ls <- make_gtr_str_dmt_spine(fn_type_1L_chr = fn_type_1L_chr,
                                                  fn_name_1L_chr = fn_name_1L_chr,
                                                  class_name_1L_chr = class_name_1L_chr,
                                                  doc_in_class_1L_lgl = doc_in_class_1L_lgl,
                                                  example_1L_lgl = example_1L_lgl)
  }
  return(fn_dmt_spine_chr_ls)
}
make_fn_dmt_tbl <- function(fns_path_chr,
                            fns_dir_chr = make_undmtd_fns_dir_chr(drop_empty_1L_lgl = T),
                            custom_dmt_ls = make_custom_dmt_ls(),
                            append_1L_lgl = T,
                            fns_env_ls = NULL,
                            fn_types_lup = NULL,
                            abbreviations_lup = NULL,
                            dv_ds_nm_1L_chr = "ready4-dev/ready4",
                            dv_url_pfx_1L_chr = deprecated(),
                            key_1L_chr = deprecated(),
                            object_type_lup = NULL,
                            server_1L_chr = deprecated(),
                            test_for_write_R_warning_fn = NULL){
  if(is.null(fns_env_ls))
    fns_env_ls <- read_fns(fns_dir_chr)
  if(is.null(abbreviations_lup))
    abbreviations_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "abbreviations_lup",
                                               piggyback_to_1L_chr = dv_ds_nm_1L_chr)
  if(is.null(object_type_lup))
    object_type_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "object_type_lup",
                                             piggyback_to_1L_chr = dv_ds_nm_1L_chr)
  fn_dmt_tbl_tb <- make_fn_dmt_tbl_tmpl(fns_path_chr,
                                        fns_dir_chr = fns_dir_chr,
                                        fns_env_ls = fns_env_ls,
                                        fn_types_lup = fn_types_lup,
                                        abbreviations_lup = abbreviations_lup,
                                        object_type_lup = object_type_lup,
                                        test_for_write_R_warning_fn = test_for_write_R_warning_fn)
  if(purrr::map_lgl(custom_dmt_ls,
                    ~ !is.null(.x)) %>% any()){
    args_ls <- append(custom_dmt_ls, list(append_1L_lgl = append_1L_lgl)) %>% purrr::discard(is.null)
    fn_dmt_tbl_tb <- rlang::exec(update_fns_dmt_tb, fns_dmt_tb = fn_dmt_tbl_tb, !!!args_ls)
  }
  return(fn_dmt_tbl_tb)
}
make_fn_dmt_tbl_tmpl <- function(fns_path_chr,
                                 fns_dir_chr = make_undmtd_fns_dir_chr(drop_empty_1L_lgl = T),
                                 fns_env_ls = NULL,
                                 fn_types_lup = NULL,
                                 abbreviations_lup = NULL,
                                 dv_ds_nm_1L_chr = "ready4-dev/ready4",
                                 dv_url_pfx_1L_chr = deprecated(),
                                 key_1L_chr = deprecated(),
                                 object_type_lup = NULL,
                                 server_1L_chr = deprecated(),
                                 test_for_write_R_warning_fn = NULL){
  if(is.null(fns_env_ls))
    fns_env_ls <- read_fns(fns_dir_chr)
  if(is.null(abbreviations_lup))
    abbreviations_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "abbreviations_lup",
                                               piggyback_to_1L_chr = dv_ds_nm_1L_chr)
  if(is.null(fn_types_lup))
    fn_types_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "fn_types_lup",
                                          piggyback_to_1L_chr = dv_ds_nm_1L_chr)
  if(is.null(object_type_lup))
    object_type_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "object_type_lup",
                                             piggyback_to_1L_chr = dv_ds_nm_1L_chr)
  file_pfx_chr <- fns_dir_chr %>% stringr::str_replace("data-raw/","") %>%
    switch("fns"="fn_", "s3" = "C3_","gnrcs"="grp_", "mthds"="mthd_","s4 = C4_")
  fn_dmt_tbl_tb <- fns_path_chr %>%
    purrr::map_dfr(~tibble::tibble(fns_chr = get_fn_nms_in_file(.x),
                                   title_chr = NA_character_,
                                   desc_chr = NA_character_,
                                   details_chr = NA_character_,
                                   inc_for_main_user_lgl = F,
                                   output_chr = NA_character_,
                                   example_lgl = F,
                                   args_ls = list(NULL),
                                   file_nm_chr = .x %>% stringr::str_replace(paste0(fns_dir_chr,"/"),""),
                                   file_pfx_chr = file_pfx_chr))
  fn_dmt_tbl_tb <- fn_dmt_tbl_tb %>%
    dplyr::mutate(title_chr = make_fn_title(fns_chr,
                                            abbreviations_lup = abbreviations_lup,
                                            fn_types_lup = fn_types_lup,
                                            object_type_lup = object_type_lup,
                                            is_generic_lgl = T#purrr::map_lgl(file_nm_chr, ~ .x == "generics.R") #is_generic_1L_lgl
    ))
  fn_types_chr <- fn_types_lup$fn_type_nm_chr
  substr(fn_types_chr, 1, 1) <- tolower(substr(fn_types_chr, 1, 1))
  fn_dmt_tbl_tb <- fn_dmt_tbl_tb %>%
    dplyr::filter(title_chr %>%
                    #tools::toTitleCase() %>%
                    purrr::map_lgl(~{
                      startsWith(.x, fn_types_chr) %>% any()
                    }))
  fn_dmt_tbl_tb <- fn_dmt_tbl_tb %>%
    dplyr::mutate(output_chr = get_outp_obj_type(fns_chr,
                                                 abbreviations_lup = abbreviations_lup,
                                                 fns_env_ls = fns_env_ls,
                                                 is_generic_lgl = purrr::map_lgl(file_nm_chr, ~ .x == "generics.R"),
                                                 object_type_lup = object_type_lup))
  fn_dmt_tbl_tb <- fn_dmt_tbl_tb %>%
    dplyr::mutate(desc_chr = make_fn_desc(fns_chr,
                                          title_chr = title_chr,
                                          output_chr = output_chr,
                                          fns_env_ls = fns_env_ls,
                                          fn_types_lup = fn_types_lup,
                                          abbreviations_lup = abbreviations_lup,
                                          test_for_write_R_warning_fn = test_for_write_R_warning_fn,
                                          is_generic_lgl = purrr::map_lgl(file_nm_chr, ~ .x == "generics.R")))
  fn_dmt_tbl_tb <- fn_dmt_tbl_tb %>%
    dplyr::mutate(args_ls = make_arg_desc_ls(fns_chr,
                                             fns_env_ls = fns_env_ls,
                                             abbreviations_lup = abbreviations_lup,
                                             object_type_lup = object_type_lup))
  return(fn_dmt_tbl_tb)
}
make_fn_title <- function(fns_chr,
                          object_type_lup = NULL,
                          abbreviations_lup = NULL,
                          dv_ds_nm_1L_chr = "ready4-dev/ready4",
                          dv_url_pfx_1L_chr = deprecated(),
                          fn_types_lup = NULL,
                          is_generic_lgl = F,
                          key_1L_chr = deprecated(),
                          server_1L_chr = deprecated()){
  if(is.null(abbreviations_lup))
    abbreviations_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "abbreviations_lup",
                                               piggyback_to_1L_chr = dv_ds_nm_1L_chr)
  if(is.null(fn_types_lup))
    fn_types_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "fn_types_lup",
                                          piggyback_to_1L_chr = dv_ds_nm_1L_chr)
  if(is.null(object_type_lup))
    object_type_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "object_type_lup",
                                             piggyback_to_1L_chr = dv_ds_nm_1L_chr)
  title_chr <- remove_obj_type_from_nm(fns_chr,
                                       object_type_lup = object_type_lup,
                                       abbreviations_lup = abbreviations_lup,
                                       fn_types_lup = fn_types_lup,
                                       is_generic_lgl = T) %>% # is_generic_lgl
    stringr::str_replace_all("_"," ") %>%
    #Hmisc::capitalize() %>%
    purrr::map_chr(~replace_abbr(.x,
                                 abbreviations_lup = abbreviations_lup) %>%
                     stringi::stri_replace_last_fixed(" R","")) %>%
    stringi::stri_replace_last_regex("\\.","")
  return(title_chr)
}
make_fn_type_lup <- function(fn_type_nm_chr = character(0),
                             fn_type_desc_chr = character(0),
                             first_arg_desc_chr = character(0),
                             second_arg_desc_chr = character(0),
                             is_generic_lgl = logical(0),
                             is_method_lgl = logical(0)#,is_type_lgl = logical(0)
){
  fn_types_lup <- tibble::tibble(fn_type_nm_chr = fn_type_nm_chr,
                                 fn_type_desc_chr = fn_type_desc_chr,
                                 first_arg_desc_chr = first_arg_desc_chr,
                                 second_arg_desc_chr = second_arg_desc_chr,
                                 is_generic_lgl = is_generic_lgl,
                                 is_method_lgl = is_method_lgl#,is_type_lgl = is_type_lgl
  ) %>%
    dplyr::arrange(fn_type_nm_chr)
  return(fn_types_lup)
}
make_fn_nms <- function(path_1L_chr = "data-raw"){
  undmtd_fns_dir_chr <- make_undmtd_fns_dir_chr(path_1L_chr,
                                                drop_empty_1L_lgl = T)
  fn_types_chr <- make_fn_types()
  fn_types_chr <- fn_types_chr[fn_types_chr %>%
                                 purrr::map_lgl(~{
                                   suffix_1L_lgl <- .x
                                   undmtd_fns_dir_chr %>%
                                     purrr::map_lgl(~endsWith(.x,
                                                              suffix_1L_lgl)) %>% any()
                                 })]
  fns_1L_chr_ls <- undmtd_fns_dir_chr %>%
    purrr::map(~list.files(.x,
                           pattern = "*.R$",
                           full.names = TRUE,
                           ignore.case = TRUE) ) %>%
    stats::setNames(fn_types_chr)
  fns_1L_chr_ls <- fns_1L_chr_ls %>% purrr::discard(~ identical(.x,character(0)))
  return(fns_1L_chr_ls)
}
make_fn_types <- function(){
  fns_type_chr <- c("fns","gnrcs","mthds")
  return(fns_type_chr)
}
make_gnrc_imports <- function(){
  generics_chr <- methods::getGenerics("package:ready4")@.Data
  gnrc_imports_chr <- rep("ready4",length(generics_chr)) %>% stats::setNames(generics_chr)
  more_generics_chr <- ls("package:generics")
  more_generics_chr <- setdiff(more_generics_chr, generics_chr)
  gnrc_imports_chr <- c(gnrc_imports_chr,
                        rep("generics",length(more_generics_chr)) %>% stats::setNames(more_generics_chr))

  return(gnrc_imports_chr)
}
make_gtr_str_dmt_spine <- function(fn_type_1L_chr,
                                   fn_name_1L_chr,
                                   class_name_1L_chr,
                                   doc_in_class_1L_lgl,
                                   example_1L_lgl = F){
  if(fn_type_1L_chr %in% c("gen_set_slot", "meth_set_slot")){
    ref_slot_1L_chr <- stringr::str_replace(fn_name_1L_chr,"<-","")
  }else{
    ref_slot_1L_chr <- fn_name_1L_chr
  }
  if(fn_type_1L_chr %in% c("gen_get_slot", "gen_set_slot"))
    fn_tags_1L_chr <- paste0(
      "#' FUNCTION_TITLE\n",
      "#' @description S4 Generic function to ",
      ifelse(fn_type_1L_chr == "gen_get_slot","get","set"),
      " the value of the slot ",
      ref_slot_1L_chr,
      "\n",
      "#' @rdname ",
      ref_slot_1L_chr,
      ifelse(fn_type_1L_chr == "gen_set_slot","_set",""),
      "-methods\n",
      "#' @param x An object ",
      class_name_1L_chr,
      "\n",
      ifelse(fn_type_1L_chr == "gen_set_slot","#' @param value Value to be assigned to x\n",""),
      "#' @details DETAILS\n",
      "#' @export\n"
    )
  if(fn_type_1L_chr %in% c("meth_get_slot", "meth_set_slot")){
    fn_tags_1L_chr <- paste0("#' ",
                             fn_name_1L_chr,
                             "\n#' @name ",
                             fn_name_1L_chr,
                             "-",
                             class_name_1L_chr,
                             "\n",
                             "#' @description FUNCTION_DESCRIPTION",
                             " for S4 objects of class ",
                             class_name_1L_chr,
                             "\n",
                             "#' @param x An object of class ",
                             class_name_1L_chr,
                             "\n",
                             ifelse(fn_type_1L_chr == "meth_set_slot",
                                    "#' @param value Value to be assigned to x\n",""),
                             ifelse(example_1L_lgl,
                                    paste0("#' @examples\n",
                                           "#' \\dontrun{\n",
                                           "#' if(interactive()){\n",
                                           "#'  #EXAMPLE1\n",
                                           "#'  }\n",
                                           "#' }\n"),""),
                             "#' @rdname ",
                             ifelse(doc_in_class_1L_lgl,
                                    class_name_1L_chr,
                                    paste0(ref_slot_1L_chr,
                                           ifelse(fn_type_1L_chr == "meth_set_slot",
                                                  "_set",
                                                  ""),
                                           "-methods\n")
                             ),
                             paste0("#' @aliases ",fn_name_1L_chr,",",class_name_1L_chr,"-method")
    )
  }
  gtr_str_dmt_spine_chr_ls <- list(fn_tags_1L_chr = fn_tags_1L_chr,
                                   ref_slot_1L_chr = ref_slot_1L_chr)
  return(gtr_str_dmt_spine_chr_ls)
}
make_lines_for_fn_dmt <- function(fn_name_1L_chr,
                                  fn_type_1L_chr,
                                  fn = NULL,
                                  fn_desc_1L_chr = NA_character_,
                                  fn_out_type_1L_chr = NA_character_,
                                  fn_title_1L_chr = NA_character_,
                                  example_1L_lgl = F,
                                  export_1L_lgl = T,
                                  class_name_1L_chr = "",
                                  details_1L_chr = "DETAILS",
                                  args_ls = NULL,
                                  import_chr = NA_character_,
                                  doc_in_class_1L_lgl = F,
                                  abbreviations_lup = NULL,
                                  dv_ds_nm_1L_chr = "ready4-dev/ready4",
                                  dv_url_pfx_1L_chr = deprecated(),
                                  fn_types_lup = NULL,
                                  import_from_chr = NA_character_,
                                  #import_mthds_from_chr = NA_character_,
                                  key_1L_chr = deprecated(),
                                  object_type_lup = NULL,
                                  server_1L_chr = deprecated()){
  if(is.null(abbreviations_lup))
    abbreviations_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "abbreviations_lup",
                                               piggyback_to_1L_chr = dv_ds_nm_1L_chr)
  if(is.null(fn_types_lup))
    fn_types_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "fn_types_lup",
                                          piggyback_to_1L_chr = dv_ds_nm_1L_chr)
  if(is.null(object_type_lup))
    object_type_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "object_type_lup",
                                             piggyback_to_1L_chr = dv_ds_nm_1L_chr)
  fn_tags_spine_ls <- make_fn_dmt_spine(fn_name_1L_chr = fn_name_1L_chr,
                                        fn_type_1L_chr = fn_type_1L_chr,
                                        fn_title_1L_chr = fn_title_1L_chr,
                                        fn = fn,
                                        fn_types_lup = fn_types_lup,
                                        example_1L_lgl = example_1L_lgl,
                                        export_1L_lgl = export_1L_lgl,
                                        details_1L_chr = details_1L_chr,
                                        class_name_1L_chr = class_name_1L_chr,
                                        doc_in_class_1L_lgl = doc_in_class_1L_lgl)
  new_tag_chr_ls <- make_new_fn_dmt(fn_type_1L_chr = fn_type_1L_chr,
                                    fn_name_1L_chr = fn_name_1L_chr,
                                    fn_desc_1L_chr = fn_desc_1L_chr,
                                    fn_det_1L_chr = details_1L_chr,
                                    fn_out_type_1L_chr = fn_out_type_1L_chr,
                                    args_ls = args_ls,
                                    class_name_1L_chr = class_name_1L_chr,
                                    fn = fn,
                                    abbreviations_lup = abbreviations_lup,
                                    object_type_lup = object_type_lup)
  fn_tags_chr <- update_fn_dmt(fn_tags_spine_ls = fn_tags_spine_ls,
                               new_tag_chr_ls = new_tag_chr_ls,
                               fn_name_1L_chr = fn_name_1L_chr,
                               fn_type_1L_chr = fn_type_1L_chr,
                               fn_types_lup = fn_types_lup,
                               import_chr = import_chr,
                               import_from_chr = import_from_chr,
                               abbreviations_lup = abbreviations_lup)
  writeLines(fn_tags_chr)
}
make_list_phrase <- function(items_chr){
  lifecycle::deprecate_soft("0.0.0.9446", "ready4fun::make_list_phrase()", "ready4::make_list_phrase()")
  list_phrase_1L_chr <- items_chr %>%
    stringr::str_c(sep="",collapse=", ") %>%
    stringi::stri_replace_last(fixed=",", replacement = " and")
  return(list_phrase_1L_chr)
}
make_manifest <- function(pkg_desc_ls,
                          copyright_holders_chr,
                          pkg_dmt_dv_dss_chr = deprecated(),
                          add_gh_site_1L_lgl = T,
                          addl_badges_ls = list(),
                          addl_pkgs_ls = make_addl_pkgs_ls(),#
                          badges_lup = tibble::tibble(),
                          build_ignore_ls = make_build_ignore_ls(),#
                          check_type_1L_chr = "ready4",
                          classify_1L_lgl = T,
                          cls_fn_ls = list(),
                          custom_dmt_ls = make_custom_dmt_ls(),
                          delete_r_dir_cnts_1L_lgl = T,
                          dev_pkg_nm_1L_chr = get_dev_pkg_nm(getwd()),
                          dev_pkgs_chr = NA_character_,
                          dss_records_ls = list(),
                          dv_url_pfx_1L_chr = character(0),
                          gh_repo_1L_chr = NA_character_,
                          import_from_chr = character(0),
                          lifecycle_stage_1L_chr = "experimental",
                          inc_pkg_meta_data_1L_lgl = F,
                          incr_ver_1L_lgl = F,
                          key_1L_chr = NULL,
                          on_cran_1L_lgl = F,
                          path_to_dmt_dir_1L_chr =  normalizePath("../../../../../Documentation/Code"),
                          path_to_pkg_logo_1L_chr = NA_character_,
                          path_to_pkg_rt_1L_chr = getwd(),
                          piggyback_to_1L_chr = NA_character_,
                          pkg_ds_ls_ls = list(),
                          ready4_type_1L_chr, #
                          s4_mthds_fn = NULL,
                          s4_mthds_ls = NULL,
                          server_1L_chr = Sys.getenv("DATAVERSE_SERVER"),
                          user_manual_fns_chr = NA_character_,
                          zenodo_badge_1L_chr = character(0)){
  if(is.na(gh_repo_1L_chr))
    gh_repo_1L_chr <- pkg_desc_ls$URL %>%
      strsplit(",") %>%
      unlist() %>%
      purrr::pluck(2) %>%
      stringr::str_trim() %>%
      stringr::str_remove("https://github.com/")
  #if(length(pkg_dmt_dv_dss_chr)<2){
  pkg_dmt_dv_dss_chr <- c(gh_repo_1L_chr,piggyback_to_1L_chr)#rep(pkg_dmt_dv_dss_chr, 2)
  #}
    if(!is.na(ready4_type_1L_chr)){
      append_ls <- list(ready4 = ready4_type_1L_chr)
    }else{
      append_ls <- NULL
    }
    if(identical(badges_lup,tibble::tibble())){
      utils::data("badges_lup", package = "ready4fun", envir = environment())
    }
    if(!identical(zenodo_badge_1L_chr,character(0))){
      badges_lup <- badges_lup %>%
        tibble::add_case(badge_names_chr = "DOI",
                         label_names_chr = "Zenodo",
                         badges_chr = zenodo_badge_1L_chr)
      append_ls <- append(append_ls,
                          list(DOI = "Zenodo"))
    }
  if(identical(import_from_chr,character(0)))
    import_from_chr <- make_gnrc_imports()
  addl_badges_ls <- append(addl_badges_ls, append_ls) %>%
    purrr::discard(is.null)
  piggyback_to_1L_chr <- ifelse(is.na(piggyback_to_1L_chr),
                                      gh_repo_1L_chr,
                                      piggyback_to_1L_chr)
  manifest_ls <- list(initial_ls = list(pkg_desc_ls = pkg_desc_ls,

                                        copyright_holders_chr = copyright_holders_chr,
                                        gh_repo_1L_chr = gh_repo_1L_chr,
                                        add_gh_site_1L_lgl = add_gh_site_1L_lgl,
                                        addl_badges_ls = addl_badges_ls,
                                        badges_lup = badges_lup,
                                        check_type_1L_chr = check_type_1L_chr,
                                        delete_r_dir_cnts_1L_lgl = delete_r_dir_cnts_1L_lgl,
                                        dev_pkg_nm_1L_chr = dev_pkg_nm_1L_chr,
                                        lifecycle_stage_1L_chr = lifecycle_stage_1L_chr,
                                        incr_ver_1L_lgl = incr_ver_1L_lgl,
                                        on_cran_1L_lgl = on_cran_1L_lgl,
                                        path_to_pkg_logo_1L_chr = path_to_pkg_logo_1L_chr,
                                        path_to_pkg_rt_1L_chr = path_to_pkg_rt_1L_chr),
                      subsequent_ls = list(abbreviations_lup = get_rds_from_pkg_dmt(fl_nm_1L_chr = "abbreviations_lup",
                                                                                     piggyback_to_1L_chr = piggyback_to_1L_chr),
                                           addl_pkgs_ls = addl_pkgs_ls,
                                           build_ignore_ls = build_ignore_ls,
                                           cls_fn_ls = cls_fn_ls,#
                                           custom_dmt_ls = custom_dmt_ls,
                                           dev_pkgs_chr = dev_pkgs_chr,
                                           dss_records_ls = dss_records_ls,
                                           dv_ds_nm_1L_chr = pkg_dmt_dv_dss_chr[2],
                                           dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                           fn_types_lup = get_rds_from_pkg_dmt(fl_nm_1L_chr = "fn_types_lup",
                                                                                piggyback_to_1L_chr = piggyback_to_1L_chr),
                                           fns_dmt_tb = tibble::tibble(),
                                           import_from_chr = import_from_chr,
                                           inc_pkg_meta_data_1L_lgl = inc_pkg_meta_data_1L_lgl,#
                                           object_type_lup = get_rds_from_pkg_dmt(fl_nm_1L_chr = "object_type_lup",
                                                                                   piggyback_to_1L_chr = piggyback_to_1L_chr),
                                           path_to_dmt_dir_1L_chr =  path_to_dmt_dir_1L_chr,#
                                           piggyback_to_1L_chr = piggyback_to_1L_chr,
                                           pkg_dmt_dv_dss_chr = pkg_dmt_dv_dss_chr,
                                           pkg_ds_ls_ls = pkg_ds_ls_ls,#
                                           seed_obj_type_lup = get_rds_from_pkg_dmt(fl_nm_1L_chr = "seed_obj_type_lup",
                                                                                     piggyback_to_1L_chr = piggyback_to_1L_chr),
                                           server_1L_chr = server_1L_chr,
                                           s4_fns_ls = list(),
                                           treat_as_words_chr = get_rds_from_pkg_dmt(fl_nm_1L_chr = "treat_as_words_chr",
                                                                                      piggyback_to_1L_chr = piggyback_to_1L_chr)))
  if(classify_1L_lgl){
    # Consider classifying pkg_ds_ls_ls
    if(!"ready4fun_badges" %in% class (manifest_ls$subsequent_ls$badges_lup))
      manifest_ls$initial_ls$badges_lup <- manifest_ls$initial_ls$badges_lup %>% ready4fun_badges()
    if(!"ready4fun_description" %in% class (manifest_ls$subsequent_ls$pkg_desc_ls))
      manifest_ls$initial_ls$pkg_desc_ls <- manifest_ls$initial_ls$pkg_desc_ls %>% ready4fun_description()
    manifest_ls$initial_ls <- manifest_ls$initial_ls %>% ready4fun_metadata_a()
    if(!"ready4fun_abbreviations" %in% class(manifest_ls$subsequent_ls$abbreviations_lup))
      manifest_ls$subsequent_ls$abbreviations_lup <- manifest_ls$subsequent_ls$abbreviations_lup %>%
      ready4fun_abbreviations()
    if(identical(manifest_ls$subsequent_ls$cls_fn_ls, list())){
      manifest_ls$subsequent_ls$cls_fn_ls <- ready4fun_executor()
    }else{
      if(!"ready4fun_executor" %in% class(manifest_ls$subsequent_ls$cls_fn_ls))
        manifest_ls$subsequent_ls$cls_fn_ls <- manifest_ls$subsequent_ls$cls_fn_ls %>%
          ready4fun_executor()
    }
    if(identical(manifest_ls$subsequent_ls$s4_fns_ls, list())){
      manifest_ls$subsequent_ls$s4_fns_ls <- ready4fun_executor()
    }else{
      if(!"ready4fun_executor" %in% class(manifest_ls$subsequent_ls$s4_fns_ls))
        manifest_ls$subsequent_ls$s4_fns_ls <- manifest_ls$subsequent_ls$s4_fns_ls %>%
          ready4fun_executor()
    }
    if(!"ready4fun_abbreviations" %in% class(manifest_ls$subsequent_ls$object_type_lup))
      manifest_ls$subsequent_ls$object_type_lup <- manifest_ls$subsequent_ls$object_type_lup %>%
      ready4fun_abbreviations()
    manifest_ls$subsequent_ls <- manifest_ls$subsequent_ls %>% ready4fun_metadata_b()
    manifest_ls <- manifest_ls %>%
      ready4fun_manifest()
  }
  return(manifest_ls)
}
make_new_fn_dmt <- function(fn_type_1L_chr,
                            fn_name_1L_chr,
                            fn_desc_1L_chr = NA_character_,
                            fn_det_1L_chr = NA_character_,
                            fn_out_type_1L_chr = NA_character_,
                            args_ls = NULL,
                            class_name_1L_chr = "",
                            fn = NULL,
                            abbreviations_lup = NULL,
                            dv_ds_nm_1L_chr = "ready4-dev/ready4",
                            dv_url_pfx_1L_chr = deprecated(),
                            key_1L_chr = deprecated(),
                            object_type_lup = NULL,
                            server_1L_chr = deprecated()){
  if(is.null(abbreviations_lup))
    abbreviations_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "abbreviations_lup",
                                               piggyback_to_1L_chr = dv_ds_nm_1L_chr)
  if(is.null(object_type_lup))
    object_type_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "object_type_lup",
                                             piggyback_to_1L_chr = dv_ds_nm_1L_chr)
  s3_class_main_1L_chr <- x_param_desc_1L_chr <- NULL
  if(!is.null(fn)){
    fn_args_chr <- get_fn_args(fn)
    fn_out_type_1L_chr <- make_ret_obj_desc(fn,
                                            abbreviations_lup = abbreviations_lup,
                                            class_name_1L_chr = class_name_1L_chr)
  }else{
    fn_args_chr <- NA_character_
  }
  if(fn_type_1L_chr == "set_class" | startsWith(fn_type_1L_chr, "s3_")){
    if(fn_type_1L_chr %in% c("set_class","s3_valid_instance"))
      short_class_desc_1L_chr <- ready4::get_from_lup_obj(abbreviations_lup,
                                                          match_var_nm_1L_chr = "short_name_chr",
                                                          match_value_xx = fn_name_1L_chr,
                                                          target_var_nm_1L_chr = "long_name_chr",
                                                          evaluate_1L_lgl = F)
    if(fn_type_1L_chr == "s3_valid_instance"){
      s3_class_main_1L_chr <- short_class_desc_1L_chr %>% `names<-`(fn_name_1L_chr)
    }
    if(fn_type_1L_chr == "s3_unvalidated_instance"){
      s3_class_main_1L_chr <- stringr::str_replace(fn_name_1L_chr,"make_new_","") %>% `names<-`(fn_name_1L_chr)
    }
    if(fn_type_1L_chr == "s3_prototype"){
      s3_class_main_1L_chr <- stringr::str_replace(fn_name_1L_chr,"make_pt_","") %>% `names<-`(fn_name_1L_chr)
    }
    if(fn_type_1L_chr == "s3_validator"){
      s3_class_main_1L_chr <- stringr::str_replace(fn_name_1L_chr,"validate_","") %>% `names<-`(fn_name_1L_chr)
    }
    if(fn_type_1L_chr == "s3_checker"){
      s3_class_main_1L_chr <- stringr::str_replace(fn_name_1L_chr,"is_","") %>% `names<-`(fn_name_1L_chr)
    }
    if(!fn_type_1L_chr %in% c("set_class","s3_valid_instance"))
      short_class_desc_1L_chr <- ready4::get_from_lup_obj(abbreviations_lup,
                                                          match_var_nm_1L_chr = "short_name_chr",
                                                          match_value_xx = s3_class_main_1L_chr,
                                                          target_var_nm_1L_chr = "long_name_chr",
                                                          evaluate_1L_lgl = F)
    if(fn_type_1L_chr == "set_class"){
      desc_start_1L_chr <- "Create a new S4 object of the class:"
      output_txt_1L_chr <- paste0("An S4 object of the ",short_class_desc_1L_chr)
    }
    if(fn_type_1L_chr == "s3_valid_instance"){
      desc_start_1L_chr <- paste0("Create a new valid instance of the ",short_class_desc_1L_chr)
      output_txt_1L_chr <- paste0("A validated instance of the ",short_class_desc_1L_chr)
      x_param_desc_1L_chr <- paste0("A prototype for the ",short_class_desc_1L_chr)
    }
    if(fn_type_1L_chr == "s3_unvalidated_instance"){
      desc_start_1L_chr <- paste0("Create a new unvalidated instance of the ",short_class_desc_1L_chr)
      x_param_desc_1L_chr <- paste0("A prototype for the ",short_class_desc_1L_chr)
      output_txt_1L_chr <- paste0("An unvalidated instance of the ",short_class_desc_1L_chr)
    }
    if(fn_type_1L_chr == "s3_prototype"){
      desc_start_1L_chr <- paste0("Create a new prototype for the ",short_class_desc_1L_chr)
      output_txt_1L_chr <- paste0("A prototype for ",short_class_desc_1L_chr)
    }
    if(fn_type_1L_chr == "s3_validator"){
      desc_start_1L_chr <- paste0("Validate an instance of the ",short_class_desc_1L_chr)
      x_param_desc_1L_chr <- paste0("An unvalidated instance of the ",short_class_desc_1L_chr)
      output_txt_1L_chr <- paste0("A prototpe for ",short_class_desc_1L_chr)
    }
    if(fn_type_1L_chr == "s3_checker"){
      desc_start_1L_chr <- paste0("Check whether an object is a valid instance of the ",short_class_desc_1L_chr)
      x_param_desc_1L_chr <- "An object of any type"
      output_txt_1L_chr <- paste0("A logical value, TRUE if a valid instance of the ",short_class_desc_1L_chr)
    }
  }

  if(fn_type_1L_chr %in% c("gen_get_slot","meth_get_slot")){
    desc_start_1L_chr <- "Get the value of the slot "
    output_txt_1L_chr <- "A XXX ..."
  }
  if(fn_type_1L_chr %in% c("gen_set_slot","meth_set_slot")){
    desc_start_1L_chr <- "Set the value of the slot "
    output_txt_1L_chr <- "NULL"
  }
  if(fn_type_1L_chr %in% c("fn",
                           "gen_std_s3_mthd",
                           "meth_std_s3_mthd",
                           "gen_std_s4_mthd",
                           "meth_std_s4_mthd")){
    desc_start_1L_chr <- fn_desc_1L_chr
    output_txt_1L_chr <- fn_out_type_1L_chr
    if(fn_type_1L_chr == "meth_std_s3_mthd"){
      x_param_desc_1L_chr <- paste0("An instance of ",
                                    stringr::str_sub(fn_name_1L_chr,
                                                     start=(1+stringi::stri_locate_last_fixed(fn_name_1L_chr,".")[1,1])) %>%
                                      ready4::get_from_lup_obj(abbreviations_lup,
                                                               match_var_nm_1L_chr = "short_name_chr",
                                                               match_value_xx = .,
                                                               target_var_nm_1L_chr = "long_name_chr",
                                                               evaluate_1L_lgl = F))
    }
    if(fn_type_1L_chr == "gen_std_s3_mthd"){
      x_param_desc_1L_chr <- "An object"
    }
  }
  if(fn_type_1L_chr == "meth_std_s4_mthd"){
    x_param_desc_1L_chr <- paste0("An object of class ", class_name_1L_chr)
  }
  if(is.null(args_ls)){
    arg_desc_chr <- NULL
    if(any(!is.na(fn_args_chr)) & !is.null(object_type_lup)){
      arg_desc_chr <- make_arg_desc(fn_args_chr,
                                    abbreviations_lup = abbreviations_lup,
                                    object_type_lup = object_type_lup,
                                    dv_ds_nm_1L_chr = dv_ds_nm_1L_chr,
                                    dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                    key_1L_chr = key_1L_chr,
                                    server_1L_chr = server_1L_chr)
      if(!is.null(arg_desc_chr)){
        names(arg_desc_chr) <- fn_args_chr
      }
    }
  }else{
    arg_desc_chr <- args_ls %>% purrr::flatten_chr() %>% stats::setNames(names(args_ls))
  }
  if(!is.null(x_param_desc_1L_chr)){
    x_param_desc_1L_chr <- x_param_desc_1L_chr %>% `names<-`("x")
    if(is.null(arg_desc_chr)){
      arg_desc_chr <- x_param_desc_1L_chr
    }else{
      arg_desc_chr <- c(x_param_desc_1L_chr,arg_desc_chr[names(arg_desc_chr)!="x"])
    }
  }
  new_fn_dmt_chr_ls <- list(desc_start_1L_chr = desc_start_1L_chr,
                            s3_class_main_1L_chr = s3_class_main_1L_chr,
                            output_txt_1L_chr = output_txt_1L_chr,
                            fn_det_1L_chr = fn_det_1L_chr,
                            arg_desc_chr = arg_desc_chr)
  return(new_fn_dmt_chr_ls)
}
make_new_entries_tb <- function(short_name_chr,
                                long_name_chr,
                                atomic_element_lgl = F,
                                r3_can_extend_lgl = F){
  new_entries_tb <- tibble::tibble(short_name_chr,
                                   long_name_chr,
                                   atomic_element_lgl = atomic_element_lgl,
                                   r3_can_extend_lgl = r3_can_extend_lgl)
  return(new_entries_tb)

}
make_obj_lup_spine <- function(seed_obj_type_lup = get_rds_from_pkg_dmt(fl_nm_1L_chr = "seed_obj_type_lup",
                                                                         piggyback_to_1L_chr = "ready4-dev/ready4"),
                               new_entries_tb = NULL){
  if(is.null(seed_obj_type_lup)){
    seed_obj_type_lup <- tibble::tibble(short_name_chr = c("df","env","fn","ls","plt","r3","r4","s3","s4","sf","tb","arr","chr","dbl","dtm","fct","int","lgl","lup","mat","mdl","prsn","rgx"),
                                        long_name_chr = c("data.frame","environment","function","list","plot","ready4 S3", "ready4 S4", "S3", "S4", "simple features object",
                                                          "tibble","array","character","double", "date","factor","integer","logical","lookup table","matrix","model","person","regular expression"),
                                        atomic_element_lgl = c(rep(F,12),rep(T,6),rep(F,4),T),
                                        r3_can_extend_lgl = c(T,F,F,T,F,rep(F,4),rep(T,14))) %>%
      dplyr::arrange(short_name_chr)
  }
  obj_lup_spine <- seed_obj_type_lup
  if(!is.null(new_entries_tb)){
    obj_lup_spine <- ready4::add_lups(obj_lup_spine,
                                      new_lup = new_entries_tb,
                                      key_var_nm_1L_chr = "short_name_chr")
  }
  return(obj_lup_spine)
}
make_obj_lup <- function(obj_lup_spine = make_obj_lup_spine()){
  obj_tb <- obj_lup_spine
  obj_tb <- dplyr::bind_rows(obj_tb %>%
                               dplyr::mutate(long_name_chr = purrr::map2_chr(long_name_chr,atomic_element_lgl,
                                                                             ~ifelse(.y,paste0(.x," vector"),.x))),
                             obj_tb %>%
                               dplyr::filter(atomic_element_lgl) %>%
                               dplyr::mutate(short_name_chr = short_name_chr %>% purrr::map_chr(~paste0(stringr::str_sub(.x,end=-5),
                                                                                                        "1L_",
                                                                                                        stringr::str_sub(.x,start=-4))),
                                             long_name_chr = paste0(long_name_chr," vector of length one")),
                             obj_tb %>%
                               dplyr::filter(r3_can_extend_lgl) %>%
                               dplyr::mutate(short_name_chr = paste0(short_name_chr,
                                                                     purrr::map_chr(atomic_element_lgl,
                                                                                    ~ ""#ifelse(.x,"_vec","")
                                                                     ),
                                                                     "_r3"),
                                             long_name_chr = paste0("ready4 S3 extension of ",
                                                                    long_name_chr,
                                                                    purrr::map_chr(atomic_element_lgl,
                                                                                   ~ ifelse(.x," vector",""))))) %>%
    dplyr::select(-3,-4)
  obj_tb <- dplyr::bind_rows(obj_tb,
                             obj_tb %>%
                               dplyr::mutate(short_name_chr = paste0(short_name_chr,"_ls"),
                                             long_name_chr = paste0("list of ",long_name_chr)) %>%
                               dplyr::bind_rows(obj_tb %>%
                                                  dplyr::mutate(short_name_chr = paste0(short_name_chr,"_r4"),
                                                                long_name_chr = paste0("ready4 S4 collection of ",long_name_chr))) %>%
                               dplyr::mutate(long_name_chr = purrr::map_chr(long_name_chr,
                                                                            ~ifelse(endsWith(.x,"vector of length one"),
                                                                                    stringr::str_replace(.x,"vector", "vectors"),
                                                                                    ifelse(endsWith(.x,"matrix"),
                                                                                           stringr::str_replace(.x,"matrix", "matrices"),
                                                                                           paste0(.x,"s"))))),
                             tibble::tibble(short_name_chr = "xx",
                                            long_name_chr = "output object of multiple potential types"))
  obj_tb <- obj_tb %>%
    dplyr::mutate(plural_lgl = F)
  return(obj_tb)
}
make_pkg_desc_ls <- function(pkg_nm_1L_chr = get_dev_pkg_nm(),
                             pkg_title_1L_chr,
                             pkg_desc_1L_chr,
                             authors_prsn,
                             #cpyr_hldr_1L_chr,
                             urls_chr){
  cpyr_hldr_1L_chr <- authors_prsn[authors_prsn %>%
                                     as.character() %>%
                                     purrr::map_lgl(~stringr::str_detect(.x,
                                                                         "\\[cph") | stringr::str_detect(.x,
                                                                                                         " cph, "))] %>%
    as.character()
  cpyr_hldr_1L_chr <- cpyr_hldr_1L_chr  %>%
    stringr::str_sub(end = -1 + (cpyr_hldr_1L_chr %>% stringr::str_locate("\\["))[1,1] %>% unname()) %>%
    stringr::str_trim()
  pkg_desc_ls <- list(
    Package = pkg_nm_1L_chr,
    Title =  pkg_title_1L_chr %>% tools::toTitleCase(),
    Description = pkg_desc_1L_chr,
    `Authors@R` = authors_prsn,
    License = usethis::use_gpl3_license(),#cpyr_hldr_1L_chr
    URL = paste0(urls_chr, collapse = ", "))
  return(pkg_desc_ls)
}
make_pkg_ds_ls <- function(db_df,
                           db_1L_chr,
                           title_1L_chr,
                           desc_1L_chr,
                           abbreviations_lup = NULL,
                           format_1L_chr = "A tibble",
                           object_type_lup = NULL,
                           simple_lup_1L_lgl = F,
                           url_1L_chr = NA_character_,
                           vars_ls = NULL){
  pkg_ds_ls <- list(db_df = db_df,
                    db_1L_chr = db_1L_chr,
                    title_1L_chr = title_1L_chr,
                    desc_1L_chr = desc_1L_chr,
                    abbreviations_lup = abbreviations_lup,
                    format_1L_chr = format_1L_chr,
                    object_type_lup = object_type_lup,
                    simple_lup_1L_lgl = simple_lup_1L_lgl,
                    url_1L_chr = url_1L_chr,
                    vars_ls = vars_ls)
  return(pkg_ds_ls)
}
make_prompt <- function(prompt_1L_chr, options_chr = NULL, force_from_opts_1L_chr = F) {
  lifecycle::deprecate_soft("0.0.0.9446", "ready4fun::make_prompt()", "ready4::make_prompt()")
  acknowledgement_1L_chr <- "This function is based on: https://debruine.github.io/posts/interactive-test/"
  con_conn <- getOption("prompt_opts.con", stdin())
  options_1L_chr <- paste(options_chr, collapse = "|")
  prompt_with_options_1L_chr <- paste0(prompt_1L_chr, " [", options_1L_chr, "]\n")
  cat(prompt_with_options_1L_chr)
  response_1L_chr <- readLines(con = con_conn, n = 1)
  if (!is.null(options_chr) & !response_1L_chr %in% options_chr & force_from_opts_1L_chr) {
    response_1L_chr  <- ready4::make_prompt(prompt_1L_chr, options_chr, force_from_opts_1L_chr = T)
  }
  return(response_1L_chr)
}
make_ret_obj_desc <- function(fn,
                              abbreviations_lup,
                              class_name_1L_chr = "",
                              starts_sentence_1L_lgl = T){
  ret_obj_nm_1L_chr <- get_return_obj_nm(fn)
  if(is.na(ret_obj_nm_1L_chr)){
    ret_obj_desc_1L_chr <- "NULL"
  }else{
    obj_type_1L_chr <- get_arg_obj_type(ret_obj_nm_1L_chr,
                                        object_type_lup = abbreviations_lup)
    obj_type_1L_chr <- ifelse(ret_obj_nm_1L_chr=="x",
                              paste0("An object",
                                     ifelse(class_name_1L_chr != "",
                                            paste0(" of class ", class_name_1L_chr),
                                            "")),
                              ifelse(identical(character(0),
                                               obj_type_1L_chr),
                                     "An object",
                                     obj_type_1L_chr))
    ret_obj_desc_1L_chr <- paste0(ifelse(ret_obj_nm_1L_chr=="x",
                                         ret_obj_nm_1L_chr,
                                         ifelse(obj_type_1L_chr == "An object",
                                                ret_obj_nm_1L_chr,
                                                ret_obj_nm_1L_chr %>% make_arg_title(match_chr = obj_type_1L_chr,
                                                                                     abbreviations_lup = abbreviations_lup,
                                                                                     object_type_lup = abbreviations_lup) %>%
                                                  add_indef_artl_to_item(abbreviations_lup = abbreviations_lup) %>%
                                                  ifelse(!starts_sentence_1L_lgl,tolower(.),.))),
                                  " (",
                                  obj_type_1L_chr %>% add_indef_artl_to_item(abbreviations_lup = abbreviations_lup),
                                  ")")
  }
  return(ret_obj_desc_1L_chr)
}
make_short_long_nms_vec <- function(long_vecs_chr = character(0),
                                    short_vecs_chr = character(0)){
  short_vecs_chr <- paste0(short_vecs_chr,"_vec")
  if(short_vecs_chr[1]=="_vec"){
    short_vecs_chr <- character(0)
  }
  short_and_long_vec_chr <- c(long_vecs_chr, short_vecs_chr)
  return(short_and_long_vec_chr)
}
make_std_fn_dmt_spine <- function(fn_name_1L_chr,
                                  fn_type_1L_chr,
                                  fn_title_1L_chr,
                                  fn,
                                  fn_types_lup = NULL,
                                  details_1L_chr = NA_character_,
                                  doc_in_class_1L_lgl = F,
                                  example_1L_lgl = F,
                                  export_1L_lgl = T,
                                  class_name_1L_chr = "",
                                  exclude_if_match_chr,
                                  piggyback_to_1L_chr = "ready4-dev/ready4"){
  assert_inp_does_not_match_terms(input_chr = fn_type_1L_chr,
                                  exclude_if_match_chr = exclude_if_match_chr)
  if(is.null(fn_types_lup))
    fn_types_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "fn_types_lup",
                                          piggyback_to_1L_chr = piggyback_to_1L_chr)

  if(!is.na(details_1L_chr)){
    if(details_1L_chr=="DETAILS")
      details_1L_chr <- NA_character_
  }
  if(startsWith(fn_type_1L_chr,"gen_")){
    fn_tags_1L_chr <- sinew::makeOxygen(fn, print=FALSE, add_fields = c("export")) %>%
      stringr::str_replace("#' @return OUTPUT_DESCRIPTION\n","")
  }else{
    fn_tags_1L_chr <- sinew::makeOxygen(fn,
                                        print=FALSE,
                                        add_fields = c(ifelse(!is.na(details_1L_chr),"details",NA_character_),
                                                       ifelse(example_1L_lgl,"examples",NA_character_),
                                                       "rdname",
                                                       ifelse(export_1L_lgl,"export",NA_character_)) %>%
                                          purrr::discard(is.na)) %>%
      stringr::str_replace("#' @title FUNCTION_TITLE\n",
                           ifelse(fn_type_1L_chr != "meth_std_s4_mthd",
                                  "",
                                  paste0("#' ",
                                         fn_name_1L_chr,
                                         "\n#' @name ",
                                         fn_name_1L_chr,
                                         "-",
                                         class_name_1L_chr,
                                         "\n")
                           )
      )
  }
  if(fn_type_1L_chr == "meth_std_s4_mthd"){
    fn_tags_1L_chr <- fn_tags_1L_chr %>%
      # stringr::str_replace("#' @param x PARAM_DESCRIPTION\n",
      #                      paste0("#' @param x An object of class ",
      #                             class_name_1L_chr,
      #                             "\n")) %>%
      stringr::str_replace("@rdname fn",
                           paste0("@rdname ",
                                  ifelse(doc_in_class_1L_lgl,
                                         class_name_1L_chr,
                                         paste0(fn_name_1L_chr,
                                                "-methods\n")),
                                  paste0("#' @aliases ",fn_name_1L_chr,",",class_name_1L_chr,"-method")
                           )
      )
  }else{
    fn_tags_1L_chr <- fn_tags_1L_chr %>%
      stringr::str_replace("@title ","@name ") %>%
      stringr::str_replace("@rdname fn",
                           paste0("@rdname ",fn_name_1L_chr))
  }
  fn_tags_1L_chr <- paste0("#' ",
                           ifelse((startsWith(fn_type_1L_chr,"gen_")|fn_type_1L_chr %in% c("fn","meth_std_s3_mthd")|startsWith(fn_type_1L_chr,"s3_")),
                                  fn_title_1L_chr,
                                  ""),
                           "\n",
                           fn_tags_1L_chr)
  if(!fn_type_1L_chr %>% startsWith("s3") & !fn_type_1L_chr %in% c("fn",
                                                                   "gen_std_s3_mthd",
                                                                   "meth_std_s3_mthd",
                                                                   "gen_std_s4_mthd",
                                                                   "meth_std_s4_mthd"))
    fn_tags_1L_chr <- update_fn_dmt_with_slots(fn_name_1L_chr = fn_name_1L_chr,
                                               fn_dmt_1L_chr = fn_tags_1L_chr)
  std_fn_dmt_spine_chr_ls <- list(fn_tags_1L_chr = fn_tags_1L_chr,
                                  ref_slot_1L_chr = fn_name_1L_chr)
  return(std_fn_dmt_spine_chr_ls)
}
make_undmtd_fns_dir_chr <- function(path_1L_chr = "data-raw",
                                    drop_empty_1L_lgl = F){
  undocumented_fns_dir_chr <- paste0(path_1L_chr,"/",make_fn_types())
  if(drop_empty_1L_lgl)
    undocumented_fns_dir_chr <- undocumented_fns_dir_chr[undocumented_fns_dir_chr %>% purrr::map_lgl(~{
      exists_1L_lgl <- dir.exists(.x)
      ifelse(exists_1L_lgl,
             !identical(list.files(.x), character(0)),
             exists_1L_lgl)
    })]
  return(undocumented_fns_dir_chr)
}


