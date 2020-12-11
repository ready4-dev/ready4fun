make_arg_desc <- function(fn_args_chr,
                          object_type_lup = NULL,
                          abbreviations_lup = NULL){
  if(is.null(abbreviations_lup))
    utils::data("abbreviations_lup",package="ready4fun",envir = environment())
  if(is.null(object_type_lup))
    utils::data("object_type_lup",package="ready4fun",envir = environment())
  arg_desc_chr <- make_arg_type(fn_args_chr,
                                object_type_lup = object_type_lup,
                                abbreviations_lup = abbreviations_lup,
                                fn = make_arg_desc_spine)
  return(arg_desc_chr)
}
make_arg_desc_ls <- function(fn_nms_chr,
                             abbreviations_lup = NULL,
                             object_type_lup = NULL){
  if(is.null(abbreviations_lup))
    utils::data("abbreviations_lup",package="ready4fun",envir = environment())
  if(is.null(object_type_lup))
    utils::data("object_type_lup",package="ready4fun",envir = environment())
  purrr::map(fn_nms_chr,
             ~ {
               eval(parse(text = paste0("fn <- ",.x)))
               get_fn_args(fn) %>% make_arg_desc(abbreviations_lup = abbreviations_lup,
                                                 object_type_lup = object_type_lup) %>%
                 stats::setNames(get_fn_args(fn))
             }
  )
}
make_arg_desc_spine <- function(argument_nm_1L_chr,
                                    object_type_lup = NULL,
                                    abbreviations_lup = NULL){
  if(is.null(object_type_lup))
    utils::data("object_type_lup", package="ready4fun",envir = environment())
  if(is.null(abbreviations_lup))
    utils::data("abbreviations_lup", package="ready4fun",envir = environment())
  if(is.na(argument_nm_1L_chr)){
    match_1L_chr <- character(0)
  }else{
    match_1L_chr <- get_arg_obj_type(argument_nm_1L_chr,
                                            object_type_lup = object_type_lup)
  }
  arg_desc_spine <- ifelse(identical(match_1L_chr,character(0)),
                               NA_character_,
                               paste0(argument_nm_1L_chr %>% make_arg_title(match_chr = match_1L_chr,
                                                                                 abbreviations_lup = abbreviations_lup),
                                      " (",
                                      match_1L_chr %>% update_first_word_case() %>%
                                        add_indefartls_to_phrases(abbreviations_lup = abbreviations_lup,
                                                                           ignore_phrs_not_in_lup_1L_lgl = F),
                                      ")"))
  return(arg_desc_spine)
}
make_arg_title <- function(args_chr,
                                   match_chr,
                                   object_type_lup = NULL,
                                   abbreviations_lup = NULL){
  if(is.null(object_type_lup))
    utils::data("object_type_lup",package="ready4fun",envir = environment())
  if(is.null(abbreviations_lup))
    utils::data("abbreviations_lup",package="ready4fun",envir = environment())
  suffices_chr <- match_chr %>% purrr::map_chr(~{
    ifelse(.x=="NO MATCH",
           "",
           get_from_lup_obj(object_type_lup,
                            match_value_xx = .x,
                            match_var_nm_1L_chr = "long_name_chr",
                            target_var_nm_1L_chr = "short_name_chr",
                            evaluate_lgl = F))

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
                                       abbreviations_lup = NULL){#
  if(is.null(abbreviations_lup))
    utils::data("abbreviations_lup",package="ready4fun",envir = environment())
  if(is.null(object_type_lup))
    utils::data("object_type_lup",package="ready4fun",envir = environment())
  arg_type_abbr_chr <- make_arg_type(fn_args_chr,
                                                 object_type_lup = object_type_lup,
                                                 fn = make_arg_type_abbr_spine,
                                                 abbreviations_lup = abbreviations_lup)
  return(arg_type_abbr_chr)
}
make_arg_type_abbr_spine <- function(argument_nm_1L_chr,
                                    lup_tb){
  arg_type_1L_chr <- lup_tb$short_name_chr[endsWith(argument_nm_1L_chr,lup_tb$short_name_chr)]
  arg_type_abbr_spine_1L_chr <- ifelse(identical(character(0),arg_type_1L_chr),
                         NA_character_,
                         arg_type_1L_chr)
  return(arg_type_abbr_spine_1L_chr)
}
make_arg_type <- function(fn_args_chr,
                                  object_type_lup = NULL,
                                  abbreviations_lup = NULL,
                                  fn){
  if(is.null(object_type_lup))
    utils::data("object_type_lup",package="ready4fun",envir = environment())
  if(is.null(abbreviations_lup))
    utils::data("abbreviations_lup",package="ready4fun",envir = environment())
  lup_ls <- make_arg_type_lup_ls(object_type_lup)
  append_1L_lgl <- "abbreviations_lup" %in% get_fn_args(fn)
  arg_desc_chr <- fn_args_chr %>%
    purrr::map_chr(~{
      argument_nm_1L_chr <- .x
      arg_desc_1L_chr <- purrr::map_chr(lup_ls,
                                     ~ {
                                       args_ls <- list(argument_nm_1L_chr,
                                                       .x)
                                       if(append_1L_lgl)
                                         args_ls <- append(args_ls, list(abbreviations_lup))
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
make_arg_type_lup_ls <- function(object_type_lup = NULL){
  if(is.null(object_type_lup))
    utils::data("object_type_lup",package="ready4fun",envir = environment())
  new_lup <- object_type_lup %>%
    dplyr::mutate(nchar_int = nchar(short_name_chr))
  lup_ls <- new_lup$nchar_int %>% unique() %>%
    sort(decreasing = T) %>%
    purrr::map(~dplyr::filter(new_lup,nchar_int==.x))
  return(lup_ls)
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
                                 undocumented_fns_dir_chr = make_undmtd_fns_dir_chr(),
                                 custom_dmt_ls = list(details_ls = NULL,
                                                      inc_for_main_user_lgl_ls = list(force_true_chr = NA_character_,
                                                                                      force_false_chr = NA_character_),
                                                      args_ls_ls = NULL),
                                 fn_type_lup_tb,
                                 abbreviations_lup = NULL,
                                 inc_all_mthds_1L_lgl = T){
  # add assert - same length inputs to purrr
  if (is.null(abbreviations_lup))
    utils::data("abbreviations_lup", package = "ready4fun",
         envir = environment())
  all_fns_dmt_tb <- purrr::pmap_dfr(list(paths_ls,
                                         undocumented_fns_dir_chr,
                                         names(paths_ls)
  ),
  ~ {
    if(..3 == "fns")
      tb <- fn_type_lup_tb %>% dplyr::filter(!is_generic_lgl & !is_method_lgl)
    if(..3 == "gnrcs")
      tb <- fn_type_lup_tb %>% dplyr::filter(is_generic_lgl)
    if(..3 == "mthds")
      tb <- fn_type_lup_tb %>% dplyr::filter(is_method_lgl)
    fns_dmt_tb <- make_fn_dmt_tbl(..1,
                    fns_dir_chr = ..2,
                    custom_dmt_ls = custom_dmt_ls,
                    append_1L_lgl = T,
                    fn_type_lup_tb = tb,
                    abbreviations_lup = abbreviations_lup)
    if(inc_all_mthds_1L_lgl)
      fns_dmt_tb %>% dplyr::mutate(inc_for_main_user_lgl = dplyr::case_when(file_pfx_chr %in% c("grp_","mthd_") ~ T,
                                                                            TRUE ~ inc_for_main_user_lgl))
    # custom_dmt_ls$inc_for_main_user_lgl_ls$force_true_chr <- c(fns_dmt_tb %>%
    #     dplyr::filter(file_pfx_chr %in% c("grp_","mthd_")) %>%
    #     dplyr::pull(fns_chr),
    #   {
    #     slots_chr <- prototype_lup %>%
    #       dplyr::filter(pt_ns_chr == ready4fun::get_dev_pkg_nm() & !old_class_lgl) %>%
    #       dplyr::pull(fn_to_call_chr) %>%
    #       purrr::map(~getSlots(.x) %>%
    #                    names()) %>%
    #       purrr::flatten_chr() %>% unique() %>% sort()
    #     c(slots_chr,paste0(slots_chr,"<-"))
    #   },
    #   custom_dmt_ls$inc_for_main_user_lgl_ls$force_true_chr)
    # make_fn_dmt_tbl(..1,
    #                 fns_dir_chr = ..2,
    #                 custom_dmt_ls = custom_dmt_ls,
    #                 append_1L_lgl = T,
    #                 fn_type_lup_tb = tb,
    #                 abbreviations_lup = abbreviations_lup)
  })
  return(all_fns_dmt_tb)
}
make_fn_desc <-  function(fns_chr,
                          title_chr,
                          output_chr,
                          fn_type_lup_tb = NULL,
                          abbreviations_lup = NULL,
                          test_for_write_R_warning_fn = NULL,
                          is_generic_lgl = F){
  if(is.null(test_for_write_R_warning_fn))
    test_for_write_R_warning_fn <- function(x){startsWith(x,"write")}
  if(is.null(abbreviations_lup))
    utils::data("abbreviations_lup",package="ready4fun",envir = environment())
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
                                   paste0(make_fn_desc_spine(fn_name_1L_chr = fn_name_1L_chr,
                                                             fn_title_1L_chr = fn_title_1L_chr,
                                                             fn_type_lup_tb = fn_type_lup_tb,
                                                             abbreviations_lup = abbreviations_lup),
                                          ifelse(fn_output_1L_chr=="NULL",
                                                 ifelse(is_generic_1L_lgl,
                                                        "",
                                                        paste0(" The function is called for its side effects and does not return a value.",
                                                               ifelse(fn_name_1L_chr %>% test_for_write_R_warning_fn,#startsWith(fn_name_1L_chr,"write"),
                                                                      " WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour",
                                                                      ""))),
                                                 paste0(" The function returns ",
                                                        make_ret_obj_desc(eval(parse(text=fn_name_1L_chr)),
                                                                          abbreviations_lup = abbreviations_lup,
                                                                          starts_sentence_1L_lgl = T),
                                                        ".")
                                          )
                                   )
                                 }
  )
  return(fn_desc_chr)
}
make_fn_desc_spine <- function(fn_name_1L_chr,
                                       fn_title_1L_chr,
                                       fn_type_lup_tb = NULL,
                                       abbreviations_lup = NULL){
  if(is.null(fn_type_lup_tb))
    utils::data("fn_type_lup_tb", package="ready4fun", envir = environment())
  if(is.null(abbreviations_lup))
    utils::data("abbreviations_lup",package="ready4fun",envir = environment())
  fn_args_chr <- get_fn_args(eval(parse(text = fn_name_1L_chr)))
  pfx_matches_chr <- fn_type_lup_tb$fn_type_nm_chr[purrr::map_lgl(fn_type_lup_tb$fn_type_nm_chr, ~ startsWith(fn_title_1L_chr %>% tools::toTitleCase(),.x))]
  fn_type_chr <- pfx_matches_chr[nchar(pfx_matches_chr) == max(nchar(pfx_matches_chr))]
  text_elements_chr <- names(fn_type_lup_tb)[2:4] %>%
    purrr::map_chr(~ get_from_lup_obj(fn_type_lup_tb,
                                      match_var_nm_1L_chr = "fn_type_nm_chr",
                                      match_value_xx = fn_type_chr[1],
                                      target_var_nm_1L_chr = .x,
                                      evaluate_lgl = F))
  is_generic_1L_lgl <- get_from_lup_obj(fn_type_lup_tb,
                                     match_var_nm_1L_chr = "fn_type_nm_chr",
                                     match_value_xx = fn_type_chr[1],
                                     target_var_nm_1L_chr = "is_generic_lgl",
                                     evaluate_lgl = F)
  treat_as_1L_chr <- ifelse(is_generic_1L_lgl,
                         ifelse(purrr::map_lgl(abbreviations_lup$short_name_chr,
                                               ~ endsWith(fn_name_1L_chr,paste0(".",.x))) %>% any(),
                                "Method",
                                "Generic"),
                         "Function")
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
                                                                      ~ endsWith(fn_name_1L_chr,paste0(".",.x)))],
                                                       "."),
                                                paste0( " Specifically, this function implements an algorithm to ",
                                                        make_fn_title(fn_name_1L_chr,
                                                                      object_type_lup = abbreviations_lup,
                                                                      abbreviations_lup = abbreviations_lup,
                                                                      is_generic_lgl = T) %>% tolower(),
                                                        # fn_name_1L_chr %>%
                                                        #   remove_obj_type_from_nm(abbreviations_lup = abbreviations_lup) %>%
                                                        #   add_indefartls_to_phrases(abbreviations_lup = abbreviations_lup),
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
                                                        details_1L_chr = details_1L_chr,
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
                            fns_dir_chr = make_undmtd_fns_dir_chr(),
                            custom_dmt_ls = list(title_ls = NULL,
                                                 desc_ls = NULL,
                                                 details_ls = NULL,
                                                 inc_for_main_user_lgl_ls = NULL,
                                                 output_ls = NULL,
                                                 example_ls = NULL,
                                                 args_ls_ls = NULL),
                            append_1L_lgl = T,
                            fn_type_lup_tb = NULL,
                            abbreviations_lup = NULL,
                            object_type_lup = NULL,
                            test_for_write_R_warning_fn = NULL){
  if(is.null(abbreviations_lup))
    utils::data("abbreviations_lup",package="ready4fun",envir = environment())
  if(is.null(object_type_lup))
    utils::data("object_type_lup",package="ready4fun",envir = environment())
  fn_dmt_tbl_tb <- make_fn_dmt_tbl_tpl(fns_path_chr,
                                       fns_dir_chr = fns_dir_chr,
                                       fn_type_lup_tb = fn_type_lup_tb,
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
make_fn_dmt_tbl_tpl <- function(fns_path_chr,
                                   fns_dir_chr = make_undmtd_fns_dir_chr(),
                                   fn_type_lup_tb = NULL,
                                   abbreviations_lup = NULL,
                                   object_type_lup = NULL,
                                test_for_write_R_warning_fn = NULL){
  if(is.null(abbreviations_lup))
    utils::data("abbreviations_lup",package="ready4fun",envir = environment())
  if(is.null(object_type_lup))
    utils::data("object_type_lup",package="ready4fun",envir = environment())
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
                                            is_generic_lgl = purrr::map_lgl(file_nm_chr, ~ .x == "generics.R") #is_generic_1L_lgl
    ))
  fn_dmt_tbl_tb <- fn_dmt_tbl_tb %>%
    dplyr::filter(title_chr %>%
      tools::toTitleCase() %>%
      purrr::map_lgl(~{
        startsWith(.x, fn_type_lup_tb$fn_type_nm_chr) %>% any()
      }))
  fn_dmt_tbl_tb <- fn_dmt_tbl_tb %>%
    dplyr::mutate(output_chr = get_outp_obj_type(fns_chr))
  fn_dmt_tbl_tb <- fn_dmt_tbl_tb %>%
    dplyr::mutate(desc_chr = make_fn_desc(fns_chr,
                                                  title_chr = title_chr,
                                                  output_chr = output_chr,
                                                  fn_type_lup_tb = fn_type_lup_tb,
                                                  abbreviations_lup = abbreviations_lup,
                                          test_for_write_R_warning_fn = test_for_write_R_warning_fn,
                                          is_generic_lgl = purrr::map_lgl(file_nm_chr, ~ .x == "generics.R")))
  fn_dmt_tbl_tb <- fn_dmt_tbl_tb %>%
    dplyr::mutate(args_ls = make_arg_desc_ls(fns_chr,
                                             abbreviations_lup = abbreviations_lup,
                                             object_type_lup = object_type_lup))
  return(fn_dmt_tbl_tb)
}
make_fn_title <- function(fns_chr,
                                  object_type_lup = NULL,
                                  abbreviations_lup = NULL,
                                  is_generic_lgl = F){
  if(is.null(object_type_lup))
    utils::data("object_type_lup",package="ready4fun",envir = environment())
  if(is.null(abbreviations_lup))
    utils::data("abbreviations_lup",package="ready4fun",envir = environment())
  title_chr <- remove_obj_type_from_nm(fns_chr,
                                  object_type_lup = object_type_lup,
                                  abbreviations_lup = abbreviations_lup,
                                  is_generic_lgl = is_generic_lgl) %>%
    stringr::str_replace_all("_"," ") %>%
    Hmisc::capitalize() %>%
    purrr::map_chr(~replace_abbr(.x,
                                     abbreviations_lup = abbreviations_lup) %>%
                     stringi::stri_replace_last_fixed(" R",""))
  return(title_chr)
}
make_fn_type_lup <- function(fn_type_nm_chr = character(0),
                                fn_type_desc_chr = character(0),
                                first_arg_desc_chr = character(0),
                                second_arg_desc_chr = character(0),
                                is_generic_lgl = logical(0),
                                is_method_lgl = logical(0)){
  fn_type_lup_tb <- tibble::tibble(fn_type_nm_chr = fn_type_nm_chr,
                  fn_type_desc_chr = fn_type_desc_chr,
                  first_arg_desc_chr = first_arg_desc_chr,
                  second_arg_desc_chr = second_arg_desc_chr,
                  is_generic_lgl = is_generic_lgl,
                  is_method_lgl = is_method_lgl) %>%
    dplyr::arrange(fn_type_nm_chr)
  return(fn_type_lup_tb)
}
make_fn_nms <- function(path_1L_chr = "data-raw"){
  fns_1L_chr_ls <- make_undmtd_fns_dir_chr(path_1L_chr) %>%
    purrr::map(~read_fns(.x)) %>%
    stats::setNames(make_fn_types())
  fns_1L_chr_ls <- fns_1L_chr_ls %>% purrr::discard(~ identical(.x,character(0)))
  return(fns_1L_chr_ls)
}
make_fn_types <- function(){
  fns_type_chr <- c("fns","gnrcs","mthds")
  return(fns_type_chr)
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
      "#' @rdname ", #added rd
      ref_slot_1L_chr,#fn_name_1L_chr,
      ifelse(fn_type_1L_chr == "gen_set_slot","_set",""), #added
      "-methods\n", #added -methods
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
                          ifelse(fn_type_1L_chr == "gen_set_slot","#' @param value Value to be assigned to x\n",""),
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
                                        ifelse(fn_type_1L_chr == "meth_set_slot","_set",""),
                                        "-methods\n") #added
                                 # ifelse(fn_type_1L_chr == "meth_get_slot",
                                 #        fn_name_1L_chr,
                                 #        paste0(stringr::str_sub(fn_name_1L_chr,end = -3),
                                 #               "-set")
                                 #        )
                          ),
                          paste0("#' @aliases ",fn_name_1L_chr,",",class_name_1L_chr,"-method")
                          # "#' @rdname ", #added rd
                          # ref_slot_1L_chr,#fn_name_1L_chr,
                          # ifelse(fn_type_1L_chr == "gen_set_slot","_set",""), #added
                          # "-methods\n", #added -methods
                          # #' @aliases crs_nbr_vec,ready4_script_data-method
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
                                  object_type_lup = NULL){
  if(is.null(abbreviations_lup))
    utils::data("abbreviations_lup",package="ready4fun",envir = environment())
  if(is.null(object_type_lup))
    utils::data("object_type_lup",package="ready4fun",envir = environment())
  fn_tags_spine_ls <- make_fn_dmt_spine(fn_name_1L_chr = fn_name_1L_chr,
                                        fn_type_1L_chr = fn_type_1L_chr,
                                        fn_title_1L_chr = fn_title_1L_chr,
                                        fn = fn,
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
                                    fn,
                                    abbreviations_lup = abbreviations_lup,
                                    object_type_lup = object_type_lup)
  fn_tags_chr <- update_fn_dmt(fn_tags_spine_ls = fn_tags_spine_ls,
                               new_tag_chr_ls = new_tag_chr_ls,
                               fn_name_1L_chr = fn_name_1L_chr,
                               fn_type_1L_chr = fn_type_1L_chr,
                               import_chr = import_chr,
                               abbreviations_lup = abbreviations_lup)
  writeLines(fn_tags_chr)
}
make_new_fn_dmt <- function(fn_type_1L_chr,
                            fn_name_1L_chr,
                            fn_desc_1L_chr = NA_character_,
                            fn_det_1L_chr = NA_character_,
                            fn_out_type_1L_chr = NA_character_,
                            args_ls = NULL,
                            fn = NULL,
                            abbreviations_lup = NULL,
                            object_type_lup = NULL){
  if(is.null(abbreviations_lup))
    utils::data("abbreviations_lup",package="ready4fun",envir = environment())
  if(is.null(object_type_lup))
    utils::data("object_type_lup",package="ready4fun",envir = environment())
  s3_class_main_1L_chr <- x_param_desc_1L_chr <- NULL
  if(!is.null(fn)){
    fn_args_chr <- get_fn_args(fn)
    fn_out_type_1L_chr <- make_ret_obj_desc(fn,
                                            abbreviations_lup = abbreviations_lup)
  }else{
    fn_args_chr <- NA_character_
  }
  if(fn_type_1L_chr == "set_class" | startsWith(fn_type_1L_chr, "s3_")){
    if(fn_type_1L_chr %in% c("set_class","s3_valid_instance"))
    short_class_desc_1L_chr <- get_from_lup_obj(abbreviations_lup,
                                                match_var_nm_1L_chr = "short_name_chr",
                                                match_value_xx = fn_name_1L_chr,
                                                target_var_nm_1L_chr = "long_name_chr",
                                                evaluate_lgl = F)
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
    short_class_desc_1L_chr <- get_from_lup_obj(abbreviations_lup,
                                                match_var_nm_1L_chr = "short_name_chr",
                                                match_value_xx = s3_class_main_1L_chr,
                                                target_var_nm_1L_chr = "long_name_chr",
                                                evaluate_lgl = F)
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
        get_from_lup_obj(abbreviations_lup,
                                    match_var_nm_1L_chr = "short_name_chr",
                                    match_value_xx = .,
                                    target_var_nm_1L_chr = "long_name_chr",
                                    evaluate_lgl = F))
    }
    if(fn_type_1L_chr == "gen_std_s3_mthd"){
      x_param_desc_1L_chr <- "An object"
    }
  }
  if(is.null(args_ls)){
    arg_desc_chr <- NULL
    if(any(!is.na(fn_args_chr)) & !is.null(object_type_lup)){
      arg_desc_chr <- make_arg_desc(fn_args_chr,
                                    abbreviations_lup = abbreviations_lup,
                                    object_type_lup = object_type_lup)
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
make_obj_lup <- function(){
  obj_tb <- tibble::tibble(short_name_chr = c("df","fn","ls","r3","r4","s3","s4","sf","tb","arr","chr","dbl","fct","int","lgl","lup","mat","rgx"),
                           long_name_chr = c("data.frame","function","list","ready4 S3", "ready4 S4", "S3", "S4", "simple features object",
                                         "tibble","array","character","double","factor","integer","logical","lookup table","matrix","regular expression"),
                           atomic_element_lgl = c(rep(F,10),rep(T,2),F,rep(T,2),rep(F,2),T),
                           r3_element_lgl = c(T,F,T,rep(F,4),rep(T,11)))
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
                               dplyr::filter(r3_element_lgl) %>%
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
                             authors_prsns,
                             #cpyr_hldr_1L_chr,
                             urls_chr){
  cpyr_hldr_1L_chr <- authors_prsns[authors_prsns %>%
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
    `Authors@R` = authors_prsns,
    License = usethis::use_gpl3_license(),#cpyr_hldr_1L_chr
    URL = paste0(urls_chr, collapse = ", "))
  return(pkg_desc_ls)
}
make_ret_obj_desc <- function(fn,
                              abbreviations_lup,
                              starts_sentence_1L_lgl = T){
  ret_obj_nm_1L_chr <- get_return_obj_nm(fn)
  if(is.na(ret_obj_nm_1L_chr)){
    ret_obj_desc_1L_chr <- "NULL"
  }else{
    obj_type_1L_chr <- get_arg_obj_type(ret_obj_nm_1L_chr,
                                        object_type_lup = abbreviations_lup)
    ret_obj_desc_1L_chr <- paste0(ret_obj_nm_1L_chr %>% make_arg_title(match_chr = obj_type_1L_chr,
                                                                       abbreviations_lup = abbreviations_lup,
                                                                       object_type_lup = abbreviations_lup) %>%
                                    add_indef_artl_to_item(abbreviations_lup = abbreviations_lup) %>%
                                    ifelse(!starts_sentence_1L_lgl,tolower(.),.),
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
                                         details_1L_chr = NA_character_,
                                         example_1L_lgl = F,
                                         export_1L_lgl = T,
                                         class_name_1L_chr = "",
                                         exclude_if_match_chr){
  assert_inp_does_not_match_terms(input_chr = fn_type_1L_chr,
                      exclude_if_match_chr = exclude_if_match_chr)
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
      stringr::str_replace("#' @title FUNCTION_TITLE\n","")
  }
  fn_tags_1L_chr <- fn_tags_1L_chr %>%
    stringr::str_replace("@title ","@name ") %>%
    stringr::str_replace("@rdname fn",paste0("@rdname ",fn_name_1L_chr))
  fn_tags_1L_chr <- paste0("#' ",
                        ifelse((startsWith(fn_type_1L_chr,"gen_")|fn_type_1L_chr %in% c("fn","meth_std_s3_mthd")|startsWith(fn_type_1L_chr,"s3_")),fn_title_1L_chr,""),
                        "\n",fn_tags_1L_chr)
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
make_undmtd_fns_dir_chr <- function(path_1L_chr = "data-raw"){
  undocumented_fns_dir_chr <- paste0(path_1L_chr,"/",make_fn_types())
  return(undocumented_fns_dir_chr)
}


