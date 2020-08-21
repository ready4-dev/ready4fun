make_abbr_lup_tb <- function(short_name_chr_vec = NA_character_,
                             long_name_chr_vec = NA_character_,
                             no_plural_chr_vec = NA_character_,
                             custom_plural_ls = NULL,
                             overwrite_lgl = T,
                             seed_lup = NULL,
                             url_chr,
                             pkg_nm_chr){
  if(is.null(seed_lup)){
    data("object_type_lup",package="ready4fun",envir = environment())
    seed_lup <- object_type_lup
  }
  update_abbr_lup_tb(seed_lup,
                     short_name_chr_vec = short_name_chr_vec,
                     long_name_chr_vec = long_name_chr_vec,
                     no_plural_chr_vec = no_plural_chr_vec,
                     custom_plural_ls = custom_plural_ls) %>%
    write_and_doc_ds_R(db = .,
                       overwrite_lgl = overwrite_lgl,
                       db_chr = "abbreviations_lup",
                       title_chr = "Common abbreviations lookup table",
                       desc_chr = paste0("A lookup table for abbreviations commonly used in object names in the ",pkg_nm_chr,"package."),
                       format_chr = "A tibble",
                       url_chr = url_chr,
                       abbreviations_lup = .)
}
make_all_fns_dmt_tb <- function(paths_ls,
                                undocumented_fns_dir_chr,
                                custom_dmt_ls = list(details_ls = NULL,
                                                     export_ls = list(force_true_chr_vec = NA_character_,
                                                                      force_false_chr_vec = NA_character_),
                                                     args_ls_ls = NULL),
                                fn_type_lup_tb,
                                generics_lup_tb = NULL,
                                abbreviations_lup = NULL){
  # add assert - same length inputs to purrr
  if (is.null(abbreviations_lup))
    data("abbreviations_lup", package = "ready4fun",
         envir = environment())
  all_fns_dmt_tb <- purrr::pmap_dfr(list(paths_ls,
                                         undocumented_fns_dir_chr,
                                         list(fn_type_lup_tb,generics_lup_tb,generics_lup_tb) %>% purrr::discard(is.null)),
                                    ~ make_fn_dmt_tbl_tb(..1,
                                                                    fns_dir_chr = ..2,
                                                                    custom_dmt_ls = custom_dmt_ls,
                                                                    append_lgl = T,
                                                                    fn_type_lup_tb = ..3,
                                                                    abbreviations_lup = abbreviations_lup))
  return(all_fns_dmt_tb)
}
make_and_doc_fn_type_R <- function(fn_type_lup_tb = make_fn_type_lup_tb(),
                                   overwrite_lgl = T,
                                   pkg_nm_chr,
                                   url_chr = url_chr,
                                   abbreviations_lup = NULL){
  if(is.null(abbreviations_lup))
    data("abbreviations_lup",package="ready4fun",envir = environment())
  fn_type_lup_tb %>%
    write_and_doc_ds_R(overwrite_lgl = overwrite_lgl,
                       db_chr = "fn_type_lup_tb",
                       title_chr = "Function type lookup table",
                       desc_chr = paste0("A lookup table to find descriptions for different types of functions used within the ",pkg_nm_chr," package suite."),
                       format_chr = "A tibble",
                       url_chr = url_chr,
                       abbreviations_lup = abbreviations_lup)
}
make_and_doc_generics_tb_R <- function(generic_nm_chr,
                                       description_chr,
                                       overwrite_lgl = T,
                                       pkg_nm_chr,
                                       url_chr = NA_character_,
                                       abbreviations_lup = NULL){
  if (is.null(abbreviations_lup))
    data("abbreviations_lup", package = "ready4fun",
         envir = environment())
  tibble::tibble(fn_type_nm_chr = generic_nm_chr,
                 fn_type_desc_chr = description_chr,
                 first_arg_desc_chr = NA_character_,
                 second_arg_desc_chr = NA_character_,
                 is_generic_lgl = T) %>%
    dplyr::arrange(fn_type_nm_chr) %>%
    write_and_doc_ds_R(overwrite_lgl = overwrite_lgl,
                                  db_chr = "generics_lup_tb", title_chr = "Generics lookup table",
                                  desc_chr = paste0("A lookup table to find descriptions of generics exported with the ",pkg_nm_chr, " package suite."), format_chr = "A tibble",
                                  url_chr = url_chr,
                                  abbreviations_lup = abbreviations_lup)
}
make_arg_desc_chr_vec <- function(fn_args_chr_vec,
                                  object_type_lup = NULL,
                                  abbreviations_lup = NULL){
  if(is.null(abbreviations_lup))
    data("abbreviations_lup",package="ready4fun",envir = environment())
  if(is.null(object_type_lup))
    data("object_type_lup",package="ready4fun",envir = environment())
  arg_desc_chr_vec <- make_arg_type_chr_vec(fn_args_chr_vec,
                                            object_type_lup = object_type_lup,
                                            abbreviations_lup = abbreviations_lup,
                                            fn = make_arg_desc_spine_chr)
  return(arg_desc_chr_vec)
}
make_arg_desc_ls <- function(fn_nms_chr_vec,
                             abbreviations_lup = NULL,
                             object_type_lup = NULL){
  if(is.null(abbreviations_lup))
    data("abbreviations_lup",package="ready4fun",envir = environment())
  if(is.null(object_type_lup))
    data("object_type_lup",package="ready4fun",envir = environment())
  purrr::map(fn_nms_chr_vec,
             ~ {
               eval(parse(text = paste0("fn <- ",.x)))
               get_fn_args_chr_vec(fn) %>% make_arg_desc_chr_vec(abbreviations_lup = abbreviations_lup,
                                                                 object_type_lup = object_type_lup) %>%
                 stats::setNames(get_fn_args_chr_vec(fn))
             }
  )
}
make_arg_desc_spine_chr <- function(argument_nm_chr,
                                    object_type_lup = NULL,
                                    abbreviations_lup = NULL){
  if(is.null(object_type_lup))
    data("object_type_lup", package="ready4fun",envir = environment())
  if(is.null(abbreviations_lup))
    data("abbreviations_lup", package="ready4fun",envir = environment())
  if(is.na(argument_nm_chr)){
    match_chr <- character(0)
  }else{
    nchar_int <- nchar(object_type_lup$short_name_chr)
    match_chr <- object_type_lup$long_name_chr[endsWith(argument_nm_chr,
                                           paste0(ifelse(nchar(argument_nm_chr)==nchar_int,"","_"),
                                                  object_type_lup$short_name_chr))]
  }
  arg_desc_spine_chr <- ifelse(identical(match_chr,character(0)),
                               NA_character_,
                               paste0(argument_nm_chr %>% make_arg_title_chr_vec(match_chr_vec = match_chr,
                                                                                 abbreviations_lup = abbreviations_lup),
                                      " (",
                                      match_chr %>% update_first_word_case_chr() %>%
                                        add_indefartls_to_phrases_chr_vec(abbreviations_lup = abbreviations_lup,
                                                                           ignore_phrs_not_in_lup_lgl = F),
                                      ")"))
  return(arg_desc_spine_chr)
}
make_arg_title_chr_vec <- function(args_chr_vec,
                                   match_chr_vec,
                                   object_type_lup = NULL,
                                   abbreviations_lup = NULL){
  if(is.null(object_type_lup))
    data("object_type_lup",package="ready4fun",envir = environment())
  if(is.null(abbreviations_lup))
    data("abbreviations_lup",package="ready4fun",envir = environment())
  suffices_chr_vec <- match_chr_vec %>% purrr::map_chr(~{
    ifelse(.x=="NO MATCH",
           "",
           get_from_lup_obj(object_type_lup,
                            match_value_xx = .x,
                            match_var_nm_chr = "long_name_chr",
                            target_var_nm_chr = "short_name_chr",
                            evaluate_lgl = F))

  })
  title_chr_vec <- purrr::map2_chr(args_chr_vec,
                                   suffices_chr_vec,
                                   ~ ifelse(.y=="",
                                            .x,
                                            stringi::stri_replace_last_fixed(.x,
                                                                             paste0("_",.y),
                                                                             ""))) %>%
    stringr::str_replace_all("_"," ") %>%
    purrr::map_chr(~replace_abbr_chr(.x,
                                     abbreviations_lup = abbreviations_lup) %>%
                     stringi::stri_replace_last_fixed(" R","")) %>%
    Hmisc::capitalize()
  return(title_chr_vec)
}
make_arg_type_chr_vec <- function(fn_args_chr_vec,
                                  object_type_lup = NULL,
                                  abbreviations_lup = NULL,
                                  fn){
  if(is.null(object_type_lup))
    data("object_type_lup",package="ready4fun",envir = environment())
  if(is.null(abbreviations_lup))
    data("abbreviations_lup",package="ready4fun",envir = environment())
  lup_ls <- make_arg_type_lup_ls(object_type_lup)
  append_lgl <- "abbreviations_lup" %in% get_fn_args_chr_vec(fn)
  arg_desc_chr_vec <- fn_args_chr_vec %>%
    purrr::map_chr(~{
      argument_nm_chr <- .x
      arg_desc_chr <- purrr::map_chr(lup_ls,
                                     ~ {
                                       args_ls <- list(argument_nm_chr,
                                                       .x)
                                       if(append_lgl)
                                         args_ls <- append(args_ls, list(abbreviations_lup))
                                       rlang::exec(fn,!!!args_ls)
                                     }) %>%
        purrr::discard(is.na) %>%
        purrr::pluck(1)
      if(is.null(arg_desc_chr))
        arg_desc_chr <- "NO MATCH"
      arg_desc_chr
    })
  return(arg_desc_chr_vec)
}
make_arg_type_abbr_chr_vec <- function(fn_args_chr_vec,
                                       object_type_lup = NULL,
                                       abbreviations_lup = NULL){#
  if(is.null(abbreviations_lup))
    data("abbreviations_lup",package="ready4fun",envir = environment())
  if(is.null(object_type_lup))
    data("object_type_lup",package="ready4fun",envir = environment())
  arg_type_abbr_chr_vec <- make_arg_type_chr_vec(fn_args_chr_vec,
                                                 object_type_lup = object_type_lup,
                                                 fn = make_arg_type_abbr_spine_chr,
                                                 abbreviations_lup = abbreviations_lup)
  return(arg_type_abbr_chr_vec)
}
make_arg_type_abbr_spine_chr <- function(argument_nm_chr,
                                    lup_tb){
  arg_type_chr <- lup_tb$short_name_chr[endsWith(argument_nm_chr,lup_tb$short_name_chr)]
  arg_type_abbr_spine_chr <- ifelse(identical(character(0),arg_type_chr),
                         NA_character_,
                         arg_type_chr)
  return(arg_type_abbr_spine_chr)
}
make_arg_type_lup_ls <- function(object_type_lup = NULL){
  if(is.null(object_type_lup))
    data("object_type_lup",package="ready4fun",envir = environment())
  new_lup <- object_type_lup %>%
    dplyr::mutate(nchar_int = nchar(short_name_chr))
  lup_ls <- new_lup$nchar_int %>% unique() %>%
    sort(decreasing = T) %>%
    purrr::map(~dplyr::filter(new_lup,nchar_int==.x))
  return(lup_ls)
}
make_fn_desc_chr_vec <-  function(fns_chr_vec,
                                  title_chr_vec,
                                  output_chr_vec,
                                  fn_type_lup_tb = NULL,
                                  abbreviations_lup = NULL){
  if(is.null(abbreviations_lup))
    data("abbreviations_lup",package="ready4fun",envir = environment())
  fn_desc_chr_vec <- purrr::pmap_chr(list(fns_chr_vec,
                                          title_chr_vec,
                                          output_chr_vec),
                                     ~ {
                                       fn_type_chr <- stringr::str_extract(..2, '[A-Za-z]+')
                                       paste0(make_fn_desc_spine_chr_vec(fn_name_chr = ..1,
                                                                         fn_title_chr = ..2,
                                                                         fn_type_lup_tb = fn_type_lup_tb,
                                                                         abbreviations_lup = abbreviations_lup),
                                              " The function ",
                                              #..1,
                                              #"() ",
                                              ifelse(..3=="NULL",
                                                     paste0("is called for its side effects and does not return a value.",
                                                            ifelse(endsWith(..1,"_R"),
                                                                   " WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour",
                                                                   "")),
                                                     paste0("returns ",
                                                            ..3 %>% tolower() %>% add_indef_artl_to_item_chr_vec(abbreviations_lup = abbreviations_lup),".")
                                              )
                                       )
                                     }
  )
  return(fn_desc_chr_vec)
}
make_fn_desc_spine_chr_vec <- function(fn_name_chr,
                                       fn_title_chr,
                                       fn_type_lup_tb = NULL,
                                       abbreviations_lup = NULL){
  if(is.null(fn_type_lup_tb))
    data("fn_type_lup_tb", package="ready4fun", envir = environment())
  if(is.null(abbreviations_lup))
    data("abbreviations_lup",package="ready4fun",envir = environment())
  fn_args_chr_vec <- get_fn_args_chr_vec(eval(parse(text = fn_name_chr)))

  #fn_type_chr_vec <- fn_title_chr %>% stringr::word()
  pfx_matches_chr_vec <- fn_type_lup_tb$fn_type_nm_chr[purrr::map_lgl(fn_type_lup_tb$fn_type_nm_chr, ~ startsWith(fn_title_chr %>% tools::toTitleCase(),.x))]
  fn_type_chr_vec <- pfx_matches_chr_vec[nchar(pfx_matches_chr_vec) == max(nchar(pfx_matches_chr_vec))]
  text_elements_chr_vec <- names(fn_type_lup_tb)[2:4] %>%
    purrr::map_chr(~ get_from_lup_obj(fn_type_lup_tb,
                                      match_var_nm_chr = "fn_type_nm_chr",
                                      match_value_xx = fn_type_chr_vec[1],
                                      target_var_nm_chr = .x,
                                      evaluate_lgl = F))
  is_generic_lgl <- get_from_lup_obj(fn_type_lup_tb,
                                     match_var_nm_chr = "fn_type_nm_chr",
                                     match_value_xx = fn_type_chr_vec[1],
                                     target_var_nm_chr = "is_generic_lgl",
                                     evaluate_lgl = F)
  treat_as_chr <- ifelse(is_generic_lgl,
                         ifelse(purrr::map_lgl(abbreviations_lup$short_name_chr,
                                               ~ endsWith(fn_name_chr,paste0(".",.x))) %>% any(),
                                "Method",
                                "Generic"),
                         "Function")
  fn_desc_spine_chr_vec <- paste0(fn_name_chr,
                                  "() is ",
                                  add_indef_artl_to_item_chr_vec(fn_type_chr_vec[1],
                                                                   ignore_phrs_not_in_lup = F,
                                                                 abbreviations_lup = abbreviations_lup),
                                  " ",
                                  tolower(treat_as_chr),
                                  " that ",
                                  update_first_word_case_chr(text_elements_chr_vec[1]),
                                  ifelse(treat_as_chr=="Generic",
                                         "",
                                         ifelse(treat_as_chr == "Method",
                                                paste0(" This method is implemented for the ",
                                                       abbreviations_lup$long_name_chr[purrr::map_lgl(abbreviations_lup$short_name_chr,
                                                                      ~ endsWith(fn_name_chr,paste0(".",.x)))],
                                                       "."),
                                                paste0( " Specifically, this function implements an algorithm to ",
                                                        fn_name_chr %>%
                                                          remove_obj_type_from_nm_chr_vec(abbreviations_lup = abbreviations_lup) %>%
                                                          add_indefartls_to_phrases_chr_vec(abbreviations_lup = abbreviations_lup),
                                                        "."))),

                                  ifelse(ifelse(is.null(fn_args_chr_vec)|is.na(text_elements_chr_vec[2]),
                                                F,
                                                T),
                                         paste0(" Function argument ",
                                                fn_args_chr_vec[1],
                                                " specifies the ",
                                                update_first_word_case_chr(text_elements_chr_vec[2])),
                                         ""),
                                  ifelse(ifelse(is.null(fn_args_chr_vec)|is.na(text_elements_chr_vec[3]),
                                                F,
                                                length(fn_args_chr_vec>1)),
                                         paste0(" Argument ",
                                                fn_args_chr_vec[2],
                                                " provides the ",
                                                update_first_word_case_chr(text_elements_chr_vec[3])),
                                         ""))
  return(fn_desc_spine_chr_vec)
}
make_fn_dmt_tbl_tb <- function(fns_path_chr_vec,
                               fns_dir_chr,
                               pkg_nm_chr,
                               custom_dmt_ls = list(title_ls = NULL,
                                                    desc_ls = NULL,
                                                    details_ls = NULL,
                                                    export_ls = NULL,
                                                    output_ls = NULL,
                                                    example_ls = NULL,
                                                    args_ls_ls = NULL),
                               append_lgl = T,
                               fn_type_lup_tb = NULL,
                               abbreviations_lup = NULL,
                               object_type_lup = NULL){
  if(is.null(abbreviations_lup))
    data("abbreviations_lup",package="ready4fun",envir = environment())
  if(is.null(object_type_lup))
    data("object_type_lup",package="ready4fun",envir = environment())
  fn_dmt_tbl_tb <- make_fn_dmt_tbl_tpl_tb(fns_path_chr_vec,
                                          fns_dir_chr = fns_dir_chr,
                                          fn_type_lup_tb = fn_type_lup_tb,
                                          abbreviations_lup = abbreviations_lup,
                                          object_type_lup = object_type_lup)
  if(purrr::map_lgl(custom_dmt_ls,
                    ~ !is.null(.x)) %>% any()){
    args_ls <- append(custom_dmt_ls, list(append_lgl = append_lgl)) %>% purrr::discard(is.null)
    fn_dmt_tbl_tb <- rlang::exec(update_fns_dmt_tb_tb, fns_dmt_tb = fn_dmt_tbl_tb, !!!args_ls)
  }
  return(fn_dmt_tbl_tb)
}
make_fn_dmt_tbl_tpl_tb <- function(fns_path_chr_vec,
                                   fns_dir_chr,
                                   fn_type_lup_tb = NULL,
                                   abbreviations_lup = NULL,
                                   object_type_lup = NULL){
  if(is.null(abbreviations_lup))
    data("abbreviations_lup",package="ready4fun",envir = environment())
  if(is.null(object_type_lup))
    data("object_type_lup",package="ready4fun",envir = environment())
  file_pfx_chr <- fns_dir_chr %>% stringr::str_replace("data-raw/","") %>%
    switch("fns"="fn_", "s3" = "C3_","gnrcs"="grp_", "mthds"="mthd_","s4 = C4_")
  if(is.null(fn_type_lup_tb)){
    is_generic_lgl <- F
  }else{
    is_generic_lgl <- fn_type_lup_tb$is_generic_lgl[1]  # Only works if generics in separate table
  }
  fn_dmt_tbl_tb <- fns_path_chr_vec %>%
    purrr::map_dfr(~tibble::tibble(fns_chr = get_fn_nms_in_file_chr(.x),
                                   title_chr = NA_character_,
                                   desc_chr = NA_character_,
                                   details_chr = NA_character_,
                                   export_lgl = F,
                                   output_chr = NA_character_,
                                   example_lgl = F,
                                   args_ls = list(NULL),
                                   file_nm_chr = .x %>% stringr::str_replace(paste0(fns_dir_chr,"/"),""),
                                   file_pfx_chr = file_pfx_chr))
  fn_dmt_tbl_tb <- fn_dmt_tbl_tb %>%
    dplyr::mutate(title_chr = make_fn_title_chr_vec(fns_chr,
                                                    abbreviations_lup = abbreviations_lup,
                                                    is_generic_lgl = is_generic_lgl))
  fn_dmt_tbl_tb <- fn_dmt_tbl_tb %>%
    dplyr::mutate(output_chr = get_outp_obj_type_chr_vec(fns_chr))
  fn_dmt_tbl_tb <- fn_dmt_tbl_tb %>%
    dplyr::mutate(desc_chr = make_fn_desc_chr_vec(fns_chr,
                                                  title_chr_vec = title_chr,
                                                  output_chr_vec = output_chr,
                                                  fn_type_lup_tb = fn_type_lup_tb,
                                                  abbreviations_lup = abbreviations_lup))
  fn_dmt_tbl_tb <- fn_dmt_tbl_tb %>%
    dplyr::mutate(args_ls = make_arg_desc_ls(fns_chr,
                                             abbreviations_lup = abbreviations_lup,
                                             object_type_lup = object_type_lup))
  return(fn_dmt_tbl_tb)
}
make_fn_dmt_spine_chr_ls <- function(fn_name_chr,
                                     fn_type_chr,
                                     fn_title_chr = NA_character_,
                                     fn,
                                     details_chr = NA_character_,
                                     example_lgl = F,
                                     export_lgl = T,
                                     class_name_chr,
                                     doc_in_class_lgl){
  get_set_chr_vec <- c("gen_get_slot","meth_get_slot","gen_set_slot","meth_set_slot")
  if(!fn_type_chr %in% get_set_chr_vec){
    fn_dmt_spine_chr_ls <- make_std_fn_dmt_spine_chr_ls(fn_name_chr = fn_name_chr,
                                                        fn_type_chr = fn_type_chr,
                                                        fn_title_chr = fn_title_chr,
                                                        fn = fn,
                                                        details_chr = details_chr,
                                                        example_lgl = example_lgl,
                                                        export_lgl = export_lgl,
                                                        class_name_chr = class_name_chr,
                                                        exclude_if_match_chr_vec = get_set_chr_vec)
  }else{
    fn_dmt_spine_chr_ls <- make_gtr_str_dmt_spine_chr_ls(fn_type_chr = fn_type_chr,
                                                         fn_name_chr = fn_name_chr,
                                                         class_name_chr = class_name_chr,
                                                         doc_in_class_lgl = doc_in_class_lgl,
                                                         example_lgl = example_lgl)
  }
  return(fn_dmt_spine_chr_ls)
}
make_fn_title_chr_vec <- function(fns_chr_vec,
                                  object_type_lup = NULL,
                                  abbreviations_lup = NULL,
                                  is_generic_lgl = F){
  if(is.null(object_type_lup))
    data("object_type_lup",package="ready4fun",envir = environment())
  if(is.null(abbreviations_lup))
    data("abbreviations_lup",package="ready4fun",envir = environment())
  title_chr_vec <- remove_obj_type_from_nm_chr_vec(fns_chr_vec,
                                  object_type_lup = object_type_lup,
                                  abbreviations_lup = abbreviations_lup,
                                  is_generic_lgl = is_generic_lgl) %>%
    stringr::str_replace_all("_"," ") %>%
    Hmisc::capitalize() %>%
    purrr::map_chr(~replace_abbr_chr(.x,
                                     abbreviations_lup = abbreviations_lup) %>%
                     stringi::stri_replace_last_fixed(" R",""))
  return(title_chr_vec)
}

make_fn_type_lup_tb <- function(){
  fn_type_lup_tb <- tibble::tibble(fn_type_nm_chr = c("Add", "Assert", "Close", "Force",
                                                      "Get", "Import", "Make", "Read",
                                                      "Remove", "Replace", "Reset", "Rowbind",
                                                      "Transform","Unload", "Update",  "Write"),
                                   fn_type_desc_chr = c("Updates an object by adding data to that object.",
                                                        "Validates that an object conforms to required condition(s). If the object does not meet all required conditions, program execution will be stopped and an error message provided.",
                                                        "Closes specified connections.",
                                                        "Checks if a specified local or global environmental condition is met and if not, updates the specified environment to comply with the condition.",
                                                        "Retrieves a pre-existing data object from memory, local file system or online repository.",
                                                        "Reads a data object in its native format and converts it to an R object.",
                                                        "Creates a new R object.",
                                                        "Reads an R script into memory.",
                                                        "Edits an object, removing a specified element or elements.",
                                                        "Edits an object, replacing a specified element with another specified element.",
                                                        "Edits an object, overwriting the current version with a default version.",
                                                        "Performs custom rowbind operations on table objects.",
                                                        "Edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered.",
                                                        "Performs a custom detaching of a package from the search path.",
                                                        "Edits an object, while preserving core object attributes.",
                                                        "Writes a file to a specified local directory."),
                                   first_arg_desc_chr = c("Object to be updated.",
                                                          "Object on which assert validation checks are to be performed.",
                                                          NA_character_,
                                                          NA_character_,
                                                          "Where to look for the required object.",
                                                          NA_character_,
                                                          NA_character_,
                                                          "Path to object.",
                                                          "Object to be updated.",
                                                          "Object to be updated.",
                                                          NA_character_,
                                                          NA_character_,
                                                          "Object to be updated.",
                                                          "Package(s) to be detached from the search path.",
                                                          "Object to be updated.",
                                                          NA_character_),
                                   second_arg_desc_chr = c(NA_character_,
                                                           "Object containing values used for validation tests.",
                                                           NA_character_,
                                                           NA_character_,
                                                           NA_character_,
                                                           NA_character_,
                                                           NA_character_,
                                                           NA_character_,
                                                           "Object to be updated.",
                                                           "Object to be updated.",
                                                           NA_character_,
                                                           NA_character_,
                                                           "Object to be updated.",
                                                           "Package(s) to be detached from the search path.",
                                                           "Object to be updated.",
                                                           NA_character_),
                                   is_generic_lgl = F)
  return(fn_type_lup_tb)
}
make_gtr_str_dmt_spine_chr_ls <- function(fn_type_chr,
                                          fn_name_chr,
                                          class_name_chr,
                                          doc_in_class_lgl,
                                          example_lgl = F){
  if(fn_type_chr %in% c("gen_set_slot", "meth_set_slot")){
    ref_slot_chr <- stringr::str_replace(fn_name_chr,"<-","")
  }else{
    ref_slot_chr <- fn_name_chr
  }
  if(fn_type_chr %in% c("gen_get_slot", "gen_set_slot"))
    fn_tags_chr <- paste0(
      "#' FUNCTION_TITLE\n",
      "#' @description S4 Generic function to ",
      ifelse(fn_type_chr == "gen_get_slot","get","set"),
      " the value of the slot ",
      ref_slot_chr,
      "\n",
      "#' @name ",
      fn_name_chr,
      "\n",
      "#' @param x An object ",
      class_name_chr,
      "\n",
      "#' @details DETAILS\n",
      "#' @export\n"
    )
  if(fn_type_chr %in% c("meth_get_slot", "meth_set_slot")){
    fn_tags_chr <- paste0("#' ",
                          fn_name_chr,
                          "\n#' @name ",
                          fn_name_chr,
                          "-",
                          class_name_chr,
                          "\n",
                          "#' @description FUNCTION_DESCRIPTION",
                          " for S4 objects of class ",
                          class_name_chr,
                          "\n",
                          "#' @param x An object of class ",
                          class_name_chr,
                          "\n",
                          ifelse(example_lgl,
                                 paste0("#' @examples\n",
                                        "#' \\dontrun{\n",
                                        "#' if(interactive()){\n",
                                        "#'  #EXAMPLE1\n",
                                        "#'  }\n",
                                        "#' }\n"),""),
                          "#' @rdname ",
                          ifelse(doc_in_class_lgl,
                                 class_name_chr,
                                 ifelse(fn_type_chr == "meth_get_slot",fn_name_chr, paste0(stringr::str_sub(fn_name_chr,end = -3),"-set"))
                          ))
  }
  gtr_str_dmt_spine_chr_ls <- list(fn_tags_chr = fn_tags_chr,
                                   ref_slot_chr = ref_slot_chr)
  return(gtr_str_dmt_spine_chr_ls)
}
make_new_fn_dmt_chr_ls <- function(fn_type_chr,
                                   fn_name_chr,
                                   fn_desc_chr = NA_character_,
                                   fn_det_chr = NA_character_,
                                   fn_out_type_chr = NA_character_,
                                   args_ls = NULL,
                                   fn = NULL,
                                   abbreviations_lup = NULL,
                                   object_type_lup = NULL){
  if(is.null(abbreviations_lup))
    data("abbreviations_lup",package="ready4fun",envir = environment())
  if(is.null(object_type_lup))
    data("object_type_lup",package="ready4fun",envir = environment())
  s3_class_main <- NULL
  if(!is.null(fn)){
    fn_args_chr_vec <- get_fn_args_chr_vec(fn)
    fn_out_type_chr <- ifelse(is.na(fn_out_type_chr),
                              get_return_obj_nm_chr(fn) %>%
                                make_arg_desc_chr_vec(abbreviations_lup = abbreviations_lup,
                                                      object_type_lup = object_type_lup),
                              fn_out_type_chr)
  }else{
    fn_args_chr_vec <- NA_character_
  }

  if(fn_type_chr == "set_class"){
    desc_start <- "Create a new S4 object of the class:"
    output_txt <- paste0("An S4 object of the ",fn_name_chr," class")
  }
  if(fn_type_chr == "s3_valid_instance"){
    desc_start <- "Create a new valid instance of the S3 class: "
    output_txt <- paste0("A validated instance of the ",fn_name_chr," class")
  }
  if(fn_type_chr == "s3_unvalidated_instance"){
    desc_start <- "Create a new unvalidated instance of the S3 class: "
    s3_class_main <- stringr::str_replace(fn_name_chr,"new_","")
    output_txt <- paste0("An unvalidated instance of the ",s3_class_main," class")
  }
  if(fn_type_chr == "s3_prototype"){
    desc_start <- "Create a new prototype for S3 class: "
    s3_class_main <- stringr::str_replace(fn_name_chr,"make_prototype_","")
    output_txt <- paste0("A prototpe for ",s3_class_main," class")
  }
  if(fn_type_chr == "s3_validator"){
    desc_start <- "Validate an instance of the S3 class: "
    s3_class_main <- stringr::str_replace(fn_name_chr,"validate_","")
    output_txt <- paste0("A prototpe for ",s3_class_main," class")
  }
  if(fn_type_chr == "s3_checker"){
    desc_start <- "Check whether an object is a valid instance of the S3 class: "
    s3_class_main <- stringr::str_replace(fn_name_chr,"is_","")
    output_txt <- paste0("A logical value, TRUE if a valid instance of the ",s3_class_main," class")
  }
  if(fn_type_chr %in% c("gen_get_slot","meth_get_slot")){
    desc_start <- "Get the value of the slot "
    output_txt <- "A XXX ..."
  }
  if(fn_type_chr %in% c("gen_set_slot","meth_set_slot")){
    desc_start <- "Set the value of the slot "
    output_txt <- "NULL"
  }
  if(fn_type_chr %in% c("fn",
                        "gen_std_s3_mthd",
                        "meth_std_s3_mthd",
                        "gen_std_s4_mthd",
                        "meth_std_s4_mthd")){
    desc_start <- fn_desc_chr
    output_txt <- fn_out_type_chr
  }
  if(is.null(args_ls)){
    arg_desc_chr_vec <- NULL
    if(any(!is.na(fn_args_chr_vec)) & !is.null(object_type_lup)){
      arg_desc_chr_vec <- make_arg_desc_chr_vec(fn_args_chr_vec,
                                                abbreviations_lup = abbreviations_lup,
                                                object_type_lup = object_type_lup)
      if(!is.null(arg_desc_chr_vec)){
        names(arg_desc_chr_vec) <- fn_args_chr_vec
      }
    }
  }else{
    arg_desc_chr_vec <- args_ls %>% purrr::flatten_chr() %>% stats::setNames(names(args_ls))
  }
  new_fn_dmt_chr_ls <- list(desc_start = desc_start,
                            s3_class_main = s3_class_main,
                            output_txt = output_txt,
                            fn_det_chr = fn_det_chr,
                            arg_desc_chr_vec = arg_desc_chr_vec)
  return(new_fn_dmt_chr_ls)
}
make_obj_lup_tb <- function(){
  obj_tb <- tibble::tibble(short_name_chr = c("df","fn","ls","r3","r4","s3","s4","sf","tb","arr","chr","dbl","fct","int","lgl","lup","mat"),
                           long_name_chr = c("data.frame","function","list","readyforwhatsnext S3", "readyforwhatsnext S4", "S3", "S4", "simple features object",
                                         "tibble","array","character","double","factor","integer","logical","lookup table","matrix"),
                           atomic_element_lgl = c(rep(F,10),rep(T,2),F,rep(T,2),rep(F,2)),
                           r3_element_lgl = c(T,F,T,rep(F,4),rep(T,10)))
  obj_tb <- dplyr::bind_rows(obj_tb %>%
                               dplyr::mutate(long_name_chr = purrr::map2_chr(long_name_chr,atomic_element_lgl,
                                                                         ~ifelse(.y,paste0(.x," vector of length 1"),.x))),
                             obj_tb %>%
                               dplyr::filter(atomic_element_lgl) %>%
                               dplyr::mutate(short_name_chr = paste0(short_name_chr,"_vec"),
                                             long_name_chr = paste0(long_name_chr," vector")),
                             obj_tb %>%
                               dplyr::filter(r3_element_lgl) %>%
                               dplyr::mutate(short_name_chr = paste0(short_name_chr,
                                                                 purrr::map_chr(atomic_element_lgl,
                                                                                ~ ifelse(.x,"_vec","")),
                                                                 "_r3"),
                                             long_name_chr = paste0("readyforwhatsnext S3 extension of ",
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
                                                                long_name_chr = paste0("readyforwhatsnext S4 collection of ",long_name_chr))) %>%
                               dplyr::mutate(long_name_chr = purrr::map_chr(long_name_chr,
                                                                        ~ifelse(endsWith(.x,"vector of length 1"),
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
make_std_fn_dmt_spine_chr_ls <- function(fn_name_chr,
                                         fn_type_chr,
                                         fn_title_chr,
                                         fn,
                                         details_chr = NA_character_,
                                         example_lgl = F,
                                         export_lgl = T,
                                         class_name_chr = "",
                                         exclude_if_match_chr_vec){
  assert_does_not_match_terms(input_chr_vec = fn_type_chr,
                      exclude_if_match_chr_vec = exclude_if_match_chr_vec)
  if(!is.na(details_chr)){
    if(details_chr=="DETAILS")
      details_chr <- NA_character_
  }
  if(startsWith(fn_type_chr,"gen_")){
    fn_tags_chr <- sinew::makeOxygen(fn, print=FALSE, add_fields = c("export")) %>%
      stringr::str_replace("#' @return OUTPUT_DESCRIPTION\n","")
  }else{
    fn_tags_chr <- sinew::makeOxygen(fn,
                                     print=FALSE,
                                     add_fields = c(ifelse(!is.na(details_chr),"details",NA_character_),
                                                    ifelse(example_lgl,"examples",NA_character_),
                                                    "rdname",
                                                    ifelse(export_lgl,"export",NA_character_)) %>%
                                       purrr::discard(is.na)) %>%
      stringr::str_replace("#' @title FUNCTION_TITLE\n","")
  }
  fn_tags_chr <- fn_tags_chr %>%
    #stringr::str_replace("fn_name",fn_name_chr) %>%
    stringr::str_replace("@title ","@name ") %>%
    stringr::str_replace("@rdname fn",paste0("@rdname ",fn_name_chr))
  fn_tags_chr <- paste0("#' ",
                        ifelse((startsWith(fn_type_chr,"gen_")|fn_type_chr=="fn"),fn_title_chr,""),
                        "\n",fn_tags_chr)
  if(!fn_type_chr %>% startsWith("s3") & !fn_type_chr %in% c("fn",
                                                             "gen_std_s3_mthd",
                                                             "meth_std_s3_mthd",
                                                             "gen_std_s4_mthd",
                                                             "meth_std_s4_mthd"))
    fn_tags_chr <- update_fn_dmt_with_slots_chr(fn_name_chr = fn_name_chr,
                                                fn_tags_chr = fn_tags_chr)
  std_fn_dmt_spine_chr_ls <- list(fn_tags_chr = fn_tags_chr,
                                  ref_slot_chr = fn_name_chr)
  return(std_fn_dmt_spine_chr_ls)
}



