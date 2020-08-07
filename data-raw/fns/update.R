update_abbr_lup_tb <- function(abbr_tb,
                               short_name_chr_vec,
                               long_name_chr_vec,
                               no_plural_chr_vec = NA_character_,
                               custom_plural_ls = NULL,
                               pfx_rgx_chr = NA_character_){
  if(!"plural_lgl" %in% names(abbr_tb))
    abbr_tb <- dplyr::mutate(abbr_tb, plural_lgl = NA)
  if(!is.na(pfx_rgx_chr))
    abbr_tb <- abbr_tb %>%
      dplyr::mutate(long_name_chr = purrr::map_chr(long_name_chr,
                                               ~ stringi::stri_replace_first_regex(.x,pfx_rgx_chr,"")))
  new_tb <- tibble::tibble(short_name_chr = short_name_chr_vec,
                           long_name_chr = long_name_chr_vec) %>%
    add_plurals_to_abbr_lup_tb(no_plural_chr_vec = no_plural_chr_vec,
                               custom_plural_ls = custom_plural_ls) %>%
    tidyr::drop_na()
  abbr_tb <- tibble::tibble(short_name_chr = make.unique(c(abbr_tb$short_name_chr,new_tb$short_name_chr)),
                            long_name_chr = make.unique(c(abbr_tb$long_name_chr,new_tb$long_name_chr)),
                            plural_lgl = c(abbr_tb$plural_lgl,new_tb$plural_lgl)) %>%
    dplyr::arrange(short_name_chr) %>%
    dplyr::distinct()
  return(abbr_tb)
}
update_fns_dmt_tb_tb <- function(fns_dmt_tb,
                                 title_ls = NULL,
                                 desc_ls = NULL,
                                 details_ls = NULL,
                                 export_ls = NULL,
                                 output_ls = NULL,
                                 example_ls = NULL,
                                 args_ls_ls = NULL,
                                 append_lgl = T){
  lgl_vecs_ls <- list(chr_vars_to_upd_lgl_vec = list(title_ls,desc_ls,details_ls,output_ls) %>% purrr::map_lgl(~!is.null(.x)),
                      lgl_vars_to_upd_lgl_vec = list(export_ls,example_ls) %>% purrr::map_lgl(~!is.null(.x)),
                      arg_ls_to_upd_lgl_vec = !is.null(args_ls_ls))
  input_ls_ls <- list(chr_input_ls = list(variable_chr_vec = c("title_chr","desc_chr","details_chr","output_chr"),
                                          data_chr_vec = c("title_ls","desc_ls","details_ls","output_ls")),
                      lgl_input_ls = list(variable_chr_vec = c("export_lgl","example_lgl"),
                                          data_chr_vec = c("export_ls","example_ls")),
                      ls_input_ls = list(variable_chr_vec = c("args_ls"),
                                         data_chr_vec = c("args_ls_ls")))
  fns_dmt_tb <- purrr::reduce(1:3,
                              .init = fns_dmt_tb,
                              ~ {
                                idx_dbl <- .y
                                fn <- list(update_fns_dmt_tb_chr_vars_chr_vec,
                                           update_fns_dmt_tb_lgl_vars_chr_vec,
                                           update_fns_dmt_tb_ls_vars_chr_vec)[[idx_dbl]]
                                if(any(lgl_vecs_ls[[idx_dbl]])){
                                  input_ls <- input_ls_ls[[idx_dbl]] %>% purrr::map(~.x[lgl_vecs_ls[[idx_dbl]]])
                                  fns_dmt_tb <- purrr::reduce(1:length(lgl_vecs_ls[[idx_dbl]]),
                                                              .init = .x,
                                                              ~ {
                                                                eval(parse(text = paste0("new_ls <- ",input_ls[[2]][.y])))
                                                                args_ls <- list(.x,
                                                                                data_chr = input_ls[[1]][.y],
                                                                                new_ls =  new_ls,
                                                                                append_lgl = append_lgl)
                                                                if(idx_dbl==2)
                                                                  args_ls$append_lgl <- NULL
                                                                rlang::exec(fn,
                                                                            !!!args_ls)
                                                              })

                                }
                                fns_dmt_tb
                              })
  return(fns_dmt_tb)
}
update_fns_dmt_tb_ls_vars_chr_vec <- function(fns_dmt_tb,
                                              data_chr,
                                              new_ls,
                                              append_lgl){
  if(is.na(data_chr)){
    fns_dmt_tb <- fns_dmt_tb
  }else{
    fns_dmt_tb <- dplyr::mutate(fns_dmt_tb,!!rlang::sym(data_chr) := dplyr::case_when(fns_chr %in% names(new_ls) ~  purrr::map2(new_ls[names(new_ls) %in% fns_chr],
                                                                                                    names(new_ls)[names(new_ls) %in% fns_chr],
                                                                                                    ~{
                                                                                                      fn_args_chr_vec <- .x
                                                                                                      fn_nm_chr <- .y
                                                                                                      old_args_chr_vec <- fns_dmt_tb$args_ls[fns_dmt_tb$fns_chr == fn_nm_chr][[1]]
                                                                                                      if(!append_lgl)
                                                                                                        testit::assert("When not appending, each function whose argument description text is being updated must have new argument descriptions for ALL arguments.",
                                                                                                                       ifelse(length(old_args_chr_vec)==length(fn_args_chr_vec),names(old_args_chr_vec) %>% sort()==names(fn_args_chr_vec) %>% sort(),F))
                                                                                                      new_args_chr_vec <- purrr::map2_chr(fn_args_chr_vec,
                                                                                                                                     names(fn_args_chr_vec),
                                                                                                                                     ~ {
                                                                                                                                       if(append_lgl){
                                                                                                                                         paste0(old_args_chr_vec[.y],". ",.x)
                                                                                                                                       }else{
                                                                                                                                         .x
                                                                                                                                       }
                                                                                                                                     })
                                                                                                      purrr::map_chr(names(old_args_chr_vec),
                                                                                                                     ~ ifelse(.x %in% names(new_args_chr_vec),
                                                                                                                              new_args_chr_vec[.x],
                                                                                                                              old_args_chr_vec[.x])) %>%
                                                                                                        stats::setNames(names(old_args_chr_vec))
                                                                                                    }),
                                                                                      TRUE ~ !!rlang::sym(data_chr))
    )
  }
  return(fns_dmt_tb)
}
update_fns_dmt_tb_lgl_vars_chr_vec <- function(fns_dmt_tb,
                                               data_chr,
                                               new_ls){
  if(is.na(data_chr)){
    fns_dmt_tb <- fns_dmt_tb
  }else{
    fns_dmt_tb <- dplyr::mutate(fns_dmt_tb,!!rlang::sym(data_chr) := dplyr::case_when(fns_chr %in% new_ls$force_true_chr_vec ~ T,
                                                                                      fns_chr %in% new_ls$force_false_chr_vec ~ F,
                                                                                      TRUE ~ !!rlang::sym(data_chr))
    )
  }
  return(fns_dmt_tb)
}
update_fns_dmt_tb_chr_vars_chr_vec <- function(fns_dmt_tb,
                                               data_chr,
                                               new_ls,
                                               append_lgl){
  if(is.na(data_chr)){
    fns_dmt_tb <- fns_dmt_tb
  }else{
    fns_dmt_tb <- dplyr::mutate(fns_dmt_tb,!!rlang::sym(data_chr) := dplyr::case_when(fns_chr %in% names(new_ls) ~ paste0(ifelse(append_lgl,
                                                                                                                                 paste0(ifelse(is.na(!!rlang::sym(data_chr)),
                                                                                                                                               "",
                                                                                                                                               !!rlang::sym(data_chr)),
                                                                                                                                        ""),
                                                                                                                                 ""),
                                                                                                                                 fns_chr %>% purrr::map_chr(~ {
                                                                                                                                   ifelse(.x %in% names(new_ls),
                                                                                                                                          new_ls[[.x]],
                                                                                                                                          NA_character_)
                                                                                                                                 }
                                                                                                                                 )
                                                                                                                          ),
                                                                                      TRUE ~ !!rlang::sym(data_chr))
    )
  }
  return(fns_dmt_tb)
}
update_fn_dmt_with_slots_chr <- function(fn_name_chr,
                                         fn_dmt_chr){
  slots_chr_vec <- get_r4_obj_slots_chr_vec(fn_name_chr)
  fn_dmt_chr <- purrr::reduce(1:length(slots_chr_vec),
                              .init = fn_dmt_chr,
                              ~ .x %>%
                                stringr::str_replace(paste0(names(slots_chr_vec)[.y], " PARAM_DESCRIPTION"),
                                                     paste0(names(slots_chr_vec)[.y]," ", slots_chr_vec[.y])))
  return(fn_dmt_chr)

}
update_ns_chr <- function(package_chr){
  package_nm_chr <- ifelse(package_chr=="",".GlobalEnv",package_chr)
  return(package_nm_chr)
}
update_fn_dmt_chr <- function(fn_tags_spine_ls,
                              new_tag_chr_ls,
                              fn_name_chr,
                              fn_type_chr,
                              #details_chr,
                              import_chr_vec){
  fn_dmt_chr <- fn_tags_spine_ls$fn_tags_chr
  fn_dmt_chr <- fn_dmt_chr %>%
    stringr::str_replace("FUNCTION_TITLE",fn_name_chr) %>%
    stringr::str_replace("FUNCTION_DESCRIPTION",paste0(ifelse(is.na(new_tag_chr_ls$desc_start),"FUNCTION_DESCRIPTION",new_tag_chr_ls$desc_start),
                                                       ifelse(fn_type_chr %in% c("fn","gen_std_s3_mthd",
                                                                                 "meth_std_s3_mthd",
                                                                                 "gen_std_s4_mthd",
                                                                                 "meth_std_s4_mthd"),"",fn_tags_spine_ls$ref_slot_chr))) %>%
    stringr::str_replace("OUTPUT_DESCRIPTION",new_tag_chr_ls$output_txt)
  fn_dmt_chr <- fn_dmt_chr %>%
    stringr::str_replace("@details DETAILS",
                         ifelse(fn_type_chr == "s3_valid_instance" | ifelse(is.na(new_tag_chr_ls$fn_det_chr),
                                                                            F,
                                                                            new_tag_chr_ls$fn_det_chr!="DETAILS"),
                                paste0("@details ",new_tag_chr_ls$fn_det_chr),
                                ""))
  if(!is.null(new_tag_chr_ls$arg_desc_chr_vec)){
    fn_dmt_chr <- purrr::reduce(1:length(new_tag_chr_ls$arg_desc_chr_vec),
                                .init = fn_dmt_chr,
                                ~{
                                  stringr::str_replace(.x,
                                                       paste0("@param ",names(new_tag_chr_ls$arg_desc_chr_vec)[.y]," PARAM_DESCRIPTION"),
                                                       paste0("@param ",names(new_tag_chr_ls$arg_desc_chr_vec)[.y]," ",ifelse(new_tag_chr_ls$arg_desc_chr_vec[.y]=="NO MATCH",
                                                                                                                              "PARAM_DESCRIPTION",
                                                                                                                              new_tag_chr_ls$arg_desc_chr_vec[.y])))
                                })

  }
  if(!is.na(import_chr_vec))
    fn_dmt_chr <- paste0(fn_dmt_chr,
                         "\n#' @import ",
                         stringr::str_c(import_chr_vec,collapse = " "))
  return(fn_dmt_chr)
}
update_first_word_case_chr <- function(phrase_chr,
                                       fn = tolower){
  phrase_chr <- paste0(phrase_chr %>% stringr::str_sub(end=1) %>% fn,
                       phrase_chr %>% stringr::str_sub(start=2))
  return(phrase_chr)
}
