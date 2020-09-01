update_abbr_lup <- function(abbr_tb,
                               short_name_chr,
                               long_name_chr,
                               no_plural_chr = NA_character_,
                               custom_plural_ls = NULL,
                               pfx_rgx = NA_character_){
  if(!"plural_lgl" %in% names(abbr_tb))
    abbr_tb <- dplyr::mutate(abbr_tb, plural_lgl = NA)
  if(!is.na(pfx_rgx))
    abbr_tb <- abbr_tb %>%
      dplyr::mutate(long_name_chr = purrr::map_chr(long_name_chr,
                                               ~ stringi::stri_replace_first_regex(.x,pfx_rgx,"")))
  new_tb <- tibble::tibble(short_name_chr = short_name_chr,
                           long_name_chr = long_name_chr) %>%
    add_plurals_to_abbr_lup(no_plural_chr = no_plural_chr,
                               custom_plural_ls = custom_plural_ls) #%>% tidyr::drop_na()
  abbr_tb <- tibble::tibble(short_name_chr = make.unique(c(abbr_tb$short_name_chr,new_tb$short_name_chr)),
                            long_name_chr = make.unique(c(abbr_tb$long_name_chr,new_tb$long_name_chr)),
                            plural_lgl = c(abbr_tb$plural_lgl,new_tb$plural_lgl)) %>%
    dplyr::arrange(short_name_chr) %>%
    dplyr::distinct()
  return(abbr_tb)
}
update_first_word_case <- function(phrase_1L_chr,
                                       fn = tolower){
  phrase_1L_chr <- paste0(phrase_1L_chr %>% stringr::str_sub(end=1) %>% fn,
                       phrase_1L_chr %>% stringr::str_sub(start=2))
  return(phrase_1L_chr)
}
update_fn_dmt_with_slots <- function(fn_name_1L_chr,
                                         fn_dmt_1L_chr){
  slots_chr <- get_r4_obj_slots_1L_chr(fn_name_1L_chr)
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
                              import_chr){
  fn_dmt_1L_chr <- fn_tags_spine_ls$fn_tags_chr
  fn_dmt_1L_chr <- fn_dmt_1L_chr %>%
    stringr::str_replace("FUNCTION_TITLE",fn_name_1L_chr) %>%
    stringr::str_replace("FUNCTION_DESCRIPTION",
                         paste0(ifelse(is.na(new_tag_chr_ls$desc_start),
                                       "FUNCTION_DESCRIPTION",
                                       new_tag_chr_ls$desc_start),
                                ifelse(fn_type_1L_chr %in% c("fn","gen_std_s3_mthd",
                                                          "meth_std_s3_mthd",
                                                          "gen_std_s4_mthd",
                                                          "meth_std_s4_mthd"),
                                       "",
                                       fn_tags_spine_ls$ref_slot_chr))) %>%
    stringr::str_replace("OUTPUT_DESCRIPTION",new_tag_chr_ls$output_txt)
  fn_dmt_1L_chr <- fn_dmt_1L_chr %>%
    stringr::str_replace("@details DETAILS",
                         ifelse(fn_type_1L_chr == "s3_valid_instance" | ifelse(is.na(new_tag_chr_ls$fn_det_chr),
                                                                            F,
                                                                            new_tag_chr_ls$fn_det_chr!="DETAILS"),
                                paste0("@details ",new_tag_chr_ls$fn_det_chr),
                                ""))
  if(!is.null(new_tag_chr_ls$arg_desc_chr)){
    fn_dmt_1L_chr <- purrr::reduce(1:length(new_tag_chr_ls$arg_desc_chr),
                                .init = fn_dmt_1L_chr,
                                ~{
                                  stringr::str_replace(.x,
                                                       paste0("@param ",names(new_tag_chr_ls$arg_desc_chr)[.y]," PARAM_DESCRIPTION"),
                                                       paste0("@param ",names(new_tag_chr_ls$arg_desc_chr)[.y]," ",ifelse(new_tag_chr_ls$arg_desc_chr[.y]=="NO MATCH",
                                                                                                                              "PARAM_DESCRIPTION",
                                                                                                                              new_tag_chr_ls$arg_desc_chr[.y])))
                                })

  }
  if(!is.na(import_chr))
    fn_dmt_1L_chr <- paste0(fn_dmt_1L_chr,
                         "\n#' @import ",
                         stringr::str_c(import_chr,collapse = " "))
  return(fn_dmt_1L_chr)
}
update_fns_dmt_tb <- function(fns_dmt_tb,
                                 title_ls = NULL,
                                 desc_ls = NULL,
                                 details_ls = NULL,
                                 export_ls = NULL,
                                 output_ls = NULL,
                                 example_ls = NULL,
                                 args_ls_ls = NULL,
                                 append_1L_lgl = T){
  lgl_vecs_ls <- list(chr_vars_to_upd_lgl = list(title_ls,desc_ls,details_ls,output_ls) %>% purrr::map_lgl(~!is.null(.x)),
                      lgl_vars_to_upd_lgl = list(export_ls,example_ls) %>% purrr::map_lgl(~!is.null(.x)),
                      arg_ls_to_upd_lgl = !is.null(args_ls_ls))
  input_ls_ls <- list(chr_input_ls = list(variable_chr = c("title_chr","desc_chr","details_chr","output_chr"),
                                          data_chr = c("title_ls","desc_ls","details_ls","output_ls")),
                      lgl_input_ls = list(variable_chr = c("export_lgl","example_lgl"),
                                          data_chr = c("export_ls","example_ls")),
                      ls_input_ls = list(variable_chr = c("args_ls"),
                                         data_chr = c("args_ls_ls")))
  fns_dmt_tb <- purrr::reduce(1:3,
                              .init = fns_dmt_tb,
                              ~ {
                                idx_1L_dbl <- .y
                                fn <- list(update_fns_dmt_tb_chr_vars,
                                           update_fns_dmt_tb_lgl_vars,
                                           update_fns_dmt_tb_ls_vars)[[idx_1L_dbl]]
                                if(any(lgl_vecs_ls[[idx_1L_dbl]])){
                                  input_ls <- input_ls_ls[[idx_1L_dbl]] %>% purrr::map(~.x[lgl_vecs_ls[[idx_1L_dbl]]])
                                  fns_dmt_tb <- purrr::reduce(1:length(lgl_vecs_ls[[idx_1L_dbl]]),
                                                              .init = .x,
                                                              ~ {
                                                                eval(parse(text = paste0("new_ls <- ",input_ls[[2]][.y])))
                                                                args_ls <- list(.x,
                                                                                data_1L_chr = input_ls[[1]][.y],
                                                                                new_ls =  new_ls,
                                                                                append_1L_lgl = append_1L_lgl)
                                                                if(idx_1L_dbl==2)
                                                                  args_ls$append_1L_lgl <- NULL
                                                                rlang::exec(fn,
                                                                            !!!args_ls)
                                                              })

                                }
                                fns_dmt_tb
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
update_ns <- function(package_1L_chr){
  package_nm_chr <- ifelse(package_1L_chr=="",".GlobalEnv",package_1L_chr)
  return(package_nm_chr)
}
