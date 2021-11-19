remove_obj_type_from_nm <- function(nms_chr,
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
  output_chr <- make_arg_type_abbr(nms_chr,
                                   abbreviations_lup = abbreviations_lup,
                                   object_type_lup = object_type_lup,
                                   dv_ds_nm_1L_chr = dv_ds_nm_1L_chr,
                                   dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                   key_1L_chr = key_1L_chr,
                                   server_1L_chr = server_1L_chr)
  suffices_chr <- output_chr %>%
    purrr::map2_chr(is_generic_lgl,
                    ~{
                      ifelse(.x=="NO MATCH"|.y,
                             "",
                             .x)

  })
  names_chr <- purrr::map2_chr(nms_chr,
                               suffices_chr,
                                   ~ {
                                     name_1L_chr <- .x
                                     is_s3_mthd_1L_lgl <- purrr::map_lgl(abbreviations_lup$short_name_chr,
                                                                         ~ endsWith(name_1L_chr,paste0(".",.x))) %>% any()
                                     gnrc_part_1L_chr <- ifelse(is_s3_mthd_1L_lgl,
                                                                name_1L_chr %>%
                                                                  stringr::str_remove(paste0(".",
                                                                                             abbreviations_lup$short_name_chr[purrr::map_lgl(abbreviations_lup$short_name_chr,
                                                                                                                                             ~ endsWith(name_1L_chr,
                                                                                                                                                        paste0(".",
                                                                                                                                                               .x)))])),
                                                                "")
                                     ifelse(is_s3_mthd_1L_lgl,
                                            paste0(gnrc_part_1L_chr,
                                                   " - a method that ",
                                                   gnrc_part_1L_chr %>%
                                                     #Hmisc::capitalize() %>%
                                              ready4::get_from_lup_obj(fn_types_lup,
                                                                       match_var_nm_1L_chr = "fn_type_nm_chr",
                                                                       match_value_xx = .,
                                                                       target_var_nm_1L_chr = "fn_type_desc_chr",
                                                                       evaluate_1L_lgl = F) %>%
                                                tolower()),
                                            ifelse(.y=="",
                                                   .x,
                                                   stringi::stri_replace_last_fixed(.x,
                                                                                    paste0("_",.y),
                                                                                    "")))
                                   }

                                   )
  return(names_chr)

}
# remove_lbls_from_df <- function(data_df){ # Adapted from: https://rdrr.io/github/dlindholm/doctoR/src/R/clear_labels.R
#   lifecycle::deprecate_soft("0.0.0.9446", "ready4fun::remove_lbls_from_df()", "ready4::remove_lbls_from_df()")
#   unlabelled_data_df <- purrr::reduce(1:ncol(data_df),
#                 .init = data_df,
#                 ~ {
#                   class(.x[[.y]]) <- setdiff(class(.x[[.y]]), 'labelled')
#                   attr(.x[[.y]],"label") <- NULL
#                   .x
#                 }
#   )
#   return(unlabelled_data_df)
#   }
