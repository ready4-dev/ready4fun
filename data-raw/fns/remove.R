remove_obj_type_from_nm <- function(nms_chr,
                                    object_type_lup = NULL,
                                    abbreviations_lup = NULL,
                                    dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9",
                                    dv_url_pfx_1L_chr = character(0),
                                    is_generic_lgl = F,
                                    key_1L_chr = NULL,
                                    server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  if(is.null(abbreviations_lup))
    abbreviations_lup <- get_rds_from_dv("abbreviations_lup",
                                         dv_ds_nm_1L_chr = dv_ds_nm_1L_chr,
                                         dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                         key_1L_chr = key_1L_chr,
                                         server_1L_chr = server_1L_chr)
  if(is.null(object_type_lup))
    object_type_lup <- get_rds_from_dv("object_type_lup",
                                       dv_ds_nm_1L_chr = dv_ds_nm_1L_chr,
                                       dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                       key_1L_chr = key_1L_chr,
                                       server_1L_chr = server_1L_chr)
  output_chr <- make_arg_type_abbr(nms_chr,
                                   abbreviations_lup = abbreviations_lup,
                                   object_type_lup = object_type_lup,
                                   dv_ds_nm_1L_chr = dv_ds_nm_1L_chr,
                                   dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                   key_1L_chr = key_1L_chr,
                                   server_1L_chr = server_1L_chr)
  suffices_chr <- output_chr %>% purrr::map2_chr(is_generic_lgl,~{
    ifelse(.x=="NO MATCH"|.y,
           "",
           .x)

  })
  names_chr <- purrr::map2_chr(nms_chr,
                               suffices_chr,
                                   ~ {
                                     name_1L_chr <- .x
                                     ifelse(purrr::map_lgl(abbreviations_lup$short_name_chr,
                                                           ~ endsWith(name_1L_chr,paste0(".",.x))) %>% any(),
                                            paste0(name_1L_chr %>% stringr::str_remove(paste0(".",abbreviations_lup$short_name_chr[purrr::map_lgl(abbreviations_lup$short_name_chr,
                                                                                                                             ~ endsWith(name_1L_chr,paste0(".",.x)))])),
                                                   " method applied to ",
                                                    abbreviations_lup$long_name_chr[purrr::map_lgl(abbreviations_lup$short_name_chr,
                                                                                                   ~ endsWith(name_1L_chr,paste0(".",.x)))],
                                                   "."),
                                            ifelse(.y=="",
                                                   .x,
                                                   stringi::stri_replace_last_fixed(.x,
                                                                                    paste0("_",.y),
                                                                                    "")))
                                   }

                                   )
  return(names_chr)

}
remove_lbls_from_df <- function(data_df){ # Adapted from: https://rdrr.io/github/dlindholm/doctoR/src/R/clear_labels.R
  unlabelled_data_df <- purrr::reduce(1:ncol(data_df),
                .init = data_df,
                ~ {
                  class(.x[[.y]]) <- setdiff(class(.x[[.y]]), 'labelled')
                  attr(.x[[.y]],"label") <- NULL
                  .x
                }
  )
  return(unlabelled_data_df)
  }
