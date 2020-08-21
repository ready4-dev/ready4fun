remove_collate_chr_vec <- function(description_chr){
  if(!identical(which(description_chr=="Collate: "),integer(0)))
    description_chr <- description_chr[1:(which(description_chr=="Collate: ")-1)]
  return(description_chr)
}
remove_obj_type_from_nm_chr_vec <- function(nms_chr_vec,
                                            object_type_lup = NULL,
                                            abbreviations_lup = NULL,
                                            is_generic_lgl_vec = F){
  if(is.null(object_type_lup))
    data("object_type_lup",package="ready4fun",envir = environment())
  if(is.null(abbreviations_lup))
    data("abbreviations_lup",package="ready4fun",envir = environment())
  output_chr_vec <- make_arg_type_abbr_chr_vec(nms_chr_vec,
                                               abbreviations_lup = abbreviations_lup,
                                               object_type_lup = object_type_lup)
  suffices_chr_vec <- output_chr_vec %>% purrr::map2_chr(is_generic_lgl_vec,~{
    ifelse(.x=="NO MATCH"|.y,
           "",
           .x)

  })
  names_chr_vec <- purrr::map2_chr(nms_chr_vec,
                                   suffices_chr_vec,
                                   ~ {
                                     name_chr <- .x
                                     ifelse(purrr::map_lgl(abbreviations_lup$short_name_chr,
                                                           ~ endsWith(name_chr,paste0(".",.x))) %>% any(),
                                            paste0(name_chr %>% stringr::str_remove(paste0(".",abbreviations_lup$short_name_chr[purrr::map_lgl(abbreviations_lup$short_name_chr,
                                                                                                                             ~ endsWith(name_chr,paste0(".",.x)))])),
                                                   " method applied to ",
                                                    abbreviations_lup$long_name_chr[purrr::map_lgl(abbreviations_lup$short_name_chr,
                                                                                                   ~ endsWith(name_chr,paste0(".",.x)))],
                                                   "."),
                                            ifelse(.y=="",
                                                   .x,
                                                   stringi::stri_replace_last_fixed(.x,
                                                                                    paste0("_",.y),
                                                                                    "")))
                                   }

                                   )
  return(names_chr_vec)

}
